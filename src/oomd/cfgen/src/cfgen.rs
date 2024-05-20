// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

#![deny(warnings)]

mod types;

use libcfgen::prelude::*;
use types::*;

fn oomd_json(node: &Node) -> json::JsonValue {
    let attrs = get_attributes(node);
    default_json_config(&attrs)
    // TODO(chengxiong) add other templates
}

fn oomd_dropin(node: &Node) -> Dropin {
    // TODO(chengxiong): implement this
    libcfgen::DropinBuilder::new()
        .with_recommended_heap_profiling("fb-oomd")
        .build(node)
}

fn default_json_config(attrs: &ConfigParams) -> json::JsonValue {
    let mut rulesets = json::Array::new();
    rulesets.push(rule_system_overview(attrs));
    rulesets.append(&mut rules_restart_cgroup_on_mem_threshold(attrs));
    rulesets.push(rule_protection_against_heavy_workload_thrashing(attrs));
    if attrs.fbtax2.io_latency_supported {
        rulesets.push(rule_protection_against_wdb_io_thrashing(attrs));
    }
    if !attrs.fbtax2.disable_swap_protection {
        rulesets.push(rule_protection_against_low_swap(attrs));
    }
    if attrs.senpai.target.is_some() {
        rulesets.push(rule_senpai_ruleset(attrs));
    }
    rulesets.append(&mut attrs.fbtax2.oomd_extra_rulesets.clone());
    rulesets.push(rule_senpai_drop_in_ruleset(attrs));
    rulesets.push(rule_tw_container_drop_in_ruleset());

    // TODO(chengxiong): add more rule sections
    json::object! {
      "rulesets": rulesets,
      "version": "1.0.0",
    }
}

fn rule_system_overview(attrs: &ConfigParams) -> json::JsonValue {
    let cgroup = if [HostType::ShellServer, HostType::OnDemand].contains(&attrs.host_type) {
        attrs.oomd2.oomd_target.as_str()
    } else {
        "workload.slice"
    };

    let mut rule = json::object! {
        "name": "system overview",
        "silence-logs": "engine",
        "detectors": [
            [
                "records system stats",
                {
                    "name": "dump_cgroup_overview",
                    "args": {
                        "cgroup": cgroup,
                    }
                }
            ]
        ],
        "actions": [
          {
            "name": "continue",
            "args": {},
          }
        ]
    };

    if attrs.host_type == HostType::OnDemand {
        rule["drop-in"] = json::object! {
          "detectors": true,
          "actions": true,
        };
    }

    rule
}

fn rules_restart_cgroup_on_mem_threshold(attrs: &ConfigParams) -> json::Array {
    attrs
        .oomd2
        .oomd_restart_threshold
        .iter()
        .map(|(cgroup, params)| {
            json::object! {
              "name": format!("restart {} on memory threshold", cgroup),
              "detectors":[
                [
                  "memory usage above",
                  {
                    "name": attrs.oomd2.plugins["memory_above"].as_str(),
                    "args": {
                      "cgroup": cgroup.as_str(),
                      "threshold_anon": params.threshold.as_str(),
                      "duration": params.duration.as_str(),
                    }
                  }
                ]
              ],
              "actions":[
                {
                  "name": "systemd_restart",
                  "args": {
                    "service": params.service_name.as_str(),
                    "post_action_delay": params.post_action_delay.as_str(),
                    "dry": "false",
                  }
                }
              ]
            }
        })
        .collect::<json::Array>()
}

fn rule_protection_against_heavy_workload_thrashing(attrs: &ConfigParams) -> json::JsonValue {
    let mut action = json::object! {
      "name": "kill_by_pg_scan",
      "args": {
        "cgroup": "workload.slice/workload-tw.slice/*",
        "recursive": "true",
      }
    };

    if attrs.fbtax2.post_workload_kill_delay.is_some() {
        action["args"]["post_action_delay"] =
            json::JsonValue::String(attrs.fbtax2.post_workload_kill_delay.clone().unwrap());
    }

    json::object! {
      "name": "protection against heavy workload thrashing",
      "drop-in": {
        "disable-on-drop-in": true,
        "detectors": true,
        "actions": true,
      },
      "detectors": rule_protection_against_heavy_workload_thrashing_detectors(attrs),
      "actions": [
        action,
      ]
    }
}

fn rule_protection_against_heavy_workload_thrashing_detectors(
    attrs: &ConfigParams,
) -> json::JsonValue {
    if attrs.fbtax2.io_cost_supported {
        let mut detector = json::array! {
          "sustained high workload memory pressure",
        };

        if !attrs.fbtax2.blacklisted_jobs.is_empty() {
            _ = detector.push(fbtax2_blacklisted_jobs_detector_rule(attrs));
        }

        _ = detector.push(json::object! {
          "name": "pressure_above",
          "args": {
            "cgroup": attrs.fbtax2.workload_monitoring_slice.as_str(),
            "resource": "memory",
            "threshold": attrs.fbtax2.workload_high_pressure_threshold.as_str(),
            "duration": attrs.fbtax2.workload_high_pressure_duration.as_str(),
          }
        });

        _ = detector.push(json::object! {
          "name": "memory_reclaim",
          "args": {
            "cgroup": attrs.fbtax2.workload_monitoring_slice.as_str(),
            "duration": "10",
          }
        });

        return json::array! {detector};
    }

    let mut fast_gorwing_mem_pressure_detector = json::array! {
      "detects fast growing memory pressure",
    };

    if !attrs.fbtax2.blacklisted_jobs.is_empty() {
        _ = fast_gorwing_mem_pressure_detector.push(fbtax2_blacklisted_jobs_detector_rule(attrs));
    }

    _ = fast_gorwing_mem_pressure_detector.push(json::object! {
      "name": "pressure_above",
      "args": {
        "cgroup": attrs.fbtax2.workload_monitoring_slice.as_str(),
        "resource": "memory",
        "threshold": "80",
        "duration": "60",
      }
    });

    _ = fast_gorwing_mem_pressure_detector.push(json::object! {
      "name": "memory_reclaim",
      "args": {
        "cgroup": attrs.fbtax2.workload_monitoring_slice.as_str(),
        "duration": "10",
      }
    });

    let mut slow_growing_mem_pressure_detector = json::array! {
      "detects slow growing memory pressure"
    };

    if !attrs.fbtax2.blacklisted_jobs.is_empty() {
        _ = slow_growing_mem_pressure_detector.push(fbtax2_blacklisted_jobs_detector_rule(attrs));
    }

    _ = slow_growing_mem_pressure_detector.push(json::object! {
      "name": "pressure_rising_beyong",
      "args": {
        "cgroup": attrs.fbtax2.workload_monitoring_slice.as_str(),
        "resource": "memory",
        "threshold": "60",
        "duration": "90",
      }
    });

    _ = slow_growing_mem_pressure_detector.push(json::object! {
      "name": "memory_reclaim",
      "args": {
        "cgroup": attrs.fbtax2.workload_monitoring_slice.as_str(),
        "duration": "10",
      }
    });

    json::array! {
      fast_gorwing_mem_pressure_detector,
      slow_growing_mem_pressure_detector,
    }
}

fn fbtax2_blacklisted_jobs_detector_rule(attrs: &ConfigParams) -> json::JsonValue {
    json::object! {
      "name": "exists",
      "args": {
        "cgroup": attrs.fbtax2.blacklisted_jobs.join(","),
        "negate": true,
      }
    }
}

fn rule_protection_against_wdb_io_thrashing(attrs: &ConfigParams) -> json::JsonValue {
    json::object! {
      "name": "protection against wdb io thrashing",
      "detectors": [
        [
          "low pressure in workload and high io in wdb",
          {
            "name": "pressure_rising_beyond",
            "args": {
              "cgroup": "workload.slice",
              "resource": "io",
              "threshold": if attrs.fbtax2.on_ssd {"10"} else {"15"},
              "duration": if attrs.fbtax2.on_ssd {"0"} else {"180"},
            }
          },
          {
            "name": "pressure_rising_beyond",
            "args": {
              "cgroup": "system.slice",
              "resource": "io",
              "threshold": if attrs.fbtax2.on_ssd {"60"} else {"85"},
              "duration": if attrs.fbtax2.on_ssd {"0"} else {"180"},
            }
          }
        ]
      ],
      "actions": [
        {
          "name": "kill_by_pressure",
          "args": {
            "cgroup": "system.slice/*",
            "recursive": "true",
            "resource": "io",
          }
        }
      ]
    }
}

fn rule_protection_against_low_swap(attrs: &ConfigParams) -> json::JsonValue {
    let mut detector = json::array! {
          format!("free swap goes below {} percent", attrs.fbtax2.low_swap_threshold)
    };

    if !attrs.fbtax2.blacklisted_jobs.is_empty() {
        _ = detector.push(fbtax2_blacklisted_jobs_detector_rule(attrs));
    }

    _ = detector.push(json::object! {
      "name": "swap_free",
      "args": {
        "threshold_pct": attrs.fbtax2.low_swap_threshold.as_str(),
      }
    });

    json::object! {
      "name": "protection against low swap",
      "detectors": [
        detector
      ],
      "actions": [
        {
          "name": "kill_by_swap_usage",
          "args": {
            "cgroup": "system.slice/*,workload.slice/workload-wdb.slice/*,workload.slice/workload-tw.slice/*",
            "biased_swap_kill": "true",
            "recursive": "true",
          }
        }
      ]
    }
}

fn rule_senpai_ruleset(attrs: &ConfigParams) -> json::JsonValue {
    let mut action_args = json::object! {
      "io_pressure_pct": attrs.senpai.io_pressure_pct.as_str(),
      "memory_high_timeout_ms": attrs.senpai.memory_high_timeout_ms.as_str(),
      "scuba_logger_dataset": attrs.senpai.scuba_logger_dataset.as_str(),
    };

    if attrs.senpai.limit_min_bytes.is_some() {
        action_args["limit_min_bytes"] =
            json::JsonValue::String(attrs.senpai.limit_min_bytes.clone().unwrap());
    }

    if attrs.senpai.target.is_some() {
        action_args["cgroup"] = json::JsonValue::String(attrs.senpai.target.clone().unwrap());
    }

    json::object! {
      "name": "senpai ruleset",
      "silence-logs": attrs.senpai.silence_logs.as_str(),
      "detectors": [
        [
          "continue detector group",
          {
            "name": "continue",
            "args": {}
          }
        ]
      ],
      "actions": [
        {
          "name": "senpai_poking",
          "args": action_args.clone(),
        }
      ]
    }
}

fn rule_senpai_drop_in_ruleset(attrs: &ConfigParams) -> json::JsonValue {
    json::object! {
      "name": "senpai drop-in ruleset",
      "silence-logs": "engine",
      "drop-in": {
        "actions": true,
        "disable-on-drop-in": true,
      },
      "detectors": [
        [
          if attrs.disable_senpai_dropin {
            "stop detector group"
          } else {
            "continue detector group"
          },
          if attrs.disable_senpai_dropin {
            json::object! {
              "name": "exists",
              "args": {
                "cgroup": "/",
                "negate": true,
              }
            }
          } else {
            json::object! {
              "name": "continue",
              "args": {},
            }
          }
        ]
      ],
      "actions": [
        {
          "name": "continue",
          "args": {},
        }
      ]
    }
}

fn rule_tw_container_drop_in_ruleset() -> json::JsonValue {
    json::object! {
      "name": "tw_container drop-in ruleset",
      "prekill_hook_timeout": "45",
      "drop-in": {
          "detectors": true,
          "actions": true,
          "disable-on-drop-in": true
      },
      "detectors": [
          [
              "continue",
              {
                  "name": "stop",
                  "args": {}
              }
          ]
      ],
      "actions": [
          {
              "name": "continue",
              "args": {}
          }
      ],
    }
}

fn get_attributes(node: &Node) -> ConfigParams {
    ConfigParams {
        host_type: get_host_type(node),
        fbtax2: FBTax2Attributes {
            blacklisted_jobs: fbtax2_blacklisted_jobs(node),
            on_ssd: on_ssd(node),
            io_latency_supported: io_latency_supported(node),
            io_cost_supported: io_cost_supported(node),
            disable_swap_protection: false,
            workload_high_pressure_threshold: String::from("80"),
            workload_high_pressure_duration: String::from("180"),
            workload_monitoring_slice: String::from("workload.slice/workload-tw.slice"),
            post_workload_kill_delay: None,
            oomd_extra_rulesets: Vec::new(),
            low_swap_threshold: String::from("10"),
        },
        oomd2: Oomd2Attributes {
            blacklisted_jobs: Vec::new(),
            disable_swap_protection: false,
            plugins: convert_args!(btreemap!(
              "pressure_above" => "pressure_above",
              "pressure_rising_beyond" => "pressure_rising_beyond",
              "swap_free" => "swap_free",
              "kill_by_memory_size_or_growth" => "kill_by_memory_size_or_growth",
              "kill_by_swap_usage" => "kill_by_swap_usage",
              "memory_above" => "memory_above",
              "memory_reclaim"=> "memory_reclaim",
              "senpai" => "senpai",
            )),
            oomd_dry: true,
            oomd_disable_on_drop_in: false,
            oomd_target: String::from("system.slice"),
            oomd_action_target: String::from("system.slice"),
            oomd_high_threshold: String::from("80"),
            oomd_high_threshold_duration: String::from("60"),
            oomd_threshold: String::from("60"),
            oomd_threshold_duration: String::from("90"),
            oomd_min_swap_pct: String::from("15"),
            oomd_restart_threshold: oomd2_oomd_restart_threshold(),
            oomd_reclaim_duation: String::from("10"),
            oomd_post_action_delay: String::from("15"),
        },
        devserver: DevServerAttributes {
            user_mempress: String::from("60"),
            system_mempress: String::from("80"),
        },
        senpai: SenpaiAttributes {
            silence_logs: String::from("engine"),
            target: senpai_targets(node),
            limit_min_bytes: senpai_limit_min_bytes(node),
            io_pressure_pct: String::from("1.0"),
            memory_high_timeout_ms: String::from("20"),
            scuba_logger_dataset: String::from("perfpipe_senpai_events"),
        },
        disable_senpai_dropin: false,
    }
}

fn oomd2_oomd_restart_threshold() -> BTreeMap<String, OomdRestartThreshold> {
    btreemap! {
      String::from("smc_proxy.service") => OomdRestartThreshold{
        threshold: String::from("15G"),
        duration: String::from("10"),
        post_action_delay: String::from("20"),
        service_name: String::from("smc_proxy.service")}
    }
}

fn on_ssd(_node: &Node) -> bool {
    true
    // TODO(chengxiong): add this logic https://fburl.com/code/dqdu7ves
}

fn io_latency_supported(_node: &Node) -> bool {
    // Historically, we set this to `false` whe:
    // 1. the host has file `/sys/fs/cgroup/io.cost.qos`
    // 2. the host is not in `fbtax2_iocost_exclude` smc tier
    // The fact is that, as we have migraed and keeps migrating to newer kernel versions,
    // the file `/sys/fs/cgroup/io.cost.qos` is always present. Also, there is only one
    // host in `fbtax2_iocost_exclude` smc tier. So, we can just return true here.
    false
}

fn io_cost_supported(_node: &Node) -> bool {
    // Historically, we set this to `true` whe:
    // 1. the host has file `/sys/fs/cgroup/io.cost.qos`
    // 2. the host is not in `fbtax2_iocost_exclude` smc tier
    // The fact is that, as we have migraed and keeps migrating to newer kernel versions,
    // the file `/sys/fs/cgroup/io.cost.qos` is always present. Also, there is only one
    // host in `fbtax2_iocost_exclude` smc tier. So, we can just return true here.
    true
}

fn fbtax2_blacklisted_jobs(node: &Node) -> Vec<&'static str> {
    if get_host_type(node) == HostType::TwShared {
        return vec![
            // This ML model has extremely high memory usage, they need to fix
            // their stuff at some point.
            "workload.slice/workload-tw.slice/sigrid_online_trainer*",
            "workload.slice/workload-tw.slice/*.reservation.slice/sigrid_online_trainer*",
            "workload.slice/workload-tw.slice/*.allotment.slice/sigrid_online_trainer*",
            //Pensieve analyzes very large JVM heapdumps. The workflow is
            // memory intensive (includes tmpfs operation) and tries to use as
            // much memory as available in order to increase the processing
            // throughput.
            "workload.slice/workload-tw.slice/analyzer*",
            "workload.slice/workload-tw.slice/*.reservation.slice/analyzer*",
            "workload.slice/workload-tw.slice/*.allotment.slice/analyzer*",
            // Tangram/bumblebee handles it's own sub cgroups. It is ok with
            // memory pressure and has it's own drop-in config for long thrashing
            // cgroups.
            "workload.slice/workload-tw.slice/bumblebee.*",
            "workload.slice/workload-tw.slice/*.reservation.slice/bumblebee.*",
            "workload.slice/workload-tw.slice/*.allotment.slice/bumblebee.*",
        ];
    }
    vec![]
}

fn senpai_targets(node: &Node) -> Option<String> {
    if get_host_type(node) == HostType::TwShared {
        return Some(String::from(
            "system.slice,workload.slice/workload-wdb.slice,hostcritical.slice,workload.slice/workload-wdb.slice/*,hostcritical.slice/*",
        ));
    }
    None
}

fn senpai_limit_min_bytes(node: &Node) -> Option<String> {
    if get_host_type(node) == HostType::TwShared {
        let min_bytes = 100 * 1024 * 1024;
        return Some(min_bytes.to_string());
    }
    None
}

fn get_host_type(node: &Node) -> HostType {
    // TODO(chengxiong): add logic to determine host types.
    if node.hostname_prefix() == "twshared".into() {
        return HostType::TwShared;
    }
    HostType::Default
}

fn main() -> anyhow::Result<()> {
    let mut b = libcfgen::Builder::new();
    b = b.dynamic_json("oomd2.json", |node| Ok(oomd_json(node)));
    b = b.dropin(|node| Ok(oomd_dropin(node)));
    b.run()
}

#[cfg(test)]
mod tests {
    use libcfgentest::*;
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case::shard99("twshared2434.02.cco1", HostType::TwShared)]
    fn test_get_host_type(#[case] hostname: &str, #[case] expected: HostType) {
        let node = FakeNodeBuilder::new().hostname(hostname).build();
        assert_eq!(get_host_type(&node), expected);
    }
}
