// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

#![deny(warnings)]

mod types;

use libcfgen::prelude::*;
use types::*;

const CONFIG_VERSION: &str = "1.0.0";

fn oomd_json(node: &Node) -> json::JsonValue {
    let attrs = get_attributes(node);
    match attrs.host_type {
        HostType::DevServer => devserver_json_config(node, &attrs),
        HostType::OnDemand => od_json_config(&attrs),
        _ => default_json_config(&attrs),
    }
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
        rulesets.push(rule_fbtax2_protection_against_low_swap(attrs));
    }
    if attrs.senpai.target.is_some() {
        rulesets.push(rule_senpai_ruleset(attrs));
    }
    rulesets.append(&mut attrs.fbtax2.oomd_extra_rulesets.clone());
    rulesets.push(rule_senpai_drop_in_ruleset(attrs));
    rulesets.push(rule_tw_container_drop_in_ruleset(attrs));

    json::object! {
      "rulesets": rulesets,
      "version": CONFIG_VERSION,
    }
}

fn devserver_json_config(node: &Node, attrs: &ConfigParams) -> json::JsonValue {
    let mut rulesets = json::Array::new();
    rulesets.push(rule_system_overview(attrs));
    rulesets.push(rule_user_session_protection(node, attrs));
    if !attrs.oomd2.disable_swap_protection {
        rulesets.push(rule_oomd2_protection_against_low_swap(attrs));
    }
    rulesets.push(rule_senpai_drop_in_ruleset(attrs));
    rulesets.append(&mut rules_restart_cgroup_on_mem_threshold(attrs));
    rulesets.push(rule_tw_container_drop_in_ruleset(attrs));
    json::object! {
      "rulesets": rulesets,
      "version": CONFIG_VERSION,
    }
}

fn od_json_config(attrs: &ConfigParams) -> json::JsonValue {
    let mut rulesets = json::Array::new();
    rulesets.push(rule_system_overview(attrs));
    rulesets.push(rule_protection_against_high_memory_pressure(attrs));
    rulesets.append(&mut rules_restart_cgroup_on_mem_threshold(attrs));
    rulesets.push(rule_senpai_drop_in_ruleset(attrs));
    rulesets.push(rule_od_protection_against_low_swap(attrs));
    json::object! {
      "rulesets": rulesets,
      "version": CONFIG_VERSION,
    }
}

fn rule_system_overview(attrs: &ConfigParams) -> json::JsonValue {
    let mut rule = json::object! {
        "name": "system overview",
        "silence-logs": "engine",
        "detectors": [
            [
                "records system stats",
                {
                    "name": "dump_cgroup_overview",
                    "args": {
                        "cgroup": attrs.oomd2.oomd_target.as_str(),
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
      "name": "pressure_rising_beyond",
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

fn rule_fbtax2_protection_against_low_swap(attrs: &ConfigParams) -> json::JsonValue {
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

fn rule_oomd2_protection_against_low_swap(attrs: &ConfigParams) -> json::JsonValue {
    json::object! {
      "name": "protection against low swap",
      "detectors": [
        [
          format!("free swap goes below {}%", attrs.oomd2.swap_protection_detect_threshold),
          {
            "name": "swap_free",
            "args": {
              "threshold_pct": attrs.oomd2.swap_protection_detect_threshold.as_str(),
            }
          }
        ]
      ],
      "actions": [
        {
          "name": "kill_by_swap_usage",
          "args": {
            "cgroup": attrs.oomd2.kill_target.as_str(),
            "threshold": attrs.oomd2.swap_protection_kill_threshold.as_str(),
            "recursive": true,
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
      "silence-logs": if attrs.host_type == HostType::OnDemand {"engine,plugins"} else {"engine"},
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

fn rule_tw_container_drop_in_ruleset(attrs: &ConfigParams) -> json::JsonValue {
    let mut rule = json::object! {
      "name": "tw_container drop-in ruleset",
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
    };

    if attrs.host_type != HostType::DevServer {
        rule["prekill_hook_timeout"] = json::JsonValue::String(String::from("45"));
    }

    rule
}

fn rule_user_session_protection(node: &Node, attrs: &ConfigParams) -> json::JsonValue {
    let mut user_pressure_detector = json::array! {
      format!("user pressure above {} for 300s", attrs.devserver.user_mempress),
      {
        "name": "pressure_above",
        "args": {
          "cgroup": "user.slice,workload.slice,www.slice",
          "resource": "memory",
          "threshold": attrs.devserver.user_mempress.as_str(),
          "duration": "300",
        }
      },
    };

    let mut system_pressure_detector = json::array! {
      format!("system pressure above {} for 300s", attrs.devserver.system_mempress),
      {
        "name": "pressure_above",
        "args": {
          "cgroup": "system.slice",
          "resource": "memory",
          "threshold": attrs.devserver.system_mempress.as_str(),
          "duration": "300"
        },
      }
    };

    if node.in_dynamic_smc_tier("devbig") {
        _ = user_pressure_detector.push(json::object! {
        "name": "nr_dying_descendants",
        "args": {
            "cgroup": "/",
            "count": "30000",
            "lte": "true"
        }
        });

        _ = system_pressure_detector.push(json::object! {
          "name": "nr_dying_descendants",
          "args": {
              "cgroup": "/",
              "count": "30000",
              "lte": "true"
          }
        });
    }

    _ = user_pressure_detector.push(json::object! {
        "name": "memory_reclaim",
        "args": {
            "cgroup": "user.slice,workload.slice,www.slice",
            "duration": "30"
        }
    });

    _ = system_pressure_detector.push(json::object! {
      "name": "memory_reclaim",
      "args": {
          "cgroup": "system.slice",
          "duration": "30"
      }
    });

    json::object! {
      "name": "user session protection",
      "detectors": [
        user_pressure_detector,
        system_pressure_detector,
      ],
      "actions": [
        {
          "name": "kill_by_memory_size_or_growth",
          "args": {
            "cgroup": attrs.oomd2.kill_target.as_str(),
            "recursive": true,
          }
        }
      ]
    }
}

fn rule_protection_against_high_memory_pressure(attrs: &ConfigParams) -> json::JsonValue {
    json::object! {
      "name": "protection against high memory pressure",
      "drop-in": {
        "detectors": true,
        "actions": true,
        "disable-on-drop-in": attrs.oomd2.oomd_disable_on_drop_in,
      },
      "detectors": [
        [
          "detects fast growing memory pressure",
          {
            "name": attrs.oomd2.plugins["pressure_above"].as_str(),
            "args": {
              "cgroup": attrs.oomd2.oomd_target.as_str(),
              "resource": "memory",
              "threshold": attrs.oomd2.oomd_high_threshold.as_str(),
              "duration": attrs.oomd2.oomd_high_threshold_duration.as_str(),
            }
          },
          {
            "name": attrs.oomd2.plugins["memory_reclaim"].as_str(),
            "args": {
              "cgroup": attrs.oomd2.oomd_target.as_str(),
              "duration": attrs.oomd2.oomd_reclaim_duation.as_str(),
            }
          }
        ],
        [
          "detects slow growing memory pressure",
          {
            "name": attrs.oomd2.plugins["pressure_rising_beyond"].as_str(),
            "args": {
              "cgroup": attrs.oomd2.oomd_target.as_str(),
              "resource": "memory",
              "threshold": attrs.oomd2.oomd_threshold.as_str(),
              "duration": attrs.oomd2.oomd_threshold_duration.as_str(),
            }
          },
          {
            "name": attrs.oomd2.plugins["memory_reclaim"].as_str(),
            "args": {
              "cgroup": attrs.oomd2.oomd_target.as_str(),
              "duration": attrs.oomd2.oomd_reclaim_duation.as_str(),
            }
          }
        ]
      ],
      "actions": [
        {
          "name": attrs.oomd2.plugins["kill_by_memory_size_or_growth"].as_str(),
          "args": {
            "cgroup": attrs.oomd2.oomd_action_target.as_str(),
            "dry": if attrs.oomd2.oomd_dry { "true" } else {"false"},
          }
        }
      ]
    }
}

fn rule_od_protection_against_low_swap(attrs: &ConfigParams) -> json::JsonValue {
    json::object! {
      "name": "protection against low swap",
      "drop-in": {
        "detectors": true,
        "actions": true,
        "disable-on-drop-in": attrs.oomd2.oomd_disable_on_drop_in,
      },
      "detectors": [
        [
          "free swap goes below 5 percent",
          {
            "name": attrs.oomd2.plugins["swap_free"].as_str(),
            "args": {
              "threshold_pct": "5",
            }
          }
        ]
      ],
      "actions": [
        {
          "name": attrs.oomd2.plugins["kill_by_swap_usage"].as_str(),
          "args": {
            "cgroup": attrs.oomd2.oomd_action_target.as_str(),
            "dry": if attrs.oomd2.oomd_dry { "true" } else {"false"},
          }
        }
      ]
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
            kill_target: String::from("user.slice/,system.slice/,workload.slice/,www.slice/"),
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
            oomd_disable_on_drop_in: true,
            oomd_target: oomd2_oomd_target(node),
            oomd_action_target: String::from("system.slice/*"),
            oomd_high_threshold: String::from("80"),
            oomd_high_threshold_duration: String::from("60"),
            oomd_threshold: String::from("60"),
            oomd_threshold_duration: String::from("90"),
            oomd_restart_threshold: oomd2_oomd_restart_threshold(),
            oomd_reclaim_duation: String::from("10"),
            oomd_post_action_delay: String::from("15"),
            swap_protection_detect_threshold: String::from("5"),
            swap_protection_kill_threshold: String::from("5"),
        },
        devserver: DevServerAttributes {
            // TODO(chengxiong): add overriding logic for user_mempress and system_mempress.
            // Like this: https://fburl.com/code/rjcg895c
            user_mempress: String::from("40"),
            system_mempress: String::from("60"),
        },
        senpai: SenpaiAttributes {
            silence_logs: String::from("engine"),
            target: senpai_targets(node),
            limit_min_bytes: senpai_limit_min_bytes(node),
            io_pressure_pct: String::from("1.0"),
            memory_high_timeout_ms: String::from("20"),
            scuba_logger_dataset: String::from("perfpipe_senpai_events"),
        },
        disable_senpai_dropin: disable_senpai_dropin(node),
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

fn on_ssd(node: &Node) -> bool {
    node.has_ssd_root()
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
    if should_enable_senpai(node) {
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

fn oomd2_oomd_target(node: &Node) -> String {
    match get_host_type(node) {
        HostType::DevServer => String::from("system.slice"),
        HostType::OnDemand => {
            String::from("system.slice,workload.slice/workload-tw.slice/quicksand*.service")
        }
        _ => String::from("workload.slice"),
    }
}

fn disable_senpai_dropin(node: &Node) -> bool {
    if get_host_type(node) == HostType::OnDemand {
        return true;
    }
    false
}

fn should_enable_senpai(node: &Node) -> bool {
    get_host_type(node) == HostType::TwShared && on_ssd(node)
}

fn get_host_type(node: &Node) -> HostType {
    if node.hostname_prefix() == TWSHARED {
        return HostType::TwShared;
    }

    if node.hostname_prefix() == OD {
        return HostType::OnDemand;
    }

    if node.is_devserver() {
        return HostType::DevServer;
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
    #[case::shard99("devvm3170.cln0", HostType::DevServer)]
    #[case::shard99("od2228.eag1", HostType::OnDemand)]
    fn test_get_host_type(#[case] hostname: &str, #[case] expected: HostType) {
        let node = FakeNodeBuilder::new().hostname(hostname).build();
        assert_eq!(get_host_type(&node), expected);
    }
}
