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
    let mut rulesets = Vec::new();
    rulesets.push(rule_system_overview(attrs));
    rulesets.append(&mut rules_restart_cgroup_on_mem_threshold(attrs));
    // TODO(chengxiong): add more rule sections
    json::object! {
      "rulesets": rulesets,
    }
}

fn rule_system_overview(attrs: &ConfigParams) -> json::JsonValue {
    let cgroup = if [HostType::ShellServer, HostType::OnDemand].contains(&attrs.host_type) {
        attrs.oomd2.oomd_target.as_str()
    } else {
        "workload.slice"
    };

    let mut rule = json::object! {
        "name": "system_overview",
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

fn rules_restart_cgroup_on_mem_threshold(attrs: &ConfigParams) -> Vec<json::JsonValue> {
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
        .collect::<Vec<_>>()
}

fn get_attributes(node: &Node) -> ConfigParams {
    ConfigParams {
        host_type: get_host_type(node),
        fbtax2: FBTax2Attributes {
            blacklisted_jobs: Vec::new(),
            on_ssd: on_ssd(node),
            io_latency_supported: io_latency_supported(node),
            io_cost_supported: false,
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
            target: None,
            limit_min_bytes: None,
            io_pressure_pct: String::from("1.0"),
            memory_high_timeout_ms: String::from("20"),
            scuba_logger_dataset: String::from("perfpipe_senpai_events"),
        },
    }
}

fn oomd2_oomd_restart_threshold() -> BTreeMap<String, OomdRestartThreshold> {
    btreemap! {
      String::from("smc_proxy.service") => OomdRestartThreshold{
        threshold: String::from("10G"),
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
    false
    // TODO(chengxiong): add this logic https://fburl.com/code/dqdu7ves
}

fn get_host_type(_node: &Node) -> HostType {
    // TODO(chengxiong): add logic to determine host types.
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
    #[case::shard99("twshared2434.02.cco1", HostType::Default)]
    fn test_get_host_type(#[case] hostname: &str, #[case] expected: HostType) {
        let node = FakeNodeBuilder::new().hostname(hostname).build();
        assert_eq!(get_host_type(&node), expected);
    }
}
