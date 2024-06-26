// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

#![deny(warnings)]

mod types;

use anyhow::anyhow;
use libcfgen::prelude::*;
use types::*;

const CONFIG_VERSION: &str = "1.0.0";
// 30GB
const DEVVM_MEM_PRESSURE_BYTES_THRESHOLD: u64 = 30 * 1024 * 1024 * 1024;

fn oomd_json(node: &Node) -> Result<json::JsonValue, anyhow::Error> {
    let attrs = get_attributes(node);
    match attrs.host_type {
        HostType::DevServer => devserver_json_config(node, &attrs),
        HostType::OnDemand => od_json_config(&attrs),
        _ => default_json_config(&attrs),
    }
}

fn oomd_dropin(_node: &Node) -> Dropin {
    let args = [
        "--interval 1",
        "--config /etc/oomd2.json",
        "--drop-in-dir /run/oomd/dropin",
    ];
    let environment = convert_args!(btreemap!(
        "OOMD_ARGS" => args.join(" "),
    ));

    Dropin {
        environment,
        ..Default::default()
    }
}

// Rules generated from this function are based on the original
// oomd_fbtax.json.erb template: https://fburl.com/code/wq92o1k9.
fn default_json_config(attrs: &ConfigParams) -> Result<json::JsonValue, anyhow::Error> {
    let mut rulesets = Vec::new();
    rulesets.push(ruleset_system_overview(attrs));
    rulesets.append(&mut rulesets_restart_cgroup_on_mem_threshold(attrs));
    // Tupperware job killing.  Monitor workload-tw.slice and kill
    // workload-tw.slice/*.  For now, we don't monitor or kill
    // workload-wdb.slice.
    rulesets.push(ruleset_protection_against_heavy_workload_thrashing(attrs));
    if attrs.fbtax2.io_latency_supported {
        // We can't set io.latency aggressive enough because it loses
        // too much total work.  Let's help io.latency if system is
        // spamming for too long.  Hopefully, we should be able to
        // remove this with io.weight.
        rulesets.push(ruleset_protection_against_wdb_io_thrashing(attrs));
    }
    if !attrs.fbtax2.disable_swap_protection {
        rulesets.push(ruleset_fbtax2_protection_against_low_swap(attrs));
    }
    if attrs.senpai.target.is_some() {
        rulesets.push(ruleset_senpai_ruleset(attrs));
    }
    rulesets.append(&mut attrs.fbtax2.oomd_extra_rulesets.clone());
    rulesets.push(ruleset_senpai_drop_in_ruleset(attrs));
    rulesets.push(ruleset_tw_container_drop_in_ruleset(attrs));

    let config = OomdConfig {
        rulesets,
        version: CONFIG_VERSION.to_string(),
    };

    serialize_oomd_config(&config)
}

// Rules generated from this function are based on the original
// oomd_devserver.json.erb template: https://fburl.com/code/3omvj54x.
fn devserver_json_config(
    node: &Node,
    attrs: &ConfigParams,
) -> Result<json::JsonValue, anyhow::Error> {
    let mut rulesets = Vec::new();
    rulesets.push(ruleset_system_overview(attrs));
    rulesets.push(ruleset_user_session_protection(node, attrs));
    if !attrs.oomd2.disable_swap_protection {
        rulesets.push(ruleset_oomd2_protection_against_low_swap(attrs));
    }
    rulesets.push(ruleset_senpai_drop_in_ruleset(attrs));
    rulesets.append(&mut rulesets_restart_cgroup_on_mem_threshold(attrs));
    rulesets.push(ruleset_tw_container_drop_in_ruleset(attrs));

    let config = OomdConfig {
        rulesets,
        version: CONFIG_VERSION.to_string(),
    };

    serialize_oomd_config(&config)
}

// Rules generated from this function are based on the original
// oomd_ondemand.json.erb template: https://fburl.com/code/9z7w4irm.
fn od_json_config(attrs: &ConfigParams) -> Result<json::JsonValue, anyhow::Error> {
    let mut rulesets = Vec::new();
    rulesets.push(ruleset_system_overview(attrs));
    rulesets.push(ruleset_protection_against_high_memory_pressure(attrs));
    rulesets.append(&mut rulesets_restart_cgroup_on_mem_threshold(attrs));
    rulesets.push(ruleset_senpai_drop_in_ruleset(attrs));
    rulesets.push(ruleset_od_protection_against_low_swap(attrs));

    let config = OomdConfig {
        rulesets,
        version: CONFIG_VERSION.to_string(),
    };

    serialize_oomd_config(&config)
}

fn serialize_oomd_config(config: &OomdConfig) -> Result<json::JsonValue, anyhow::Error> {
    let json_output_result = serde_json::to_string(config);
    let json_output = match json_output_result {
        Ok(json_str) => json_str,
        Err(err) => return Err(anyhow!(err)),
    };
    let parsed_json_result = json::parse(json_output.as_str());
    match parsed_json_result {
        Ok(json_value) => Ok(json_value),
        Err(err) => Err(anyhow!(err)),
    }
}

fn ruleset_system_overview(attrs: &ConfigParams) -> RuleSet {
    let mut rule = RuleSet {
        name: "system overview".to_string(),
        silence_logs: Some("engine".to_string()),
        drop_in: None,
        detectors: vec![detector!(
            detector_name!("records system stats"),
            detector_rule!(
              name: "dump_cgroup_overview",
              args: detector_rule_args!(
                cgroup: attrs.oomd2.oomd_target.clone()
              )
            )
        )],
        actions: vec![action!(
          name: "continue",
          args: action_args!()
        )],
        prekill_hook_timeout: None,
    };

    if attrs.host_type == HostType::OnDemand {
        rule.drop_in = Some(DropIn {
            disable_on_drop_in: None,
            detectors: Some(true),
            actions: Some(true),
        });
    }
    rule
}

fn rulesets_restart_cgroup_on_mem_threshold(attrs: &ConfigParams) -> Vec<RuleSet> {
    attrs
        .oomd2
        .oomd_restart_threshold
        .iter()
        .map(|(cgroup, params)| RuleSet {
            name: format!("restart {} on memory threshold", cgroup),
            silence_logs: None,
            drop_in: None,
            detectors: vec![detector!(
                detector_name!("memory usage above"),
                detector_rule!(
                  name: attrs.oomd2.plugins["memory_above"].as_str(),
                  args: detector_rule_args!(
                    cgroup: cgroup.clone(),
                    threshold_anon: params.threshold.clone(),
                    duration: params.duration.clone()
                  )
                )
            )],
            actions: vec![action!(
              name: "systemd_restart",
              args: action_args!(
                service: params.service_name.as_str(),
                post_action_delay: params.post_action_delay.as_str(),
                dry: "false"
              )
            )],
            prekill_hook_timeout: None,
        })
        .collect::<Vec<RuleSet>>()
}

fn ruleset_protection_against_heavy_workload_thrashing(attrs: &ConfigParams) -> RuleSet {
    let mut action = action! (
      name: "kill_by_pg_scan",
      args: action_args! (
        cgroup: "workload.slice/workload-tw.slice/*",
        recursive: "true"
      )
    );

    if attrs.fbtax2.post_workload_kill_delay.is_some() {
        action.args.post_action_delay = attrs.fbtax2.post_workload_kill_delay.clone();
    }

    RuleSet {
        name: "protection against heavy workload thrashing".to_string(),
        silence_logs: None,
        drop_in: Some(DropIn {
            disable_on_drop_in: Some(true),
            detectors: Some(true),
            actions: Some(true),
        }),
        detectors: ruleset_protection_against_heavy_workload_thrashing_detectors(attrs),
        actions: vec![action],
        prekill_hook_timeout: None,
    }
}

fn ruleset_protection_against_heavy_workload_thrashing_detectors(
    attrs: &ConfigParams,
) -> Vec<Detector> {
    if attrs.fbtax2.io_cost_supported {
        vec![detector!(
            detector_name!("sustained high workload memory pressure"),
            if !attrs.fbtax2.blacklisted_jobs.is_empty() {
                fbtax2_blacklisted_jobs_detector_rule(attrs)
            } else {
                skip_this_detector_rule!()
            },
            detector_rule!(
                name: "pressure_above",
                args: detector_rule_args!(
                  cgroup: attrs.fbtax2.workload_monitoring_slice.clone(),
                  resource: "memory".to_string(),
                  threshold: attrs.fbtax2.workload_high_pressure_threshold.clone(),
                  duration: attrs.fbtax2.workload_high_pressure_duration.clone()
                )
            ),
            detector_rule!(
              name: "memory_reclaim",
              args: detector_rule_args! (
                cgroup: attrs.fbtax2.workload_monitoring_slice.clone(),
                duration: "10".to_string()
              )
            )
        )]
    } else {
        // Note that these conditions are a lot more liberal than
        // they should really be.  This is because we can't
        // protect hostcritical's IOs from workload using
        // io.latency without risking severely impacting workload,
        // so oomd is used as an side-channel enforcement
        // mechanism with tighter configuration.
        //
        vec![
            detector!(
                detector_name!("detects fast growing memory pressure"),
                if !attrs.fbtax2.blacklisted_jobs.is_empty() {
                    fbtax2_blacklisted_jobs_detector_rule(attrs)
                } else {
                    skip_this_detector_rule!()
                },
                detector_rule!(
                    name: "pressure_above",
                    args: detector_rule_args!(
                      cgroup: attrs.fbtax2.workload_monitoring_slice.clone(),
                      resource: "memory".to_string(),
                      threshold: "80".to_string(),
                      duration: "60".to_string()
                    )
                ),
                detector_rule!(
                  name: "memory_reclaim",
                  args: detector_rule_args! (
                    cgroup: attrs.fbtax2.workload_monitoring_slice.clone(),
                    duration: "10".to_string()
                  )
                )
            ),
            detector!(
                detector_name!("detects slow growing memory pressure"),
                if !attrs.fbtax2.blacklisted_jobs.is_empty() {
                    fbtax2_blacklisted_jobs_detector_rule(attrs)
                } else {
                    skip_this_detector_rule!()
                },
                detector_rule!(
                    name: "pressure_rising_beyond",
                    args: detector_rule_args!(
                      cgroup: attrs.fbtax2.workload_monitoring_slice.clone(),
                      resource: "memory".to_string(),
                      threshold: "60".to_string(),
                      duration: "90".to_string()
                    )
                ),
                detector_rule!(
                  name: "memory_reclaim",
                  args: detector_rule_args! (
                    cgroup: attrs.fbtax2.workload_monitoring_slice.clone(),
                    duration: "10".to_string()
                  )
                )
            ),
        ]
    }
}

fn fbtax2_blacklisted_jobs_detector_rule(attrs: &ConfigParams) -> DetectorElement {
    detector_rule!(
      name: "exists",
      args: detector_rule_args! (
        cgroup: attrs.fbtax2.blacklisted_jobs.join(","),
        negate: true
      )
    )
}

fn ruleset_protection_against_wdb_io_thrashing(attrs: &ConfigParams) -> RuleSet {
    RuleSet {
        name: "protection against wdb io thrashing".to_string(),
        silence_logs: None,
        drop_in: None,
        detectors: vec![detector!(
            detector_name!("low pressure in workload and high io in wdb"),
            detector_rule!(
                name: "pressure_rising_beyond",
                args: detector_rule_args!(
                  cgroup: "workload.slice".to_string(),
                  resource: "io".to_string(),
                  threshold: if attrs.fbtax2.on_ssd {"10".to_string()} else {"15".to_string()},
                  duration: if attrs.fbtax2.on_ssd {"0".to_string()} else {"180".to_string()}
                )
            ),
            detector_rule!(
                name: "pressure_rising_beyond",
                args: detector_rule_args!(
                  cgroup: "system.slice".to_string(),
                  resource: "io".to_string(),
                  threshold: if attrs.fbtax2.on_ssd {"60".to_string()} else {"85".to_string()},
                  duration: if attrs.fbtax2.on_ssd {"0".to_string()} else {"180".to_string()}
                )
            )
        )],
        actions: vec![action!(
          name: "kill_by_pressure",
          args: action_args!(
            cgroup: "system.slice/*",
            recursive: "true",
            resource: "io"
          )
        )],
        prekill_hook_timeout: None,
    }
}

fn ruleset_fbtax2_protection_against_low_swap(attrs: &ConfigParams) -> RuleSet {
    RuleSet {
        name: "protection against low swap".to_string(),
        silence_logs: None,
        drop_in: None,
        detectors: vec![detector!(
            detector_name!(format!(
                "free swap goes below {} percent",
                attrs.fbtax2.low_swap_threshold
            )),
            if !attrs.fbtax2.blacklisted_jobs.is_empty() {
                fbtax2_blacklisted_jobs_detector_rule(attrs)
            } else {
                skip_this_detector_rule!()
            },
            detector_rule!(
              name: "swap_free",
              args: detector_rule_args!(
              threshold_pct: attrs.fbtax2.low_swap_threshold.clone()
              )
            )
        )],
        actions: vec![action!(
          name: "kill_by_swap_usage",
          args: action_args!(
            cgroup: "system.slice/*,workload.slice/workload-wdb.slice/*,workload.slice/workload-tw.slice/*",
            biased_swap_kill: "true",
            recursive: "true"
          )
        )],
        prekill_hook_timeout: None,
    }
}

fn ruleset_oomd2_protection_against_low_swap(attrs: &ConfigParams) -> RuleSet {
    RuleSet {
        name: "protection against low swap".to_string(),
        silence_logs: None,
        drop_in: None,
        detectors: vec![detector!(
            detector_name!(
                format!(
                    "free swap goes below {}%",
                    attrs.oomd2.swap_protection_detect_threshold
                )
                .as_str()
            ),
            detector_rule!(
              name: "swap_free",
              args: detector_rule_args!(
                threshold_pct: attrs.oomd2.swap_protection_detect_threshold.clone()
              )
            )
        )],
        actions: vec![action! (
          name: "kill_by_swap_usage",
          args: action_args!(
            cgroup: attrs.oomd2.kill_target.as_str(),
            threshold: attrs.oomd2.swap_protection_kill_threshold.as_str(),
            recursive: "true"
          )
        )],
        prekill_hook_timeout: None,
    }
}

fn ruleset_senpai_ruleset(attrs: &ConfigParams) -> RuleSet {
    let mut action_args = action_args!(
      io_pressure_pct: attrs.senpai.io_pressure_pct.as_str(),
      memory_high_timeout_ms: attrs.senpai.memory_high_timeout_ms.as_str(),
      scuba_logger_dataset: attrs.senpai.scuba_logger_dataset.as_str()
    );

    if attrs.senpai.limit_min_bytes.is_some() {
        action_args.limit_min_bytes = attrs.senpai.limit_min_bytes.clone();
    }

    if attrs.senpai.target.is_some() {
        action_args.cgroup = attrs.senpai.target.clone();
    }

    RuleSet {
        name: "senpai ruleset".to_string(),
        silence_logs: Some(attrs.senpai.silence_logs.clone()),
        drop_in: None,
        detectors: vec![detector!(
            detector_name!("continue detector group"),
            detector_rule!(
                name: "continue",
                args: detector_rule_args!()
            )
        )],
        actions: vec![action!(
          name: "senpai_poking",
          args: action_args
        )],
        prekill_hook_timeout: None,
    }
}

fn ruleset_senpai_drop_in_ruleset(attrs: &ConfigParams) -> RuleSet {
    // This allows tw integration to run senpai on particular tasks via
    // drop-in configs.
    RuleSet {
        name: "senpai drop-in ruleset".to_string(),
        silence_logs: Some(if attrs.host_type == HostType::OnDemand {
            "engine,plugins".to_string()
        } else {
            "engine".to_string()
        }),
        drop_in: Some(DropIn {
            disable_on_drop_in: Some(true),
            actions: Some(true),
            detectors: None,
        }),
        detectors: vec![detector!(
            if attrs.disable_senpai_dropin {
                detector_name!("stop detector group")
            } else {
                detector_name!("continue detector group")
            },
            if attrs.disable_senpai_dropin {
                // Effectively a stop plugin so that drop-ins could be
                // inserted but won't run.
                detector_rule!(
                  name: "exists",
                  args: detector_rule_args!(
                    cgroup: "/".to_string(),
                    negate: true
                  )
                )
            } else {
                detector_rule!(
                  name: "continue",
                  args: detector_rule_args!()
                )
            }
        )],
        // no-op for drop-in override
        actions: vec![action!(
          name: "continue",
          args: action_args!()
        )],
        prekill_hook_timeout: None,
    }
}

fn ruleset_tw_container_drop_in_ruleset(attrs: &ConfigParams) -> RuleSet {
    // This allows tw integration to not set memory.max on particular tasks via
    // drop-in configs.
    RuleSet {
        name: "tw_container drop-in ruleset".to_string(),
        silence_logs: None,
        drop_in: Some(DropIn {
            detectors: Some(true),
            actions: Some(true),
            disable_on_drop_in: Some(true),
        }),
        detectors: vec![detector!(
            detector_name!("continue"),
            detector_rule!(
              name: "stop",
              args: detector_rule_args!()
            )
        )],
        actions: vec![action!(
          name: "continue",
          args: action_args!()
        )],
        prekill_hook_timeout: if attrs.host_type != HostType::DevServer {
            Some("45".to_string())
        } else {
            None
        },
    }
}

fn ruleset_user_session_protection(node: &Node, attrs: &ConfigParams) -> RuleSet {
    let user_pressure_detector = detector!(
        detector_name!(
            format!(
                "user pressure above {} for 300s",
                attrs.devserver.user_mempress
            )
            .as_str()
        ),
        detector_rule!(
          name: "pressure_above",
          args: detector_rule_args!(
            cgroup: "user.slice,workload.slice,www.slice".to_string(),
            resource: "memory".to_string(),
            threshold: attrs.devserver.user_mempress.clone(),
            duration: "300".to_string()
          )
        ),
        maybe_nr_dying_descendants_rule(node),
        detector_rule!(
          name: "memory_reclaim",
          args: detector_rule_args!(
            cgroup: "user.slice,workload.slice,www.slice".to_string(),
            duration: "30".to_string()
          )
        )
    );

    let system_pressure_detector = detector!(
        detector_name!(
            format!(
                "system pressure above {} for 300s",
                attrs.devserver.system_mempress
            )
            .as_str()
        ),
        detector_rule!(
          name: "pressure_above",
          args: detector_rule_args!(
            cgroup: "system.slice".to_string(),
            resource: "memory".to_string(),
            threshold: attrs.devserver.system_mempress.clone(),
            duration: "300".to_string()
          )
        ),
        maybe_nr_dying_descendants_rule(node),
        detector_rule!(
          name: "memory_reclaim",
          args: detector_rule_args!(
            cgroup: "system.slice".to_string(),
            duration: "30".to_string()
          )
        )
    );

    RuleSet {
        name: "user session protection".to_string(),
        silence_logs: None,
        drop_in: None,
        detectors: vec![user_pressure_detector, system_pressure_detector],
        actions: vec![action!(
          name: "kill_by_memory_size_or_growth",
          args: action_args!(
            cgroup: attrs.oomd2.kill_target.as_str(),
            recursive: "true".to_string()
          )
        )],
        prekill_hook_timeout: None,
    }
}

fn maybe_nr_dying_descendants_rule(node: &Node) -> DetectorElement {
    if node.is_devserver() && node.hostname_prefix() == DEVBIG {
        // See https://fb.workplace.com/groups/linux.fbk/permalink/2924541514245339/
        detector_rule!(
          name: "nr_dying_descendants",
          args: detector_rule_args!(
            cgroup: "/".to_string(),
            count: "30000".to_string(),
            lte: "true".to_string()
          )
        )
    } else {
        skip_this_detector_rule!()
    }
}

fn ruleset_protection_against_high_memory_pressure(attrs: &ConfigParams) -> RuleSet {
    RuleSet {
        name: "protection against high memory pressure".to_string(),
        silence_logs: None,
        drop_in: Some(DropIn {
            detectors: Some(true),
            actions: Some(true),
            disable_on_drop_in: Some(attrs.oomd2.oomd_disable_on_drop_in),
        }),
        detectors: vec![
            detector!(
                detector_name!("detects fast growing memory pressure"),
                detector_rule!(
                  name: attrs.oomd2.plugins["pressure_above"].as_str(),
                  args: detector_rule_args!(
                    cgroup: attrs.oomd2.oomd_target.clone(),
                    resource: "memory".to_string(),
                    threshold: attrs.oomd2.oomd_high_threshold.clone(),
                    duration: attrs.oomd2.oomd_high_threshold_duration.clone()
                  )
                ),
                detector_rule!(
                  name: attrs.oomd2.plugins["memory_reclaim"].as_str(),
                  args: detector_rule_args!(
                    cgroup: attrs.oomd2.oomd_target.clone(),
                    duration: attrs.oomd2.oomd_reclaim_duation.clone()
                  )
                )
            ),
            detector!(
                detector_name!("detects slow growing memory pressure"),
                detector_rule!(
                  name: attrs.oomd2.plugins["pressure_rising_beyond"].as_str(),
                  args: detector_rule_args!(
                    cgroup: attrs.oomd2.oomd_target.clone(),
                    resource: "memory".to_string(),
                    threshold: attrs.oomd2.oomd_threshold.clone(),
                    duration: attrs.oomd2.oomd_threshold_duration.clone()
                  )
                ),
                detector_rule!(
                  name: attrs.oomd2.plugins["memory_reclaim"].as_str(),
                  args: detector_rule_args!(
                    cgroup: attrs.oomd2.oomd_target.clone(),
                    duration: attrs.oomd2.oomd_reclaim_duation.clone()
                  )
                )
            ),
        ],
        actions: vec![action!(
          name: attrs.oomd2.plugins["kill_by_memory_size_or_growth"].as_str(),
          args: action_args!(
            cgroup: attrs.oomd2.oomd_action_target.as_str(),
            dry: if attrs.oomd2.oomd_dry { "true" } else {"false"}
          )
        )],
        prekill_hook_timeout: None,
    }
}

fn ruleset_od_protection_against_low_swap(attrs: &ConfigParams) -> RuleSet {
    RuleSet {
        name: "protection against low swap".to_string(),
        silence_logs: None,
        drop_in: Some(DropIn {
            detectors: Some(true),
            actions: Some(true),
            disable_on_drop_in: Some(attrs.oomd2.oomd_disable_on_drop_in),
        }),
        detectors: vec![detector!(
            detector_name!("free swap goes below 5 percent"),
            detector_rule!(
              name: attrs.oomd2.plugins["swap_free"].as_str(),
              args: detector_rule_args!(
                threshold_pct: "5".to_string()
              )
            )
        )],
        actions: vec![action!(
          name: attrs.oomd2.plugins["kill_by_swap_usage"].as_str(),
          args: action_args!(
            cgroup: attrs.oomd2.oomd_action_target.as_str(),
            dry: if attrs.oomd2.oomd_dry { "true" } else {"false"}
          )
        )],
        prekill_hook_timeout: None,
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
            oomd_extra_rulesets: oomd_extra_rulesets(node),
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
            oomd_restart_threshold: oomd2_oomd_restart_threshold(node),
            oomd_reclaim_duation: String::from("10"),
            oomd_post_action_delay: String::from("15"),
            swap_protection_detect_threshold: String::from("5"),
            swap_protection_kill_threshold: String::from("5"),
        },
        devserver: DevServerAttributes {
            user_mempress: devserver_user_mempress(node),
            system_mempress: devserver_system_mempress(node),
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

fn devserver_user_mempress(node: &Node) -> String {
    if use_high_mem_pressure_threshold_on_dev(node) {
        String::from("60")
    } else {
        String::from("40")
    }
}

fn devserver_system_mempress(node: &Node) -> String {
    if use_high_mem_pressure_threshold_on_dev(node) {
        String::from("80")
    } else {
        String::from("60")
    }
}

fn use_high_mem_pressure_threshold_on_dev(node: &Node) -> bool {
    // We used to have this complicated logic to determine the dev server memory
    // pressure threshold: https://fburl.com/code/wr6lou7i.
    // Here, we simplified it down to only the few branches which actually set the
    // threshold.
    if node.hostname_prefix() == DEVGPU && node.server_type() != FbServerType::TYPE_XVII_INFERENCE {
        return true;
    }
    if node.hostname_prefix() == DEVVM
        && !is_devvm_for_ai(node)
        && node.mem_total_bytes() < DEVVM_MEM_PRESSURE_BYTES_THRESHOLD
    {
        return true;
    }
    if node.hostname_prefix() == DEV && node.server_type() == FbServerType::TYPE_X_SEARCH {
        return true;
    }
    false
}

fn is_devvm_for_ai(node: &Node) -> bool {
    node.hostname_prefix() == DEVVM
        && [
            FbServerType::TYPE_XVI_TRAINING,
            FbServerType::TYPE_XVII_INFERENCE,
            FbServerType::TYPE_XX_GPU_TC,
        ]
        .contains(&node.server_type())
}

fn oomd_extra_rulesets(node: &Node) -> Vec<RuleSet> {
    if get_host_type(node) != HostType::Dns {
        return vec![];
    }

    vec![
        RuleSet {
            name: "restart workload.slice/unbound-local-wrapper.service on memory threshold"
                .to_string(),
            silence_logs: None,
            drop_in: None,
            detectors: vec![detector!(
                detector_name!("memory usage above"),
                detector_rule!(
                  name: "memory_above",
                  args: detector_rule_args!(
                    cgroup: "workload.slice/unbound-local-wrapper.service".to_string(),
                    threshold_anon: "20%".to_string(),
                    duration: "60".to_string()
                  )
                )
            )],
            actions: vec![action!(
              name: "systemd_restart",
              args: action_args!(
                service: "unbound-local-wrapper.service",
                post_action_delay: "180"
              )
            )],
            prekill_hook_timeout: None,
        },
        RuleSet {
            name: "restart workload.slice/unbound-internet-wrapper.service on memory threshold"
                .to_string(),
            silence_logs: None,
            drop_in: None,
            detectors: vec![detector!(
                detector_name!("memory usage above"),
                detector_rule!(
                  name: "memory_above",
                  args: detector_rule_args!(
                    cgroup: "workload.slice/unbound-internet-wrapper.service".to_string(),
                    threshold_anon: "27%".to_string(),
                    duration: "60".to_string()
                  )
                )
            )],
            actions: vec![action!(
              name: "systemd_restart",
              args: action_args!(
                service: "unbound-internet-wrapper.service",
                post_action_delay: "180"
              )
            )],
            prekill_hook_timeout: None,
        },
    ]
}

fn oomd2_oomd_restart_threshold(node: &Node) -> BTreeMap<String, OomdRestartThreshold> {
    if [HostType::GEdge, HostType::Fna].contains(&get_host_type(node)) {
        btreemap! {}
    } else {
        btreemap! {
          String::from("smc_proxy.service") => OomdRestartThreshold{
            threshold: String::from("15G"),
            duration: String::from("10"),
            post_action_delay: String::from("20"),
            service_name: String::from("smc_proxy.service")}
        }
    }
}

fn on_ssd(node: &Node) -> bool {
    node.storage().has_ssd_root()
}

fn on_sandisk_sd7_sn6_s2(node: &Node) -> bool {
    node.storage().has_disk_model("SanDisk SD7SN6S256G")
}

fn io_latency_supported(node: &Node) -> bool {
    // Historically, we set this to `false` whe:
    // 1. the host has file `/sys/fs/cgroup/io.cost.qos`
    // 2. the host is not in `fbtax2_iocost_exclude` smc tier
    // The fact is that, as we have migraed and keeps migrating to newer kernel versions,
    // the file `/sys/fs/cgroup/io.cost.qos` is always present. Also, there is only one
    // host in `fbtax2_iocost_exclude` smc tier. So, we can just return true here.
    !should_setup_iocost(node)
}

fn io_cost_supported(node: &Node) -> bool {
    // Historically, we set this to `true` whe:
    // 1. the host has file `/sys/fs/cgroup/io.cost.qos`
    // 2. the host is not in `fbtax2_iocost_exclude` smc tier
    // The fact is that, as we have migraed and keeps migrating to newer kernel versions,
    // the file `/sys/fs/cgroup/io.cost.qos` is always present. Also, there is only one
    // host in `fbtax2_iocost_exclude` smc tier. So, we can just return true here.
    should_setup_iocost(node)
}

fn should_setup_iocost(node: &Node) -> bool {
    ![HostType::Synmon, HostType::Dns].contains(&get_host_type(node))
}

fn fbtax2_blacklisted_jobs(node: &Node) -> Vec<&'static str> {
    if [
        HostType::TwShared,
        HostType::Tw,
        HostType::FnEdge,
        HostType::GEdge,
        HostType::Fna,
    ]
    .contains(&get_host_type(node))
    {
        return vec![
            // This ML model has extremely high memory usage, they need to fix
            // their stuff at some point.
            "workload.slice/workload-tw.slice/sigrid_online_trainer*",
            "workload.slice/workload-tw.slice/*.reservation.slice/sigrid_online_trainer*",
            "workload.slice/workload-tw.slice/*.allotment.slice/sigrid_online_trainer*",
            // Pensieve analyzes very large JVM heapdumps. The workflow is
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
    // Replicating this logic : https://fburl.com/code/1o9lw85h
    if !on_ssd(node) || on_sandisk_sd7_sn6_s2(node) {
        return None;
    }

    match get_host_type(node) {
        HostType::TwShared | HostType::Tw | HostType::FnEdge | HostType::GEdge | HostType::Fna => {
            Some(String::from(
                "system.slice,workload.slice/workload-wdb.slice,hostcritical.slice,workload.slice/workload-wdb.slice/*,hostcritical.slice/*",
            ))
        }
        HostType::Synmon => Some(String::from("system.slice")),
        _ => None,
    }
}

fn senpai_limit_min_bytes(node: &Node) -> Option<String> {
    if [
        HostType::TwShared,
        HostType::Tw,
        HostType::Synmon,
        HostType::FnEdge,
        HostType::GEdge,
        HostType::Fna,
    ]
    .contains(&get_host_type(node))
    {
        let min_bytes = 100 * 1024 * 1024;
        return Some(min_bytes.to_string());
    }
    None
}

fn oomd2_oomd_target(node: &Node) -> String {
    match get_host_type(node) {
        HostType::DevServer => String::from("system.slice"),
        HostType::OnDemand => String::from(
            "system.slice,system.slice/system-hgcache\\x2dupdater.slice/hgcache-updater*.service",
        ),
        _ => String::from("workload.slice"),
    }
}

fn disable_senpai_dropin(node: &Node) -> bool {
    if get_host_type(node) == HostType::OnDemand {
        return true;
    }
    false
}

fn get_host_type(node: &Node) -> HostType {
    if node.hostname_prefix() == TWSHARED {
        return HostType::TwShared;
    }

    if node.hostname_prefix() == TW {
        return HostType::Tw;
    }

    if node.hostname_prefix() == OD {
        return HostType::OnDemand;
    }

    if node.hostname_prefix() == SYNMON {
        return HostType::Synmon;
    }

    if node.hostname_prefix() == DNS {
        return HostType::Dns;
    }

    if node.hostname_prefix() == FNEDGE {
        return HostType::FnEdge;
    }

    if node.hostname_prefix() == GEDGE {
        return HostType::GEdge;
    }

    if node.hostname_prefix() == FNA {
        return HostType::Fna;
    }

    if node.is_devserver() {
        return HostType::DevServer;
    }

    HostType::Default
}

fn main() -> anyhow::Result<()> {
    let mut b = libcfgen::Builder::new();
    b = b.dynamic_json(
        if libcfgen::context()?
            .is_some_and(|c| c.consumer() == libcfgen::ConfigConsumer::METALOS_WDS)
        {
            "etc/oomd2.json"
        } else {
            "oomd2.json"
        },
        oomd_json,
    );
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
