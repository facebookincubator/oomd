use std::collections::BTreeMap;

use libcfgen::prelude::json::JsonValue;

#[derive(Debug, PartialEq, Eq)]
pub enum HostType {
    Default,
    DevServer,
    OnDemand,
    TwShared,
}

pub struct ConfigParams {
    pub host_type: HostType,
    pub fbtax2: FBTax2Attributes,
    pub oomd2: Oomd2Attributes,
    pub devserver: DevServerAttributes,
    pub senpai: SenpaiAttributes,
    pub disable_senpai_dropin: bool,
}

pub struct FBTax2Attributes {
    pub blacklisted_jobs: Vec<&'static str>,
    pub on_ssd: bool,
    pub io_latency_supported: bool,
    pub io_cost_supported: bool,
    pub disable_swap_protection: bool,
    pub workload_high_pressure_threshold: String,
    pub workload_high_pressure_duration: String,
    pub workload_monitoring_slice: String,
    pub post_workload_kill_delay: Option<String>,
    pub oomd_extra_rulesets: Vec<JsonValue>,
    pub low_swap_threshold: String,
}

pub struct Oomd2Attributes {
    pub blacklisted_jobs: Vec<&'static str>,
    pub disable_swap_protection: bool,
    pub kill_target: String,
    pub plugins: BTreeMap<String, String>,
    pub oomd_dry: bool,
    pub oomd_disable_on_drop_in: bool,
    pub oomd_target: String,
    pub oomd_action_target: String,
    pub oomd_high_threshold: String,
    pub oomd_high_threshold_duration: String,
    pub oomd_threshold: String,
    pub oomd_threshold_duration: String,
    pub oomd_restart_threshold: BTreeMap<String, OomdRestartThreshold>,
    pub oomd_reclaim_duation: String,
    pub oomd_post_action_delay: String,
    pub swap_protection_detect_threshold: String,
    pub swap_protection_kill_threshold: String,
}

pub struct DevServerAttributes {
    pub user_mempress: String,
    pub system_mempress: String,
}

pub struct SenpaiAttributes {
    pub silence_logs: String,
    pub target: Option<String>,
    pub limit_min_bytes: Option<String>,
    pub io_pressure_pct: String,
    pub memory_high_timeout_ms: String,
    pub scuba_logger_dataset: String,
}

pub struct OomdRestartThreshold {
    pub threshold: String,
    pub duration: String,
    pub post_action_delay: String,
    pub service_name: String,
}
