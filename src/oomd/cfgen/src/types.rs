use std::collections::BTreeMap;

use serde::Serialize;
use serde::Serializer;

pub const DUMMY_DETECTOR_RULE_NAME: &str = "god_please_ignore_this_detector_rule_please";

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
    pub oomd_extra_rulesets: Vec<RuleSet>,
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

#[derive(Serialize)]
pub struct OomdConfig {
    pub rulesets: Vec<RuleSet>,
    pub version: String,
}

#[derive(Clone, Serialize)]
pub struct RuleSet {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none", rename = "silence-logs")]
    pub silence_logs: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", rename = "drop-in")]
    pub drop_in: Option<DropIn>, // need logic to deserialize with name "drop-in"
    pub detectors: Vec<Detector>,
    pub actions: Vec<Action>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prekill_hook_timeout: Option<String>,
}

pub type Detector = Vec<DetectorElement>;

#[derive(Clone)]
pub enum DetectorElement {
    String(String),
    Rule(DetectorRule),
}

impl Serialize for DetectorElement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            DetectorElement::String(ref val) => serializer.serialize_str(val),
            DetectorElement::Rule(ref val) => val.serialize(serializer),
        }
    }
}

#[derive(Clone, Serialize)]
pub struct DetectorRule {
    pub name: String,
    pub args: DetectorRuleArgs,
}

#[derive(Clone, Serialize)]
pub struct DetectorRuleArgs {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cgroup: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lte: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub threshold_anon: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resource: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub threshold: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub negate: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub threshold_pct: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub count: Option<String>,
}

#[derive(Clone, Serialize)]
pub struct DropIn {
    #[serde(skip_serializing_if = "Option::is_none", rename = "disable-on-drop-in")]
    pub disable_on_drop_in: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detectors: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub actions: Option<bool>,
}

#[derive(Clone, Serialize)]
pub struct Action {
    pub name: String,
    pub args: ActionArgs,
}

#[derive(Clone, Serialize)]
pub struct ActionArgs {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub biased_swap_kill: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cgroup: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dry: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub post_action_delay: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recursive: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resource: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub service: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub threshold: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub io_pressure_pct: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory_high_timeout_ms: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub scuba_logger_dataset: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub limit_min_bytes: Option<String>,
}

#[macro_export]
macro_rules! detector {
    ($($val:expr),*) => {{
        let mut vec = Vec::new();
        $(
            match $val {
                DetectorElement::String(_) => {
                    vec.push(DetectorElement::from($val))
                }
                DetectorElement::Rule(e) => {
                    if e.name != types::DUMMY_DETECTOR_RULE_NAME {
                        vec.push(DetectorElement::from($val));
                    }
                }
            }
        )*
        vec
    }};
}

#[macro_export]
macro_rules! detector_name {
    ($val:expr) => {
        DetectorElement::String($val.to_string())
    };
}

#[macro_export]
macro_rules! detector_rule {
    ($name:ident : $name_val:expr, $args:ident : $args_val:expr) => {
        DetectorElement::Rule(DetectorRule {
            $name: $name_val.to_string(),
            $args: $args_val,
        })
    };
}

#[macro_export]
macro_rules! skip_this_detector_rule {
    () => {
        DetectorElement::Rule(DetectorRule {
            name: DUMMY_DETECTOR_RULE_NAME.to_string(),
            args: detector_rule_args!(),
        })
    };
}

#[macro_export]
macro_rules! detector_rule_args {
    ($($name:ident : $val:expr),*) => {{
        #[allow(unused_mut)]
        let mut ret = DetectorRuleArgs{
            cgroup: None,
            duration: None,
            lte: None,
            threshold_anon: None,
            resource: None,
            threshold: None,
            negate: None,
            threshold_pct: None,
            count: None,
        };

        $(
            ret.$name = Some($val);
        )*

        ret
    }};
}

#[macro_export]
macro_rules! action {
    ($name:ident : $name_val:expr, $args:ident : $args_val:expr) => {
        Action {
            $name: $name_val.to_string(),
            $args: $args_val,
        }
    };
}

#[macro_export]
macro_rules! action_args {
    ($($name:ident : $val:expr),*) => {{
        #[allow(unused_mut)]
        let mut ret = ActionArgs{
            biased_swap_kill: None,
            cgroup: None,
            dry: None,
            post_action_delay: None,
            recursive: None,
            resource: None,
            service: None,
            threshold: None,
            io_pressure_pct: None,
            memory_high_timeout_ms: None,
            scuba_logger_dataset: None,
            limit_min_bytes: None,
        };

        $(
            ret.$name = Some($val.to_string());
        )*

        ret
    }};
}
