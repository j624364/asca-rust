pub mod args;
pub mod config;
pub mod convert;
pub mod parse;
pub mod run;
pub mod seq;
pub mod util;

use asca::RuleGroup;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct AscaJson {
    #[serde(default)]
    pub into: Vec<String>,
    #[serde(default)]
    pub from: Vec<String>,
    pub words: Vec<String>,
    pub rules: Vec<RuleGroup>,
}
