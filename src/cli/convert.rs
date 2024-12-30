use std::{io, path::PathBuf};

use super::{parse, util::{self, JSON_FILE_EXT, RULE_FILE_EXT, WORD_FILE_EXT}, AscaJson};

/// Convert a rsca file and a wsca file into a asca-web json file
pub fn from_asca(words: Option<PathBuf>, rules: Option<PathBuf>, output: Option<PathBuf>) -> io::Result<()> {
    let (words, _) = parse::parse_wsca(&util::validate_file_exists(words.as_deref(), &[WORD_FILE_EXT, "txt"], "word")?)?;
    let rules = parse::parse_rsca(&util::validate_file_exists(rules.as_deref(), &[RULE_FILE_EXT, "txt"], "rule")?)?;

    let json = serde_json::to_string_pretty(&(AscaJson { words, rules }))?;

    if let Some(path) = &output {
        util::write_to_file(path, json, JSON_FILE_EXT, None)
    } else {
        util::dir_create_file(&format!("out.{}", JSON_FILE_EXT), json, None)
    }
}

/// Split a asca-web json file into two separate rsca and wsca files
pub fn from_json(path: Option<PathBuf>, words_path: Option<PathBuf>, rules_path: Option<PathBuf>) -> io::Result<()> {

    let file = util::file_open(&util::validate_file_exists(path.as_deref(), &["json"], "json")?)?;
    let json: AscaJson = serde_json::from_reader(file)?;

    let words = json.words.join("\n");
    let rules = util::to_rsca_format(json.rules)?;

    if let Some(path) = words_path {
        util::write_to_file(&path, words, WORD_FILE_EXT, None)?;
    } else {
        util::dir_create_file(&format!("out.{}", WORD_FILE_EXT), words, None)?;
    }

    if let Some(path) = rules_path {
        util::write_to_file(&path, rules, RULE_FILE_EXT, None)?;
    } else {
        util::dir_create_file(&format!("out.{}", RULE_FILE_EXT), rules, None)?;
    }

    Ok(())
}