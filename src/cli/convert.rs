use std::{io, path::PathBuf};

use colored::Colorize;

use super::{parse, seq::{get_all_rules, get_config, get_orig_words, get_words}, util::{self, JSON_FILE_EXT, RULE_FILE_EXT, WORD_FILE_EXT}, AscaJson};

/// Convert a rsca file and a wsca file into an asca-web json file
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

/// Split an asca-web json file into two separate rsca and wsca files
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

/// Convert a tag within a config file to an asca-web json file
pub fn from_seq(config_dir: Option<PathBuf>, tag: String, output: Option<PathBuf>, recurse: bool) -> io::Result<()> {
    let dir_path = util::validate_directory(config_dir)?;
    let conf = get_config(&dir_path)?;

    let Some(seq) = conf.iter().find(|c| c.tag == tag) else {
        let possible_tags = conf.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
        return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), tag.yellow(), possible_tags)))
    };

    // TODO: replace with if-let chain when stable
    let json = if seq.from.is_some() && recurse {
        // get original words list and entire rule history
        let words = get_orig_words(&conf, &dir_path, seq)?;
        let rules = get_all_rules(&conf, seq)?;

        serde_json::to_string_pretty(&(AscaJson { words, rules }))?
    } else {
        let words = get_words(&conf, &dir_path, &None, seq, &mut std::collections::HashMap::new())?;

        let mut rules = vec![];
        for entry in &seq.entries {
            rules.extend_from_slice(&entry.rules);
        }

        serde_json::to_string_pretty(&(AscaJson { words, rules }))?
    };

    if let Some(path) = &output {
        util::write_to_file(path, json, JSON_FILE_EXT, None)
    } else {
        util::dir_create_file(&format!("out.{}", JSON_FILE_EXT), json, None)
    }
}