use std::{io, path::{Path, PathBuf}};

use asca::RuleGroup;

use crate::cli::util;

pub struct ASCAConfig {
    pub tag: String,
    pub entries: Vec<Entry>
}

impl ASCAConfig {
    fn new() -> Self {
        Self { tag: String::new(), entries: Vec::new() }
    }
}

pub struct Entry {
    pub name: PathBuf,
    pub rules: Vec<RuleGroup>
}

impl Entry {
    pub fn from(name: PathBuf, rules: Vec<RuleGroup>) -> Self {
        Self { name, rules }
    }
}



pub fn parse_config(path: &Path) -> io::Result<Vec<ASCAConfig>> {
    let mut result: Vec<ASCAConfig> = vec![];
    let mut ac = ASCAConfig::new();

    for line in util::file_read(path)?.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#')  {
            continue;
        }

        if line.starts_with('@') {
            if !ac.entries.is_empty() {
                if ac.tag.is_empty() {
                    println!("Warning: {path:?} Entries without a tag will be ignored");
                } else {
                    result.push(ac);
                }
            }
            ac = ASCAConfig::new();

            let mut chars = line.chars();
            chars.next();
            ac.tag = chars.as_str().trim().to_string();

            continue;
        }

        let mut file_path = path.to_path_buf();
        file_path.set_file_name(line);
        file_path.set_extension("rasca");

        if file_path.is_file() {
            let entry_rules = parse_rasca_file(&file_path)?;
            ac.entries.push(Entry::from(file_path, entry_rules));
        } else {
            return Err(io::Error::other(format!("Error: Cannot find {file_path:?}")))
        }
    }
    if ac.tag.is_empty() {
        println!("Warning: {path:?} Entries without a tag will be ignored");
    } else {
        result.push(ac);
    }
    Ok(result)
}

// TODO: We can do better
pub fn parse_rasca_file(rule_file_path: &Path) -> io::Result<Vec<RuleGroup>> {
    let mut rules = Vec::new();
    let mut r = RuleGroup::new();
    for line in util::file_read(rule_file_path)?.lines() {
        let line = line.trim();
        if line.starts_with('@') {
            if !r.is_empty() {
                rules.push(r);
            }
            r = RuleGroup::new();

            let mut chars = line.chars();
            chars.next();
            r.name = chars.as_str().trim().to_string();

            continue;
        }
        if line.starts_with('#') {
            let mut chars = line.chars();
            chars.next();
            if !r.description.is_empty() {
                r.description.push('\n');
            }
            r.description += chars.as_str().trim();
            continue;
        }

        if line.is_empty() {
            if !r.is_empty() && !r.description.is_empty() {
                rules.push(r);
                r = RuleGroup::new();
            }
            continue;
        }

        if r.description.is_empty() {
            r.rule.push(line.to_string());
            continue;
        } 

        rules.push(r);
        r = RuleGroup::new();
        r.rule.push(line.to_string());
    }
    rules.push(r);
    Ok(rules)
}


pub fn parse_wasca_file(path: &Path) -> io::Result<Vec<String>> {
    Ok(util::file_read(path)?.lines().map(|s| s.to_owned()).collect::<Vec<String>>())
}