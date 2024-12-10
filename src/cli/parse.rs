use std::{fs, io, path::PathBuf};

use asca::RuleGroup;

// TODO: We can do better
pub fn parse_rasca_file(rule_file_path: PathBuf) -> io::Result<Vec<RuleGroup>> {
    let mut rules = Vec::new();
    let mut r = RuleGroup::new();
    for line in fs::read_to_string(rule_file_path)?.lines() {
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

        let line = line.trim();

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

    Ok(rules)
}


pub fn parse_wasca_file(word_file_path: PathBuf) -> io::Result<Vec<String>> {
    Ok(fs::read_to_string(word_file_path)?.lines().map(|s| s.to_owned()).collect::<Vec<String>>())
}