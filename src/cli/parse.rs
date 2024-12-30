use std::{io, path::Path};

use asca::RuleGroup;

use super::util;

// TODO: We can do better
pub fn parse_rsca(rule_file_path: &Path) -> io::Result<Vec<RuleGroup>> {
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


pub fn parse_wsca(path: &Path) -> io::Result<(Vec<String>, Vec<String>)> {
    Ok(util::file_read(path)?.lines().map(|line| {
        let mut line_iter = line.trim().split('#');

        let word = line_iter.next().unwrap().trim().to_owned();
        let comment = line_iter.collect::<String>().trim().to_owned();
        (word, comment)
    }).collect::<(Vec<String>, Vec<String>)>())
}