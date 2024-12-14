use std::{collections::HashSet, io, path::Path, str::Chars};

use asca::RuleGroup;
use crate::cli::seq::Entry;

use super::{seq::{ASCAConfig, RuleFilter}, util::{self, RULE_FILE_EXT}};


pub fn parse_config(path: &Path) -> io::Result<Vec<ASCAConfig>> {
    let mut result: Vec<ASCAConfig> = vec![];
    let mut ac = ASCAConfig::new();
    let mut tag_map = HashSet::new();

    for (line_num, line) in util::file_read(path)?.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#')  {
            continue;
        }

        if line.starts_with('[') {
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

            let mut tag = String::new();

            loop {
                match chars.next() {
                    Some(']') => break,
                    Some(c) => tag.push(c),
                    None => return Err(io::Error::other(format!("Error: Tag not closed on line {line_num}"))),
                }
            }

            tag = tag.trim().to_string();

            if !tag_map.insert(tag.clone()) {
                return Err(io::Error::other(format!("Parse Error: tag '{tag}' declared more than once in config, at line {line_num}")))
            }
            ac.tag = tag;

            let rest = chars.as_str().trim().to_string();
            if !rest.is_empty() {
                ac.words = Some(rest)
            }
            continue;
        }

        let mut chars = line.chars();
        let mut rule_file = String::new();
        let mut rule_filter: Option<RuleFilter> = None;

        loop {
            match chars.next() {
                Some('!') => {
                    rule_filter = get_exclusions(&mut chars);
                },
                Some('~') => {
                    rule_filter = get_selections(&mut chars);
                },
                Some(c) => rule_file.push(c),
                None => break,
            }
        }

        let rule_file = rule_file.trim();
        let mut file_path = path.to_path_buf();
        file_path.set_file_name(rule_file);
        file_path.set_extension(RULE_FILE_EXT);

        if file_path.is_file() {
            let entry_rules = parse_rsca(&file_path)?;
            match rule_filter {
                Some(rf) => match rf {
                    RuleFilter::Only(rule_str) => {
                        match entry_rules.iter().find(|r| r.name.to_lowercase() == rule_str.to_lowercase()).cloned() {
                            Some(rule) => {
                                file_path.set_file_name(format!("{}_only_{}", rule_file, sanitise_str(&rule_str)));
                                // let name = format!("{}-only-{}", file_path.to_str().unwrap(), rule_str.replace(' ', "-"));
                                ac.entries.push(Entry::from(&file_path, &[rule]));
                            },
                            None => return Err(io::Error::other(format!("Parse Error: Could not find rule '{}' in '{}'.\nMake sure the rule name matches exactly!", rule_str, rule_file))),
                        }
                    },
                    RuleFilter::Without(rule_str) => {
                        let before_len = entry_rules.len();
                        let entries = entry_rules.iter().filter(|r| r.name.to_lowercase() != rule_str.to_lowercase()).cloned().collect::<Vec<_>>();
                        if entries.len() == before_len {
                            return Err(io::Error::other(format!("Parse Error: Could not find rule '{}' in '{}'.\nMake sure the rule name matches exactly!", rule_str, rule_file)))
                        }
                        file_path.set_file_name(format!("{}_excl_{}", rule_file, sanitise_str(&rule_str)));
                        // let name = format!("{}-excl-{}", file_path.to_str().unwrap(), rule_str.replace(' ', "-"));
                        ac.entries.push(Entry::from(&file_path, &entries));
                    },
                    RuleFilter::OnlyMult(filters) => {
                        let mut entries = Vec::new();
                        for filter in &filters {
                            match entry_rules.iter().find(|r| r.name.to_lowercase() == filter.to_lowercase()) {
                                Some(entry) => entries.push(entry.clone()),
                                None => return Err(io::Error::other(format!("Parse Error: Could not find rule '{}' in '{}'.\nMake sure the rule name matches exactly!", filter, rule_file))),
                            }
                        }
                        file_path.set_file_name(format!("{}_only-mult_{}", rule_file, sanitise_str(&filters[0])));
                        ac.entries.push(Entry::from(&file_path, &entries));
                    },
                    RuleFilter::WithoutMult(filters) => {
                        let before_len = entry_rules.len();
                        let entries = entry_rules.iter().filter(|r| {
                            !filters.contains(&r.name.to_lowercase())
                    }).cloned().collect::<Vec<_>>();
                        println!("{}  {}", entries.len(), before_len);
                        if entries.len() == before_len {
                            return Err(io::Error::other(format!("Parse Error: Could not find any of the excluded rules in '{}'.\nMake sure the rule names match exactly!", rule_file)))
                        }
                        file_path.set_file_name(format!("{}_excl-mult_{}", rule_file, sanitise_str(&filters[0])));
                        ac.entries.push(Entry::from(&file_path, &entries));
                    },
                },
                None => ac.entries.push(Entry::from(&file_path, &entry_rules)),
            }
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

fn get_selections(chars: &mut Chars<'_>) -> Option<RuleFilter> {
    trim_front(chars);
    let mut rule_names = Vec::new();
    let mut rule_name = String::new();
    loop {
        match chars.next() {
            Some(',') => if !rule_name.is_empty() {
                rule_names.push(rule_name.trim().to_lowercase().to_string());
                rule_name.clear();
            },
            Some(c) => rule_name.push(c),
            None => {
                if !rule_name.is_empty() {
                    rule_names.push(rule_name.trim().to_lowercase().to_string());
                }
                break
            },
        }
    }
    match rule_names.len().cmp(&1) {
        std::cmp::Ordering::Greater => Some(RuleFilter::OnlyMult(rule_names)),
        std::cmp::Ordering::Equal   => Some(RuleFilter::Only(rule_names[0].clone())),
        std::cmp::Ordering::Less    => None,
    }
}

fn get_exclusions(chars: &mut Chars<'_>) -> Option<RuleFilter> {
    trim_front(chars);
    let mut rule_names = Vec::new();
    let mut rule_name = String::new();
    loop {
        match chars.next() {
            Some(',') => if !rule_name.is_empty() {
                rule_names.push(rule_name.trim().to_lowercase().to_string());
                rule_name.clear();
            },
            Some(c) => rule_name.push(c),
            None => {
                if !rule_name.is_empty() {
                    rule_names.push(rule_name.trim().to_lowercase().to_string());
                }
                break
            },
        }
    }
    match rule_names.len().cmp(&1) {
        std::cmp::Ordering::Greater => Some(RuleFilter::WithoutMult(rule_names)),
        std::cmp::Ordering::Equal   => Some(RuleFilter::Without(rule_names[0].clone())),
        std::cmp::Ordering::Less    => None,
    }
}

fn trim_front(chars: &mut Chars<'_>) {
    let mut n = 0;
    for ch in chars.clone() {
        if ch.is_whitespace() {
            n += 1;
        } else {
            break
        }
    }
    for _ in 0..n {
        chars.next();
    }
}

pub(super) fn sanitise_str(str: &str) -> String {
    // 26 is arbitrary, but we want to make sure that the file names don't get too long
    str.chars().take(26)
    .map(|ch| match ch { 
        ' ' | '*' | '/' | '\\' | 
        '?' | ':' | '|' | '\0'  | 
        '<' | '>' | '%' | '"'
        => '-', 
        _ => ch.to_ascii_lowercase()
    }).collect()
}

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


pub fn parse_wsca(path: &Path) -> io::Result<Vec<String>> {
    Ok(util::file_read(path)?.lines().map(|s| s.to_owned()).collect::<Vec<String>>())
}