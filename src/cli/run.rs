use std::{io, path::{Path, PathBuf}};

use asca::RuleGroup;
use colored::Colorize;

use super::{args::InGroup, parse, util::{self, LINE_ENDING, RULE_FILE_EXT, WORD_FILE_EXT}, AscaJson};

// Handle comparing the output to the contents of a wsca file.
fn print_comparison(result: &[String], comp_path: &Path) -> io::Result<()> {
    let (comparison, _) = parse::parse_wsca(&util::validate_file_exists(Some(comp_path), &[WORD_FILE_EXT, "txt"], "word")?)?;
    println!("{} {} {}\n", comp_path.to_str().expect("File path has been validated").bright_blue().bold(), "|".bright_red().bold(), "OUTPUT".bright_green().bold());
    for (comp, res) in comparison.iter().zip(result) {
        if comp.is_empty() && res.is_empty() {
            println!()
        } else if res != comp {
            println!("{} {} {}", comp.bright_blue().bold(), "|".bright_red().bold(), res.bright_green().bold());
        } else {
            println!("{} {} {}", comp, "|".bright_red().bold(), res);
        }
    }
    // In edge cases where one file is longer then the other
    match result.len().cmp(&comparison.len()) {
        std::cmp::Ordering::Equal => {},
        std::cmp::Ordering::Less => for i in comparison.iter().skip(result.len()) {
            println!("{} {} {}", i.bright_blue().bold(), "|".bright_red().bold(), "*".bright_green().bold());
        },
        std::cmp::Ordering::Greater => for i in result.iter().skip(comparison.len()) {
            println!("{} {} {}", "*".bright_blue().bold(), "|".bright_red().bold(), i.bright_green().bold());
        },
    }
    Ok(())
}

// Handle printing result to the terminal
fn print_result(result: &[String], start_words: &[String], maybe_compare: Option<PathBuf>) -> io::Result<()> {
    if let Some(comp_path) = maybe_compare {
        print_comparison(result, &comp_path)
    } else {
        println!("OUTPUT");
        println!("{} {} {}\n", "BEFORE".bright_blue().bold(), "=>".bright_red().bold(), "AFTER".bright_green().bold());
        for (bef, aft) in start_words.iter().zip(result) {
            if bef.is_empty() && aft.is_empty() {
                println!();
            } else {
                println!("{} {} {}", bef.bright_blue().bold(), "=>".bright_red().bold(), aft.bright_green().bold());
            }
        }
        println!();
        Ok(())
    }
}

/// Get input file. 
/// If -j,validate and parse words and rules from it. If -w, use those words instead.
/// Else, validate and parse -r and -w.
fn get_input(i_group: InGroup, input: Option<PathBuf>) -> io::Result<(Vec<String>, Vec<RuleGroup>)> {
    let InGroup { from_json, rules } = i_group;
    if let Some(json) = from_json {
        let json_file_path = util::validate_file_exists(Some(&json), &["json"], "json")?;

        let file = util::file_open(&json_file_path)?;
        let json: AscaJson = serde_json::from_reader(file)?;

        if input.is_some() {
            let (words, _) = parse::parse_wsca(&util::validate_file_exists(input.as_deref(), &[WORD_FILE_EXT, "txt"], "word")?)?;
            Ok((words, json.rules))
        } else {
            Ok((json.words, json.rules))
        }
    } else {
        let (words, _) = parse::parse_wsca(&util::validate_file_exists(input.as_deref(), &[WORD_FILE_EXT, "txt"], "word")?)?;
        let rules = parse::parse_rsca(&util::validate_file_exists(rules.as_deref(), &[RULE_FILE_EXT, "txt"], "rule")?)?;

        Ok((words, rules))
    }
}

/// Handle writing the result to file
fn output_result(output: Option<PathBuf>, res: &[String]) -> io::Result<()> {
    if let Some(path) = &output {
        util::write_to_file(path, res.join(LINE_ENDING), WORD_FILE_EXT, None)?;
    }
    Ok(())
}

pub(crate) fn run(in_group: InGroup, maybe_words: Option<PathBuf>, maybe_output: Option<PathBuf>, maybe_compare: Option<PathBuf>) -> io::Result<()> {
    let (words, rules) = get_input(in_group, maybe_words)?;

    match asca::run(&rules, &words, &[], &[]) {
        Ok(res) => {
            print_result(&res, &words, maybe_compare)?;
            output_result(maybe_output, &res)
        },
        Err(err) => { 
            util::print_asca_errors(err, &words, &rules, &[], &[]); 
            Ok(())
        },
    }
}