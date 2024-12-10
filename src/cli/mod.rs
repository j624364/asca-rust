pub mod util;
pub mod args;
pub mod parse;

use args::InGroup;
use asca::{error::ASCAError, RuleGroup};
use colored::Colorize;
use parse::{parse_rasca_file, parse_wasca_file};
use util::{ask, to_rasca_format, validate_file_exists, write_to_file, LINE_ENDING};

use std::{fs, io, path::PathBuf};


#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct AscaJson {
    pub words: Vec<String>,
    pub rules: Vec<RuleGroup>,
}

fn get_input(i_group: InGroup, input: Option<PathBuf>) -> io::Result<(Vec<String>, Vec<RuleGroup>)> {
    let InGroup {from_json, rules} = i_group;
    if let Some(json) = from_json {
        let json_file_path = validate_file_exists(Some(json), &["json"], "json")?;

        let file = fs::File::open(json_file_path)?;
        let json: AscaJson = serde_json::from_reader(file)?;

        if input.is_some() {
            let words = parse_wasca_file(validate_file_exists(input, &["wasca", "txt"], "word")?)?;
            Ok((words, json.rules))
        } else {
            Ok((json.words, json.rules))
        }
    } else {
        let words = parse_wasca_file(validate_file_exists(input, &["wasca", "txt"], "word")?)?;
        let rules = parse_rasca_file(validate_file_exists(rules, &["rasca", "txt"], "rule")?)?;

        Ok((words, rules))
    }
}


fn deal_with_output(output: Option<PathBuf>, res: &[String]) -> io::Result<()> {
    if let Some(path) = &output {
        let content = res.join(LINE_ENDING);
        write_to_file(path, content, "wasca")?;
    }
    
    Ok(())
}

pub fn conv_asca(words: Option<PathBuf>, rules: Option<PathBuf>, output: Option<PathBuf>) -> io::Result<()> {
    let words = parse_wasca_file(validate_file_exists(words, &["wasca", "txt"], "word")?)?;
    let rules = parse_rasca_file(validate_file_exists(rules, &["rasca", "txt"], "rule")?)?;

    let json = serde_json::to_string_pretty(&(AscaJson { words, rules }))?;

    if let Some(path) = &output {
        write_to_file(path, json, "json")?;
    } else {
        let p = PathBuf::from("out.json");

        if p.exists() {
            if ask(&(format!("File {p:?} already exists in current directory, do you wish to overwrite it?")))? {
                fs::write(&p, json)?;
                println!("Written to file {:?}", p);
            }
        } else {
            fs::write(&p, json)?;
            println!("Created file `{p:?}` in current directory")
        }
    }


    Ok(())
}

pub fn conv_json(path: Option<PathBuf>, words: Option<PathBuf>, rules: Option<PathBuf>) -> io::Result<()> {

    let json_path = validate_file_exists(path, &["json"], "json")?;

    let file = fs::File::open(json_path)?;
    let json: AscaJson = serde_json::from_reader(file)?;

    let words_wasca = json.words.join("\n");

    if let Some(w_path) = words {
        write_to_file(&w_path, words_wasca, "wasca")?;
    } else {
        let p = PathBuf::from("out.wasca");

        if p.exists() {
            if ask(&(format!("File {p:?} already exists in current directory, do you wish to overwrite it?")))? {
                fs::write(&p, words_wasca)?;
                println!("Written to file {:?}", p);
            }
        } else {
            fs::write(&p, words_wasca)?;
            println!("Created file `{p:?}` in current directory")
        }
    }

    let rules_rasca = to_rasca_format(json.rules)?;

    if let Some(r_path) = rules {
        write_to_file(&r_path, rules_rasca, "rasca")?;
    } else {
        let p = PathBuf::from("out.rasca");

        if p.exists() {
            if ask(&(format!("File {p:?} already exists in current directory, do you wish to overwrite it?")))? {
                fs::write(&p, rules_rasca)?;
                println!("Written to file {:?}", p);
            }
        } else {
            fs::write(&p, rules_rasca)?;
            println!("Created file `{p:?}` in current directory")
        }
    }

    Ok(())
}


pub fn run(i_group: InGroup, input: Option<PathBuf>, output: Option<PathBuf>) -> io::Result<()> {

    let (words, rules) = get_input(i_group, input)?;

    let result = asca::run(&rules, &words);

    match result {
        Ok(res) => {
            for (i, r) in res.iter().enumerate() {
                if r.is_empty() {
                    println!();
                } else {
                    println!("{} {} {}", words[i].bright_blue().bold(), "=>".bright_red().bold(), r.bright_green().bold());
                }
            }
            println!();
            deal_with_output(output, &res)?;
        },
        Err(err) => match err {
            asca::error::Error::WordSyn(e) => println!("{}", e.format_word_error(&words)),
            asca::error::Error::WordRun(e) => println!("{}", e.format_word_error(&words)),
            asca::error::Error::RuleSyn(e) => println!("{}", e.format_rule_error(&rules)),
            asca::error::Error::RuleRun(e) => println!("{}", e.format_rule_error(&rules)),
        },
    }
    Ok(())
}



