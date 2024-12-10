mod cli; 
use asca::{error::ASCAError, RuleGroup};
use cli::args::*;
use colored::Colorize;

use std::{ffi::OsStr, fs, io, path::PathBuf, process::exit};
use clap::Parser;

#[cfg(windows)]
const LINE_ENDING : &str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING : &str = "\n";

fn ask(question: &str) -> bool {
    println!("{}",question);
    loop {
        let mut buf = String::new();
        let _ = std::io::stdin().read_line(&mut buf);
        match buf.chars().next() {
            Some('y' | 'Y') => return true,
            Some('n' | 'N') => return false,
            _ => println!("yes/no only."),
        }
    }
}

fn get_dir_files(path: &str, valid_extensions: &[&str]) -> Result<Vec<PathBuf>, io::Error> {
    Ok(fs::read_dir(path)?
        // Filter out entries which we couldn't read
        .filter_map(|res| res.ok())
        // Turn entries to paths
        .map(|dir_entry| dir_entry.path())
        // Filter out files with the wrong extension
        .filter_map(|path| {
            if path.extension().map_or(false, |ext| match_exts(ext, valid_extensions)) {
                Some(path)
            } else {
                None
            }
        })
    .collect::<Vec<_>>())
}

fn match_exts(ext: &OsStr, valid_extensions: &[&str]) -> bool {
    for &ve in valid_extensions {
        if ve == ext {
            return true
        }
    }
    false
}

// TODO: better errors
fn validate_file_exists(maybe_path: Option<PathBuf>, valid_extensions: &[&str]) -> Result<PathBuf, io::Error> {
    match maybe_path {
        // Probably don't have to check if path exists as checking if it has an extension should be enough
        Some(path) => match path.extension() {
            Some(ext) => match match_exts(ext, valid_extensions) {
                true => return Ok(path),
                false => println!("File is not of the right type. Must be .txt, .wasca, or .rasca"), // TODO: programatically print valid extensions
            },
            None => println!("Given path is not a file"),
        },
        None => {
            let files = get_dir_files(".", valid_extensions)?;
            match files.len().cmp(&1) {
                std::cmp::Ordering::Greater => println!("More than one matching file found in current directory. Please specify."),
                std::cmp::Ordering::Less    => println!("No matching files found in current directory"),
                std::cmp::Ordering::Equal   => return Ok(files[0].clone()),
            }
        }
    }
    exit(1);
}

#[derive(Debug, serde::Deserialize)]
struct AscaJson {
    pub words: Vec<String>,
    pub rules: Vec<RuleGroup>,
}

// TODO: We can do better
fn parse_rasca(rule_file_path: PathBuf) -> Result<Vec<RuleGroup>, io::Error> {
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

    println!("{:#?}", rules);

    Ok(rules)
}

fn parse_wasca(word_file_path: PathBuf) -> Result<Vec<String>, io::Error> {
    Ok(fs::read_to_string(word_file_path)?.lines().map(|s| s.to_owned()).collect::<Vec<String>>())
}

fn get_input(i_group: InGroup, input: Option<PathBuf>) -> Result<(Vec<String>, Vec<RuleGroup>), io::Error> {
    let InGroup {from_json, rules} = i_group;
    if let Some(json) = from_json {
        let json_file_path = validate_file_exists(Some(json), &["json"])?;

        let file = fs::File::open(json_file_path).expect("file should open read only");
        let json: AscaJson = serde_json::from_reader(file).expect("file should be proper JSON");

        if input.is_some() {
            let words = parse_wasca(validate_file_exists(input, &["wasca", "txt"])?)?;
            Ok((words, json.rules))
        } else {
            Ok((json.words, json.rules))
        }
    } else {
        let words = parse_wasca(validate_file_exists(input, &["wasca", "txt"])?)?;
        let rules = parse_rasca(validate_file_exists(rules, &["rasca", "txt"])?)?;

        Ok((words, rules))
    }
}

fn deal_with_output(res: &[String], o_group: OutGroup, words: &[String], rules: Vec<RuleGroup>) -> Result<(), io::Error> {

    #[derive(Debug, serde::Deserialize, serde::Serialize)]
    struct Js {
        words: Vec<String>,
        rules: Vec<RuleGroup>
    }


    let OutGroup {to_json, output} = o_group;

    let (path, content) = if let Some(json_path) = &to_json {
        let obj = Js { words: words.into(), rules };
        let json_obj = serde_json::to_string_pretty(&obj)?;

        (json_path, json_obj)

    } else if let Some(out_path) = &output {
        (out_path, res.join(LINE_ENDING))
    } else {
        return Ok(())
    };

    if path.is_file() {
        // if path is an extant file, overwrite on accept
        if ask(&(format!("File {path:?} already exists, do you wish to overwrite it? [y/n]"))) {
            fs::write(path, content).expect("Unable to write file");
            println!("Written to file {:?}", path);
        }
    } else if path.is_dir() {
        // if path is dir, write to file of <dir>/out.ext
        let p = if to_json.is_some() { PathBuf::from("out.json") } else { PathBuf::from("out.txt") };
        if p.exists() {
            if ask(&(format!("File {p:?} already exists, do you wish to overwrite it? [y/n]"))) {
                fs::write(&p, content).expect("Unable to write file");
                println!("Written to file {:?}", p);
            }
        } else {
            fs::write(&p, content).expect("Unable to write file");
                println!("Written to file {:?}", p);
        }
    } else if path.extension().map_or(false, |ext| ext == "wasca" || ext == "json") {
        // create file <out_path> and write to it
        fs::write(&path, content).expect("Unable to write file");
        println!("Written to file {:?}", path);
    } else {
        // Wrong file type?
    }

    
    Ok(())
}

fn cli(i_group: InGroup, input: Option<PathBuf>, o_group: OutGroup) -> Result<(), io::Error> {

    let (words, rules) = get_input(i_group, input)?;

    // println!("{:#?}", rules);

    let res = asca::run(&rules, &words);

    match res {
        Ok(rs) => {
            for (i, r) in rs.iter().enumerate() {
                if r.is_empty() {
                    println!();
                } else {
                    println!("{} {} {}", words[i].bright_blue().bold(), "=>".bright_red().bold(), r.bright_green().bold());
                }
            }
            println!();
            deal_with_output(&rs, o_group, &words, rules)?;
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

fn main() {
    let args = CliArgs::parse();
    match args.cmd {
        Command::Run { i_group, input, o_group } => {
            if let Err(e) = cli(i_group, input, o_group) {
                println!("{e}");
                exit(1);
            }
        },
        Command::Tui => println!("tui coming soon..."),
    }
}