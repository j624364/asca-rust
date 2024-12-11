pub mod util;
pub mod args;
pub mod parse;

use args::InGroup;
use asca::{error::ASCAError, RuleGroup};
use colored::Colorize;
use parse::{parse_config, parse_rasca_file, parse_wasca_file, ASCAConfig};
use util::{ask, to_rasca_format, validate_file_exists, write_to_file, LINE_ENDING};

use std::{fs, io, path::{Path, PathBuf}, process::exit};


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
        let rules = parse_rasca_file(&validate_file_exists(rules, &["rasca", "txt"], "rule")?)?;

        Ok((words, rules))
    }
}


fn deal_with_output(output: Option<PathBuf>, res: &[String]) -> io::Result<()> {
    if let Some(path) = &output {
        let content = res.join(LINE_ENDING);
        write_to_file(path, content, "wasca", None)?;
    }
    
    Ok(())
}

pub fn conv_asca(words: Option<PathBuf>, rules: Option<PathBuf>, output: Option<PathBuf>) -> io::Result<()> {
    let words = parse_wasca_file(validate_file_exists(words, &["wasca", "txt"], "word")?)?;
    let rules = parse_rasca_file(&validate_file_exists(rules, &["rasca", "txt"], "rule")?)?;

    let json = serde_json::to_string_pretty(&(AscaJson { words, rules }))?;

    if let Some(path) = &output {
        write_to_file(path, json, "json", None)?;
    } else {
        let p = PathBuf::from("out.json");

        if p.exists() {
            if ask(&(format!("File {p:?} already exists in current directory, do you wish to overwrite it?")), None)? {
                fs::write(&p, json)?;
                println!("Wrote to file {:?}", p);
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
        write_to_file(&w_path, words_wasca, "wasca", None)?;
    } else {
        let p = PathBuf::from("out.wasca");

        if p.exists() {
            if ask(&(format!("File {p:?} already exists in current directory, do you wish to overwrite it?")), None)? {
                fs::write(&p, words_wasca)?;
                println!("Wrote to file {:?}", p);
            }
        } else {
            fs::write(&p, words_wasca)?;
            println!("Created file `{p:?}` in current directory")
        }
    }

    let rules_rasca = to_rasca_format(json.rules)?;

    if let Some(r_path) = rules {
        write_to_file(&r_path, rules_rasca, "rasca", None)?;
    } else {
        let p = PathBuf::from("out.rasca");

        if p.exists() {
            if ask(&(format!("File {p:?} already exists in current directory, do you wish to overwrite it?")), None)? {
                fs::write(&p, rules_rasca)?;
                println!("Wrote to file {:?}", p);
            }
        } else {
            fs::write(&p, rules_rasca)?;
            println!("Created file `{p:?}` in current directory")
        }
    }

    Ok(())
}

fn print_result(result: &[String], words: &[String], maybe_compare: Option<PathBuf>) -> io::Result<()> {
    if let Some(compare_path) = maybe_compare {
        let comp = compare_path.clone();
        let path_str = comp.to_str().unwrap();
        let compare = parse_wasca_file(validate_file_exists(Some(compare_path), &["wasca", "txt"], "word")?)?;
        println!("{} {} {}\n", path_str.bright_blue().bold(), "=>".bright_red().bold(), "OUTPUT".bright_green().bold());
        for (comp, res) in compare.iter().zip(result) {
            if comp.is_empty() && res.is_empty() {
                println!()
            } else {
                println!("{} {} {}", comp.bright_blue().bold(), "=>".bright_red().bold(), res.bright_green().bold());
            }
        }
        // In case one file is longer then the other
        match result.len().cmp(&compare.len()) {
            std::cmp::Ordering::Equal => {},
            std::cmp::Ordering::Less => for i in compare.iter().skip(result.len()) {
                println!("{} {} {}", i.bright_blue().bold(), "=>".bright_red().bold(), "*".bright_green().bold());
            },
            std::cmp::Ordering::Greater => for i in result.iter().skip(compare.len()) {
                println!("{} {} {}", "*".bright_blue().bold(), "=>".bright_red().bold(), i.bright_green().bold());
            },
        }

    } else {
        println!("OUTPUT");
        println!("{} {} {}\n", "BEFORE".bright_blue().bold(), "=>".bright_red().bold(), "AFTER".bright_green().bold());
        for (bef, aft) in words.iter().zip(result) {
            if bef.is_empty() && aft.is_empty() {
                println!();
            } else {
                println!("{} {} {}", bef.bright_blue().bold(), "=>".bright_red().bold(), aft.bright_green().bold());
            }
        }
        println!();
    }

    Ok(())
}

fn print_seq_result(trace: &[Vec<String>], tag: &str) {
    debug_assert!(!trace.is_empty());
    println!("OUTPUT - {}", tag);
    let arr = "=>".bright_red().bold();

    let num_seqs = trace.len();
    let num_words = trace[0].len();

    for word in 0..num_words {
        if trace[0][word].is_empty() {
            println!();
            continue;
        }

        let mut str = format!("{}", &trace[0][word].bright_blue().bold());
        for seq in 1..num_seqs-1 {
            str = format!("{str} {arr}");
            str = format!("{str} {}", &trace[seq][word]);

        }
        str = format!("{str} {arr}");
        str = format!("{str} {}", &trace[num_seqs-1][word].bright_green().bold());
        println!("{}", str)

    }
}

fn validate_directory(maybe_path: Option<PathBuf>) -> io::Result<PathBuf> {
    match maybe_path {
        Some(path) => {
            if path.is_dir() {
                Ok(path)
            } else {
                println!("Error: Given path is not a directory");
                exit(1);
            }
        },
        None => Ok(PathBuf::from(".")),
    }
}



fn get_seq(dir: &Path) -> io::Result<Vec<ASCAConfig>> {
    let maybe_conf = util::get_dir_files(dir.to_str().unwrap(), &["asca"])?;

    if maybe_conf.is_empty() {
        println!("Error: No config file found in directory {:?}", dir);
        exit(1)
    } else if maybe_conf.len() > 1 {
        println!("Error: Multiple config files found in directory {:?}", dir);
        exit(1)
    }

    parse_config(maybe_conf[0].as_path())

}

fn output_seq(dir: &Path, tag: &str, trace: &[Vec<String>], seq_names: &[PathBuf], overwrite: bool, last_only: bool) -> io::Result<()> {
    // Create <out/tag> subfolder within <dir> if doesn't exist
    // Create files for each seq, <seq_name>.wasca, and write to each
    let mut path = dir.to_path_buf();
    path.push("out");
    path.push(tag);
    
    if !path.exists() {
        fs::create_dir_all(&path)?;
        println!("Created dir {path:?}");
    }

    if !last_only {
        for (seq, name) in seq_names.iter().enumerate() {
            let content = trace[seq+1].join("\n");
            let mut p = path.clone();
            let name = format!("{}-{}", seq+1, name.file_name().unwrap().to_os_string().into_string().unwrap());
            p.push(name);
            p.set_extension("wasca");
            write_to_file(&p, content, "wasca", overwrite.then_some(true))?;
        }
    } else {
        let name = seq_names.last().unwrap();
        let content = trace.last().unwrap().join("\n");
        let mut p = path.clone();
        let name = name.file_name().unwrap();
        p.push(name);
        p.set_extension("wasca");
        write_to_file(&p, content, "wasca", overwrite.then_some(true))?;
    }

    Ok(())
}

pub fn sequence(dir_path: Option<PathBuf>, words: Option<PathBuf>, output: bool, overwrite: bool, last_only: bool) -> io::Result<()> {
    let words = parse_wasca_file(validate_file_exists(words, &["wasca", "txt"], "word")?)?;

    let dir = validate_directory(dir_path)?;
    let rule_confs= get_seq(&dir)?;

    for conf in rule_confs {
        let mut files = Vec::new();
        let mut trace = Vec::new();
        trace.push(words.clone());
        for (i, seq) in conf.entries.iter().enumerate() {
            files.push(seq.name.clone());
            match asca::run(&seq.rules, &trace[i]) {
                Ok(res) => {
                    trace.push(res);
                },
                Err(err) => {
                    print_asca_errors(err, &words, &seq.rules);
                    return Ok(())
                },
            }
        }
        print_seq_result(&trace, &conf.tag);
        println!();

        if !trace.is_empty() {
            if output {
                output_seq(&dir, &conf.tag, &trace, &files, overwrite, last_only)?;
            }
        } else {
            unreachable!("No output")
        }
    }

    Ok(())
}


pub fn run(i_group: InGroup, input: Option<PathBuf>, output: Option<PathBuf>, compare: Option<PathBuf>) -> io::Result<()> {

    let (words, rules) = get_input(i_group, input)?;

    let result = asca::run(&rules, &words);

    match result {
        Ok(res) => {
            print_result(&res, &words, compare)?;
            deal_with_output(output, &res)?;
        },
        Err(err) => print_asca_errors(err, &words, &rules),
    }
    Ok(())
}

fn print_asca_errors(err: asca::error::Error, words: &[String], rules: &[RuleGroup]) {
    match err {
        asca::error::Error::WordSyn(e) => println!("{}", e.format_word_error(words)),
        asca::error::Error::WordRun(e) => println!("{}", e.format_word_error(words)),
        asca::error::Error::RuleSyn(e) => println!("{}", e.format_rule_error(rules)),
        asca::error::Error::RuleRun(e) => println!("{}", e.format_rule_error(rules)),
    }
}



