mod cli; 
use cli::args::*;
use colored::Colorize;

use std::{fs, io, path::PathBuf, process::exit};
use clap::Parser;

fn get_dir_files(path: &str, extension: &str) -> Result<Vec<PathBuf>, io::Error> {
    Ok(fs::read_dir(path)?
        // Filter out entries which we couldn't read
        .filter_map(|res| res.ok())
        // Turn entries to paths
        .map(|dir_entry| dir_entry.path())
        // Filter out files with the wrong extension
        .filter_map(|path| {
            if path.extension().map_or(false, |ext| ext == extension) {
                Some(path)
            } else {
                None
            }
        })
    .collect::<Vec<_>>())
}

// TODO: better errors
fn validate_file_exists(maybe_path: Option<PathBuf>, extension: &str) -> Result<PathBuf, io::Error> {
    match maybe_path {
        // Probably don't have to check if path exists as checking if it has an extension should be enough
        Some(path) => match path.extension() {
            Some(ext) => match ext == extension {
                true => return Ok(path),
                false => println!("File is not of the right type. Must be .{extension}"),
            },
            None => println!("Given path is not a file"),
        },
        None => {
            let files = get_dir_files(".", extension)?;
            match files.len().cmp(&1) {
                std::cmp::Ordering::Greater => println!("More than one .{extension} file found in current directory. Please specify."),
                std::cmp::Ordering::Less    => println!("No .{extension} files found in current directory"),
                std::cmp::Ordering::Equal   => return Ok(files[0].clone()),
            }
        }
    }
    exit(1);
}

fn run_cli(rules: Option<PathBuf>, input: Option<PathBuf>, output: Option<PathBuf>) -> Result<(), io::Error> {

    let rule_file_path = validate_file_exists(rules, "asca")?;
    let word_file_path = validate_file_exists(input, "txt")?;
    let rules: Vec<String> = fs::read_to_string(rule_file_path)?.lines().map(|s| s.to_owned()).collect();
    let word: Vec<String> = fs::read_to_string(word_file_path)?.lines().map(|s| s.to_owned()).collect();
    

    let res = asca::run(rules, word.clone());

    if res.len() == 1 && res[0].contains("Error") {
        println!("{}", res[0])
    } else {
        for (i, r) in res.iter().enumerate() {
            if r.is_empty() {
                println!();
            } else {
                println!("{} {} {}", word[i].bright_blue().bold(), "=>".bright_red().bold(), r.bright_green().bold());
            }
        }
    }

    if let Some(out_path) = output {
        // validate path and then write to file
    }

    Ok(())
}

fn main() {
    let args = CliArgs::parse();

    match args.cmd {
        Command::Run { rules, input, output } => {
            if let Err(e) = run_cli(rules, input, output) {
                println!("{e}");
                exit(1);
            }
        },
        Command::Tui => todo!(),
    }

    // let res = asca::run(unparsed_rules, unparsed_words);

    // for r in res {
    //     println!("{r}");
    // }
}