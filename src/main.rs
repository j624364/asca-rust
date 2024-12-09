mod cli; 
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
                false => println!("File is not of the right type. Must be .txt or .asca"), // TODO: programatically print valid extensions
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

fn cli(rules: Option<PathBuf>, input: Option<PathBuf>, output: Option<PathBuf>) -> Result<(), io::Error> {

    let rule_file_path = validate_file_exists(rules, &["asca", "txt"])?;
    let word_file_path = validate_file_exists(input, &["txt"])?;
    let rules: Vec<String> = fs::read_to_string(rule_file_path)?.lines().map(|s| s.to_owned()).collect();
    let word: Vec<String> = fs::read_to_string(word_file_path)?.lines().map(|s| s.to_owned()).collect();
    

    let res = asca::run_cli(&rules, &word);

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

    println!();

    if let Some(out_path) = output {
        if out_path.is_file() {
            // if path is an extant file, overwrite on accept
            if ask(&(format!("File {out_path:?} already exists, do you wish to overwrite it? [y/n]"))) {
                fs::write(&out_path, res.join(LINE_ENDING)).expect("Unable to write file");
                println!("Written to file {:?}", out_path);
            }
        } else if out_path.is_dir() {
            // if path is dir, write to file of <dir>/out.txt
            let p = PathBuf::from("out.txt");
            if p.exists() {
                if ask(&(format!("File {p:?} already exists, do you wish to overwrite it? [y/n]"))) {
                    fs::write(&out_path, res.join(LINE_ENDING)).expect("Unable to write file");
                    println!("Written to file {:?}", out_path);
                }
            } else {
                fs::write(&p, res.join(LINE_ENDING)).expect("Unable to write file");
                println!("Written to file 'out.txt'");
            }
        } else if out_path.extension().map_or(false, |ext| ext == "txt") {
            // create file <out_path> and write to it
            fs::write(&out_path, res.join(LINE_ENDING)).expect("Unable to write file");
            println!("Written to file {:?}", out_path);
        } else {
            // Wrong file type?
        }
    }

    Ok(())
}

fn main() {
    let args = CliArgs::parse();
    match args.cmd {
        Command::Run { rules, input, output } => {
            if let Err(e) = cli(rules, input, output) {
                println!("{e}");
                exit(1);
            }
        },
        Command::Tui => todo!("tui coming soon"),
    }
}