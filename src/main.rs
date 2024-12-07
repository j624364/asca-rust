mod cli;

use std::{path::PathBuf, process::exit};

use clap::Parser;
use cli::args::*;

fn get_current_dir_files(extension: &str) -> Vec<PathBuf> {
    std::fs::read_dir(".").expect("Current directory exists and we have permission to view contents")
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
    .collect::<Vec<_>>()
}


fn get_file(maybe_path: Option<PathBuf>, extension: &str) -> PathBuf {
    match maybe_path {
        Some(path) => {
            // Probably don't have to check if path exists as checking if it has an extension should be enough
            match path.extension() {
                Some(ext) => {
                    if ext == extension {
                        return path
                    } else {
                        println!("File is not of the right type. Must be .{extension}");
                    }
                },
                None => println!("Given path is not a file"),
            }
            exit(1);
        },
        None => {
            let files = get_current_dir_files(extension);

            match files.len().cmp(&1) {
                std::cmp::Ordering::Greater => println!("More than one .{extension} file found in current directory. Please specify."),
                std::cmp::Ordering::Less    => println!("No .{extension} files found in current directory"),
                std::cmp::Ordering::Equal   => return files[0].clone(),
            }
            exit(1)
        },
    }
}

fn main() {
    let args = CliArgs::parse();

    match args.cmd {
        Command::Run { rules, input, output } => {
            let rule_file = get_file(rules, "asca");
            println!("{:?}", rule_file);
            let word_file = get_file(input, "txt");
            println!("{:?}", word_file);
        },
        Command::Tui => todo!(),
    }

    // let res = asca::run(unparsed_rules, unparsed_words);

    // for r in res {
    //     println!("{r}");
    // }
}