mod cli; 
use cli::args::{CliArgs, Command, Conv};

use std::{io, process::exit};
use clap::Parser;


fn run() -> io::Result<()> {
    match CliArgs::parse().cmd {
        Command::Run { i_group, words: input, compare, output } => cli::run::run(i_group, input, output, compare),
        Command::Conv(conv) => match conv {
            Conv::Asca { words, rules, output } => cli::convert::from_asca(words, rules, output),
            Conv::Json { path, words, rules }   => cli::convert::from_json(path, words, rules),
        },
        // Command::Mult { rules, words, compare, output } => todo!(),
        Command::Seq { path, tag, words, all_steps, output, overwrite , no_overwrite, output_all } => {
            let ow = match (overwrite, no_overwrite) {
                (true, false) => Some(true),
                (false, true) => Some(false),
                _ => None
            };
            cli::seq::run(path, words, tag, output, ow, output_all, all_steps)
        },
        // Command::Tui => println!("tui coming soon..."),
    }
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        exit(1);
    }
}