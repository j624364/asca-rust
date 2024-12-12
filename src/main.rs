mod cli; 
use cli::args::{CliArgs, Command, Conv};


use std::process::exit;
use clap::Parser;


fn main() {
    let args = CliArgs::parse();
    match args.cmd {
        Command::Run { i_group, words: input, compare, output } => {
            if let Err(e) = cli::run(i_group, input, output, compare) {
                println!("{e}");
                exit(1);
            }
        },
        Command::Conv(conv) => match conv {
            Conv::Asca { words, rules, output } => {
                if let Err(e) = cli::conv_asca(words, rules, output) {
                    println!("{e}");
                    exit(1);
                }
            },
            Conv::Json { path, words, rules } => {
                if let Err(e) = cli::conv_json(path, words, rules) {
                    println!("{e}");
                    exit(1);
                }
            },
        },
        // Command::Mult { rules, words, compare, output } => {
        //     todo!()
        // },
        Command::Seq { path, tag, words, output, overwrite , no_overwrite, last_only } => {
            let ow = if overwrite {
                Some(true)
            } else if no_overwrite { 
                Some(false)
            } else {
                None
            };
            if let Err(e) = cli::sequence(path, words, output, ow, last_only, tag) {
                println!("{e}");
                exit(1);
            }
        },
        Command::Tui => println!("tui coming soon..."),
    }
}