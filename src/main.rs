mod cli; 
use cli::args::{CliArgs, Command, Conv};


use std::process::exit;
use clap::Parser;


fn main() {
    let args = CliArgs::parse();
    match args.cmd {
        Command::Run { i_group, words: input, output } => {
            if let Err(e) = cli::run(i_group, input, output) {
                println!("{e}");
                exit(1);
            }
        },
        Command::Convert(conv) => match conv {
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
        Command::Tui => println!("tui coming soon..."),
    }
}