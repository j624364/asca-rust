mod cli; 
use cli::args::{CliArgs, AscaCommand, Conv};

use std::{io, process::exit};
use clap::{CommandFactory, Parser};


fn run() -> io::Result<()> {

    let cmd = CliArgs::parse();

    if let Some(generator) = cmd.generator {
        let mut cmd = CliArgs::command();
        eprintln!("Generating completion file for {generator:?}...");
        cli::args::print_completions(generator, &mut cmd);
        Ok(())
    } else {
        match cmd.cmd {
            Some(AscaCommand::Run { i_group, words: input, compare, output }) => cli::run::run(i_group, input, output, compare),
            Some(AscaCommand::Conv(conv) )=> match conv {
                Conv::Asca { words, rules, output } => cli::convert::from_asca(words, rules, output),
                Conv::Json { path, words, rules }   => cli::convert::from_json(path, words, rules),
            },
            // Some(AscaCommand::Mult { rules, words, compare, output }) => todo!(),
            Some(AscaCommand::Seq { path, tag, words, all_steps, output, overwrite , no_overwrite, output_all }) => {
                let ow = match (overwrite, no_overwrite) {
                    (true, false) => Some(true),
                    (false, true) => Some(false),
                    _ => None
                };
                cli::seq::run(path, words, tag, output, ow, output_all, all_steps)
            },
            // Some(AscaCommand::Tui) => println!("tui coming soon..."),
            None => Ok(()),
        } 
    }
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        exit(1);
    }
}