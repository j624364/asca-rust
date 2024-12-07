use std::path::PathBuf;

use clap:: {
    Args,
    Parser, 
    Subcommand,
};

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct CliArgs {
    #[clap(subcommand)]
    pub cmd: Command,
}

#[derive(Debug, Subcommand)]
// #[clap(color=clap::ColorChoice::Always)]
pub enum Command {
    /// Run cli 
    Run {
        /// File path to desired asca file
        #[arg(short, long)]
        rules: Option<PathBuf>,
        #[arg(short, long)]
        /// File Path to the words.txt file
        input: Option<PathBuf>,
        /// Desired path of output file
        #[arg(short, long)]

        output: Option<PathBuf>,
    }, 
    /// Enter tui interface
    Tui
}