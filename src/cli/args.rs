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
        #[clap(flatten)]
        i_group: InGroup,

        #[arg(short, long, verbatim_doc_comment)]
        /// File Path to a text file containing the words to be changed
        /// - If not provided, asca will look for a valid file in the current directory
        input: Option<PathBuf>,

        #[clap(flatten)]
        o_group: OutGroup
        
    }, 
    /// Enter tui
    Tui
}

#[derive(Debug, Args)]
#[group(multiple = false)]
pub struct InGroup {
    /// File path to an asca-web json file, mutually exclusive with -r.
    /// - If not provided, asca will look for a valid file in the current directory
    /// - If -i is supplied, those words will be used instead of those defined in the json.
    #[arg(short, long, verbatim_doc_comment)]
    pub from_json: Option<PathBuf>,

    /// File path to an asca file containing the rules to be applied
    /// - If not provided, asca will look for a valid file in the current directory
    #[arg(short, long, verbatim_doc_comment)]
    pub rules: Option<PathBuf>,
}

#[derive(Debug, Args)]
#[group(multiple = false)]
pub struct OutGroup {
    /// Create an asca-web json file from the input words and rules, mutually exclusive with -o
    #[arg(short='j', long, verbatim_doc_comment)]
    pub to_json: Option<PathBuf>,

    /// Desired path of output file
    #[arg(short, long, verbatim_doc_comment)]
    pub output: Option<PathBuf>,
        
}