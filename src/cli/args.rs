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

        /// Path to the wsca file containing the words to be changed
        /// - If not provided, asca will look for a valid file in the current directory
        #[arg(short, long, verbatim_doc_comment)]
        words: Option<PathBuf>,

        /// Path of a wsca file to compare with the output 
        #[arg(short, long, verbatim_doc_comment)]
        compare: Option<PathBuf>,

        /// Desired path of output file
        /// - If a directory is provided, asca will create an out.wsca file in that directory
        #[arg(short, long, verbatim_doc_comment)]
        output: Option<PathBuf>,
    },
    // Mult {
    //     /// 
    //     rules: Vec<PathBuf>,

    //     /// Path to the wsca file containing the words to be changed
    //     /// - If not provided, asca will look for a valid file in the current directory
    //     #[arg(short, long, verbatim_doc_comment)]
    //     words: Option<PathBuf>,

    //     /// Path of a wsca file to compare with the output 
    //     #[arg(short, long, verbatim_doc_comment)]
    //     compare: Option<PathBuf>,

    //     /// Desired path of output file
    //     /// - If a directory is provided, asca will create an out.wsca file in that directory
    //     #[arg(short, long, verbatim_doc_comment)]
    //     output: Option<PathBuf>,
    // },
    Seq {
        /// Directory containing the rule files
        path: Option<PathBuf>,

        /// Run all defined sequences in the config file
        #[arg(short='a', long, action, verbatim_doc_comment)]
        all: bool,

        /// Path to the wsca file containing the words to be changed
        /// - If not provided, asca will look for a valid file in the current directory
        #[arg(short, long, verbatim_doc_comment)]
        words: Option<PathBuf>,

        /// When given, asca will create an out folder within the path directory
        #[arg(short, long, action, verbatim_doc_comment)]
        output: bool,

        /// Accept cases where an output file will be overwritten
        #[arg(short='y', long, action, verbatim_doc_comment)]
        overwrite: bool,

        /// Only last iteration will be saved, i.e. no steps
        #[arg(short='l', long, action, verbatim_doc_comment)]
        last_only: bool,
    },
    /// Convert between an asca-web json file and the wsca/rsca format
    #[clap(subcommand)]
    Convert(Conv),
    /// Enter tui
    Tui
}

#[derive(Debug, Subcommand)]
pub enum Conv {
    Asca {
        /// The path of the word file to convert
        #[arg(short, long, verbatim_doc_comment)]
        words: Option<PathBuf>,
        /// The path of the rule file to convert
        #[arg(short, long, verbatim_doc_comment)]
        rules: Option<PathBuf>,
        /// The desired path of the output json file
        #[arg(short, long, verbatim_doc_comment)]
        output: Option<PathBuf>,
    },
    Json {
        /// The path of the Json file to convert
        #[arg(short, long, verbatim_doc_comment)]
        path: Option<PathBuf>,
        /// The desired path of the output word file
        #[arg(short, long, verbatim_doc_comment)]
        words: Option<PathBuf>,
        /// The desired path of the output rule file
        #[arg(short, long, verbatim_doc_comment)]
        rules: Option<PathBuf>,
    },
}

#[derive(Debug, Args)]
#[group(multiple = false)]
pub struct InGroup {
    /// Path to an asca-web json file, mutually exclusive with -r.
    /// - If not provided, asca will look for a valid file in the current directory
    /// - If --words is supplied, those words will be used instead of those defined in the json.
    #[arg(short='j', long, verbatim_doc_comment)]
    pub from_json: Option<PathBuf>,

    /// Path to an asca file containing the rules to be applied, mutually exclusive with -j.
    /// - If not provided, asca will look for a valid file in the current directory
    #[arg(short, long, verbatim_doc_comment)]
    pub rules: Option<PathBuf>,
}