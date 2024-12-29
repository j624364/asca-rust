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
    /// Run basic cli.
    Run {
        #[clap(flatten)]
        i_group: InGroup,

        /// Path to a wsca file containing the input words.
        /// - If this and -j are not provided, asca will look for a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        words: Option<PathBuf>,

        /// Path of a wsca file to compare with the result.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        compare: Option<PathBuf>,

        /// Desired path of output file.
        /// - If a directory is provided, asca will create an out.wsca file in that directory.
        /// - If not provided, asca will not write to any file.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        output: Option<PathBuf>,
    },
    // Mult {
    //     /// 
    //     rules: Vec<PathBuf>,

    //     /// Path to the wsca file containing the words to be changed
    //     /// - If not provided, asca will look for a  file in the current directory
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
    /// Run an asca config.
    Seq {
        /// Path to a directory containing an asca config file.
        #[arg(value_hint=clap::ValueHint::DirPath)]
        path: Option<PathBuf>,
        
        /// Run a given tag in the config file.
        /// - If not provided, all tags in the config will be run.
        #[arg(short='t', long, verbatim_doc_comment)]
        tag: Option<String>,

        /// Path to a wsca file.
        /// - If provided, these will be used instead of the word files defined in the config.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        words: Option<PathBuf>,

        /// Print all intermediate steps
        #[arg(short, long, action, verbatim_doc_comment)]
        all_steps: bool,

        /// When given, asca will create an out folder within the [PATH] directory.
        #[arg(short, long, action, verbatim_doc_comment)]
        output: bool,

        /// Accept cases where an output file would be overwritten.
        /// - Requires --output
        #[arg(short='y', long, action, requires="output", conflicts_with="no_overwrite", verbatim_doc_comment)]
        overwrite: bool,

        /// Reject cases where an output file would be overwritten.
        /// - Requires --output
        #[arg(short='n', long, action, requires="output", conflicts_with="overwrite", verbatim_doc_comment)]
        no_overwrite: bool,

        /// Only the final iteration will be saved (i.e. no intermediate steps).
        /// - Requires --output
        #[arg(short='l', long, action, requires="output", verbatim_doc_comment)]
        last_only: bool,
    },
    /// Convert between an asca-web json file and the wsca/rsca format.
    #[clap(subcommand)]
    Conv(Conv),
    // /// Enter tui.
    // Tui
}

#[derive(Debug, Subcommand)]
pub enum Conv {
    /// Convert a word file and rule file into an asca-web json file.
    Asca {
        /// The path of the word file to convert.
        /// - If not provided, asca will look for a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        words: Option<PathBuf>,
        /// The path of the rule file to convert.
        /// - If not provided, asca will look for a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        rules: Option<PathBuf>,
        /// The desired path of the output json file.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        output: Option<PathBuf>,
    },
    /// Convert a json file into separate word and rule files
    Json {
        /// The path of the Json file to convert.
        /// - If not provided, asca will look for a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        path: Option<PathBuf>,
        /// The desired path of the output word file.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        words: Option<PathBuf>,
        /// The desired path of the output rule file.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        rules: Option<PathBuf>,
    },
}

#[derive(Debug, Args)]
#[group(multiple = false)]
pub struct InGroup {
    /// Path to an asca-web json file, mutually exclusive with -r.
    /// - If -w is supplied, those words will be used instead of those defined in the json.
    #[arg(short='j', long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
    pub from_json: Option<PathBuf>,

    /// Path to an rsca file containing the rules to be applied, mutually exclusive with -j.
    /// - If neither -j nor -r is provided, asca will look for a file in the current directory.
    #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
    pub rules: Option<PathBuf>,
}