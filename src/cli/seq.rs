use std::{collections::HashMap, io, path::{Path, PathBuf}};
use colored::Colorize;

use asca::RuleGroup;
use super::{config::{lexer::Lexer, parser::Parser}, parse::parse_wsca, util::{self, CONF_FILE_EXT, WORD_FILE_EXT}};


#[derive(Debug, Clone)]
pub struct ASCAConfig {
    pub tag: String,
    pub from: Option<String>,
    pub words: Vec<String>,
    pub entries: Vec<Entry>
}

impl ASCAConfig {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self { tag: String::new(), from: None, words: vec![], entries: Vec::new() }
    }
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: PathBuf,
    pub rules: Vec<RuleGroup>
}

impl Entry {
    pub fn from(name: &Path, rules: &[RuleGroup]) -> Self {
        Self { name: name.to_path_buf(), rules: rules.to_vec() }
    }
}

pub enum RuleFilter {
    Only(String),
    Without(String),
    OnlyMult(Vec<String>),
    WithoutMult(Vec<String>),
}

type SeqTrace = Vec<Vec<String>>;


/// Read config file and return result
fn get_config(dir: &Path) -> io::Result<Vec<ASCAConfig>> {
    let maybe_conf = util::get_dir_files(dir.to_str().unwrap(), &[CONF_FILE_EXT])?;

    if maybe_conf.is_empty() {
        return Err(io::Error::other(format!("Error: No config file found in directory {dir:?}")))
    } else if maybe_conf.len() > 1 {
        return Err(io::Error::other(format!("Error: Multiple config files found in directory {dir:?}")))
    }

    let tokens = Lexer::new(&util::file_read(maybe_conf[0].as_path())?.chars().collect::<Vec<_>>()).tokenise()?;

    Parser::new(tokens, maybe_conf[0].as_path()).parse()
}

/// Determine which word file to use, validate, and parse it into a vec of word strings
fn get_words(rule_seqs: &[ASCAConfig], dir: &Path, words_path: &Option<PathBuf>, conf: &ASCAConfig, seq_cache: &mut HashMap<String, Vec<String>>) -> io::Result<Vec<String>> {
    let mut words = if let Some(from_tag) = &conf.from {
        if let Some(x) = seq_cache.get(from_tag) {
            x.clone()
        } else {
            let Some(seq) = rule_seqs.iter().find(|c| c.tag == *from_tag) else {
                return Err(io::Error::other(format!("Error: Could not find tag '{from_tag}' in config")))
            };
            if let Some((t, _)) = run_sequence(rule_seqs, dir, words_path, seq, seq_cache)? {
                let w = t.last().unwrap().clone();
                seq_cache.insert(seq.tag.clone(), w.clone());
                w
            } else {
                return Ok(vec![])
            }
        }
    } else { 
        vec![] 
    };

    if let Some(ref wp) = words_path {
        let (mut w, _) = parse_wsca(&util::validate_file_exists(Some(wp), &[WORD_FILE_EXT, "txt"], "word")?)?;
        words.append(&mut w);
    } else if !conf.words.is_empty() {
        for w in &conf.words {
            let mut wp = dir.to_path_buf();
            wp.push(w);
            wp.set_extension(WORD_FILE_EXT);
            let (mut w_file, _) = parse_wsca(&util::validate_file_exists(Some(&wp), &[WORD_FILE_EXT, "txt"], "word")?)?;

            if !words.is_empty() {
                words.push("".to_string());
            }

            words.append(&mut w_file);
        }
    } 
    
    if words.is_empty() {
        return Err(io::Error::other(format!("Error: No input words defined for config '{}'.\nYou can specify a word file at runtime with the -w option", conf.tag)))
    }

    Ok(words)
}

/// Handle writing the result to file
fn output_result(dir: &Path, tag: &str, trace: &[Vec<String>], seq_names: &[PathBuf], overwrite: Option<bool>, output_all: bool) -> io::Result<()> {
    // Create <out/tag> subfolder within <dir> if doesn't exist
    // Create files for each seq, <seq_name>.wsca, and write to each
    let mut path = dir.to_path_buf();
    path.push("out");
    path.push(tag);
    
    if !path.exists() {
        util::dir_create_all(&path)?;
    }

    if output_all {
        for (seq, name) in seq_names.iter().enumerate() {
            let content = trace[seq+1].join("\n");
            let mut p = path.clone();
            let name = format!("{}_{}", seq+1, name.file_name().unwrap().to_os_string().into_string().unwrap());
            p.push(name);
            p.set_extension(WORD_FILE_EXT);
            util::write_to_file(&p, content, WORD_FILE_EXT, overwrite)?;
        }
    } else {
        let name = seq_names.last().unwrap().file_name().unwrap();
        let content = trace.last().unwrap().join("\n");
        let mut p = path.clone();
        p.push(name);
        p.set_extension(WORD_FILE_EXT);
        util::write_to_file(&p, content, WORD_FILE_EXT, overwrite)?;
    }

    Ok(())
}

// Handle printing result to the terminal
fn print_result(trace: &[Vec<String>], tag: &str, all_steps: bool) {
    debug_assert!(!trace.is_empty());
    println!("\nOUTPUT - {}", tag);
    let arr = "=>".bright_red().bold();

    let num_steps = trace.len();
    let num_words = trace[0].len();

    for word in 0..num_words {
        if trace[0][word].is_empty() {
            println!();
            continue;
        }
        // Concat start word
        let mut str = format!("{}", &trace[0][word].bright_blue().bold());
        if all_steps {
            // Concat intermediate steps
            for step in trace.iter().take(num_steps-1).skip(1) {
                str = format!("{str} {arr} {}", &step[word]);
            }
        }
        // Concat final result
        str = format!("{str} {arr} {}", &trace[num_steps-1][word].bright_green().bold());
        println!("{}", str)
    }
    println!();
}

/// Pass a sequence to ASCA and return a trace and the name of each file that was used
pub fn run_sequence(rule_seqs: &[ASCAConfig], dir: &Path, words_path: &Option<PathBuf>, seq: &ASCAConfig, seq_cache: &mut HashMap<String, Vec<String>>) -> io::Result<Option<(SeqTrace, Vec<PathBuf>)>> {
    let mut files = Vec::new();
    let mut trace = Vec::new();

    let words = get_words(rule_seqs, dir, words_path, seq, seq_cache)?;
    if words.is_empty() { return Ok(None) }
    trace.push(words.clone());
    for (i, entry) in seq.entries.iter().enumerate() {
        files.push(entry.name.clone());
        match asca::run(&entry.rules, &trace[i]) {
            Ok(res) => trace.push(res),
            Err(err) => {
                util::print_asca_errors(err, &words, &entry.rules);
                return Ok(None)
            },
        }
    }
    Ok(Some((trace, files)))
}

/// Run a given sequence, then deal with output if necessary
fn handle_sequence(rule_seqs: &[ASCAConfig], seq_cache: &mut HashMap<String, Vec<String>>, dir_path: &Path, words_path: &Option<PathBuf>, seq: &ASCAConfig, flags: &SeqFlags) -> io::Result<()> {   
    if let Some((trace, files)) = run_sequence(rule_seqs, dir_path, words_path, seq, seq_cache)? {
        if trace.is_empty() {
            return Err(io::Error::other(format!("Error: No words in input for tag '{}'", seq.tag)))
        }
        seq_cache.insert(seq.tag.clone(), trace.last().unwrap().clone());

        print_result(&trace, &seq.tag, flags.all_steps);
        if flags.output {
            return output_result(dir_path, &seq.tag, &trace, &files, flags.overwrite, flags.output_all)
        } 
    }
    Ok(())
}

pub(crate) fn run(maybe_dir_path: Option<PathBuf>, words_path: Option<PathBuf>, maybe_tag: Option<String>, output: bool, overwrite: Option<bool>, output_all: bool, all_steps: bool) -> io::Result<()> {
    let dir_path = util::validate_directory(maybe_dir_path)?;
    let rule_seqs = get_config(&dir_path)?;

    let mut seq_cache = HashMap::new();

    let flags = SeqFlags { output, overwrite, output_all, all_steps };

    if let Some(tag) = maybe_tag {
        // Would be better if this was a hashmap
        // but even if the file was unreasonably long as to cause slowdowns, we'd have other issues
        let Some(seq) = rule_seqs.iter().find(|c| c.tag == tag) else {
            return Err(io::Error::other(format!("Error: Could not find tag '{tag}' in config")))
        };
        handle_sequence(&rule_seqs, &mut seq_cache, &dir_path, &words_path, seq, &flags)
    } else {
        // Run all sequences
        for seq in &rule_seqs {
            handle_sequence(&rule_seqs, &mut seq_cache, &dir_path, &words_path, seq, &flags)?;
        }
        Ok(())
    }
}

struct SeqFlags {
    output: bool,
    overwrite: Option<bool>,
    output_all: bool,
    all_steps: bool,
}