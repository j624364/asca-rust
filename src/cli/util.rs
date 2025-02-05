use std::{
    ffi::OsStr,
    fmt::Debug,
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
};

use asca::RuleGroup;
use colored::Colorize;
// use asca::Transformation;

#[cfg(windows)]
pub const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
pub const LINE_ENDING: &str = "\n";

pub const RULE_FILE_EXT: &str = "rsca";
pub const WORD_FILE_EXT: &str = "wsca";
pub const CONF_FILE_EXT: &str = "asca";
pub const JSON_FILE_EXT: &str = "json";
pub const ALIAS_FILE_EXT: &str = "alias";

pub(super) fn ask(question: &str, auto: Option<bool>) -> io::Result<bool> {
    if let Some(ans) = auto {
        return Ok(ans);
    }
    loop {
        print!(":: {} [y/N]: ", question);
        io::stdout().flush()?;
        let mut buf = String::new();
        let _ = std::io::stdin().read_line(&mut buf);
        match buf.chars().next() {
            Some('y' | 'Y') => return Ok(true),
            Some('\r') | Some('\n') | Some('n' | 'N') => return Ok(false),
            _ => println!("{}", "   yes/no only".yellow()),
        }
    }
}

pub(super) fn get_dir_files(path: &str, valid_extensions: &[&str]) -> io::Result<Vec<PathBuf>> {
    Ok(dir_read(path)?
        // Filter out entries which we couldn't read
        .filter_map(|res| res.ok())
        // Turn entries to paths
        .map(|dir_entry| dir_entry.path())
        // Filter out files with the wrong extension
        .filter_map(|path| {
            if path
                .extension()
                .is_some_and(|ext| match_exts(ext, valid_extensions))
            {
                Some(path)
            } else {
                None
            }
        })
        .collect::<Vec<_>>())
}

fn match_exts(ext: &OsStr, valid_extensions: &[&str]) -> bool {
    for &ve in valid_extensions {
        if ve == ext {
            return true;
        }
    }
    false
}

fn create_ext_list(valid_extensions: &[&str]) -> String {
    if valid_extensions.is_empty() {
        unreachable!()
    } else if valid_extensions.len() == 1 {
        format!(".{}", valid_extensions[0])
    } else if valid_extensions.len() == 2 {
        format!(".{} or .{}", valid_extensions[0], valid_extensions[1])
    } else {
        let mut exts_str = String::new();
        for (i, e) in valid_extensions.iter().enumerate() {
            match i.cmp(&(valid_extensions.len() - 2)) {
                std::cmp::Ordering::Greater => exts_str.push_str(&format!(".{}", e)),
                std::cmp::Ordering::Less => exts_str.push_str(&format!(".{}, ", e)),
                std::cmp::Ordering::Equal => exts_str.push_str(&format!(".{}, or ", e)),
            }
        }
        exts_str
    }
}

pub(super) fn validate_directory(maybe_path: Option<PathBuf>) -> io::Result<PathBuf> {
    match maybe_path {
        Some(path) => {
            if path.is_dir() {
                Ok(path)
            } else {
                Err(util_err(format!(
                    "{} is not a directory",
                    format!("{path:?}").yellow()
                )))
            }
        }
        None => Ok(PathBuf::from(".")),
    }
}

pub(super) fn validate(path: &Path, valid_extensions: &[&str]) -> io::Result<PathBuf> {
    match path.extension() {
        Some(ext) => {
            if match_exts(ext, valid_extensions) {
                Ok(path.to_path_buf())
            } else {
                let exts_str = create_ext_list(valid_extensions);
                Err(util_err(format!(
                    "File {} is not of the right type. Must be {}",
                    format!("{path:?}").yellow(),
                    exts_str
                )))
            }
        }
        None => Err(util_err(format!(
            "Given path {} is not a file",
            format!("{path:?}").yellow()
        ))),
    }
}

pub(super) fn validate_or_get_path(
    maybe_path: Option<&Path>,
    valid_extensions: &[&str],
    kind: &str,
) -> io::Result<PathBuf> {
    match maybe_path {
        // Probably don't have to check if path exists as checking if it has an extension should be enough
        Some(path) => validate(path, valid_extensions),
        None => {
            let files = get_dir_files(".", valid_extensions)?;
            match files.len().cmp(&1) {
                std::cmp::Ordering::Greater => Err(util_err(format!(
                    "More than one matching {} file found in current directory. Please specify.",
                    kind
                ))),
                std::cmp::Ordering::Less => Err(util_err(format!(
                    "No matching {} files found in current directory",
                    kind
                ))),
                std::cmp::Ordering::Equal => Ok(files[0].clone()),
            }
        }
    }
}

pub(super) fn file_open<P: AsRef<Path> + Debug + ?Sized>(path: &P) -> io::Result<fs::File> {
    match fs::File::open(path) {
        Ok(file) => Ok(file),
        Err(e) => {
            println!("Error occured when reading file {path:?}");
            Err(map_io_error(e))
        }
    }
}

pub(super) fn file_write<P: AsRef<Path> + Debug + ?Sized>(
    path: &P,
    content: String,
) -> io::Result<()> {
    if let Err(e) = fs::write(path, content) {
        println!("Error occurred writing to file {path:?}");
        return Err(map_io_error(e));
    }
    println!(":: Wrote to file {:?}", path);
    Ok(())
}

pub(super) fn file_create_write<P: AsRef<Path> + Debug + ?Sized>(
    path: &P,
    content: String,
) -> io::Result<()> {
    if let Err(e) = fs::write(path, content) {
        println!("Error occurred writing to file {path:?}");
        return Err(map_io_error(e));
    }
    println!(":: Created file {path:?} in current directory");
    Ok(())
}

pub(super) fn file_read<P: AsRef<Path> + Debug + ?Sized>(path: &P) -> io::Result<String> {
    match fs::read_to_string(path) {
        Ok(dir) => Ok(dir),
        Err(e) => {
            println!("Error occurred when reading file {path:?}");
            Err(map_io_error(e))
        }
    }
}

pub(super) fn dir_create_all<P: AsRef<Path> + Debug + ?Sized>(path: &P) -> io::Result<()> {
    if let Err(e) = fs::create_dir_all(path) {
        println!("Error occurred when creating {path:?}");
        return Err(map_io_error(e));
    }
    println!(":: Created dir {path:?}");
    Ok(())
}

pub(super) fn dir_read<P: AsRef<Path> + Debug + ?Sized>(path: &P) -> io::Result<fs::ReadDir> {
    match fs::read_dir(path) {
        Ok(dir) => Ok(dir),
        Err(e) => {
            println!("Error occurred when reading file {path:?}");
            Err(map_io_error(e))
        }
    }
}

pub(super) fn dir_create_file<P: AsRef<Path> + Debug + ?Sized>(
    path: &P,
    content: String,
    auto: Option<bool>,
) -> io::Result<()> {
    let path = path.as_ref();
    if path.exists() {
        if ask(
            &(format!("File {path:?} already exists, do you wish to overwrite it?")),
            auto,
        )? {
            file_write(&path, content)
        } else {
            Ok(())
        }
    } else {
        file_create_write(&path, content)
    }
}

pub(super) fn write_to_file(
    path: &Path,
    content: String,
    extension: &str,
    auto: Option<bool>,
) -> io::Result<()> {
    // if path is an extant file, ask for overwrite
    // if path is a dir, create file in dir, if extant ask for overwrite
    // if path does not exist and has a valid extension, create and write
    // else error
    if path.is_file() {
        if path.extension().is_some_and(|ext| ext == extension) {
            if ask(
                &(format!("File {path:?} already exists, do you wish to overwrite it?")),
                auto,
            )? {
                return file_write(path, content);
            }
            Ok(())
        } else {
            Err(util_err(format!(
                "Provided file '{}' has the wrong extension. Must be .{:?}",
                format!("{path:?}").yellow(),
                extension
            )))
        }
    } else if path.is_dir() {
        // if path is dir, write to file of <dir>/out.<extension>
        let mut p = PathBuf::from("out");
        p.set_extension(extension);
        dir_create_file(&p, content, auto)
    } else {
        match path.extension() {
            Some(ext) => {
                if ext == extension {
                    file_write(path, content)
                } else {
                    Err(util_err(format!(
                        "Provided file '{}' has the wrong extension. Must be .{:?}",
                        format!("{path:?}").yellow(),
                        extension
                    )))
                }
            }
            None => Err(util_err(format!(
                "Provided dir '{:?}' does not exist",
                format!("{path:?}").yellow()
            ))),
        }
    }
}

/// Make sure string has no illegal filename characters
pub(super) fn sanitise_str(str: &str) -> String {
    // 26 is arbitrary, but we want to make sure that the file names don't get too long
    str.chars()
        .take(26)
        .map(|ch| match ch {
            ' ' | '*' | '/' | '\\' | '?' | ':' | '|' | '\0' | '<' | '>' | '%' | '"' => '-',
            _ => ch.to_ascii_lowercase(),
        })
        .collect()
}

pub(super) fn to_rsca_format(rules: Vec<RuleGroup>) -> String {
    let mut result = String::new();
    for rg in rules {
        let name_str = format!("@ {}\n", rg.name);

        let mut rule_str = String::new();
        for r in rg.rule {
            rule_str.push_str(&format!("    {}\n", r));
        }

        let mut desc_str = String::new();
        for d in rg.description.split('\n') {
            desc_str.push_str(&format!("# {}\n", d));
        }

        result.push_str(&name_str);
        result.push_str(&rule_str);
        result.push_str(&desc_str);
    }
    result
}

pub(super) fn to_alias(into: Vec<String>, from: Vec<String>) -> String {
    let mut result = String::from("@into\n");

    for to in into {
        result.push_str(&format!("    {}\n", to));
    }

    result.push_str("@from\n");

    for fr in from {
        result.push_str(&format!("    {}\n", fr));
    }

    result
}

fn util_err<S: Into<String>>(message: S) -> io::Error {
    io::Error::other(format!("{} {}", "Error:".bright_red(), message.into()))
}

pub(super) fn map_io_error(error: io::Error) -> io::Error {
    match error.kind() {
        io::ErrorKind::NotFound => util_err("File or directory was not found"),
        io::ErrorKind::PermissionDenied => {
            util_err("Do not have the right permissions to complete this operation")
        }
        _ => error,
    }
}

#[inline]
// Rust's built-in padding algo doesn't account for nonspacing characters for some reason
pub(super) fn fix_combining_char_pad(string: &str) -> usize {
    let mut pad = 0;
    for s in string.chars() {
        // Not an exhaustive list
        match s {
            // Combining Diacritical Marks 
            '\u{0300}'..'\u{036F}' |
            // Combining Diacritical Marks Extended
            '\u{1AB0}'..'\u{1AFF}' |
            // Combining Diacritical Marks Supplement
            '\u{1DC0}'..'\u{1DFF}' |
            // Combining Diacritical Marks for Symbols 
            '\u{20D0}'..'\u{20FF}' |
            // Cyrillic Extended-A
            '\u{2DE0}'..'\u{2DFF}' |
            // Combining Half Marks
            '\u{FE20}'..'\u{FE2F}' |
            // Dakuten & Handakuten
            '\u{3099}' | '\u{309A}' => pad += 1,
            _ => {}
        }
    }

    pad
}

pub(super) fn print_asca_errors(
    err: asca::Error,
    words: &[String],
    rules: &[RuleGroup],
    into: &[String],
    from: &[String],
) {
    match err {
        asca::Error::WordSyn(e) => println!("{}", asca::ASCAError::format_word_error(&e, words)),
        asca::Error::WordRun(e) => println!("{}", asca::ASCAError::format_word_error(&e, words)),
        asca::Error::AliasSyn(e) => {
            println!("{}", asca::ASCAError::format_alias_error(&e, into, from))
        }
        asca::Error::AliasRun(e) => {
            println!("{}", asca::ASCAError::format_alias_error(&e, into, from))
        }
        asca::Error::RuleSyn(e) => println!("{}", asca::ASCAError::format_rule_error(&e, rules)),
        asca::Error::RuleRun(e) => println!("{}", asca::ASCAError::format_rule_error(&e, rules)),
    }
}
