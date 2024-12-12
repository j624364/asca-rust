use std::{ffi::OsStr, fmt::Debug, fs, io::{self, Write}, path::{Path, PathBuf}};

use colored::Colorize;
use asca::RuleGroup;

#[cfg(windows)]
pub const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
pub const LINE_ENDING: &str = "\n";

pub const RULE_FILE_ENDING: &str = "rsca";
pub const WORD_FILE_ENDING: &str = "wsca";
pub const CONF_FILE_ENDING: &str = "asca";

pub(super) fn ask(question: &str, auto: Option<bool>) -> io::Result<bool> {
    if let Some(ans) = auto {
        return Ok(ans)
    }
    loop {
        print!(":: {} [y/N]: ", question);
        io::stdout().flush()?;
        let mut buf = String::new();
        let _ = std::io::stdin().read_line(&mut buf);
        match buf.chars().next() {
            Some('y' | 'Y') => return Ok(true),
            Some('\n') | Some('\r') |
            Some('n' | 'N') => return Ok(false),
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
            if path.extension().map_or(false, |ext| match_exts(ext, valid_extensions)) {
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
            return true
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
            match i.cmp(&(valid_extensions.len()-2)) {
                std::cmp::Ordering::Greater => exts_str.push_str(&format!(".{}", e)),
                std::cmp::Ordering::Less    => exts_str.push_str(&format!(".{}, ", e)),
                std::cmp::Ordering::Equal   => exts_str.push_str(&format!(".{}, or ", e)),
            }
        }
        exts_str
    }
}

pub(super) fn validate_file_exists(maybe_path: Option<PathBuf>, valid_extensions: &[&str], kind: &str) -> io::Result<PathBuf> {
    match maybe_path {
        // Probably don't have to check if path exists as checking if it has an extension should be enough
        Some(path) => match path.extension() {
            Some(ext) => if match_exts(ext, valid_extensions) {
                Ok(path)
            } else {
                let exts_str = create_ext_list(valid_extensions);
                Err(io::Error::other(format!("File {:?} is not of the right type. Must be {}", path, exts_str)))
            },
            None => Err(io::Error::other(format!("Given path {path:?} is not a file"))),
        },
        None => {
            let files = get_dir_files(".", valid_extensions)?;
            match files.len().cmp(&1) {
                std::cmp::Ordering::Greater => Err(io::Error::other(format!("More than one matching {} file found in current directory. Please specify.", kind))),
                std::cmp::Ordering::Less    => Err(io::Error::other(format!("No matching {} files found in current directory", kind))),
                std::cmp::Ordering::Equal   => Ok(files[0].clone()),
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

pub(super) fn file_write<P: AsRef<Path> + Debug + ?Sized>(path: &P, content: String) -> io::Result<()> {
    if let Err (e) = fs::write(path, content) {
        println!("Error occurred writing to file {path:?}");
        return Err(map_io_error(e))
    }
    println!(":: Wrote to file {:?}", path);
    Ok(())
}

pub(super) fn file_create_write<P: AsRef<Path> + Debug + ?Sized>(path: &P, content: String) -> io::Result<()> {
    if let Err (e) = fs::write(path, content) {
        println!("Error occurred writing to file {path:?}");
        return Err(map_io_error(e))
    }
    println!(":: Created file `{path:?}` in current directory");
    Ok(())
}

pub(super) fn file_read<P: AsRef<Path> + Debug + ?Sized>(path: &P) -> io::Result<String> {
    match fs::read_to_string(path) {
        Ok(dir) => Ok(dir),
        Err(e) => {            
            println!("Error occurred when reading file {path:?}");
            Err(map_io_error(e))
        },
    }
}

pub(super) fn dir_create_all<P: AsRef<Path> + Debug + ?Sized>(path: &P) -> io::Result<()> {
    if let Err(e) = fs::create_dir_all(path) {            
        println!("Error occurred when creating {path:?}");
        return Err(map_io_error(e))
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
        },
    }
}

pub(super) fn write_to_file(path: &Path, content: String, extension: &str, auto: Option<bool>) -> io::Result<()> {
    // if path is an extant file, ask for overwrite
    // if path is a dir, create file in dir, if extant ask for overwrite
    // if path does not exist and has a valid extension, create and write
    // else error
    if path.is_file() {
        if path.extension().map_or(false, |ext| ext == extension) {
            if ask(&(format!("File {path:?} already exists, do you wish to overwrite it?")), auto)? {
                return file_write(path, content)
            }
            Ok(())
        } else {
            Err(io::Error::other(format!("Provided file '{:?}' has the wrong extension. Must be .{:?}", path, extension)))
        }
    } else if path.is_dir() {
        // if path is dir, write to file of <dir>/out.ext
        let mut p = PathBuf::from("out");
        p.set_extension(extension);
        if p.exists() {
            if ask(&(format!("File {p:?} already exists, do you wish to overwrite it?")), auto)? {
                return file_write(&p, content)
            }
            Ok(())
        } else {
            file_write(&p, content)
        }
    } else {
        match path.extension() {
            Some(ext) => if ext == extension {
                file_write(path, content)
            } else {
                Err(io::Error::other(format!("Provided file '{:?}' has the wrong extension. Must be .{:?}", path, extension)))
            },
            None => Err(io::Error::other(format!("Provided dir '{:?}' does not exist", path))),
        } 
    }
}

pub(super) fn to_rsca_format(rules: Vec<RuleGroup>) -> io::Result<String> {
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
    Ok(result)
}

pub(super) fn map_io_error(error: io::Error) -> io::Error {
    match error.kind() {
        io::ErrorKind::NotFound => io::Error::other("File or directory was not found"),
        io::ErrorKind::PermissionDenied => io::Error::other("Do not have the right permissions to complete this operation"),
        _ => error,
    }
}