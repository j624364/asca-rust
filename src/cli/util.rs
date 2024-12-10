use std::{ffi::OsStr, fs, io::{self, Write}, path::PathBuf, process::exit};

use asca::RuleGroup;

#[cfg(windows)]
pub const LINE_ENDING : &str = "\r\n";
#[cfg(not(windows))]
pub const LINE_ENDING : &str = "\n";

pub fn ask(question: &str) -> io::Result<bool> {
    print!(" :: {} [y/N]: ",question);
    io::stdout().flush()?;
    loop {
        let mut buf = String::new();
        let _ = std::io::stdin().read_line(&mut buf);
        match buf.chars().next() {
            Some('y' | 'Y') => return Ok(true),
            Some('\n') | Some('\r') |
            Some('n' | 'N') => return Ok(false),
            _ => println!("yes/no only."),
        }
    }
}

fn get_dir_files(path: &str, valid_extensions: &[&str]) -> io::Result<Vec<PathBuf>> {
    Ok(fs::read_dir(path)?
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
    if valid_extensions.len() == 0 {
        unreachable!()
    } else if valid_extensions.len() == 1 {
        format!(".{}", valid_extensions[0])
    } else if valid_extensions.len() == 2 {
        format!(".{} or .{}", valid_extensions[0], valid_extensions[1])
    } else {
        let mut exts_str = String::new();
        for (i, e) in valid_extensions.iter().enumerate() {
            exts_str.push_str(&format!(".{}", e));
            if i < valid_extensions.len() - 2 {
                exts_str.push_str(", ");
            } else if i == valid_extensions.len() - 2 {
                exts_str.push_str(", or ");
            }
        }
        exts_str
    }
}

// TODO: better errors
pub fn validate_file_exists(maybe_path: Option<PathBuf>, valid_extensions: &[&str], kind: &str) -> io::Result<PathBuf> {
    match maybe_path {
        // Probably don't have to check if path exists as checking if it has an extension should be enough
        Some(path) => match path.extension() {
            Some(ext) => match match_exts(ext, valid_extensions) {
                true => return Ok(path),
                false => {
                    let exts_str = create_ext_list(valid_extensions);
                    println!("File {:?} is not of the right type. Must be {}", path, exts_str)
                },
            },
            None => println!("Given path is not a file"),
        },
        None => {
            let files = get_dir_files(".", valid_extensions)?;
            match files.len().cmp(&1) {
                std::cmp::Ordering::Greater => println!("More than one matching {} file found in current directory. Please specify.", kind),
                std::cmp::Ordering::Less    => println!("No matching {} files found in current directory", kind),
                std::cmp::Ordering::Equal   => return Ok(files[0].clone()),
            }
        }
    }
    exit(1);
}

pub fn write_to_file(path: &PathBuf, content: String, extension: &str) -> io::Result<()> {
    if path.is_file() {
        // if path is an extant file, overwrite on accept
        if path.extension().map_or(false, |ext| ext == extension) {
            if ask(&(format!("File {path:?} already exists, do you wish to overwrite it?")))? {
                fs::write(path, content)?;
                println!("Written to file {:?}", path);
            }
        } else {
            println!("Provided file '{:?}' has the wrong extension. Must be .{:?}", path, extension);
            exit(1);
        }
    } else if path.is_dir() {
        // if path is dir, write to file of <dir>/out.ext
        let p = PathBuf::from(format!("out.{:?}", extension));
        if p.exists() {
            if ask(&(format!("File {p:?} already exists, do you wish to overwrite it?")))? {
                fs::write(&p, content)?;
                println!("Written to file {:?}", p);
            }
        } else {
            fs::write(&p, content)?;
            println!("Written to file {:?}", p);
        }
    } else if path.extension().map_or(false, |ext| ext == extension) {
        // create file <out_path> and write to it
        fs::write(path, content)?;
        println!("Written to file {:?}", path);
    } else {
        println!("Provided file '{:?}' has the wrong extension. Must be .{:?}", path, extension);
        exit(1);
    }

    Ok(())
    
}

pub fn to_rasca_format(rules: Vec<RuleGroup>) -> io::Result<String> {
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