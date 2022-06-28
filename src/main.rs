mod lexer;
mod trie;
mod parser;

use serde_json;
use std::fs;
use std::collections::HashMap;
use std::time::Instant;
use colored::Colorize;

use lexer::Lexer;
use parser::*;
use trie::Trie;


// use crate::lexer::TokenKind;

fn run() {
    todo!();
}

fn main() {
    
    let file = fs::read_to_string("src\\cardinals.json").expect("file should open read only");
    let json: HashMap<String, HashMap<String, Option<usize>>>  = serde_json::from_str(&file).expect("JSON was not well-formatted");
    let mut cardinals_trie = Trie::new();

    for (k,_) in &json {
        cardinals_trie.insert(k.as_str());
    }
    assert!(cardinals_trie.contains("b"));
    // let test= String::from("[]...[] > &");
    // let test= String::from("[+voi, -sg, +PLACE]")
    // let test= String::from("[+voi, -sg, αPLACE]...C > &");
    // let test= String::from("V > [+long] / _C#");

    //let test= String::from("%:[tone:214] > [tone:35] / _%:[tone:214] ");
    //let test= String::from("t͡ɕ...b͡β > &");
    
    
    let mut tokens = Vec::new();
    const ITERS: u32 = 100;
    
    let start  = Instant::now();
    let test= String::from("* / &");

    for _ in 1..ITERS {
        
        let mut lex = Lexer::new(test.clone(), &cardinals_trie);        
        tokens = lex.get_all_tokens();
    }
    let dur = start.elapsed();


    tokens.clone().into_iter().for_each(|t| {
        println!("{}", t);
    });

    println!("Total Time: {:?}", dur);
    println!("Time per Iteration: {:?}", dur/ITERS);


    let mut parser = Parser:: new(tokens, &json);

    let rule = parser.parse();

    match rule {
            Ok(r) => {
                print!("{:?}", r); 
            },
            Err(e) => {
                use SyntaxError::*;
                match e.clone() {
                    ExpectedEndL(t) | ExpectedArrow(t) | ExpectedFeature(t) | ExpectedMatrix(t) => {
                        let start = t.position.start;
                        let end = t.position.end;

                        let marg = "\n    |     ";
                        let arrs = " ".repeat(start) + &"^".repeat(end-start) + "\n";

                        println!("{}", format!("{}{}{}{}{}{}", 
                            format!("Error").bright_red().bold(),
                            format!(": {}", e).bold(), 
                            format!("{}", marg).bright_cyan().bold(), 
                            test, 
                            format!("{}", marg).bright_cyan().bold(), 
                            format!("{}", arrs).bright_red().bold()
                        ))
                    },
                    _ => println!("{}", format!("{}{}", 
                            format!("Error").red().bold(),
                            format!(": {}", e).bold()
                        ))
                }
            }
        }

    // println!("\n--------------------------------\n");
    // println!("{:#?}", json.get("k"));
    // println!("\n--------------------------------\n");
    // println!("{}", cardinals_trie);
    // println!("{:#?}", cardinals_trie.find("p\u{361}"));
    
}

// let mut trie = trie::Trie::new();
// trie.insert("a");
// trie.insert("to");
// trie.insert("tea");
// trie.insert("apples");
// trie.insert("an");
// trie.insert("test");
// trie.insert("tea");
// assert!(trie.contains("test"));
// assert!(trie.contains("to"));
// assert!(trie.contains("tea"));
// assert!(!trie.contains("airplane"));
// println!("{}", trie);
// assert_eq!(trie.find("te"), vec!["test", "tea"]);
// assert_eq!(trie.find("a"), vec!["a", "apples", "an"]);
// trie.insert("test");
// trie.insert("test");
// assert_eq!(trie.length(), 7);

// let mut lexer  = Lexer::new(test, &cardinals_trie);
// let mut parser = Parser::new(lexer, &json);

// let mut parser = Parser::new(Lexer::new(test, &cardinals_trie), &json);