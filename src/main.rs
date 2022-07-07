mod lexer;
mod trie;
mod parser;
mod word;


use serde_json;
use std::fs;
use std::collections::HashMap;
use std::time::Instant;
use colored::Colorize;

use lexer::*;
use parser::*;
use trie::*;
use word::*;


// use crate::lexer::TokenKind;

fn run() {
    todo!();
}

fn main() {
    
    let file = fs::read_to_string("src\\cardinals.json").expect("Err: Could not open Cardinals JSON file");
    let json: HashMap<String, HashMap<String, Option<usize>>>  = serde_json::from_str(&file).expect("Err: Could not parse Cardinals JSON file");
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

    let mut w = Word::new("ˈnaˌki.sa".to_owned());

    w.setup_text();
    
    
    let mut tokens;
    const ITERS: u32 = 1;
    
    let start  = Instant::now();
    let test= String::from("V:[+syll]...l > & / _,C");
    let mut rule: Result<Rule, SyntaxError> = Err(SyntaxError::EmptyInput);

    for _ in 0..ITERS {
        
        let mut lex = Lexer::new(test.clone(), &cardinals_trie);        
        tokens = lex.get_all_tokens();
    
        // tokens.clone().into_iter().for_each(|t| {
            //     println!("{}", t);
            // });
        let mut parser = Parser:: new(tokens, &json);

        rule = parser.parse();
    }

    let dur = start.elapsed();
        println!("Total Time: {:?}", dur);
        println!("Average Time per Iteration: {:?}", dur/ITERS);


    match rule {
            Ok(r) => {
                print!("{}", r); 
            },
            Err(e) => {
                use SyntaxError::*;
                match e.clone() {
                    OptMathError(t, _, _) | 
                    UnknownChar(t) | 
                    ExpectedEndL(t) | 
                    ExpectedArrow(t) | 
                    ExpectedComma(t) | 
                    ExpectedColon(t) | 
                    ExpectedMatrix(t) | 
                    ExpectedSegment(t) | 
                    ExpectedFeature(t) | 
                    ExpectedUnderline(t) | 
                    ExpectedRightBracket(t)  => {
                        let start = t.position.start;
                        let end = t.position.end;

                        let marg = "\n    |     ";
                        let arrs = " ".repeat(start) + &"^".repeat(end-start) + "\n";

                        println!("{}", format!("{}{}{}{}{}{}", 
                            format!("Syntax Error").bright_red().bold(),
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