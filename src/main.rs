mod lexer;
mod trie;
mod parser;
mod word;
mod rule;
mod error;

use serde_json;
use std::{collections::HashMap, time::Instant};
use colored::Colorize;
use lazy_static::lazy_static;

use lexer::*;
use parser::*;
use trie::*;
use word::*;
use rule::*;
use error::*;

// fn run() {
//     todo!();
// }

const FILE: &str = include_str!("cardinals.json");
lazy_static! {
    static ref JSON: HashMap<String, HashMap<String, Option<usize>>> = serde_json::from_str(&FILE).unwrap();
    static ref CARDINALS: Trie = {
        let mut m = Trie::new();
        JSON.iter().for_each(|(k,_)| {
            m.insert(k.as_str());
        });

        m
    };    
}



fn main() {
    // let test = String::from("[]...[] > &");
    // let test = String::from("[+voi, -sg, αPLACE]")
    // let test = String::from("[+voi, -sg, αPLACE]...C > &");
    // let test = String::from("V > [+long] / _C#");
    // let test = String::from("%:[tone:214] > [tone:35] / _%:[tone:214] ");
    // let test = String::from("t͡ɕ...b͡β > &");

    let w = Word::new("ˌna.kiˈsa".to_owned()).unwrap();
    // let mut w = Word::new("ˌna.kiˈ:a".to_owned()).unwrap();
    println!("{}", w);
        
    let mut tokens;
    const ITERS: u32 = 1;
    
    let start  = Instant::now();
    // let test = String::from("V:[+syll]...l > & / _,C");
    // let test = String::from("C=1 V=2 > 2 1  / _C");
    let test= String::from("t͡ɕ...b͡β > &");
    // let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");
    let mut rule: Result<Rule, SyntaxError> = Err(SyntaxError::EmptyInput);

    for _ in 0..ITERS {
        
        let mut lex = Lexer::new(test.clone());        
        tokens = lex.get_all_tokens();
    
        // tokens.clone().into_iter().for_each(|t| {
        //         println!("{}", t);
        //     });
        let mut parser = Parser:: new(tokens);

        rule = parser.parse();
    }


    let dur = start.elapsed();
    println!("\nTotal Time: {:?}", dur);
    println!("Average Time per Iteration: {:?}\n", dur/ITERS);


    match rule {
            Ok(r) => {
                print!("{}", r); 
            },
            Err(e) => {
                use SyntaxError::*;
                match e.clone() {
                    OptMathError(t, _, _) | 
                    UnknownIPA(t) | 
                    UnknownChar(t) | 
                    UnknownVariable(t) | 
                    ExpectedEndL(t) | 
                    ExpectedArrow(t) | 
                    ExpectedComma(t) | 
                    ExpectedColon(t) | 
                    ExpectedMatrix(t) | 
                    ExpectedSegment(t) | 
                    ExpectedFeature(t) | 
                    ExpectedVariable(t) | 
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
    
}