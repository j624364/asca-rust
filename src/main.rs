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

use lexer ::*;
use parser::*;
use trie  ::*;
use word  ::*;
use rule  ::*;
use error ::*;

const FILE: &str = include_str!("cardinals.json");
lazy_static! {
    static ref JSON: HashMap<String, Segment> = serde_json::from_str(&FILE).unwrap();
    static ref CARDINALS: Trie = {
        let mut m = Trie::new();
        JSON.iter().for_each(|(k,_)| {
            m.insert(k.as_str());
        });

        m
    };    
}

fn parse_rules(unparsed_rules: Vec<String>) -> Result<Vec<Rule>,RuleSyntaxError> {
    
    let mut rules: Vec<Rule> = vec![];
    for r in unparsed_rules{
        let tokens = Lexer::new(r).get_all_tokens(); // should return error
        
        rules.push(Parser:: new(tokens).parse()?);
    }

    Ok(rules)
}

fn parse_words(unparsed_words: Vec<String>) -> Result<Vec<Word>,WordSyntaxError> {
    
    let mut words: Vec<Word> = vec![];
    for w in unparsed_words {
        words.push(Word::new(w)?);
    }

    Ok(words)
}

fn apply_rules(rules: Vec<Rule>, words: Vec<Word>, trace: bool) -> Result<(Vec<Word>, Vec<Vec<String>>), RuntimeError> {
    // todo: work out tracing

    let mut transformed_words: Vec<Word> = vec![];

    let mut traced_words: Vec<Vec<String>> = vec![];

    for (i, w) in words.iter().enumerate() {
        let mut wb = w.clone();

        if trace {
            traced_words.push(vec![]);
            traced_words[i].push(word_to_string(wb.clone())?);

        }

        for r in &rules {
            wb = r.apply(wb.clone())?;

            if trace {
                traced_words[i].push(word_to_string(wb.clone())?);
            }
        }
        transformed_words.push(wb);
    }

    Ok((transformed_words, traced_words))
}

fn word_to_string(word: Word) -> Result<String, RuntimeError> {
    todo!();
}

fn words_to_string(words: Vec<Word>) -> Result<Vec<String>, RuntimeError> {

    let mut wrds_str: Vec<String> = vec![];

    for w in words {
        wrds_str.push(word_to_string(w)?);
    }

    Ok(wrds_str)
}

fn run(unparsed_rules: Vec<String>, unparsed_words: Vec<String>, trace: bool) -> Result<(Vec<String>, Vec<Vec<String>>), Error> {
    let words = parse_words(unparsed_words)?;
    let rules = parse_rules(unparsed_rules)?;

    let (res, trace_res) = apply_rules(rules, words, trace)?;

    Ok((words_to_string(res)?, trace_res))

}

fn main2() {
    let unparsed_rules: Vec<String> = vec![
        String::from("r > l"),
    ];

    let unparsed_words: Vec<String> = vec![
        String::from("a.ri"),
    ];

    let trace = false;

    let res = run(unparsed_rules, unparsed_words, trace);
}

fn main() {
    // let test = String::from("[]...[] > &");
    // let test = String::from("[+voi, -sg, αPLACE]...C > &");
    // let test = String::from("V > [+long] / _C#");
    // let test = String::from("%:[tone:214] > [tone:35] / _%:[tone:214] ");
    // let test = String::from("t͡ɕ...b͡β > &");
    // let test = String::from("r...V > &");
    // let test = String::from("V:[+syll]...l > & / _,C");
    // let test = String::from("C=1 V=2 > 2 1  / _C");
    // let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");
    
    // let w = Word::new("ˌna.kiˈsa".to_owned()).unwrap();
    let w = Word::new("a.ki.ra".to_owned()).unwrap();
    // let mut w = Word::new("ˌna.kiˈ:a".to_owned()).unwrap();
    println!("{}", w);
    
    let mut tokens;
    const ITERS: u32 = 1;
    
    let start  = Instant::now();
    // let test= String::from("t͡ɕ...b͡β > &");
    // let test= String::from("r > l");
    let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");
    let mut maybe_rule: Result<Rule, RuleSyntaxError> = Err(RuleSyntaxError::EmptyInput);

    for _ in 0..ITERS {
        
        let mut lex = Lexer::new(test.clone());
        tokens = lex.get_all_tokens();
    
        // tokens.clone().into_iter().for_each(|t| {
        //         println!("{}", t);
        //     });
        let mut parser = Parser:: new(tokens);

        maybe_rule = parser.parse();
    }

    let dur = start.elapsed();
    println!("\nTotal Time: {:?}", dur);
    println!("Average Time per Iteration: {:?}\n", dur/ITERS);

    match maybe_rule {
            Ok(r) => {
                print!("{}", r); 
                match r.apply(w) {
                    Ok(_) => {},
                    Err(_) => {}
                }
            },
            Err(e) => {
                use RuleSyntaxError::*;
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
                    ExpectedRightBracket(t) |
                    BadSyllableMatrix(t)  => {
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