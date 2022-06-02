mod lexer;
mod trie;
mod parser;

use serde_json;
use std::fs;
use std::collections::HashMap;

use lexer::{Lexer, Token};
use parser::Parser;
use trie::Trie;

// use crate::lexer::TokenKind;

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
    let test= String::from("%:[tone:214] > [tone:35] / _%:[tone:214] ");

    let mut lex = Lexer::new(test, &cardinals_trie);

    // let mut token_list: Vec<Token> =  Vec::new();
    // loop {
    //     let next_token = lex.get_next_token();

    //     println!("{:?}", next_token);

    //     match next_token.kind {
    //         TokenKind::Eol => {
    //             token_list.push(next_token);
    //             break
    //         },
    //         _ => {token_list.push(next_token);}
    //     }
    // }
    
    let tokens = lex.get_all_tokens();
    tokens.clone().into_iter().for_each(|t| {
        println!("{:?}", t);
    });

    let mut parser = Parser:: new(tokens, &json);

    let rule = parser.parse();

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