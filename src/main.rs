use serde_json::{Value, Map};
use std::fs;
use std::collections::HashMap;

mod lexer;
mod trie;

fn main() {
    
    // let test= String::from("[]...[] > &");
    // let test= String::from("[+voi, -sg, +PLACE]")
    let test= String::from("[+voi, -sg, αPLACE]...C > &");

    let mut lex = lexer::Lexer::new(test);

    let tokens = lex.get_all_tokens();

    tokens.into_iter().for_each(|t| {
        println!("{:?}", t);
    });

    println!("\n--------------------------------\n");

    let file = fs::read_to_string("src\\cardinals.json").expect("file should open read only");
  
    let json: HashMap<String, HashMap<String, Option<usize>>>  = serde_json::from_str(&file).expect("JSON was not well-formatted");

    //let obj: Map<String, Value> = json.as_object().unwrap().clone();


    println!("{:#?}", json.get("k"));

    println!("\n--------------------------------\n");

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

    let mut cardinals_trie = trie::Trie::new();

    for (k, v) in json {
        cardinals_trie.insert(k.as_str());
    }

    println!("{}", cardinals_trie);

    println!("{:#?}", cardinals_trie.find("p\u{361}ɸ"));

}
