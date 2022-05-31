use serde_json::{Value, Map};
use std::fs;
use std::collections::HashMap;

mod lexer;

pub struct CardinalValue {
    grapheme: String,
    root: Option<usize>,
    manner: Option<usize>,
    laryngeal: Option<usize>,
    labial: Option<usize>,
    coronal: Option<usize>,
    dorsal: Option<usize>,
    pharyngeal: Option<usize>,
}

fn main() {
    
    // let test= String::from("[]...[] > &");
    // let test= String::from("[+voi, -sg, +PLACE]")
    let test= String::from("[+voi, -sg, αPLACE]...C > &");

    let mut lex = lexer::Lexer::new(test);

    let next = lex.get_all_tokens();

    next.into_iter().for_each(|t| {
        println!("{:?}", t);
    });

    println!("\n\n");

    let file = fs::read_to_string("src\\cardinals.json").expect("file should open read only");
  
    let json: HashMap<String, HashMap<String, Option<usize>>>  = serde_json::from_str(&file).expect("JSON was not well-formatted");

    //let obj: Map<String, Value> = json.as_object().unwrap().clone();


    println!("{:#?}", json.get("k"))
}
