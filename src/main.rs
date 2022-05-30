mod lexer;

fn main() {
    
    // let test= String::from("[]...[] > &");
    // let test= String::from("[+voi, -sg, +PLACE]")
    let test= String::from("[+voi, -sg, αPLACE]");

    let mut lex = lexer::Lexer::new(test);

    let next = lex.get_all_tokens();

    next.into_iter().for_each(|t| {
        println!("{:?}", t);
    });
}
