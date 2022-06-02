use std::collections::HashMap;
use crate::lexer::{Token, TokenKind, Position};


#[derive(Debug)]
pub enum ParseKind {
    Segment,
    Parameter,
    Expr,
    EmptySet,
    Boundary,
    Ellipsis,
    Set,
    Matrix,
}

#[derive(Debug)]
pub struct ParseItem {
    kind: ParseKind,
    value: Vec<Token>,
    pos: Position,
}

impl ParseItem {

    pub fn new(k: ParseKind, v: Vec<Token>, p: Position) -> Self {
        Self { kind: k, value: v, pos: p }
    }
}


#[derive(Debug)]
pub struct Rule {
    pub input: Vec<ParseItem>,
    pub output: Vec<ParseItem>,
    pub context: Option<Vec<ParseItem>>,
    pub except: Option<Vec<ParseItem>>,
}

impl Rule {

    pub fn new(i: Vec<ParseItem>, o: Vec<ParseItem>, c :Option<Vec<ParseItem>>, e :Option<Vec<ParseItem>>) -> Self {
        Self { input: i, output: o, context: c, except: e }
    }
}

pub struct Parser {
    token_list: Vec<Token>,
    cardinals: HashMap<String, HashMap<String, Option<usize>>>, 
    pos: usize,
    curr_tkn: Token,

    forw_pos: usize,
    forw_tkn: Token,
}

impl Parser {
    
    pub fn new(lst: Vec<Token>, c: &HashMap<String, HashMap<String, Option<usize>>>) -> Self {
        let mut s = Self { 
            token_list: lst, 
            cardinals: c.clone(),
            pos: 0, 
            forw_pos: 0, 
            curr_tkn: Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { start: 0, end: 1 } }, 
            forw_tkn: Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { start: 0, end: 1 } }, 
        };
       
        s.curr_tkn = s.token_list[s.pos].clone();
        s.forw_tkn = s.token_list[s.pos].clone();

        s
    }

    fn lookahead(&mut self) {
        self.forw_pos += 1;

        self.forw_tkn = if self.has_more_tokens() {
            self.token_list[self.forw_pos].clone()
        } else {
            Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { start: self.forw_pos, end: self.forw_pos+1 } }
        }
    }

    fn has_more_tokens(&self) -> bool { self.forw_pos < self.token_list.len() }

    fn advance(&mut self) {
        self.pos = self.forw_pos;
        self.curr_tkn = self.forw_tkn.clone();
    }

    fn retreat(&mut self) {
        self.forw_pos = self.pos;
        self.forw_tkn = self.curr_tkn.clone();
    }

    fn expect(&mut self, kind: TokenKind) -> bool {
        match self.forw_tkn.kind {
            kind => {
                self.lookahead();
                return true
            }
            _ => return false
        }
    }

    fn get_except(&mut self) -> Option<Vec<ParseItem>> {
        if self.expect(TokenKind::Pipe) {
            todo!("Exceptions not yet implemented")
        } else {
            None
        }
    }

    fn get_context(&mut self) -> Option<Vec<ParseItem>> {
        if self.expect(TokenKind::Slash) {
            todo!("Context not yet implemented")
        } else {
            None
        }
    }

    fn get_output(&mut self) -> Option<Vec<ParseItem>> {
        todo!("Output not yet implemented")
    }

    fn get_input_expr(&mut self) -> Option<ParseItem> {
        todo!() // (TERM / ELLIPSIS)+
    }

    fn get_empty(&mut self) -> Option<ParseItem> {

        if !self.expect(TokenKind::Star) && !self.expect(TokenKind::EmptySet) {
            return None
        }
        let token = self.forw_tkn.clone();
        self.lookahead();
        self.advance();

        return Some(ParseItem::new(ParseKind::EmptySet, vec![token.clone()], token.position));

    }

    fn get_input(&mut self) -> Option<Vec<ParseItem>> {
        todo!("Input not yet implemented") // inp_expr / empty
    }

    
    fn rule(&mut self) -> Option<Rule> {

        let input = self.get_input();

        match input {
            Some(_) => {},
            None => panic!("Expected output, received unknown expression") 
        }

        if !self.expect(TokenKind::Arrow) || !self.expect(TokenKind::GreaterThan) {
            match input.unwrap()[0].kind {
                ParseKind::EmptySet => panic!("The input of an insertion rule must contain only `*` or `∅`"),
                _ => panic!("Expected >, -> or =>, received {}", self.forw_tkn.value)
            }
            
        }

        let output = self.get_output();

        match output {
            Some(_) => {},
            None => panic!("Expected output, received unknown expression") 
        }

        if self.expect(TokenKind::Eol) {
            return Some(Rule::new(input.unwrap(), output.unwrap(), None, None))
        }

        let context = self.get_context();
        
        let except = self.get_except();

        if self.expect(TokenKind::Eol) {
            return Some(Rule::new(input.unwrap(), output.unwrap(), context, except))
        }

        panic!("Unknown expression");

    }
    

    pub fn parse(&mut self) -> Rule {
        let rule = self.rule();

        match rule {
            Some(r) => {
                print!("{:?}", r); 
                return r
            },
            None => panic!("Could not parse rule!")
        }

    }

}