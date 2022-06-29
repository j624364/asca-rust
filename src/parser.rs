use std::{collections::HashMap, fmt};
use super::lexer::*;

#[derive(Debug, Clone)]
pub enum RuntimeError { 

}



#[derive(Debug, Clone)]
pub enum SyntaxError {
    UnknownChar(Token),
    ExpectedEndL(Token),
    ExpectedArrow(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ExpectedMatrix(Token),
    ExpectedSegment(Token),
    ExpectedFeature(Token),
    ExpectedRightBracket(Token),
    InsertErr,
    DeleteErr,
    EmptyInput,
    EmptyOutput,
    OptMathError(Token, usize, usize),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OptMathError(_, l, h)  => write!(f, "Second argument '{}' must be >= first argument '{}'", h, l),
            Self::UnknownChar(token)          => write!(f, "No known primative'{}'. Known primatives are (C)onsonant, (O)bstruent, (S)onorant, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::ExpectedEndL(token)         => write!(f, "Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)        => write!(f, "Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)        => write!(f, "Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)        => write!(f, "Expected ':', but received '{}'", token.value),
            Self::ExpectedMatrix(token)       => write!(f, "Expected '[', but received '{}'", token.value),
            Self::ExpectedSegment(token)      => write!(f, "Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedFeature(token)      => write!(f, "{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedRightBracket(token) => write!(f, "Expected ')', but received '{}'", token.value),
            Self::InsertErr   => write!(f, "The input of an insertion rule must only contain `*` or `∅`"),
            Self::DeleteErr   => write!(f, "The output of a deletion rule must only contain `*` or `∅`"),
            Self::EmptyInput  => write!(f, "Input cannot be empty. Use `*` or '∅' to indicate insertion"),
            Self::EmptyOutput => write!(f, "Output cannot be empty. Use `*` or '∅' to indicate deletion"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseKind {
    EmptySet   (Token),
    Boundary   (Token),
    Ellipsis   (Token),
    Metathesis (Token),
    IPA        (Token),
    Matrix     (Vec<Token>),
    Syllable   (Vec<Token>),
    Set        (Vec<Item>),
    Optional   (Vec<Item>, usize, usize),
}

#[derive(Debug, Clone)]
pub struct Item {
    kind: ParseKind,
    position: Position,
}

impl Item {

    pub fn new(k: ParseKind, p: Position) -> Self {
        Self { kind: k, position: p }
    }
}


#[derive(Debug)]
pub struct Rule {
    pub input:   Vec<Vec<Item>>,    // to support multi-rules
    pub output:  Vec<Vec<Item>>,    // these need to be
    pub context: Vec<Vec<Item>>,    // Vec<Vec<Item>>
    pub except:  Vec<Vec<Item>>,    
}                                   

impl Rule {
    pub fn new(i: Vec<Vec<Item>>, o: Vec<Vec<Item>>, c :Vec<Vec<Item>>, e :Vec<Vec<Item>>) -> Self {
        Self { input: i, output: o, context: c, except: e }
    }

    pub fn apply(&self, word: String /* Need a `Word` struct == Vec<Vec<IPA>,Supra>*/, trace: bool) -> Result<String, RuntimeError> {
        todo!()
    }
}

pub struct Parser<'a> {
    token_list: Vec<Token>,
    cardinals: &'a HashMap<String, HashMap<String, Option<usize>>>, 
    pos: usize,
    forw_pos: usize,
    curr_tkn: Token,
    forw_tkn: Token,
}

impl<'a> Parser<'a> {
    
    pub fn new(lst: Vec<Token>, c: &'a HashMap<String, HashMap<String, Option<usize>>>) -> Self {
        let mut s = Self { 
            token_list: lst, 
            cardinals: c,
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

    fn peek_kind(&self, knd: TokenKind) -> bool {
        if self.forw_tkn.kind == knd {
            return true
        } else {
            return false
        }
    }

    fn expect(&mut self, knd: TokenKind) -> bool {
        
        // match self.forw_tkn.kind {
        //     knd. => {
        //         self.lookahead();
        //         return true
        //     }
        //     _ => return false
        // }

        if self.forw_tkn.kind == knd {
            self.lookahead();
            return true
        } else {
            return false
        }
    }

    fn eat(&mut self) -> Token {
        let token = self.forw_tkn.clone();
        self.lookahead();
        self.advance();
        token
    }

    fn eat_expect(&mut self, knd: TokenKind) -> Option<Token> {
        if self.forw_tkn.kind == knd {
            return Some(self.eat())
        } else {
            return None
        }
    }

    fn get_except_block(&mut self) -> Result<Vec<Vec<Item>>, SyntaxError> {
        if self.expect(TokenKind::Pipe) {
            todo!("Exceptions not yet implemented")
        } else {
            Ok(Vec::new())
        }
    }

    fn get_context(&mut self) -> Result<Vec<Vec<Item>>, SyntaxError> {
        if self.expect(TokenKind::Slash) {
            todo!("Context not yet implemented")
        } else {
            Ok(Vec::new())
        }
    }
    
    fn join_char_params(&mut self, char: Item, params: Item) -> Result<Item, SyntaxError> {
        // returns matrix or Err
        todo!()
    }

    fn char_to_matrix(&mut self, chr: Token) -> Result<Item, SyntaxError> {
        // returns matrix or None
        use TokenKind::*;
        use FeatType::*;

        let syll_m = Token::new(Feature(Syllabic), "-".to_string(), chr.position.start, chr.position.end);
        let syll_p = Token::new(Feature(Syllabic), "+".to_string(), chr.position.start, chr.position.end);
        
        let cons_m = Token::new(Feature(Consonantal), "-".to_string(), chr.position.start, chr.position.end);
        let cons_p = Token::new(Feature(Consonantal), "+".to_string(), chr.position.start, chr.position.end);
        
        let sonr_m = Token::new(Feature(Sonorant), "-".to_string(), chr.position.start, chr.position.end);
        let sonr_p = Token::new(Feature(Sonorant), "+".to_string(), chr.position.start, chr.position.end);

        let appr_m = Token::new(Feature(Approximant), "-".to_string(), chr.position.start, chr.position.end);
        let appr_p = Token::new(Feature(Approximant), "+".to_string(), chr.position.start, chr.position.end);



        let x = match chr.value.as_str() {
            "C" => vec![syll_m],                         // -syll                     // Consonant
            "O" => vec![cons_p, sonr_m, syll_m],         // +cons, -son, -syll        // Obstruent
            "S" => vec![cons_p, sonr_p, syll_m],         // +cons, +son, -syll        // Sonorant
            "L" => vec![cons_p, sonr_p, syll_m, appr_p], // +cons, +son, -syll, +appr // Liquid
            "N" => vec![cons_p, sonr_p, syll_m, appr_m], // +cons, +son, -syll, -appr // Nasal
            "G" => vec![cons_m, sonr_p, syll_m],         // -cons, +son, -syll        // Glide
            "v" => vec![cons_m, sonr_p, syll_p],         // -cons, +son, +syll        // Vowel
            _ => return Err(SyntaxError::UnknownChar(chr)),
        };


        Ok(Item::new(ParseKind::Matrix(x), Position { start: chr.position.start, end: chr.position.end }))
    }

    fn is_feature(&self) -> bool{
        
        match self.forw_tkn.kind {
            TokenKind::Feature(_) => return true,
            _ =>  return false,
        }
    }

    fn get_matrix_args(&mut self) -> Result<Vec<Token>, SyntaxError> {
        // returns matrix or None
        let mut args = Vec::new();
        while self.has_more_tokens() {
            if self.expect(TokenKind::RightSquare) {
                self.advance();
                break;
            }
            
            if self.expect(TokenKind::Comma) {
                continue;
            }
            
            if self.is_feature(){
                args.push(self.forw_tkn.clone());
                self.lookahead();
                continue;
            }
            
            return Err(SyntaxError::ExpectedFeature(self.forw_tkn.clone()))
        }
        
        Ok(args)

    }

    fn get_matrix(&mut self) -> Result<Item, SyntaxError> { 
        let start = self.token_list[self.forw_pos-1].position.start;
        let args = self.get_matrix_args()?;
        let end = self.token_list[self.forw_pos-1].position.end;
        
        Ok(Item::new(ParseKind::Matrix(args), Position { start, end }))
    }

    fn get_char(&mut self) -> Result<Item, SyntaxError> {
        // returns matrix or None
        let chr = self.char_to_matrix(self.forw_tkn.clone())?;
        self.lookahead();

        if !self.expect(TokenKind::Colon) {
            return Ok(chr)
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(SyntaxError::ExpectedMatrix(self.forw_tkn.clone()))
        }
        
        let params = self.get_matrix()?;

        let joined = self.join_char_params(chr, params)?;

        Ok(joined)
    }
    
    fn get_segment(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns IPA / Matrix or None

        if self.peek_kind(TokenKind::Cardinal) {
            let token = self.forw_tkn.clone();
            self.lookahead();
            self.advance();
            return Ok(Some(Item::new(ParseKind::IPA(token.clone()), token.position)))
        }

        if self.peek_kind(TokenKind::Primative) {
            let maybe_char = self.get_char()?;
            return Ok(Some(maybe_char))
        }

        if self.expect(TokenKind::LeftSquare) {
            let maybe_matrix = self.get_matrix()?;
            return Ok(Some(maybe_matrix))
        }

        return Ok(None)
    }

    fn get_optionals(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns optionals or None

        let start = self.forw_tkn.position.start;

        if !self.expect(TokenKind::LeftBracket) {
            return Ok(None);
        }

        let mut segs = Vec::new();
        let mut lo_bound: usize = 0;
        let mut hi_bound: usize = 0;
        while self.has_more_tokens() {
            if self.peek_kind(TokenKind::RightBracket) {
                break;
            }

            if let Some(x) = self.get_segment()? {
                segs.push(x);
                continue;
            }

            if self.peek_kind(TokenKind::Comma){
                break;
            }

            return Err(SyntaxError::ExpectedSegment(self.forw_tkn.clone()))
        }

        // Todo: This needs cleaning up!

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.forw_pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, 1), Position { start, end })))
        }

        if !self.expect(TokenKind::Comma) {
            return Err(SyntaxError::ExpectedComma(self.forw_tkn.clone()))
        }

        if let Some(x) = self.eat_expect(TokenKind::Number) {
            lo_bound = x.value.parse().expect("Could not parse string to number. This is probably an error with the parser itself");
        }

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.forw_pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, lo_bound), Position { start, end })))
        }

        if !self.expect(TokenKind::Colon) {
            return Err(SyntaxError::ExpectedColon(self.forw_tkn.clone()))
        }

        if let Some(x) = self.eat_expect(TokenKind::Number) {
            hi_bound = x.value.parse().expect("Could not parse string to number. This is probably an error with the parser itself");
            if hi_bound < lo_bound {
                return Err(SyntaxError::OptMathError(x, lo_bound, hi_bound))
            }
        }

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.forw_pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, lo_bound, hi_bound), Position { start, end })))
        }

        Err(SyntaxError::ExpectedRightBracket(self.forw_tkn.clone()))
    }

    fn get_set(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns set of segs
        let start = self.forw_tkn.position.start;

        if !self.expect(TokenKind::LeftCurly) {
            return Ok(None)
        }

        let mut segs = Vec::new();

        while self.has_more_tokens() {
            if self.expect(TokenKind::RightCurly) {
                break;
            }

            if self.expect(TokenKind::Comma) {
                continue;
            }

            if let Some(x) = self.get_segment()? {
                segs.push(x);
                continue;
            }

            return Err(SyntaxError::ExpectedSegment(self.forw_tkn.clone()))
        }
        let end = self.token_list[self.forw_pos-1].position.end;
        Ok(Some(Item::new(ParseKind::Set(segs.clone()), Position { start, end })))

    }

    fn get_syll(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns Syll or None
        let start = self.forw_tkn.position.start;

        if !self.expect(TokenKind::Syllable) {
            return Ok(None)
        }

        if !self.expect(TokenKind::Colon) {
            let end = self.forw_tkn.position.start - 1;
            return Ok(Some(Item::new(ParseKind::Syllable(Vec::new()), Position { start, end })))
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(SyntaxError::ExpectedMatrix(self.forw_tkn.clone()))
        }

        let m = self.get_matrix_args()?;

        let end = self.token_list[self.forw_pos-1].position.end;

        Ok(Some(Item::new(ParseKind::Syllable(m), Position { start, end })))
    }

    fn get_term(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns syllable / set / segment / optionals

        if let Some(x) = self.get_syll()? {
            return Ok(Some(x))
        }

        if let Some(x) = self.get_set()? {
            return Ok(Some(x))
        }

        if let Some(x) = self.get_segment()? {
            return Ok(Some(x))
        }

        if let Some(x) = self.get_optionals()? {
            return Ok(Some(x))
        }

        Ok(None)
    }

    fn get_input_term(&mut self) -> Result<Vec<Item>, SyntaxError>{
        // returns list of terms and ellipses
        let mut terms = Vec::<Item>::new();

        loop {
            if let Some(el) = self.eat_expect(TokenKind::Ellipsis) {
                terms.push(Item::new(ParseKind::Ellipsis(el.clone()), el.position));
                continue;
            }

            if self.peek_kind(TokenKind::GreaterThan) { // So we can try simple rules that don't call functions we are yet to implement
                break                                       //  We can probably remove this after parser logic is done
            }                                               // Though it wouldn't hurt performance to leave it in

            if let Some(trm) = self.get_term()? {
                terms.push(trm)
            } else {
                break
            }

        }

        Ok(terms)

    }

    fn get_output_element(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns syllable / set / segment
        if let Some(x) = self.get_syll()? {
            return Ok(Some(x))
        }

        if let Some(x) = self.get_set()? {
            return Ok(Some(x))
        }

        if let Some(x) = self.get_segment()? {
            return Ok(Some(x))
        }

        Ok(None)
    }

    fn get_output_term(&mut self) -> Result<Vec<Item>, SyntaxError>{ 
        // returns list of sylls, sets, and/or segments

        let mut terms = Vec::<Item>::new();

        loop {
            if let Some(trm) = self.get_output_element()? {
                terms.push(trm)
            } else {
                break
            }
        }

        Ok(terms)
    }

    fn get_empty(&mut self) -> Vec<Item> {
        if !self.peek_kind(TokenKind::Star) && !self.peek_kind(TokenKind::EmptySet) {
            return Vec::new()
        }
        let token = self.eat();
        vec![Item::new(ParseKind::EmptySet(token.clone()), token.position)]
    }

    fn get_input(&mut self) -> Result<Vec<Vec<Item>>, SyntaxError> {
        // returns empty / list of input terms
        let mut inputs = Vec::new();
        let maybe_empty = self.get_empty();

        if !maybe_empty.is_empty() {
            inputs.push(maybe_empty);
            return Ok(inputs)
        }

        loop {
            let x = self.get_input_term()?;
            inputs.push(x);

            if !self.expect(TokenKind::Comma) {
                break
            }
        }

        if inputs.is_empty() {
            return Err(SyntaxError::EmptyInput)
        }

        Ok(inputs)

    }

    fn get_output(&mut self) -> Result<Vec<Vec<Item>>, SyntaxError> {
        // returns empty / metathesis / list of output erms 
        let mut outputs = Vec::new();

        // check for deletion rule
        let maybe_empty = self.get_empty();
        if !maybe_empty.is_empty() {
            outputs.push(maybe_empty);
            return Ok(outputs)
        }
        // check for metathesis
        if let Some(el) = self.eat_expect(TokenKind::Ampersand) {
            outputs.push(vec![Item::new(ParseKind::Metathesis(el.clone()), el.position)]);
            return Ok(outputs)
        }
        // todo: add check for `+`

        loop {
            let x = self.get_output_term()?;
            outputs.push(x);

            if !self.expect(TokenKind::Comma) {
                break
            }
        }

        if outputs.is_empty() {
            return Err(SyntaxError::EmptyOutput)
        }

        Ok(outputs)
    }

    
    fn rule(&mut self) -> Result<Rule, SyntaxError> {

        let input = self.get_input()?;

        //println!("Input: {:?}", input);
        if !self.expect(TokenKind::Arrow) && !self.expect(TokenKind::GreaterThan) {
            match input[0][0].kind {
                ParseKind::EmptySet(_) => { 
                    if self.peek_kind(TokenKind::Comma) {
                        return Err(SyntaxError::InsertErr)
                    } else {
                        return Err(SyntaxError::ExpectedArrow(self.forw_tkn.clone()))
                    }
                },
                _ => return Err(SyntaxError::ExpectedArrow(self.forw_tkn.clone()))
            }
        }

        let output = self.get_output()?;

        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, Vec::new(), Vec::new()))
        }

        if !self.peek_kind(TokenKind::Slash) && !self.peek_kind(TokenKind::Pipe) {
            match input[0][0].kind {
                ParseKind::EmptySet(_) | ParseKind::Metathesis(_) => { 
                    if self.peek_kind(TokenKind::Comma) {
                        return Err(SyntaxError::DeleteErr)
                    } else {
                        return Err(SyntaxError::ExpectedEndL(self.forw_tkn.clone()))
                    }
                },
                _ => return Err(SyntaxError::ExpectedEndL(self.forw_tkn.clone()))
            }
        }

        let context = self.get_context()?;
        
        let except = self.get_except_block()?;

        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, context, except))
        }
        
        Err(SyntaxError::ExpectedEndL(self.forw_tkn.clone()))

    }
    

    pub fn parse(&mut self) -> Result<Rule, SyntaxError> {
        self.rule()
    }

}