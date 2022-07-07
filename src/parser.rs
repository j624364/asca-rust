use std::{collections::HashMap, fmt};
use super::lexer::*;

#[derive(Debug, Clone)]
pub enum RuntimeError { 

}



#[derive(Debug, Clone)]
pub enum SyntaxError {
    OptMathError(Token, usize, usize),
    UnknownIPA(Token),
    UnknownChar(Token),
    ExpectedEndL(Token),
    ExpectedArrow(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ExpectedMatrix(Token),
    ExpectedSegment(Token),
    ExpectedFeature(Token),
    ExpectedUnderline(Token),
    ExpectedRightBracket(Token),
    InsertErr,
    DeleteErr,
    EmptyInput,
    EmptyOutput,
    EmptyEnv,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OptMathError(_, l, h)  => write!(f, "Second argument '{}' must be >= first argument '{}'", h, l),
            Self::UnknownIPA(token)           => write!(f, "Could not get value of IPA '{}'.", token.value),
            Self::UnknownChar(token)          => write!(f, "No known primative'{}'. Known primatives are (C)onsonant, (O)bstruent, (S)onorant, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::ExpectedEndL(token)         => write!(f, "Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)        => write!(f, "Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)        => write!(f, "Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)        => write!(f, "Expected ':', but received '{}'", token.value),
            Self::ExpectedMatrix(token)       => write!(f, "Expected '[', but received '{}'", token.value),
            Self::ExpectedSegment(token)      => write!(f, "Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedFeature(token)      => write!(f, "{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedUnderline(token)    => write!(f, "Expected '_', but received '{}'", token.value),
            Self::ExpectedRightBracket(token) => write!(f, "Expected ')', but received '{}'", token.value),
            Self::InsertErr   => write!(f, "The input of an insertion rule must only contain `*` or `∅`"),
            Self::DeleteErr   => write!(f, "The output of a deletion rule must only contain `*` or `∅`"),
            Self::EmptyInput  => write!(f, "Input cannot be empty. Use `*` or '∅' to indicate insertion"),
            Self::EmptyOutput => write!(f, "Output cannot be empty. Use `*` or '∅' to indicate deletion"),
            Self::EmptyEnv    => write!(f, "Environment cannot be empty following a seperator."),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseKind {
    EmptySet   (Token),
    Boundary   (Token),
    Ellipsis   (Token),
    Metathesis (Token),
    IPA        (HashMap<String, Option<usize>>, Vec<Token>),
    Matrix     (Vec<Token>), //(HashMap<String, Option<usize>>),
    Syllable   (Vec<Token>),
    Set        (Vec<Item>),
    Optional   (Vec<Item>, usize, usize),
    Environment(Vec<Item>, Vec<Item>),
}

impl ParseKind {
    fn matrix(self) -> Option<Vec<Token>> {
        match self {
            ParseKind::Matrix(f) => Some(f),
            _ => None
        }
    }
}

impl fmt::Display for ParseKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseKind::EmptySet(t) |
            ParseKind::Boundary(t) |
            ParseKind::Ellipsis(t) |
            ParseKind::Metathesis(t) => write!(f, "{}", t),
            ParseKind::IPA(_, _) => todo!(),
            ParseKind::Matrix(tokens) | 
            ParseKind::Syllable(tokens) => {
                let tt = tokens.iter()
                .fold(String::new(), |acc, i| acc + &i.to_string() + ", ");
                
                write!(f, "[{}]", tt)
            },
            ParseKind::Set(_) => todo!(),
            ParseKind::Optional(_, _, _) => todo!(),
            ParseKind::Environment(bef, aft) => {
                let xb = bef.iter()
                .fold(String::new(), |acc, i| acc + &i.to_string() + ", ");

                let xa = aft.iter()
                .fold(String::new(), |acc, i| acc + &i.to_string() + ", ");
                
                
                if xb.is_empty() && xa.is_empty() {
                    write!(f, "[{}] __ [{}]", xb, xa)
                } else if xb.is_empty() {
                    write!(f, "[{}] __ {}", xb, xa)
                } else if xa.is_empty() {
                    write!(f, "{} __ [{}]", xb, xa)
                } else {
                    write!(f, "{} __ {}", xb, xa)
                }

            },
        }
    }
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

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}


#[derive(Debug)]
pub struct Rule {
    pub input:   Vec<Vec<Item>>,    // to support multi-rules
    pub output:  Vec<Vec<Item>>,    // these need to be
    pub context: Vec<Item>,         // Vec<Vec<Item>>
    pub except:  Vec<Item>,    
}                                   

impl Rule {
    pub fn new(i: Vec<Vec<Item>>, o: Vec<Vec<Item>>, c :Vec<Item>, e :Vec<Item>) -> Self {
        Self { input: i, output: o, context: c, except: e }
    }

    pub fn apply(&self, word: String /* Need a `Word` struct == Vec<Vec<IPA>,Supra>*/, trace: bool) -> Result<String, RuntimeError> {
        todo!()
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Rule ->")?;

        writeln!(f, "    Input = [")?;
        self.input.iter().for_each(|i| {
            println!("        {:?}", i);
        });
        writeln!(f, "    ]")?;

        writeln!(f, "    Output = [")?;
        self.output.iter().for_each(|o| {
            println!("        {:?}", o);
        });
        writeln!(f, "    ]")?;

        writeln!(f, "    Context = [")?;
        self.context.iter().for_each(|c| {
            println!("        {}", c);
        });
        writeln!(f, "    ]")?;

        writeln!(f, "    Exception = [")?;
        self.except.iter().for_each(|e| {
            println!("        {}", e);
        });
        writeln!(f, "    ]")?;


        Ok(())
    }
}

pub struct Parser<'a> {
    token_list: Vec<Token>,
    cardinals: &'a HashMap<String, HashMap<String, Option<usize>>>, 
    curr_pos: usize,
    curr_tkn: Token,
}

impl<'a> Parser<'a> {
    
    pub fn new(lst: Vec<Token>, c: &'a HashMap<String, HashMap<String, Option<usize>>>) -> Self {
        let mut s = Self { 
            token_list: lst, 
            cardinals: c,
            curr_pos: 0, 
            curr_tkn: Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { start: 0, end: 1 } }, 
        };
       
        s.curr_tkn = s.token_list[s.curr_pos].clone();

        s
    }

    fn lookahead(&mut self) {
        self.curr_pos += 1;

        self.curr_tkn = if self.has_more_tokens() {
            self.token_list[self.curr_pos].clone()
        } else {
            Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { start: self.curr_pos, end: self.curr_pos+1 } }
        }
    }

    fn has_more_tokens(&self) -> bool { self.curr_pos < self.token_list.len() }

    // fn advance(&mut self) {
    //     self.pos = self.forw_pos;
    //     self.curr_tkn = self.forw_tkn.clone();
    // }

    // fn retreat(&mut self) {
    //     self.forw_pos = self.pos;
    //     self.forw_tkn = self.curr_tkn.clone();
    // }

    fn peek_kind(&self, knd: TokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            return true
        } else {
            return false
        }
    }

    fn expect(&mut self, knd: TokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            self.lookahead();
            return true
        } else {
            return false
        }
    }

    fn eat(&mut self) -> Token {
        let token = self.curr_tkn.clone();
        self.lookahead();
        token
    }

    fn eat_expect(&mut self, knd: TokenKind) -> Option<Token> {
        if self.curr_tkn.kind == knd {
            return Some(self.eat())
        } else {
            return None
        }
    }

    fn get_bound(&mut self) -> Option<Item> { 
        if !self.peek_kind(TokenKind::SyllBoundary) && !self.peek_kind(TokenKind::WordBoundary) {
            return None
        }
        let token = self.eat();
        Some(Item::new(ParseKind::Boundary(token.clone()), token.position))
    }

    fn get_env_elements(&mut self) -> Result<Vec<Item>, SyntaxError> {
        // returns list of boundaries, ellipses and terms
        let mut els = Vec::new();

        loop {
            if let Some(x) = self.get_bound() {
                els.push(x);
                continue;
            }

            if let Some(el) = self.eat_expect(TokenKind::Ellipsis) {
                els.push(Item::new(ParseKind::Ellipsis(el.clone()), el.position));
                continue;
            }

            if let Some(x) = self.get_term()? {
                els.push(x);
                continue;
            }

            break;
        }

        Ok(els)
    }

    fn get_env_term(&mut self) -> Result<Item, SyntaxError> {
        // returns env elements
        let start = self.curr_tkn.position.start;
        
        let before  = self.get_env_elements()?;

        if !self.expect(TokenKind::Underline) {
            return Err(SyntaxError::ExpectedUnderline(self.curr_tkn.clone()))
        }

        let after = self.get_env_elements()?;

        let end = self.token_list[self.curr_pos-1].position.end;

        Ok(Item::new(ParseKind::Environment(before, after), Position { start, end }))
    }

    fn get_spec_env(&mut self) -> Result<Option<Vec<Item>>, SyntaxError> {

        let start = self.curr_tkn.position.start;

        if !self.expect(TokenKind::Underline) {
            return Ok(None)
        }

        if !self.expect(TokenKind::Comma) {
            return Ok(None)
        }

        let x = self.get_env_elements()?;

        let end = self.token_list[self.curr_pos-1].position.end;

        let mut v = Vec::new();

        v.push(Item::new(ParseKind::Environment(x.clone(), Vec::new()), Position { start, end }));
        v.push(Item::new(ParseKind::Environment(Vec::new(), x), Position { start, end }));

        return Ok(Some(v))

    }

    fn get_env(&mut self) -> Result<Vec<Item>, SyntaxError> { 
        // returns environment

        if let Some(s) = self.get_spec_env()? {
            return Ok(s)
        }

        let mut envs = Vec::new();
 
        loop {
            let x = self.get_env_term()?;
            envs.push(x);

            if !self.expect(TokenKind::Comma) {
                break
            }
        }

        if envs.is_empty() {
            return Err(SyntaxError::EmptyEnv)
        }

        Ok(envs)
    }

    fn get_except_block(&mut self) -> Result<Vec<Item>, SyntaxError> {
        if! self.expect(TokenKind::Pipe) {
            return Ok(Vec::new())
        }
        self.get_env()
    }

    fn get_context(&mut self) -> Result<Vec<Item>, SyntaxError> {
        if !self.expect(TokenKind::Slash) {
            return Ok(Vec::new())
        }
        self.get_env()
    }

    fn join_ipa_with_params(&self, ipa: HashMap<String, Option<usize>>, params: Item) -> Result<ParseKind, SyntaxError> {
        // use TokenKind::*;
        // use FeatType::*;

        //let x = self.cardinals;
        //let y = x.get(&ipa.value);


        println!("{:?}", ipa);
        //println!("{:?}", y.unwrap());
        println!();
        println!("{:?}", params);
        println!();
        // probably gonna have to convert ipa/matrix to Segment form
        // i.e. Root:u32 = 5, Manner:u32 = 8, Lar:u32 = 2, etc.

        if let ParseKind::Matrix(p) = params.kind {
            Ok(ParseKind::IPA(ipa, p))
        }
        else {
            unreachable!()
        }

    }
    
    fn join_char_with_params(&mut self, character: Item, parameters: Item) -> Result<Item, SyntaxError> {
        // returns matrix or Err

        // use TokenKind::*;
        // use FeatType::*;
        
        // panics should be unreachable()
        let chars = character.kind.matrix().expect("Critical Error: Char is not a matrix!");
        let params = parameters.kind.matrix().expect("Critical Error: Param is not a matrix!"); 

        let mut asdf = params.clone();

        let remaining: Vec<Token> = chars.into_iter().filter(|c| params.iter().find(|p| {
            return p.kind.feature().unwrap() != c.kind.feature().unwrap()
        }).is_some()).collect();

        asdf.extend(remaining);

        asdf.iter().for_each(|t| print!("{}, ", t));

        // probably gonna have to convert matrix to values at some point
        Ok(Item::new(ParseKind::Matrix(asdf), Position { start: character.position.start, end: parameters.position.end }))
    }

    fn ipa_to_vals(&self, ipa: Token) -> Result<HashMap<String, Option<usize>>, SyntaxError> {
        
        let x = self.cardinals;
        let y = x.get(&ipa.value);

        match y {
            Some(z) => Ok(z.clone()),
            None => return Err(SyntaxError::UnknownIPA(ipa))
        }
    }

    // fn feature_to_val(&self, feat: TokenKind) -> Option<(String, usize)> {
    //     use FeatType::*;
    //     match feat {
    //         TokenKind::Feature(x) => {
    //             match x {
    //                 Consonantal     => return Some(("root".to_string(), 4)),
    //                 Sonorant        => return Some(("root".to_string(), 2)),
    //                 Syllabic        => return Some(("root".to_string(), 1)),
    //                 Continuant      => return Some(("manner".to_string(), 64)),
    //                 Approximant     => return Some(("manner".to_string(), 32)),
    //                 Lateral         => return Some(("manner".to_string(), 16)),
    //                 Nasal           => return Some(("manner".to_string(), 8)),
    //                 DelayedRelease  => return Some(("manner".to_string(), 4)),
    //                 Strident        => return Some(("manner".to_string(), 2)),
    //                 Rhotic          => return Some(("manner".to_string(), 1)),
    //                 Voice           => return Some(("lar".to_string(), 4)),
    //                 SpreadGlottis   => return Some(("lar".to_string(), 2)),
    //                 ConstrGlottis   => return Some(("lar".to_string(), 1)),
    //                 Round           => return Some(("lab".to_string(), 1)),
    //                 Anterior        => return Some(("cor".to_string(), 2)),
    //                 Distributed     => return Some(("cor".to_string(), 1)),
    //                 Front           => return Some(("dor".to_string(), 32)),
    //                 Back            => return Some(("dor".to_string(), 16)),
    //                 High            => return Some(("dor".to_string(), 8)),
    //                 Low             => return Some(("dor".to_string(), 4)),
    //                 Tense           => return Some(("lab".to_string(), 2)),
    //                 Reduced         => return Some(("lab".to_string(), 1)),
    //                 AdvancedTongueRoot  => return Some(("phr".to_string(), 2)),
    //                 RetractedTongueRoot => return Some(("phr".to_string(), 1)),
    //                 _ => return None
    //             }
    //         },
    //         _ => unreachable!()
    //     }
    // }

    fn matrix_to_val(&self, matrix:Item) -> Result<HashMap<String, Option<usize>>, SyntaxError> {
        println!("{:?}", matrix);

        if let Some(v) = matrix.kind.matrix() {

            println!("{:?}", v);
        }



        unreachable!();
    }

    fn char_to_matrix(&self, chr: Token) -> Result<Item, SyntaxError> {
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
            "V" => vec![cons_m, sonr_p, syll_p],         // -cons, +son, +syll        // Vowel
            _ => return Err(SyntaxError::UnknownChar(chr)),
        };


        Ok(Item::new(ParseKind::Matrix(x), Position { start: chr.position.start, end: chr.position.end }))
    }

    fn is_feature(&self) -> bool{
        
        match self.curr_tkn.kind {
            TokenKind::Feature(_) => return true,
            _ =>  return false,
        }
    }

    fn get_matrix_args(&mut self) -> Result<Vec<Token>, SyntaxError> {
        // returns matrix or None
        let mut args = Vec::new();
        while self.has_more_tokens() {
            if self.expect(TokenKind::RightSquare) {
                break;
            }
            
            if self.expect(TokenKind::Comma) {
                continue;
            }
            
            if self.is_feature(){
                args.push(self.curr_tkn.clone());
                self.lookahead();
                continue;
            }
            
            return Err(SyntaxError::ExpectedFeature(self.curr_tkn.clone()))
        }
        
        Ok(args)

    }

    fn get_matrix(&mut self) -> Result<Item, SyntaxError> { 
        let start = self.token_list[self.curr_pos-1].position.start;
        let args = self.get_matrix_args()?;
        let end = self.token_list[self.curr_pos-1].position.end;
        
        Ok(Item::new(ParseKind::Matrix(args), Position { start, end }))
    }

    fn get_char(&mut self) -> Result<Item, SyntaxError> {
        // returns matrix or None
        let chr = self.char_to_matrix(self.curr_tkn.clone())?;
        self.lookahead();

        if !self.expect(TokenKind::Colon) {
            return Ok(chr)
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(SyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }
        
        let params = self.get_matrix()?;

        let joined = self.join_char_with_params(chr, params)?;

        Ok(joined)
    }

    fn get_ipa(&mut self) -> Result<Item, SyntaxError> {
        // returns IPA or Matrix

        let ipa = self.ipa_to_vals(self.curr_tkn.clone())?;
        let pos = self.curr_tkn.position;
        self.lookahead();

        if !self.expect(TokenKind::Colon) {
            return Ok(Item::new(ParseKind::IPA(ipa.clone(), Vec::new()), pos))
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(SyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let params = self.get_matrix()?;

        let joined = self.join_ipa_with_params(ipa, params.clone())?;

        Ok(Item::new(joined, Position {start: pos.start, end: params.position.end}))
    }
    
    fn get_segment(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns IPA / Matrix or None

        if self.peek_kind(TokenKind::Cardinal) {
            let maybe_ipa = self.get_ipa()?;
            return Ok(Some(maybe_ipa))
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

        let start = self.curr_tkn.position.start;

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

            return Err(SyntaxError::ExpectedSegment(self.curr_tkn.clone()))
        }

        // Todo: This needs cleaning up!

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.curr_pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, 1), Position { start, end })))
        }

        if !self.expect(TokenKind::Comma) {
            return Err(SyntaxError::ExpectedComma(self.curr_tkn.clone()))
        }

        if let Some(x) = self.eat_expect(TokenKind::Number) {
            lo_bound = x.value.parse().expect("Could not parse string to number. This is probably an error with the parser itself");
        }

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.curr_pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, lo_bound), Position { start, end })))
        }

        if !self.expect(TokenKind::Colon) {
            return Err(SyntaxError::ExpectedColon(self.curr_tkn.clone()))
        }

        if let Some(x) = self.eat_expect(TokenKind::Number) {
            hi_bound = x.value.parse().expect("Could not parse string to number. This is probably an error with the parser itself");
            if hi_bound < lo_bound {
                return Err(SyntaxError::OptMathError(x, lo_bound, hi_bound))
            }
        }

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.curr_pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, lo_bound, hi_bound), Position { start, end })))
        }

        Err(SyntaxError::ExpectedRightBracket(self.curr_tkn.clone()))
    }

    fn get_set(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns set of segs
        let start = self.curr_tkn.position.start;

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

            return Err(SyntaxError::ExpectedSegment(self.curr_tkn.clone()))
        }
        let end = self.token_list[self.curr_pos-1].position.end;
        Ok(Some(Item::new(ParseKind::Set(segs.clone()), Position { start, end })))

    }

    fn get_syll(&mut self) -> Result<Option<Item>, SyntaxError> {
        // returns Syll or None
        let start = self.curr_tkn.position.start;

        if !self.expect(TokenKind::Syllable) {
            return Ok(None)
        }

        if !self.expect(TokenKind::Colon) {
            let end = self.curr_tkn.position.start - 1;
            return Ok(Some(Item::new(ParseKind::Syllable(Vec::new()), Position { start, end })))
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(SyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let m = self.get_matrix_args()?;

        let end = self.token_list[self.curr_pos-1].position.end;

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

    fn get_input_term(&mut self) -> Result<Vec<Item>, SyntaxError> {
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

        // if let Some(x) = self.get_set()? {
        //     return Ok(Some(x))
        // }

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

        if !self.expect(TokenKind::Arrow) && !self.expect(TokenKind::GreaterThan) {
            match input[0][0].kind {
                ParseKind::EmptySet(_) => { 
                    if self.peek_kind(TokenKind::Comma) {
                        return Err(SyntaxError::InsertErr)
                    } else {
                        return Err(SyntaxError::ExpectedArrow(self.curr_tkn.clone()))
                    }
                },
                _ => return Err(SyntaxError::ExpectedArrow(self.curr_tkn.clone()))
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
                        return Err(SyntaxError::ExpectedEndL(self.curr_tkn.clone()))
                    }
                },
                _ => return Err(SyntaxError::ExpectedEndL(self.curr_tkn.clone()))
            }
        }

        let context = self.get_context()?;
        
        let except = self.get_except_block()?;

        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, context, except))
        }
        
        Err(SyntaxError::ExpectedEndL(self.curr_tkn.clone()))

    }
    

    pub fn parse(&mut self) -> Result<Rule, SyntaxError> {
        self.rule()
    }

}