use std::{
    collections::HashMap, 
    fmt
};

use crate::{
    JSON    , 
    lexer::*, 
    rule ::*, 
    error::*, 
    word ::Segment
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModKind {
    Negative,
    Positive,
    Alpha(char),
    InversAlpha(char),
    Number(String),
    Unspecified
}

impl Default for ModKind {
    fn default() -> Self { ModKind::Unspecified }
}

macro_rules! modArrInit {
    () => {
        [();FeatType::Tone as usize + 1].map(|_| ModKind::default())
    };
}

// macro_rules! map {
    //     ($($k:expr => $v:expr),* $(,)?) => {{
    //         core::convert::From::from([$(($k, $v),)*])
    //     }};
    // }

// pub type Modifiers = HashMap<FeatType, ModKind>;
pub type Modifiers = [ModKind; FeatType::Tone as usize + 1];
type Items = Vec<Item>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseKind {
    EmptySet   (Token),
    Boundary   (Token),
    Ellipsis   (Token),
    Metathesis (Token),
    Variable   (Token  , Modifiers),
    IPA        (Segment, Modifiers),
    Matrix     (Modifiers),
    Syllable   (Modifiers), // just stress, tone
    Set        (Items),
    Optional   (Items, usize, usize),
    Environment(Items, Items),
}

impl ParseKind {
    fn as_matrix(&self) -> Option<&Modifiers> {
        if let Self::Matrix(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn is_matrix(&self) -> bool {
        matches!(self, Self::Matrix(..))
    }

    // pub fn is_ipa(&self) -> bool {
    //     matches!(self, Self::IPA(..))
    // }
    //
    // /// Returns `true` if the parse kind is [`Set`].
    // ///
    // /// [`Set`]: ParseKind::Set
    // pub fn is_set(&self) -> bool {
    //     matches!(self, Self::Set(..))
    // }
}

impl fmt::Display for ParseKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseKind::Variable(t, p) => {
                // let tt = p.iter()
                // .fold(String::new(), |acc, i| acc + &i.to_string() + ", ");

                // write!(f, "{} = [{}]", t, tt)
                write!(f, "{} = {:?}", t, p)
            },
            ParseKind::EmptySet(t) |
            ParseKind::Boundary(t) |
            ParseKind::Ellipsis(t) |
            ParseKind::Metathesis(t) => write!(f, "{}", t),

            ParseKind::IPA(s, m) => write!(f, "{} = {:?}", s, m),

            ParseKind::Matrix(tokens) | 
            ParseKind::Syllable(tokens) => {
                write!(f, "{:#?}", tokens)
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

            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item {
    pub kind: ParseKind,
    pub position: Position,
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

pub struct Parser {
    token_list: Vec<Token>,
    pos: usize,
    curr_tkn: Token,
    var_map: HashMap<usize,Item>
}

impl Parser {
    pub fn new(lst: Vec<Token>) -> Self {
        let mut s = Self { 
            token_list: lst, 
            pos: 0, 
            curr_tkn: Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { start: 0, end: 1 } },
            var_map: HashMap::new(), 
        };
        s.curr_tkn = s.token_list[s.pos].clone();

        s
    }

    fn advance(&mut self) {
        self.pos += 1;

        self.curr_tkn = if self.has_more_tokens() {
            self.token_list[self.pos].clone()
        } else {
            Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { start: self.pos, end: self.pos+1 } }
        }
    }

    fn has_more_tokens(&self) -> bool { self.pos < self.token_list.len() }

    fn peek_expect(&self, knd: TokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            return true
        } else {
            return false
        }
    }

    fn expect(&mut self, knd: TokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            self.advance();
            return true
        } else {
            return false
        }
    }

    fn eat(&mut self) -> Token {
        let token = self.curr_tkn.clone();
        self.advance();
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
        if !self.peek_expect(TokenKind::SyllBoundary) && !self.peek_expect(TokenKind::WordBoundary) {
            return None
        }
        let token = self.eat();
        Some(Item::new(ParseKind::Boundary(token.clone()), token.position))
    }

    fn get_env_elements(&mut self) -> Result<Items, RuleSyntaxError> {
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

    fn get_env_term(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns env elements

        let start = self.curr_tkn.position.start;
        
        let before  = self.get_env_elements()?;

        if !self.expect(TokenKind::Underline) {
            return Err(RuleSyntaxError::ExpectedUnderline(self.curr_tkn.clone()))
        }

        let after = self.get_env_elements()?;

        let end = self.token_list[self.pos-1].position.end;

        Ok(Item::new(ParseKind::Environment(before, after), Position { start, end }))
    }

    fn get_spec_env(&mut self) -> Result<Option<Items>, RuleSyntaxError> {

        let start = self.curr_tkn.position.start;
        let pstn = self.pos;

        if !self.expect(TokenKind::Underline) {
            return Ok(None)
        }

        assert_eq!(pstn, self.pos - 1);

        if !self.expect(TokenKind::Comma) {
            self.pos -=2;
            self.advance();
            return Ok(None)
        }

        let x = self.get_env_elements()?;

        if self.expect(TokenKind::Underline) {
            self.pos = pstn-1;
            self.advance();
            return Ok(None)
        }

        let end = self.token_list[self.pos-1].position.end;

        let mut v = Vec::new();

        v.push(Item::new(ParseKind::Environment(x.clone(), Vec::new()), Position { start, end }));
        v.push(Item::new(ParseKind::Environment(Vec::new(), x), Position { start, end }));

        return Ok(Some(v))

    }

    fn get_env(&mut self) -> Result<Items, RuleSyntaxError> { 
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
            return Err(RuleSyntaxError::EmptyEnv)
        }

        Ok(envs)
    }

    fn get_except_block(&mut self) -> Result<Items, RuleSyntaxError> {
        if! self.expect(TokenKind::Pipe) {
            return Ok(Vec::new())
        }
        self.get_env()
    }

    fn get_context(&mut self) -> Result<Items, RuleSyntaxError> {
        if !self.expect(TokenKind::Slash) {
            return Ok(Vec::new())
        }
        self.get_env()
    }

    fn join_ipa_with_params(&self, ipa: Segment, params: Item) -> Result<ParseKind, RuleSyntaxError> {
        match params.kind {
            ParseKind::Matrix(p) => Ok(ParseKind::IPA(ipa, p)),
            _ => unreachable!("\nCritical Error: 'Parameters' being joined with IPA Segment is not a matrix!\nThis is a bug."),
        }

    }
    
    fn join_char_with_params(&mut self, character: Item, parameters: Item) -> Item {
        let mut chr = character.kind.as_matrix().expect("\nCritical Error: 'Char' being joined with 'Parameters' is not a matrix!\nThis is a bug.").clone();
        let params = parameters.kind.as_matrix().expect("\nCritical Error: 'Parameters' being joined with 'Char' is not a matrix!\nThis is a bug.").clone(); 
        
        // TODO: test this
        for (i, p) in params.iter().enumerate() {
            if let ModKind::Unspecified = p {
                continue
            }
            chr[i] = p.clone()
        }
        
        Item::new(ParseKind::Matrix(chr), Position { start: character.position.start, end: parameters.position.end })
    }

    fn ipa_to_vals(&self, ipa: Token) -> Result<Segment, RuleSyntaxError> {
        match JSON.get(&ipa.value) {
            Some(z) => Ok(z.clone()),
            None => return Err(RuleSyntaxError::UnknownIPA(ipa))
        }
    }

    fn char_to_matrix(&self, chr: Token) -> Result<Item, RuleSyntaxError> {
        // returns matrix or None
        use FeatType::*;
        use ModKind::*;

        const SYLL_M: (FeatType, ModKind) = (Syllabic, Negative);
        const SYLL_P: (FeatType, ModKind) = (Syllabic, Positive);

        const CONS_M: (FeatType, ModKind) = (Consonantal, Negative);
        const CONS_P: (FeatType, ModKind) = (Consonantal, Positive);
        
        const SONR_M: (FeatType, ModKind) = (Sonorant, Negative);
        const SONR_P: (FeatType, ModKind) = (Sonorant, Positive);

        const APPR_M: (FeatType, ModKind) = (Approximant, Negative);
        const APPR_P: (FeatType, ModKind) = (Approximant, Positive);



        let char_vals = match chr.value.as_str() {
            "C" => vec![SYLL_M],                         // -syll                     // Consonant
            "O" => vec![CONS_P, SONR_M, SYLL_M],         // +cons, -son, -syll        // Obstruent
            "S" => vec![CONS_P, SONR_P, SYLL_M],         // +cons, +son, -syll        // Sonorant
            "L" => vec![CONS_P, SONR_P, SYLL_M, APPR_P], // +cons, +son, -syll, +appr // Liquid
            "N" => vec![CONS_P, SONR_P, SYLL_M, APPR_M], // +cons, +son, -syll, -appr // Nasal
            "G" => vec![CONS_M, SONR_P, SYLL_M],         // -cons, +son, -syll        // Glide
            "V" => vec![CONS_M, SONR_P, SYLL_P],         // -cons, +son, +syll        // Vowel
            _ => return Err(RuleSyntaxError::UnknownChar(chr)),
        };

        let mut args = modArrInit!(); 

        for (feature, value) in char_vals {
            // args.insert(feature, value);
            args[feature as usize] = value
        }


        Ok(Item::new(ParseKind::Matrix(args), Position { start: chr.position.start, end: chr.position.end }))
    }

    fn is_feature(&self) -> bool{
        
        match self.curr_tkn.kind {
            TokenKind::Feature(_) => return true,
            _ =>  return false,
        }
    }

    fn feature_to_modifier(&self, feature: FeatType, value: String) -> (FeatType, ModKind) {

        match value.as_str() {
            "+" => return (feature, ModKind::Positive),
            "-" => return (feature, ModKind::Negative),
            "α"|"β"|"γ"|"δ"|"ε"|"ζ"|"η"|"θ"|"ι"|"κ"|"λ"|"μ"|"ν"|"ξ"|"ο"|"π"|"ρ"|"σ"|"ς"|"τ"|"υ"|"φ"|"χ"|"ψ"|"ω" => {
                return (feature, ModKind::Alpha(value.chars().collect::<Vec<char>>()[0])) // TODO: what the fuck is this
            },
            "-α"|"-β"|"-γ"|"-δ"|"-ε"|"-ζ"|"-η"|"-θ"|"-ι"|"-κ"|"-λ"|"-μ"|"-ν"|"-ξ"|"-ο"|"-π"|"-ρ"|"-σ"|"-ς"|"-τ"|"-υ"|"-φ"|"-χ"|"-ψ"|"-ω" => {
                return (feature, ModKind::InversAlpha(value.chars().collect::<Vec<char>>()[1]))
            }
            _ => if feature == FeatType::Tone || feature == FeatType::Stress { // we already know its a number if this matches thanks to the lexer
                return (feature, ModKind::Number(value))
            } else {
                unreachable!();
            }
        }
    }

    fn curr_token_to_modifier(&self) -> (FeatType, ModKind) {
        match self.curr_tkn.kind {
            TokenKind::Feature(x) => self.feature_to_modifier(x, self.curr_tkn.value.clone()),
            _ => unreachable!(),
        }
    }

    fn get_matrix_args(&mut self, is_syll: bool) -> Result<Modifiers, RuleSyntaxError> {
        // returns matrix or None
        
        let mut args = modArrInit!();
        while self.has_more_tokens() {
            if self.expect(TokenKind::RightSquare) {
                break;
            }
            
            if self.expect(TokenKind::Comma) {
                continue;
            }
            
            if self.is_feature() {
                let (f, b) = self.curr_token_to_modifier();

                if (f != FeatType::Tone && f != FeatType::Stress) && is_syll {
                    return Err(RuleSyntaxError::BadSyllableMatrix(self.curr_tkn.clone()))
                }

                // args.insert(f, b);
                args[f as usize] = b;
                self.advance();
                continue;
            }
            
            return Err(RuleSyntaxError::ExpectedFeature(self.curr_tkn.clone()))
        }


        
        Ok(args)

    }

    fn get_matrix(&mut self) -> Result<Item, RuleSyntaxError> { 
        let start = self.token_list[self.pos-1].position.start;
        let args = self.get_matrix_args(false)?;
        let end = self.token_list[self.pos-1].position.end;
        
        Ok(Item::new(ParseKind::Matrix(args), Position { start, end }))
    }

    fn get_char(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns matrix or None
        let chr = self.char_to_matrix(self.curr_tkn.clone())?;
        self.advance();

        if self.expect(TokenKind::Equals) {
            match self.eat_expect(TokenKind::Number) {
                Some(n) => {
                    let num = n.value.parse::<usize>().unwrap();

                    match self.var_map.get(&num) {
                        Some(val) => return Err(RuleSyntaxError::AlreadyInitialisedVariable(val.clone(), chr, num)),
                        None => self.var_map.insert(num, chr.clone())
                    };
                },
                None => return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()))
            }
        }

        if !self.expect(TokenKind::Colon) {
            return Ok(chr)
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }
        
        let params = self.get_matrix()?;

        let joined = self.join_char_with_params(chr, params);

        Ok(joined)
    }

    fn get_ipa(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns IPA or Matrix

        let ipa = self.ipa_to_vals(self.curr_tkn.clone())?;
        let pos = self.curr_tkn.position;
        self.advance();

        if !self.expect(TokenKind::Colon) {
            return Ok(Item::new(ParseKind::IPA(ipa.clone(), modArrInit!()), pos))
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let params = self.get_matrix()?;

        let joined = self.join_ipa_with_params(ipa, params.clone())?;

        Ok(Item::new(joined, Position {start: pos.start, end: params.position.end}))
    }
    
    fn get_segment(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns IPA / Matrix, with varable number, or None

        if self.peek_expect(TokenKind::Cardinal) {
            let ipa = self.get_ipa()?;
            return Ok(Some(ipa))
        }

        if self.peek_expect(TokenKind::Primative) {
            let chr = self.get_char()?;

            if self.expect(TokenKind::Equals) {
                match self.eat_expect(TokenKind::Number) {
                    Some(n) => {
                        let num = n.value.parse::<usize>().unwrap();

                        match self.var_map.get(&num) {
                            Some(val) => return Err(RuleSyntaxError::AlreadyInitialisedVariable(val.clone(), chr, num)),
                            None => self.var_map.insert(num, chr.clone())
                        };

                        return Ok(Some(chr))
                    },
                    None => return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()))
                }
            }
            return Ok(Some(chr))
        }

        if self.expect(TokenKind::LeftSquare) {
            let matrix = self.get_matrix()?;
            return Ok(Some(matrix))
        }

        return Ok(None)
    }

    fn join_var_with_params(&mut self, mt: Item, params: Item) -> Result<Item, RuleSyntaxError> {

        assert!(params.kind.is_matrix());

        // let kind = ParseKind::Variable(mt.clone(), p);

        // let pos = Position { start: mt.position.start, end: params.position.end };

        // Ok(Item::new(kind, pos))

        if !mt.kind.is_matrix() {
            // variable can only be assigned to a primative or a matrix
            return Err(RuleSyntaxError::BadVariableAssignment(mt))
        }
        
        Ok(self.join_char_with_params(mt, params))
    }

    fn get_var(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        
        match self.eat_expect(TokenKind::Number) {

            Some(t) => {

                // does 't' exist in var_map?
                // if so, replace with value in var_map
                // if not, throw UnknownVariableError

                let num_val = t.value.parse::<usize>().unwrap();

                let mut matched_segs: Item;

                match self.var_map.get(&num_val) {
                    Some(m) => {
                        matched_segs = m.clone();
                        matched_segs.position = t.position;
                    },
                    None => return Err(RuleSyntaxError::UnknownVariable(t))
                }

                if !self.expect(TokenKind::Colon) {
                    return Ok(Some(matched_segs))
                }

                if !self.expect(TokenKind::LeftSquare) {
                    return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
                }

                let params = self.get_matrix()?;

                let joined = self.join_var_with_params(matched_segs, params)?;

                Ok(Some(joined))

            },
            None => return Ok(None)
        }
            
    }

    fn get_optionals(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns optionals or None

        let start = self.curr_tkn.position.start;

        if !self.expect(TokenKind::LeftBracket) {
            return Ok(None)
        }

        let mut segs = Vec::new();
        let mut lo_bound: usize = 0;
        let mut hi_bound: usize = 0;
        while self.has_more_tokens() {
            if self.peek_expect(TokenKind::RightBracket) {
                break;
            }

            if let Some(x) = self.get_segment()? {
                segs.push(x);
                continue;
            }

            if self.peek_expect(TokenKind::Comma){
                break;
            }

            return Err(RuleSyntaxError::ExpectedSegment(self.curr_tkn.clone()))
        }

        // TODO: This needs cleaning up!

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, 1), Position { start, end })))
        }

        if !self.expect(TokenKind::Comma) {
            return Err(RuleSyntaxError::ExpectedComma(self.curr_tkn.clone()))
        }

        if let Some(x) = self.eat_expect(TokenKind::Number) {
            lo_bound = x.value.parse().expect("Could not parse string to number. This is probably an error with the parser itself");
        }

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, lo_bound), Position { start, end })))
        }

        if !self.expect(TokenKind::Colon) {
            return Err(RuleSyntaxError::ExpectedColon(self.curr_tkn.clone()))
        }

        if let Some(x) = self.eat_expect(TokenKind::Number) {
            hi_bound = x.value.parse().expect("Could not parse string to number. This is probably an error with the parser itself");
            if hi_bound < lo_bound {
                return Err(RuleSyntaxError::OptMathError(x, lo_bound, hi_bound))
            }
        }

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, lo_bound, hi_bound), Position { start, end })))
        }

        Err(RuleSyntaxError::ExpectedRightBracket(self.curr_tkn.clone()))
    }

    fn get_set(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns set of segs
        let start = self.curr_tkn.position.start;

        if !self.expect(TokenKind::LeftCurly) {
            return Ok(None)
        }

        let mut segs = Vec::new();

        // TODO: this while condition may mean  "/ _{A, B, C <eol>" is valid input
        // may have to return SyntaxError::ExpectedRightBracketAtEol
        // bug or feature? ¯\_(ツ)_/¯
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

            return Err(RuleSyntaxError::ExpectedSegment(self.curr_tkn.clone()))
        }
        let end = self.token_list[self.pos-1].position.end;
        Ok(Some(Item::new(ParseKind::Set(segs.clone()), Position { start, end })))

    }

    fn get_syll(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns Syll or None
        let start = self.curr_tkn.position.start;

        if !self.expect(TokenKind::Syllable) {
            return Ok(None)
        }

        if !self.expect(TokenKind::Colon) {
            let end = self.curr_tkn.position.start - 1;
            return Ok(Some(Item::new(ParseKind::Syllable(modArrInit!()), Position { start, end })))
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let m = self.get_matrix_args(true)?;

        let end = self.token_list[self.pos-1].position.end;

        Ok(Some(Item::new(ParseKind::Syllable(m), Position { start, end })))
    }

    fn get_term(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
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

        if let Some(x) = self.get_var()? {
            return Ok(Some(x))
        }

        Ok(None)
    }

    fn get_input_term(&mut self) -> Result<Items, RuleSyntaxError> {
        // returns list of terms and ellipses
        let mut terms = Vec::<Item>::new();

        loop {
            if let Some(el) = self.eat_expect(TokenKind::Ellipsis) {
                terms.push(Item::new(ParseKind::Ellipsis(el.clone()), el.position));
                continue;
            }

            if self.peek_expect(TokenKind::GreaterThan) { // So we can try simple rules that don't call functions we are yet to implement
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

    fn get_output_element(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns syllable / segment / variable
        if let Some(x) = self.get_syll()? {
            return Ok(Some(x))
        }

        // if let Some(x) = self.get_set()? {
        //     return Ok(Some(x))
        // }

        if let Some(x) = self.get_segment()? {
            return Ok(Some(x))
        }

        if let Some(x) = self.get_var()? {
            return Ok(Some(x))
        }

        Ok(None)
    }

    fn get_output_term(&mut self) -> Result<Items, RuleSyntaxError>{ 
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

    fn get_empty(&mut self) -> Items {
        if !self.peek_expect(TokenKind::Star) && !self.peek_expect(TokenKind::EmptySet) {
            return Vec::new()
        }
        let token = self.eat();
        vec![Item::new(ParseKind::EmptySet(token.clone()), token.position)]
    }

    fn get_input(&mut self) -> Result<Vec<Items>, RuleSyntaxError> {
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
            return Err(RuleSyntaxError::EmptyInput)
        }

        Ok(inputs)

    }

    fn get_output(&mut self) -> Result<Vec<Items>, RuleSyntaxError> {
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
            return Err(RuleSyntaxError::EmptyOutput)
        }

        Ok(outputs)
    }
  
    fn rule(&mut self) -> Result<Rule, RuleSyntaxError> {
        let mut flg_delete: u8 = 0; 
        let mut flg_metath: u8 = 0; 
        let /*mut*/ flg_redupl: u8 = 0; 

        let input = self.get_input()?;

        let flg_insert: u8 = match input[0][0].kind {
            ParseKind::EmptySet(_) => 8,
            _ => 0
        };

        if !self.expect(TokenKind::Arrow) && !self.expect(TokenKind::GreaterThan) {
            match flg_insert {
                8 => { 
                    if self.peek_expect(TokenKind::Comma) {
                        return Err(RuleSyntaxError::InsertErr)
                    } else {
                        return Err(RuleSyntaxError::ExpectedArrow(self.curr_tkn.clone()))
                    }
                },
                _ => return Err(RuleSyntaxError::ExpectedArrow(self.curr_tkn.clone()))
            }
        }

        let output = self.get_output()?;
        
        match output[0][0].kind {
            ParseKind::Metathesis(_) => flg_metath = 1,
            ParseKind::EmptySet(_)   => flg_delete = 2,
            // ParseKind::Duplicate(_)  => flg_redupl = 4,
            _ => {}
        }

        let rule_type = flg_delete | flg_insert | flg_metath | flg_redupl;
        
        // if flg_insert != 0 && ((flg_metath != 0) | (flg_redupl != 0) | (flg_delete != 0)) {
        if rule_type != 1 && rule_type % 2 != 0 {

            println!("\n\n{}", rule_type);

            panic!("\nAHHHHHHHHHHHH\n");
            //todo: Add Syntax error
        }

        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, Vec::new(), Vec::new(), rule_type, self.var_map.clone()))
        }

        if !self.peek_expect(TokenKind::Slash) && !self.peek_expect(TokenKind::Pipe) {
            match input[0][0].kind {
                ParseKind::EmptySet(_) | ParseKind::Metathesis(_
                ) => { 
                    if self.peek_expect(TokenKind::Comma) {
                        return Err(RuleSyntaxError::DeleteErr)
                    } else {
                        return Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone()))
                    }
                },
                _ => return Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone()))
            }
        }

        let context = self.get_context()?;
        
        let except = self.get_except_block()?;

        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, context, except, rule_type, self.var_map.clone()))
        }
        
        Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone()))

    }
    
    pub fn parse(&mut self) -> Result<Rule, RuleSyntaxError> {
        self.rule()
    }

}


// let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");

#[cfg(test)]
mod parser_tests {

    // macro_rules! map {
    //     ($($k:expr => $v:expr),* $(,)?) => {{
    //         core::convert::From::from([$(($k, $v),)*])
    //     }};
    // }

    use super::*;
    use crate::JSON;

    fn setup(test_str: &str) -> Vec<Token> {

        Lexer::new(String::from(test_str)).get_all_tokens()

    }

    #[test]
    fn test_multi_rule() {
        let maybe_result = Parser:: new(setup("%:[+stress], % > [-stress], [+stress] / _ , #_")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.input.len(), 2);
        assert_eq!(result.output.len(), 2);
        assert_eq!(result.context.len(), 2);
        assert!(result.except.is_empty());
        assert_eq!(result.rule_type, 0);
        assert!(result.variables.is_empty());


        let mut x = modArrInit!();
        let     y = modArrInit!();
        x[FeatType::Stress as usize] = ModKind::Positive;
        let exp_input = vec![ 
            Item::new(ParseKind::Syllable(x), Position { start: 0, end: 11 }),
            Item::new(ParseKind::Syllable(y), Position { start: 13, end: 14 }),
        ];

        let mut x = modArrInit!();
        let mut y = modArrInit!();
        x[FeatType::Stress as usize] = ModKind::Negative;
        y[FeatType::Stress as usize] = ModKind::Positive;
        let exp_output = vec![
            Item::new(ParseKind::Matrix(x), Position { start: 17, end: 26 }),
            Item::new(ParseKind::Matrix(y), Position { start: 28, end: 37 }),
            ];
            
        let exp_context: Items = vec![
            Item::new(ParseKind::Environment(vec![], vec![]), Position { start: 40, end: 41 }),
            Item::new(ParseKind::Environment(vec![Item::new(ParseKind::Boundary(Token::new(TokenKind::WordBoundary, "#".to_owned(), 44, 45)), Position { start: 44, end: 45 })], vec![]), Position { start: 44, end: 46 }),
        ];

        assert_eq!(result.input[0][0], exp_input[0]);
        assert_eq!(result.input[1][0], exp_input[1]);

        assert_eq!(result.output[0][0], exp_output[0]);
        assert_eq!(result.output[1][0], exp_output[1]);

        assert_eq!(result.context[0], exp_context[0]);
        assert_eq!(result.context[1], exp_context[1]);
    }

    #[test]
    fn test_metathesis() {
        let maybe_result = Parser:: new(setup("t͡ɕ...b͡β > &")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.input.len(), 1);
        assert_eq!(result.output.len(), 1);
        assert!(result.context.is_empty());
        assert!(result.except.is_empty());
        assert_eq!(result.rule_type, 1);
        assert!(result.variables.is_empty());

        let exp_input_res = vec![
            Item::new(ParseKind::IPA(JSON.get("t͡ɕ").unwrap().clone(), modArrInit!()), Position { start: 0, end: 3 }),
            Item::new(ParseKind::Ellipsis(Token::new(TokenKind::Ellipsis, "...".to_owned(), 3, 6)), Position { start: 3, end: 6 }),
            Item::new(ParseKind::IPA(JSON.get("b͡β").unwrap().clone(), modArrInit!()), Position { start: 6, end: 9 }),
        ];

        assert_eq!(result.input[0][0], exp_input_res[0]);
        assert_eq!(result.input[0][1], exp_input_res[1]);
        assert_eq!(result.input[0][2], exp_input_res[2]);
    }

    #[test]
    fn test_variables_plain() {

        let mut x = modArrInit!();
        x[FeatType::Syllabic as usize] = ModKind::Negative;
        
        let c = Item::new(ParseKind::Matrix(x), Position { start: 0, end: 1 });

        let mut y = modArrInit!();
        y[FeatType::Consonantal as usize] = ModKind::Negative;
        y[FeatType::Sonorant as usize] = ModKind::Positive;
        y[FeatType::Syllabic as usize] = ModKind::Positive;

        let v = Item::new(ParseKind::Matrix(y), Position { start: 4, end: 5 });

        let maybe_result = Parser:: new(setup("C=1 V=2 > 2 1 / _C")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.input.len(), 1);
        assert_eq!(result.output.len(), 1);
        assert_eq!(result.context.len(), 1);
        assert!(result.except.is_empty());
        assert_eq!(result.rule_type, 0);

        assert_eq!(result.variables.len(), 2);
        assert!(result.variables.contains_key(&1));
        assert!(result.variables.contains_key(&2));
        assert_eq!(result.variables.get(&1), Some(&c));
        assert_eq!(result.variables.get(&2), Some(&v));
        assert_eq!(result.variables.get(&3), None);
    }
}
