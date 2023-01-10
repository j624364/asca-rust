use std::{
    collections::HashMap, 
    fmt
};

use crate::{
    lexer::*, 
    rule ::*, 
    error::*, 
    word ::Segment,
    rule ::RuleType,
    CARDINALS_MAP, 
};

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum ModKind {
//     Negative,
//     Positive,
//     Alpha(char),
//     InversAlpha(char),
//     Number(String),
//     Unspecified
// } 
// impl Default for ModKind {
//     fn default() -> Self { ModKind::Unspecified }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinMod {
    Negative,
    Positive
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlphaMod {
    Alpha(char),
    InversAlpha(char)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SegMKind{
    Binary(BinMod),
    Alpha(AlphaMod),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SuprMKind{
    Number(String),
    Alpha(AlphaMod),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mods {
    Binary(BinMod),
    Number(String),
    Alpha(AlphaMod),
}

// Modkinds {
//     Binary(Neg | Pos),
//     Alpha(Reg | Inv),
//     Number(String),     
//     Unspecified
// }

// macro_rules! map {
    //     ($($k:expr => $v:expr),* $(,)?) => {{
    //         core::convert::From::from([$(($k, $v),)*])
    //     }};
    // }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Modifiers {
    pub nodes: [Option<SegMKind>; NodeType::Pharyngeal as usize + 1],
    pub feats: [Option<SegMKind>; FType::RetractedTongueRoot as usize + 1],
    pub suprs: [Option<SuprMKind>; 3], // [Stress, Length, Tone]
}

impl Modifiers {
    pub fn new() -> Self {
        Self { 
            nodes: [();NodeType::Pharyngeal as usize + 1].map(|_| None), 
            feats: [();FType::RetractedTongueRoot as usize + 1].map(|_| None), 
            suprs: [();3].map(|_| None)
        }
    }
}

type Items = Vec<Item>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseKind {
    EmptySet   ,
    WordBound  ,
    SyllBound  ,
    Ellipsis   ,
    Metathesis ,
    Variable   (Token  , Modifiers),
    Ipa        (Segment, Modifiers),
    Matrix     (Modifiers),
    Syllable   (Option<SuprMKind>, Option<SuprMKind>), // (Stress, Tone)  // PrimStress/SecStress converts to Stress
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
                write!(f, "{} = {:#?}", t, p)
            },
            ParseKind::EmptySet   => write!(f, "∅"),
            ParseKind::WordBound  => write!(f, "#"),
            ParseKind::SyllBound  => write!(f, "$"),
            ParseKind::Ellipsis   => write!(f, "…"),
            ParseKind::Metathesis => write!(f, "&"),

            ParseKind::Ipa(s, m) => write!(f, "{:?} + {:?}", s, m),

            ParseKind::Matrix(tokens) => {
                write!(f, "{:#?}", tokens)
            },
            ParseKind::Syllable(str, tone) => {
                write!(f, "{:#?} : {:#?}", str, tone)
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

// TODO: Allow Set in Output if there is a matching set in input
pub struct Parser {
    token_list: Vec<Token>,
    line: usize,
    pos: usize,
    curr_tkn: Token,
    var_map: HashMap<usize,Item>
}

impl Parser {
    pub fn new(lst: Vec<Token>, line: usize) -> Self {
        let mut s = Self { 
            token_list: lst, 
            line,
            pos: 0, 
            curr_tkn: Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { line, start: 0, end: 1 } },
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
            Token { kind: TokenKind::Eol, value: "eol".to_string(), position: Position { line: self.line, start: self.pos, end: self.pos+1 } }
        }
    }

    fn has_more_tokens(&self) -> bool { self.pos < self.token_list.len() }

    fn peek_expect(&self, knd: TokenKind) -> bool { self.curr_tkn.kind == knd }

    fn expect(&mut self, knd: TokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            self.advance();
            true
        } else {
            false
        }
    }

    fn eat(&mut self) -> Token {
        let token = self.curr_tkn.clone();
        self.advance();
        token
    }

    fn eat_expect(&mut self, knd: TokenKind) -> Option<Token> {
        if self.curr_tkn.kind == knd {
            Some(self.eat())
        } else {
            None
        }
    }

    fn get_bound(&mut self) -> Option<Item> { 
        if self.peek_expect(TokenKind::SyllBoundary) {
            let token = self.eat();
            return Some(Item::new(ParseKind::SyllBound, token.position))
        }

        if self.peek_expect(TokenKind::WordBoundary) {
            let token = self.eat();
            return Some(Item::new(ParseKind::WordBound, token.position))
        }

        None
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
                els.push(Item::new(ParseKind::Ellipsis, el.position));
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

        Ok(Item::new(ParseKind::Environment(before, after), Position { line: self.line, start, end }))
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

        let v = vec![
            Item::new(ParseKind::Environment(x.clone(), Vec::new()), Position { line: self.line, start, end }),
            Item::new(ParseKind::Environment(Vec::new(), x), Position { line: self.line, start, end })
        ];

        Ok(Some(v))

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
            ParseKind::Matrix(p) => Ok(ParseKind::Ipa(ipa, p)),
            _ => unreachable!("\nCritical Error: 'Parameters' being joined with IPA Segment is not a matrix!\nThis is a bug."),
        }

    }
    
    fn join_char_with_params(&mut self, character: Item, parameters: Item) -> Item {
        let mut chr = character.kind.as_matrix().expect("\nCritical Error: 'Char' being joined with 'Parameters' is not a matrix!\nThis is a bug.").clone();
        let params = parameters.kind.as_matrix().expect("\nCritical Error: 'Parameters' being joined with 'Char' is not a matrix!\nThis is a bug.").clone(); 
        
        // TODO: test this
        for (i, p) in params.nodes.iter().enumerate() {
            if p.is_none() {
                continue
            }
            chr.nodes[i] = *p
        }

        for (i, p) in params.feats.iter().enumerate() {
            if p.is_none() {
                continue
            }
            chr.feats[i] = *p
        }

        for (i, p) in params.suprs.iter().enumerate() {
            if p.is_none() {
                continue
            }
            chr.suprs[i] = p.clone()
        }
        
        Item::new(ParseKind::Matrix(chr), Position { line: self.line, start: character.position.start, end: parameters.position.end })
    }

    fn ipa_to_vals(&self, ipa: Token) -> Result<Segment, RuleSyntaxError> {
        match CARDINALS_MAP.get(&ipa.value) {
            Some(z) => Ok(*z),
            None => Err(RuleSyntaxError::UnknownIPA(ipa))
        }
    }

    fn char_to_matrix(&self, chr: Token) -> Result<Item, RuleSyntaxError> {
        // returns matrix or None
        use FType::*;
        use SegMKind::*;

        const SYLL_M: (FType, SegMKind) = (Syllabic, Binary(BinMod::Negative));
        const SYLL_P: (FType, SegMKind) = (Syllabic, Binary(BinMod::Positive));

        const CONS_M: (FType, SegMKind) = (Consonantal, Binary(BinMod::Negative));
        const CONS_P: (FType, SegMKind) = (Consonantal, Binary(BinMod::Positive));
        
        const SONR_M: (FType, SegMKind) = (Sonorant, Binary(BinMod::Negative));
        const SONR_P: (FType, SegMKind) = (Sonorant, Binary(BinMod::Positive));

        const APPR_M: (FType, SegMKind) = (Approximant, Binary(BinMod::Negative));
        const APPR_P: (FType, SegMKind) = (Approximant, Binary(BinMod::Positive));



        let char_vals = match chr.value.as_str() {
            "C" => vec![SYLL_M],                         // -syll                     // Consonant
            "O" => vec![CONS_P, SONR_M, SYLL_M],         // +cons, -son, -syll        // Obstruent
            "S" => vec![CONS_P, SONR_P, SYLL_M],         // +cons, +son, -syll        // Sonorant
            "L" => vec![CONS_P, SONR_P, SYLL_M, APPR_P], // +cons, +son, -syll, +appr // Liquid
            "N" => vec![CONS_P, SONR_P, SYLL_M, APPR_M], // +cons, +son, -syll, -appr // Nasal
            "G" => vec![CONS_M, SONR_P, SYLL_M],         // -cons, +son, -syll        // Glide
            "V" => vec![CONS_M, SONR_P, SYLL_P],         // -cons, +son, +syll        // Vowel
            _ => return Err(RuleSyntaxError::UnknownGrouping(chr)),
        };

        let mut args = Modifiers::new(); 

        for (feature, value) in char_vals {
            // args.insert(feature, value);
            args.feats[feature as usize] = Some(value)
        }


        Ok(Item::new(ParseKind::Matrix(args), Position { line: self.line, start: chr.position.start, end: chr.position.end }))
    }

    fn is_feature(&self) -> bool{ matches!(self.curr_tkn.kind, TokenKind::Feature(_)) }

    fn feature_to_modifier(&self, feature: FeatType, value: String) -> (FeatType, Mods) {

        match value.as_str() { //todo: come up with something better than this
            "+" => {
                if feature == FeatType::Supr(SupraType::Long) {
                    return (FeatType::Supr(SupraType::Length), Mods::Number("2".to_owned()))
                } 
                if feature == FeatType::Supr(SupraType::Overlong) {
                    return (FeatType::Supr(SupraType::Length), Mods::Number("3".to_owned()))
                }
                if feature == FeatType::Supr(SupraType::PrimStress) {
                    return (FeatType::Supr(SupraType::Stress), Mods::Number("1".to_owned()))
                }
                if feature == FeatType::Supr(SupraType::SecStress) {
                    return (FeatType::Supr(SupraType::Stress), Mods::Number("2".to_owned()))
                }
                (feature, Mods::Binary(BinMod::Positive))
            },
            "-" => {
                if feature == FeatType::Supr(SupraType::Long) {
                    return (FeatType::Supr(SupraType::Length), Mods::Number("1".to_owned()))
                } 
                if feature == FeatType::Supr(SupraType::Overlong) {
                    return (FeatType::Supr(SupraType::Length), Mods::Number("<3".to_owned()))
                }
                if feature == FeatType::Supr(SupraType::PrimStress) {
                    return (FeatType::Supr(SupraType::Stress), Mods::Number("0".to_owned()))
                }
                if feature == FeatType::Supr(SupraType::SecStress) {
                    return (FeatType::Supr(SupraType::Stress), Mods::Number("<2".to_owned()))
                }
                (feature, Mods::Binary(BinMod::Negative))
            },
            "α"|"β"|"γ"|"δ"|"ε"|"ζ"|"η"|"θ"|"ι"|"κ"|"λ"|"μ"|"ν"|"ξ"|"ο"|"π"|"ρ"|"σ"|"ς"|"τ"|"υ"|"φ"|"χ"|"ψ"|"ω" => 
                (feature, Mods::Alpha(AlphaMod::Alpha(value.chars().collect::<Vec<char>>()[0]))), // TODO: there MUST be a better way to do this, I feel like YandereDev rn
            "-α"|"-β"|"-γ"|"-δ"|"-ε"|"-ζ"|"-η"|"-θ"|"-ι"|"-κ"|"-λ"|"-μ"|"-ν"|"-ξ"|"-ο"|"-π"|"-ρ"|"-σ"|"-ς"|"-τ"|"-υ"|"-φ"|"-χ"|"-ψ"|"-ω" =>
                (feature, Mods::Alpha(AlphaMod::InversAlpha(value.chars().collect::<Vec<char>>()[1]))),
            _ => if feature == FeatType::Supr(SupraType::Tone) 
            || feature == FeatType::Supr(SupraType::Length) 
            || feature == FeatType::Supr(SupraType::Stress) { // we already know its a number if this matches thanks to the lexer
                (feature, Mods::Number(value))
            } else {
                unreachable!();
            }
        }
    }

    fn curr_token_to_modifier(&self) -> (FeatType, Mods) {
        match self.curr_tkn.kind {
            TokenKind::Feature(x) => self.feature_to_modifier(x, self.curr_tkn.value.clone()),
            _ => unreachable!(),
        }
    }

    fn get_matrix_args(&mut self, is_syll: bool) -> Result<Modifiers, RuleSyntaxError> {
        // returns matrix or None
        
        let mut args = Modifiers::new();
        while self.has_more_tokens() {
            if self.expect(TokenKind::RightSquare) {
                break;
            }
            
            if self.expect(TokenKind::Comma) {
                continue;
            }
            
            if self.is_feature() {
                let (ft, mk) = self.curr_token_to_modifier();

                if (
                    ft != FeatType::Supr(SupraType::Tone) && ft != FeatType::Supr(SupraType::Stress)
                   ) && is_syll {
                    return Err(RuleSyntaxError::BadSyllableMatrix(self.curr_tkn.clone()))
                }

                match ft {
                    FeatType::Node(t) => args.nodes[t as usize] = match mk {
                        Mods::Binary(b) => Some(SegMKind::Binary(b)),
                        Mods::Alpha(a)  => Some(SegMKind::Alpha(a)),
                        Mods::Number(_) => unreachable!(),
                    },
                    FeatType::Feat(t) => args.feats[t as usize] = match mk {
                        Mods::Binary(b) => Some(SegMKind::Binary(b)),
                        Mods::Alpha(a)  => Some(SegMKind::Alpha(a)),
                        Mods::Number(_) => unreachable!(),
                    },
                    FeatType::Supr(t) => args.suprs[t as usize - 4] = match mk { // TODO: '-4'  is a hack 
                        Mods::Number(n) => Some(SuprMKind::Number(n)),
                        Mods::Alpha(a)  => Some(SuprMKind::Alpha(a)),
                        Mods::Binary(_) => unreachable!(),
                    }
                }
                // args[f as usize] = b;
                self.advance();
                continue;
            }
            
            if self.curr_tkn.kind == TokenKind::Eol {
                return Err(RuleSyntaxError::UnexpectedEol(self.curr_tkn.clone(), ']'))
            }
            return Err(RuleSyntaxError::ExpectedFeature(self.curr_tkn.clone()))
        }


        
        Ok(args)

    }

    fn get_matrix(&mut self) -> Result<Item, RuleSyntaxError> { 
        let start = self.token_list[self.pos-1].position.start;
        let args = self.get_matrix_args(false)?;
        let end = self.token_list[self.pos-1].position.end;
        
        Ok(Item::new(ParseKind::Matrix(args), Position { line: self.line, start, end }))
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
            return Ok(Item::new(ParseKind::Ipa(ipa, Modifiers::new()), pos))
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let params = self.get_matrix()?;

        let joined = self.join_ipa_with_params(ipa, params.clone())?;

        Ok(Item::new(joined, Position { line: self.line, start: pos.start, end: params.position.end }))
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

        Ok(None)
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
            None => Ok(None)
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

        // TODO: This might need cleaning up!

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, 1), Position { line: self.line, start, end })))
        }

        if !self.expect(TokenKind::Comma) {
            return Err(RuleSyntaxError::ExpectedComma(self.curr_tkn.clone()))
        }

        if let Some(x) = self.eat_expect(TokenKind::Number) {
            lo_bound = x.value.parse().expect("Could not parse string to number. This is probably an error with the parser itself");
        }

        if self.expect(TokenKind::RightBracket) {
            let end = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, lo_bound), Position { line: self.line, start, end })))
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
            return Ok(Some(Item::new(ParseKind::Optional(segs, lo_bound, hi_bound), Position { line: self.line, start, end })))
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

        // NOTE: this while condition may allow  "/ _{A, B, C <eol>" as valid input
        // should probably return SyntaxError::ExpectedRightBracketAtEol
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
        Ok(Some(Item::new(ParseKind::Set(segs.clone()), Position { line: self.line, start, end })))

    }

    fn get_syll(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns Syll or None
        let start = self.curr_tkn.position.start;

        if !self.expect(TokenKind::Syllable) {
            return Ok(None)
        }

        if !self.expect(TokenKind::Colon) {
            let end = self.curr_tkn.position.start - 1;
            return Ok(Some(Item::new(ParseKind::Syllable(None, None), Position { line: self.line, start, end })))
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let m = self.get_matrix_args(true)?;

        let end = self.token_list[self.pos-1].position.end;
        
        Ok(Some(Item::new(ParseKind::Syllable( m.suprs[SupraType::Stress as usize - 4].clone(),  m.suprs[SupraType::Tone as usize - 4].clone()), Position { line: self.line, start, end })))
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
                terms.push(Item::new(ParseKind::Ellipsis, el.position));
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

        let mut terms = Vec::new();

        while let Some(trm) = self.get_output_element()? {
            terms.push(trm);
        }

        Ok(terms)
    }

    fn get_empty(&mut self) -> Items {
        if !self.peek_expect(TokenKind::Star) && !self.peek_expect(TokenKind::EmptySet) {
            return Vec::new()
        }
        let token = self.eat();
        vec![Item::new(ParseKind::EmptySet, token.position)]
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
            outputs.push(vec![Item::new(ParseKind::Metathesis, el.position)]);
            return Ok(outputs)
        }
        // NOTE: need to add check for `+` (Duplication) if/when added

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

        let input = self.get_input()?;

        let mut rule_type: Option<RuleType> = match input[0][0].kind {
            ParseKind::EmptySet => Some(RuleType::Insertion),
            _ => None
        };

        if !self.expect(TokenKind::Arrow) && !self.expect(TokenKind::GreaterThan) {
            match rule_type {
                Some(_) => { 
                    if self.peek_expect(TokenKind::Comma) {
                        return Err(RuleSyntaxError::InsertErr)
                    } else {
                        return Err(RuleSyntaxError::ExpectedArrow(self.curr_tkn.clone()))
                    }
                },
                None => return Err(RuleSyntaxError::ExpectedArrow(self.curr_tkn.clone()))
            }
        }

        let output = self.get_output()?;

        match output[0][0].kind {
            ParseKind::Metathesis => match rule_type {
                Some(_) => todo!("Syntax Error: Insertion + Metathesis"),
                None => rule_type = Some(RuleType::Metathesis),
            },
            ParseKind::EmptySet   => match rule_type {
                Some(_) => todo!("Syntax Error: Insertion + Deletion"),
                None => rule_type = Some(RuleType::Deletion)
            },
            // ParseKind::Duplicate(_)  => {},
            _ => {}
        }

        let rt= rule_type.unwrap_or(RuleType::Substitution);
        
        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, Vec::new(), Vec::new(), rt, self.var_map.clone()))
        }

        if !self.peek_expect(TokenKind::Slash) && !self.peek_expect(TokenKind::Pipe) {
            match rt {
                RuleType::Substitution => return Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone())),
                _ => {
                    if self.peek_expect(TokenKind::Comma) {
                        return Err(RuleSyntaxError::DeleteErr)
                    } else {
                        return Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone()))
                    }
                }
            }
        }

        let context = self.get_context()?;
        
        let except = self.get_except_block()?;

        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, context, except, rt, self.var_map.clone()))
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
    use crate::CARDINALS_MAP;

    fn setup(test_str: &str) -> Vec<Token> {

        Lexer::new(&String::from(test_str),0).get_all_tokens().unwrap()

    }

    #[test]
    fn test_multi_rule() {
        let maybe_result = Parser:: new(setup("%:[+stress], % > [-stress], [+stress] / _ , #_"),0).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.input.len(), 2);
        assert_eq!(result.output.len(), 2);
        assert_eq!(result.context.len(), 2);
        assert!(result.except.is_empty());
        assert_eq!(result.rule_type, RuleType::Substitution);
        assert!(result.variables.is_empty());


        let exp_input = vec![ 
            Item::new(ParseKind::Syllable(Some(SuprMKind::Number("1".to_owned())), None), Position { line: 0, start: 0, end: 11 }),
            Item::new(ParseKind::Syllable(None, None), Position { line: 0, start: 13, end: 14 }),
        ];

        let mut x = Modifiers::new();
        let mut y = Modifiers::new();
        x.suprs[SupraType::Stress as usize - 4] = Some(SuprMKind::Number("0".to_owned()));
        y.suprs[SupraType::Stress as usize - 4] = Some(SuprMKind::Number("1".to_owned()));
        let exp_output = vec![
            Item::new(ParseKind::Matrix(x), Position { line: 0, start: 17, end: 26 }),
            Item::new(ParseKind::Matrix(y), Position { line: 0, start: 28, end: 37 }),
            ];
            
        let exp_context: Items = vec![
            Item::new(ParseKind::Environment(vec![], vec![]), Position { line: 0, start: 40, end: 41 }),
            Item::new(ParseKind::Environment(vec![Item::new(ParseKind::WordBound, Position { line: 0, start: 44, end: 45 })], vec![]), Position { line: 0, start: 44, end: 46 }),
        ];

        assert_eq!(result.input[0][0], exp_input[0], "1");
        assert_eq!(result.input[1][0], exp_input[1], "2");

        assert_eq!(result.output[0][0], exp_output[0], "3");
        assert_eq!(result.output[1][0], exp_output[1], "4");

        assert_eq!(result.context[0], exp_context[0], "5");
        assert_eq!(result.context[1], exp_context[1], "6");
    }

    #[test]
    fn test_metathesis() {
        let maybe_result = Parser:: new(setup("t͡ɕ...b͡β > &"),0).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.input.len(), 1);
        assert_eq!(result.output.len(), 1);
        assert!(result.context.is_empty());
        assert!(result.except.is_empty());
        assert_eq!(result.rule_type, RuleType::Metathesis);
        assert!(result.variables.is_empty());

        let exp_input_res = vec![
            Item::new(ParseKind::Ipa(CARDINALS_MAP.get("t͡ɕ").unwrap().clone(), Modifiers::new()), Position { line: 0, start: 0, end: 3 }),
            Item::new(ParseKind::Ellipsis, Position { line: 0, start: 3, end: 6 }),
            Item::new(ParseKind::Ipa(CARDINALS_MAP.get("b͡β").unwrap().clone(), Modifiers::new()), Position { line: 0, start: 6, end: 9 }),
        ];

        assert_eq!(result.input[0][0], exp_input_res[0]);
        assert_eq!(result.input[0][1], exp_input_res[1]);
        assert_eq!(result.input[0][2], exp_input_res[2]);
    }

    #[test]
    fn test_variables_plain() {

        let mut x = Modifiers::new();
        x.feats[FType::Syllabic as usize] = Some(SegMKind::Binary(BinMod::Negative));
        
        let c = Item::new(ParseKind::Matrix(x), Position { line: 0, start: 0, end: 1 });

        let mut y = Modifiers::new();
        y.feats[FType::Consonantal as usize] = Some(SegMKind::Binary(BinMod::Negative));
        y.feats[FType::Sonorant as usize] = Some(SegMKind::Binary(BinMod::Positive));
        y.feats[FType::Syllabic as usize] = Some(SegMKind::Binary(BinMod::Positive));

        let v = Item::new(ParseKind::Matrix(y), Position { line: 0, start: 4, end: 5 });

        let maybe_result = Parser:: new(setup("C=1 V=2 > 2 1 / _C"), 0).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.input.len(), 1);
        assert_eq!(result.output.len(), 1);
        assert_eq!(result.context.len(), 1);
        assert!(result.except.is_empty());
        assert_eq!(result.rule_type, RuleType::Substitution);

        assert_eq!(result.variables.len(), 2);
        assert!(result.variables.contains_key(&1));
        assert!(result.variables.contains_key(&2));
        assert_eq!(result.variables.get(&1), Some(&c));
        assert_eq!(result.variables.get(&2), Some(&v));
        assert_eq!(result.variables.get(&3), None);
    }
}
