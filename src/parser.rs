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
pub enum Mods {
    Binary(BinMod),
    Number(String),
    Alpha(AlphaMod),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Supr {
    pub kind: SupraType,
    pub modifier: SegMKind
}

impl Supr {
    pub fn new(kind: SupraType, modifier:SegMKind) -> Self {
        Self {kind, modifier}
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Supras {
    pub stress: Option<Supr>,
    pub length: Option<Supr>,
    pub tone: Option<String>,
}

impl Supras {
    pub fn new(stress: Option<Supr>, length: Option<Supr>, tone: Option<String>) -> Self {
        Supras {stress, length, tone }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Modifiers {
    pub nodes: [Option<SegMKind>; NodeType::count()],
    pub feats: [Option<SegMKind>; FType::count()],
    pub suprs: Supras, 
}

impl Modifiers {
    pub fn new() -> Self {
        debug_assert_eq!(NodeType::Pharyngeal as usize + 1, NodeType::count());
        debug_assert_eq!(FType::RetractedTongueRoot as usize + 1, FType::count());

        Self { 
            nodes: [();NodeType::count()].map(|_| None), 
            feats: [();FType::count()].map(|_| None), 
            suprs: Supras::new(None, None, None)
        }
    }
}

// type Items = Vec<Item>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseKind {
    EmptySet   ,
    WordBound  ,
    SyllBound  ,
    Ellipsis   ,
    Metathesis ,
    Variable   (Token  , Option<Modifiers>),
    Ipa        (Segment, Option<Modifiers>),
    Matrix     (Modifiers),
    Syllable   (Option<Supr>, Option<String>), // (Stress, Tone) 
    Set        (Vec<Item>),
    Optional   (Vec<Item>, usize, usize),
    Environment(Vec<Item>, Vec<Item>),
}

impl ParseKind {
    fn as_matrix(&self) -> Option<&Modifiers> {
        if let Self::Matrix(v) = self {
            Some(v)
        } else {
            None
        }
    }

    // pub fn is_matrix(&self) -> bool {
    //     matches!(self, Self::Matrix(..))
    // }
    //
    // pub fn is_ipa(&self) -> bool {
    //     matches!(self, Self::IPA(..))
    // }
    //
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

            ParseKind::Optional(..) => todo!(),

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
            curr_tkn: Token { kind: TokenKind::Eol, value: String::new(), position: Position::new(line, 0, 1 ) },
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
            Token { kind: TokenKind::Eol, value: String::new(), position: Position::new(self.line, self.pos, self.pos+1) }
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
        if let Some(token) = self.eat_expect(TokenKind::SyllBoundary) {
            return Some(Item::new(ParseKind::SyllBound, token.position))
        }
        if let Some(token) = self.eat_expect(TokenKind::WordBoundary) {
            return Some(Item::new(ParseKind::WordBound, token.position))
        }
        None
    }

    fn get_env_elements(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
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

        if self.peek_expect(TokenKind::Underline) {
            return Err(RuleSyntaxError::TooManyUnderlines(self.curr_tkn.clone()))
        }

        let end = self.token_list[self.pos-1].position.end;

        Ok(Item::new(ParseKind::Environment(before, after), Position::new(self.line, start, end)))
    }

    fn get_spec_env(&mut self) -> Result<Option<Vec<Item>>, RuleSyntaxError> {

        let start = self.curr_tkn.position.start;
        let pstn = self.pos;

        if !self.expect(TokenKind::Underline) {
            return Ok(None)
        }

        debug_assert_eq!(pstn, self.pos - 1);

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
            Item::new(ParseKind::Environment(x.clone(), Vec::new()), Position::new(self.line, start, end)),
            Item::new(ParseKind::Environment(Vec::new(), x), Position::new(self.line, start, end))
        ];

        Ok(Some(v))

    }

    fn get_env(&mut self) -> Result<Vec<Item>, RuleSyntaxError> { 
        // returns environment
        if let Some(s) = self.get_spec_env()? { return Ok(s) }

        let mut envs = Vec::new();
        loop {
            let x = self.get_env_term()?;
            envs.push(x);

            if !self.expect(TokenKind::Comma) {
                break
            }
        }

        if envs.is_empty() { return Err(RuleSyntaxError::EmptyEnv) }

        Ok(envs)
    }

    fn get_except_block(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        if !self.expect(TokenKind::Pipe) {
            return Ok(Vec::new())
        }
        self.get_env()
    }

    fn get_context(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        if !self.expect(TokenKind::Slash) {
            return Ok(Vec::new())
        }
        self.get_env()
    }

    fn join_group_with_params(&mut self, character: Item, parameters: Item) -> Item {
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

        chr.suprs.stress = if params.suprs.stress.is_none() {chr.suprs.stress} else {params.suprs.stress};
        chr.suprs.length = if params.suprs.length.is_none() {chr.suprs.length} else {params.suprs.length};
        chr.suprs.tone   = if params.suprs.tone.is_none()   {chr.suprs.tone}   else {params.suprs.tone};

        Item::new(ParseKind::Matrix(chr), Position::new(self.line, character.position.start, parameters.position.end ))
    }

    fn ipa_to_vals(&self, ipa: Token) -> Result<Segment, RuleSyntaxError> {
        match CARDINALS_MAP.get(&ipa.value) {
            Some(z) => Ok(*z),
            None => Err(RuleSyntaxError::UnknownIPA(ipa))
        }
    }

    fn group_to_matrix(&self, chr: Token) -> Result<Item, RuleSyntaxError> {
        // returns matrix or None
        use FType::*;
        use SegMKind::*;

        const SYLL_M: (FType, SegMKind) = (Syllabic,    Binary(BinMod::Negative));
        const SYLL_P: (FType, SegMKind) = (Syllabic,    Binary(BinMod::Positive));
        const CONS_M: (FType, SegMKind) = (Consonantal, Binary(BinMod::Negative));
        const CONS_P: (FType, SegMKind) = (Consonantal, Binary(BinMod::Positive));
        const SONR_M: (FType, SegMKind) = (Sonorant,    Binary(BinMod::Negative));
        const SONR_P: (FType, SegMKind) = (Sonorant,    Binary(BinMod::Positive));
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
            args.feats[feature as usize] = Some(value)
        }


        Ok(Item::new(ParseKind::Matrix(args), Position::new(self.line, chr.position.start, chr.position.end )))
    }

    fn is_feature(&self) -> bool{ matches!(self.curr_tkn.kind, TokenKind::Feature(_)) }

    fn curr_token_to_modifier(&self) -> (FeatType, Mods) {
        match self.curr_tkn.kind {
            TokenKind::Feature(feature) => {
                let value = &self.curr_tkn.value;
                match value.as_str() {
                    "+" => (feature, Mods::Binary(BinMod::Positive)),
                    "-" => (feature, Mods::Binary(BinMod::Negative)),
                    "α"|"β"|"γ"|"δ"|"ε"|"ζ"|"η"|"θ"|"ι"|"κ"|"λ"|"μ"|"ν"|"ξ"|"ο"|"π"|"ρ"|"σ"|"ς"|"τ"|"υ"|"φ"|"χ"|"ψ"|"ω" => 
                        (feature, Mods::Alpha(AlphaMod::Alpha(value.chars().next().expect("Unable to index 'alpha' string")))),
                    "-α"|"-β"|"-γ"|"-δ"|"-ε"|"-ζ"|"-η"|"-θ"|"-ι"|"-κ"|"-λ"|"-μ"|"-ν"|"-ξ"|"-ο"|"-π"|"-ρ"|"-σ"|"-ς"|"-τ"|"-υ"|"-φ"|"-χ"|"-ψ"|"-ω" =>
                        (feature, Mods::Alpha(AlphaMod::InversAlpha(value.chars().nth(1).expect("Unable to index 'inv-alpha' string")))),
                    _ if feature == FeatType::Supr(SupraType::Tone) => (feature, Mods::Number(value.to_owned())),
                    _ => {
                        unreachable!();
                    }
                }
            },
            _ => unreachable!(),
        }
    }

    fn get_param_args(&mut self, is_syll: bool) -> Result<Modifiers, RuleSyntaxError> {
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
                    // FIXME: This is horrible
                    FeatType::Supr(t) => match mk {
                        Mods::Number(n) => args.suprs.tone = Some(n),
                        Mods::Alpha(a) => match t {
                            SupraType::Long => args.suprs.length = Some(Supr::new(SupraType::Long, SegMKind::Alpha(a))),
                            SupraType::Overlong => args.suprs.length = Some(Supr::new(SupraType::Overlong, SegMKind::Alpha(a))),
                            SupraType::Stress => args.suprs.stress = Some(Supr::new(SupraType::Stress, SegMKind::Alpha(a))),
                            SupraType::SecStress => args.suprs.stress = Some(Supr::new(SupraType::Stress, SegMKind::Alpha(a))),
                            SupraType::Tone => unreachable!("Tone cannot be `Alpha'd` (yet anyway)"),
                        },
                        Mods::Binary(b) => match t {
                            SupraType::Long => args.suprs.length = Some(Supr::new(SupraType::Long, SegMKind::Binary(b))),
                            SupraType::Overlong => args.suprs.length = Some(Supr::new(SupraType::Overlong, SegMKind::Binary(b))),
                            SupraType::Stress => args.suprs.stress = Some(Supr::new(SupraType::Stress, SegMKind::Binary(b))),
                            SupraType::SecStress => args.suprs.stress = Some(Supr::new(SupraType::Stress, SegMKind::Binary(b))),
                            SupraType::Tone => unreachable!("Tone cannot be `+/-`"),
                        },
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

    fn get_params(&mut self) -> Result<Item, RuleSyntaxError> { 
        let start = self.token_list[self.pos-1].position.start;
        let args = self.get_param_args(false)?;
        let end = self.token_list[self.pos-1].position.end;
        
        Ok(Item::new(ParseKind::Matrix(args), Position::new(self.line, start, end)))
    }

    fn get_group(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns matrix or None
        let chr = self.group_to_matrix(self.curr_tkn.clone())?;
        self.advance();

        if !self.expect(TokenKind::Colon) {
            return Ok(chr)
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }
        
        let params = self.get_params()?;

        let joined_matrix = self.join_group_with_params(chr, params);

        Ok(joined_matrix)
    }

    fn get_ipa(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns IPA or Matrix
        let ipa = self.ipa_to_vals(self.curr_tkn.clone())?;
        let pos = self.curr_tkn.position;
        self.advance();

        if !self.expect(TokenKind::Colon) {
            return Ok(Item::new(ParseKind::Ipa(ipa, None), pos))
        }
        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }
        let params = self.get_params()?;

        let joined = ParseKind::Ipa(ipa, Some(params.kind.as_matrix().expect("'Parameters' being joined with IPA Segment is not a matrix! This is a bug").clone()));

        Ok(Item::new(joined, Position::new(self.line, pos.start, params.position.end )))
    }
    
    fn get_var_assign(&mut self, n: Token, chr: &Item) -> Result<(), RuleSyntaxError> {
        // TODO: Work out Variable Assignment
        // This current strategy will not work
        let num = n.value.parse::<usize>().expect("Couldn't parse to VAR to number");
        match self.var_map.get(&num) {
            Some(val) => return Err(RuleSyntaxError::AlreadyInitialisedVariable(val.clone(), chr.clone(), num)),
            None => self.var_map.insert(num, chr.clone())
        };

        Ok(())
    }

    fn get_seg(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns IPA / Matrix, with varable number, or None
        if self.peek_expect(TokenKind::Cardinal) {
            let ipa = self.get_ipa()?;
            return Ok(Some(ipa))
        }
        if self.peek_expect(TokenKind::Group) {
            let chr = self.get_group()?;

            if self.expect(TokenKind::Equals) {

                let Some(n) = self.eat_expect(TokenKind::Number) else {
                    return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()))
                };

                self.get_var_assign(n, &chr)?;
                return Ok(Some(chr))
                
            }
            return Ok(Some(chr))
        }
        if self.expect(TokenKind::LeftSquare) {
            let params = self.get_params()?;

            if self.expect(TokenKind::Equals) {

                let Some(n) = self.eat_expect(TokenKind::Number) else {
                    return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()))
                };

                self.get_var_assign(n, &params)?;
                return Ok(Some(params))
                
            }
            return Ok(Some(params))
        }

        Ok(None)
    }

    fn get_var(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        let Some(t) = self.eat_expect(TokenKind::Number) else { return Ok(None) };
        // TODO: This should be done at runtime
        // let num_val = t.value.parse::<usize>().expect("");
        // let mut matched_segs: Item;
        // match self.var_map.get(&num_val) {
        //     Some(m) => {
        //         matched_segs = m.clone();
        //         matched_segs.position = t.position;
        //     },
        //     None => return Err(RuleSyntaxError::UnknownVariable(t))
        // }
    
    
        let mut pos = t.position;

        if !self.expect(TokenKind::Colon) {
            let var = Item::new(ParseKind::Variable(t, None), pos);
            return Ok(Some(var))
        }
        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let params = self.get_params()?;
        let matrix = params.kind.as_matrix().expect("\nCritical Error: 'Parameters' being joined with 'variable' is not a matrix!\nThis is a bug.").clone();
        
        pos.end = params.position.end;
        Ok(Some(Item::new(ParseKind::Variable(t, Some(matrix)), pos)))    
    }

    fn get_opt(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns optionals or None
        let start_pos = self.curr_tkn.position.start;

        if !self.expect(TokenKind::LeftBracket) { return Ok(None) }

        let mut segs = Vec::new();
        let mut first_bound: usize = 0;
        let mut second_bound: usize = 0;
        while self.has_more_tokens() {
            if self.peek_expect(TokenKind::RightBracket) { break; }
            if let Some(x) = self.get_bound()   { segs.push(x); continue; }
            if let Some(x) = self.get_syll()?   { segs.push(x); continue; }
            if let Some(x) = self.get_set()?    { segs.push(x); continue; }
            if let Some(x) = self.get_seg()?    { segs.push(x); continue; }
            if let Some(x) = self.get_var()?    { segs.push(x); continue; }
            if self.peek_expect(TokenKind::Comma){ break; }

            return Err(RuleSyntaxError::ExpectedSegment(self.curr_tkn.clone()))
        }
        if self.expect(TokenKind::RightBracket) {
            let end_pos = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, 1), Position::new(self.line, start_pos, end_pos))))
        }
        if !self.expect(TokenKind::Comma) {
            return Err(RuleSyntaxError::ExpectedComma(self.curr_tkn.clone()))
        }
        if let Some(x) = self.eat_expect(TokenKind::Number) {
            first_bound = x.value.parse().expect("Could not parse string to number. This is a bug!");
        }
        if self.expect(TokenKind::RightBracket) {
            let end_pos = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, 0, first_bound), Position::new(self.line, start_pos, end_pos))))
        }
        if !self.expect(TokenKind::Colon) {
            return Err(RuleSyntaxError::ExpectedColon(self.curr_tkn.clone()))
        }
        if let Some(x) = self.eat_expect(TokenKind::Number) {
            second_bound = x.value.parse().expect("Could not parse string to number. This is a bug!");
            if second_bound < first_bound { 
                return Err(RuleSyntaxError::OptMathError(x, first_bound, second_bound))
            }
        }
        if self.expect(TokenKind::RightBracket) {
            let end_pos = self.token_list[self.pos-1].position.end;
            return Ok(Some(Item::new(ParseKind::Optional(segs, first_bound, second_bound), Position::new(self.line, start_pos, end_pos))))
        }

        Err(RuleSyntaxError::ExpectedRightBracket(self.curr_tkn.clone()))
    }

    fn get_set(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns set of segs
        let start_pos = self.curr_tkn.position.start;

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
            if let Some(x) = self.get_seg()? {
                segs.push(x);
                continue;
            }
            return Err(RuleSyntaxError::ExpectedSegment(self.curr_tkn.clone()))
        }
        let end_pos = self.token_list[self.pos-1].position.end;
        Ok(Some(Item::new(ParseKind::Set(segs.clone()), Position::new(self.line, start_pos, end_pos))))

    }

    fn get_syll(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns Syll or None
        let start_pos = self.curr_tkn.position.start;

        if !self.expect(TokenKind::Syllable) {
            return Ok(None)
        }
        if !self.expect(TokenKind::Colon) {
            let end_pos = self.curr_tkn.position.start - 1;
            return Ok(Some(Item::new(ParseKind::Syllable(None, None), Position::new(self.line, start_pos, end_pos))))
        }
        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let mods = self.get_param_args(true)?;
        let end_pos = self.token_list[self.pos-1].position.end;
        Ok(Some(Item::new(ParseKind::Syllable( mods.suprs.stress,  mods.suprs.tone), Position::new(self.line, start_pos, end_pos))))
    }

    fn get_term(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns syllable / set / segment / optionals
        if let Some(x) = self.get_syll()? { return Ok(Some(x)) }
        if let Some(x) = self.get_set()?  { return Ok(Some(x)) }
        if let Some(x) = self.get_seg()?  { return Ok(Some(x)) }
        if let Some(x) = self.get_opt()?  { return Ok(Some(x)) }
        if let Some(x) = self.get_var()?  { return Ok(Some(x)) }

        Ok(None)
    }

    fn get_input_term(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        // returns list of terms and ellipses
        let mut terms = Vec::new();
        loop {
            if let Some(el) = self.eat_expect(TokenKind::Ellipsis) {
                terms.push(Item::new(ParseKind::Ellipsis, el.position));
                continue;
            }
            if self.peek_expect(TokenKind::GreaterThan) {   // So we can try simple rules that don't call functions we are yet to implement
                break                                       // We can probably remove this after parser logic is done
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
        // returns syllable / segment / variable / set
        if let Some(x) = self.get_syll()? { return Ok(Some(x)) }
        // NOTE: a set in the output only makes sense when matched to a set in the input w/ the same # of elements
        // TODO: This could be validated at end of parsing
        if let Some(x) = self.get_set()? { return Ok(Some(x)) }
        if let Some(x) = self.get_seg()? { return Ok(Some(x)) }
        if let Some(x) = self.get_var()? { return Ok(Some(x)) }

        Ok(None)
    }

    fn get_output_term(&mut self) -> Result<Vec<Item>, RuleSyntaxError>{ 
        // returns list of sylls, sets, and/or segments
        let mut terms = Vec::new();
        while let Some(trm) = self.get_output_element()? {
            terms.push(trm);
        }
        Ok(terms)
    }

    fn get_empty(&mut self) -> Vec<Item> {
        if !self.peek_expect(TokenKind::Star) && !self.peek_expect(TokenKind::EmptySet) {
            return Vec::new()
        }
        let token = self.eat();
        vec![Item::new(ParseKind::EmptySet, token.position)]
    }

    fn get_input(&mut self) -> Result<Vec<Vec<Item>>, RuleSyntaxError> {
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

    fn get_output(&mut self) -> Result<Vec<Vec<Item>>, RuleSyntaxError> {
        // returns empty / metathesis / list of output terms 
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
            if rule_type.is_some() { 
                if self.peek_expect(TokenKind::Comma) {
                    return Err(RuleSyntaxError::InsertErr)
                }
                return Err(RuleSyntaxError::ExpectedArrow(self.curr_tkn.clone()))
            }
            return Err(RuleSyntaxError::ExpectedArrow(self.curr_tkn.clone()))
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

        let rule_type= rule_type.unwrap_or(RuleType::Substitution);
        
        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, Vec::new(), Vec::new(), rule_type, self.var_map.clone()))
        }

        if !self.peek_expect(TokenKind::Slash) && !self.peek_expect(TokenKind::Pipe) {
            if let RuleType::Substitution = rule_type {
                return Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone()))
            }
            if self.peek_expect(TokenKind::Comma) {
                return Err(RuleSyntaxError::DeleteErr)
            }
            return Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone()))
        }

        let context = self.get_context()?;
        
        let except = self.get_except_block()?;

        if !self.expect(TokenKind::Eol) {
            return Err(RuleSyntaxError::ExpectedEndL(self.curr_tkn.clone()))
        }
        
        Ok(Rule::new(input, output, context, except, rule_type, self.var_map.clone()))

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

    fn setup(test_str: &str) -> Vec<Token> { Lexer::new(&String::from(test_str),0).get_all_tokens().unwrap() }

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
            Item::new(ParseKind::Syllable(Some(Supr::new(SupraType::Stress, SegMKind::Binary(BinMod::Positive))), None), Position::new(0, 0, 11)),
            Item::new(ParseKind::Syllable(None, None), Position::new(0, 13, 14)),
        ];

        let mut x = Modifiers::new();
        let mut y = Modifiers::new();
        x.suprs.stress = Some(Supr::new(SupraType::Stress, SegMKind::Binary(BinMod::Negative)));
        y.suprs.stress = Some(Supr::new(SupraType::Stress, SegMKind::Binary(BinMod::Positive)));
        let exp_output = vec![
            Item::new(ParseKind::Matrix(x), Position::new(0, 17, 26)),
            Item::new(ParseKind::Matrix(y), Position::new(0, 28, 37)),
        ];
            
        let exp_context: Vec<Item> = vec![
            Item::new(ParseKind::Environment(vec![], vec![]), Position::new(0, 40, 41)),
            Item::new(ParseKind::Environment(vec![Item::new(ParseKind::WordBound, Position::new(0, 44, 45))], vec![]), Position::new(0, 44, 46)),
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
            Item::new(ParseKind::Ipa(CARDINALS_MAP.get("t͡ɕ").unwrap().clone(), None),Position::new(0, 0, 3)),
            Item::new(ParseKind::Ellipsis, Position::new(0, 3, 6)),
            Item::new(ParseKind::Ipa(CARDINALS_MAP.get("b͡β").unwrap().clone(), None), Position::new(0, 6, 9)),
        ];

        assert_eq!(result.input[0][0], exp_input_res[0]);
        assert_eq!(result.input[0][1], exp_input_res[1]);
        assert_eq!(result.input[0][2], exp_input_res[2]);
    }

    #[test]
    fn test_variables_plain() {

        let mut x = Modifiers::new();
        x.feats[FType::Syllabic as usize] = Some(SegMKind::Binary(BinMod::Negative));
        
        let c = Item::new(ParseKind::Matrix(x), Position::new(0, 0, 1));

        let mut y = Modifiers::new();
        y.feats[FType::Consonantal as usize] = Some(SegMKind::Binary(BinMod::Negative));
        y.feats[FType::Sonorant as usize] = Some(SegMKind::Binary(BinMod::Positive));
        y.feats[FType::Syllabic as usize] = Some(SegMKind::Binary(BinMod::Positive));

        let v = Item::new(ParseKind::Matrix(y), Position::new(0, 4, 5));

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

    #[test] 
    fn test_tone() {

        let maybe_result = Parser:: new(setup("%:[tone: 123] > [tone: 321]"),0).parse();
        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();


        let exp_input = Item::new(ParseKind::Syllable(None, Some("123".to_string())), Position::new(0, 0, 13));

        let mut out = Modifiers::new();

        out.suprs.tone = Some("321".to_string());
        let exp_output = Item::new(ParseKind::Matrix(out), Position::new(0, 16, 27));

        assert_eq!(result.input[0][0], exp_input);
        assert_eq!(result.output[0][0], exp_output);
    }
}
