use std  ::fmt::{self, Display};
use serde::Deserialize;

use crate::{CARDINALS_TRIE, error::RuleSyntaxError};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum NodeType {
    Root,      
    Manner,
    Laryngeal,   
    Place,      
    Labial,      
    Coronal,     
    Dorsal,      
    Pharyngeal, 
}

impl NodeType {
    pub const fn count() -> usize { 8 }
}

impl Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeType::Root         => write!(f, "RUT"),
            NodeType::Manner       => write!(f, "MAN"),
            NodeType::Laryngeal    => write!(f, "LAR"),
            NodeType::Place        => write!(f, "PLC"),
            NodeType::Labial       => write!(f, "LAB"),
            NodeType::Coronal      => write!(f, "COR"),
            NodeType::Dorsal       => write!(f, "DOR"),
            NodeType::Pharyngeal   => write!(f, "PHR")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum SupraType {
    Long,       // ±long
    Overlong,   // ±overlong
    Stress,     // ±stress    (+ matches prim and sec, - matches unstressed)
    SecStress,  // ±secstress (+ matches sec, - matches prim and unstressed)
    Tone,       // Can only be used with : notation (e.g. Tone : 213 )
}

// impl SupraType {
//     pub const fn count(&self) -> usize { 7 }
// }

impl Display for SupraType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SupraType::Long            => write!(f, "long"),
            SupraType::Overlong        => write!(f, "overlng"),
            SupraType::Stress          => write!(f, "str"),
            SupraType::SecStress       => write!(f, "secstr"),
            SupraType::Tone            => write!(f, "tone"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum FType { 
    // can be ± || α.ω 
    /*RUT*/ Consonantal, Sonorant, Syllabic,      
    /*MAN*/ Continuant, Approximant, Lateral, Nasal, DelayedRelease, Strident, Rhotic, Click,          
    /*LAR*/ Voice, SpreadGlottis, ConstrGlottis,   
    // PLACE Node
    /*LAB*/ Bilabial, Round,          
    /*COR*/ Anterior, Distributed,     
    /*DOR*/ Front, Back, High, Low, Tense, Reduced,        
    /*PHR*/ AdvancedTongueRoot, RetractedTongueRoot, 
}

impl Display for FType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FType::Consonantal         => write!(f, "cons"),
            FType::Sonorant            => write!(f, "son"),
            FType::Syllabic            => write!(f, "syll"),
            FType::Continuant          => write!(f, "cont"),
            FType::Approximant         => write!(f, "appr"),
            FType::Lateral             => write!(f, "lat"),
            FType::Nasal               => write!(f, "nas"),
            FType::DelayedRelease      => write!(f, "d.r."),
            FType::Strident            => write!(f, "strid"),
            FType::Rhotic              => write!(f, "rho"),
            FType::Click               => write!(f, "clk"),
            FType::Voice               => write!(f, "voi"),
            FType::SpreadGlottis       => write!(f, "s.g."),
            FType::ConstrGlottis       => write!(f, "c.g."),
            FType::Bilabial            => write!(f, "bilab"),
            FType::Round               => write!(f, "rnd"),
            FType::Anterior            => write!(f, "ant"),
            FType::Distributed         => write!(f, "dis"),
            FType::Front               => write!(f, "fr"),
            FType::Back                => write!(f, "bk"),
            FType::High                => write!(f, "hi"),
            FType::Low                 => write!(f, "lo"),
            FType::Tense               => write!(f, "tens"),
            FType::Reduced             => write!(f, "red"),
            FType::AdvancedTongueRoot  => write!(f, "atr"),
            FType::RetractedTongueRoot => write!(f, "rtr")
        }
    }
}

impl FType {
    pub const fn count() -> usize { 26 }

    pub fn from_usize(value: usize) -> Self {
        use FType::*;
        match value {
            // ROOT node
             0 => {debug_assert_eq!(value, Consonantal as usize); Consonantal},
             1 => {debug_assert_eq!(value, Sonorant as usize); Sonorant},
             2 => {debug_assert_eq!(value, Syllabic as usize); Syllabic},
            // MANNER node
             3 => {debug_assert_eq!(value, Continuant as usize); Continuant},
             4 => {debug_assert_eq!(value, Approximant as usize); Approximant},
             5 => {debug_assert_eq!(value, Lateral as usize); Lateral},
             6 => {debug_assert_eq!(value, Nasal as usize); Nasal},
             7 => {debug_assert_eq!(value, DelayedRelease as usize); DelayedRelease},
             8 => {debug_assert_eq!(value, Strident as usize); Strident},
             9 => {debug_assert_eq!(value, Rhotic as usize); Rhotic},
            10 => {debug_assert_eq!(value, Click as usize); Click},
            // LAR node
            11 => {debug_assert_eq!(value, Voice as usize); Voice},
            12 => {debug_assert_eq!(value, SpreadGlottis as usize); SpreadGlottis},
            13 => {debug_assert_eq!(value, ConstrGlottis as usize); ConstrGlottis},
            // PLACE Node
            // LABIAL subnode
            14 => {debug_assert_eq!(value, Bilabial as usize); Bilabial},
            15 => {debug_assert_eq!(value, Round as usize); Round},
            // CORONAL subnode
            16 => {debug_assert_eq!(value, Anterior as usize); Anterior},
            17 => {debug_assert_eq!(value, Distributed as usize); Distributed},
            // DORSAL subnode
            18 => {debug_assert_eq!(value, Front as usize); Front},
            19 => {debug_assert_eq!(value, Back as usize); Back},
            20 => {debug_assert_eq!(value, High as usize); High},
            21 => {debug_assert_eq!(value, Low as usize); Low},
            22 => {debug_assert_eq!(value, Tense as usize); Tense},
            23 => {debug_assert_eq!(value, Reduced as usize); Reduced},
            // PHAR subnode
            24 => {debug_assert_eq!(value, AdvancedTongueRoot as usize); AdvancedTongueRoot},
            25 => {debug_assert_eq!(value, RetractedTongueRoot as usize); RetractedTongueRoot},
            _  => unreachable!("\nOut of Range Error converting `usize` to `FeatType`\nThis is a bug!\n")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum FeatType {
    Node(NodeType),
    Feat(FType),
    Supr(SupraType),
}

impl Display for FeatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatType::Node(x) => write!(f, "{x}"),
            FeatType::Feat(x) => write!(f, "{x}"),
            FeatType::Supr(x) => write!(f, "{x}"),
        }
    }
}

// #[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    LeftSquare,   // [
    RightSquare,  // ]
    LeftCurly,    // {
    RightCurly,   // }
    // LeftAngle,    // <
    // RightAngle,   // >
    LeftBracket,  // (
    RightBracket, // )
    LessThan,     // <
    GreaterThan,  // >
    Equals,       // =
    Underline,    // _
    Arrow,        // -> or =>
    Comma,        // ,
    Colon,        // :
    WordBoundary, // #
    SyllBoundary, // $
    Syllable,     // %
    Ampersand,    // %
    Group,        // Primitive Category i.e. C for Cons, V for Vowel
    Number,       // Number
    Slash,        // /
    DubSlash,  // //
    Pipe,         // | 
    Cardinal,     // IPA character
    Star,         // *
    EmptySet,     // ∅
    Ellipsis,     // ... or .. or … or ⋯
    Feature(FeatType),
    Eol,          // End of Line 
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self {
            LeftSquare   => write!(f, "LSquare"),
            RightSquare  => write!(f, "RSquare"),
            LeftCurly    => write!(f, "LCurly"),
            RightCurly   => write!(f, "RCurly"),
            // LeftAngle  => write!(f, "LAngle"),
            // RightAngle => write!(f, "RAngle"),
            LeftBracket  => write!(f, "LBrack"),
            RightBracket => write!(f, "RBrack"),
            LessThan     => write!(f, "LT"),
            GreaterThan  => write!(f, "GT"),
            Equals       => write!(f, "Eq"),
            Underline    => write!(f, "UL"),
            Arrow        => write!(f, "Arrow"),
            Comma        => write!(f, "Comma"),
            Colon        => write!(f, "Colon"),
            WordBoundary => write!(f, "WBound"),
            SyllBoundary => write!(f, "SBound"),
            Syllable     => write!(f, "Syll"),
            Ampersand    => write!(f, "Amper"),
            Group        => write!(f, "Prim"),
            Number       => write!(f, "Num"),
            Slash        => write!(f, "Slash"),
            DubSlash  => write!(f, "DoubleSlash"),
            Pipe         => write!(f, "Pipe"),
            Cardinal     => write!(f, "Cardinal"),
            Star         => write!(f, "Star"),
            EmptySet     => write!(f, "Empty"),
            Ellipsis     => write!(f, "Ellipsis"),
            Feature(x)   => write!(f, "{x}"),
            Eol          => write!(f, "End of Line"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl Position {
    pub fn new(line: usize, start: usize, end: usize) -> Self {
        Self { line, start, end }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.start, self.end)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String, 
    pub position: Position,
}

impl Token {
    pub fn new(kind: TokenKind, value: String, line: usize, start: usize, end: usize) -> Self {
        Self { kind, value, position: Position::new(line, start, end) }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut spaces = " ".to_string();
        if self.position.end   <= 9 {spaces += " "}
        if self.position.start <= 9 {spaces += " "}
        match self.kind {
            TokenKind::Feature(x) => write!(f, "{}{}`{}{}`",  self.position, spaces, self.value, x),
            _ => write!(f, "{}{}{} `{}`",  self.position, spaces, self.kind, self.value)
        }
    }
}

#[derive(Default)]
pub struct Lexer<'a> {
    source: &'a [char],
    line: usize,
    pos: usize,
    inside_matrix: bool,
    inside_option: bool,
    inside_set: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char], line: usize) -> Self {
        Self { source, line, pos: 0, inside_matrix: false , inside_option: false, inside_set: false}
    }

    pub fn has_more_chars(&self) -> bool { !self.source.is_empty() }

    fn trim_whitespace(&mut self) {
        while self.has_more_chars() && self.source[0].is_whitespace() {
            self.advance();
        }
    }

    fn chop(&mut self, n: usize) -> String {
        let token = &self.source[0..n];
        self.source = &self.source[n..];
        self.pos += n;
        token.iter().collect()
    }

    fn chop_while<P>(&mut self, mut predicate: P) -> String where P: FnMut(&char) -> bool {
        let mut n = 0;
        while n < self.source.len() && predicate(&self.source[n]) {
            n += 1;
        }
        self.chop(n)
    }

    fn curr_char(&self) -> char {
        if self.has_more_chars() {
            self.source[0]
        } else {
            '\0'
        }
    }

    pub fn next_char(&self) -> char {
        if self.source.len() > 1 {
            self.source[1]
        } else {
            '\0'
        }
    }

    pub fn advance(&mut self) {
        self.source = &self.source[1..];
        self.pos += 1;
    }

    fn get_bracket(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        let start = self.pos;
        let tokenkind: TokenKind;
        let value: String;

        match self.curr_char() {
            ')' => { tokenkind = TokenKind::RightBracket; value = ")".to_string(); self.inside_option = false },
            ']' => { tokenkind = TokenKind::RightSquare;  value = "]".to_string(); self.inside_matrix = false },
            '}' => { tokenkind = TokenKind::RightCurly;   value = "}".to_string(); self.inside_set    = false },
            '⟨' => unimplemented!(), // { tokenkind = TokenKind::LeftAngle;    value = "⟨".to_string(); },
            '⟩' => unimplemented!(), // { tokenkind = TokenKind::RightAngle;   value = "⟩".to_string(); },
            '{' => { 
                if self.inside_set {
                    return Err(RuleSyntaxError::NestedBrackets(self.line, start));
                }
                tokenkind = TokenKind::LeftCurly; value = "{".to_string();  self.inside_set = true
            },
            '(' => { 
                if self.inside_option {
                    return Err(RuleSyntaxError::NestedBrackets(self.line, start));
                }
                tokenkind = TokenKind::LeftBracket; value = "(".to_string(); self.inside_option = true
            },
            '[' => { 
                if self.inside_matrix {
                    return Err(RuleSyntaxError::NestedBrackets(self.line, start));
                }
                tokenkind = TokenKind::LeftSquare; value = "[".to_string();  self.inside_matrix = true
            },
            _ => return Ok(None)
        }
        self.advance();

        Ok(Some(Token::new(tokenkind, value, self.line, start, self.pos)))

    }

    fn get_primative(&mut self) -> Option<Token> {
        if !self.curr_char().is_ascii_uppercase() { return None }

        let start = self.pos;
        // let c = self.curr_char();
        // self.advance();

        let c = self.chop(1);
        
        Some(Token::new(TokenKind::Group, c, self.line, start, self.pos))
    }

    fn get_numeric(&mut self) -> Option<Token> {
        if !self.curr_char().is_ascii_digit() { return None }

        let start = self.pos;

        let buffer = self.chop_while(|x| x.is_ascii_digit());

        Some(Token::new(TokenKind::Number, buffer, self.line, start, self.pos))
    }

    fn get_feature(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        
        if self.curr_char() != '+' && self.curr_char() != '-' && !matches!(self.curr_char(), 'α'..='ω') {
            return Ok(None);
        }
        
        let start = self.pos;
        let val = self.curr_char();

        self.advance();
        
        let mod_val = if val == '-' && matches!(self.curr_char(), 'α'..='ω') {
            self.advance();
            let mut tmp = String::from('-'); tmp.push(self.curr_char());
            tmp
        } else {
            String::from(val)
        };

        self.trim_whitespace();
        
        let mut buffer = String::new();

        while self.curr_char().is_ascii_alphabetic() || self.curr_char() == '.' {
            buffer.push(self.curr_char());
            self.advance();
            self.trim_whitespace();
        }

        if buffer.len() <= 1 { 
            return Err(RuleSyntaxError::ExpectedAlphabetic(self.curr_char(), self.line, self.pos))
        }

        let tkn_kind = self.feature_match(buffer, start, self.pos)?;
            
        match tkn_kind {
            TokenKind::Feature(FeatType::Node(_)) => if mod_val == "+" || mod_val == "-" {
                return Err(RuleSyntaxError::WrongModNode(self.line, start))
            }, 
            TokenKind::Feature(FeatType::Supr(SupraType::Tone)) 
            => if mod_val == "+" || mod_val == "-" {
                return Err(RuleSyntaxError::WrongModTone(self.line, start))
            }
            _ => {}
        }

        Ok(Some(Token::new(tkn_kind, mod_val, self.line, start, self.pos)))
    }

    fn get_special_char(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        let start = self.pos;
        let tokenkind: TokenKind;
        let value: String;

        match self.curr_char() {
            ',' => { tokenkind = TokenKind::Comma;        value = self.chop(1) },
            ':' => { tokenkind = TokenKind::Colon;        value = self.chop(1) },
            '#' => { tokenkind = TokenKind::WordBoundary; value = self.chop(1) },
            '$' => { tokenkind = TokenKind::SyllBoundary; value = self.chop(1) },
            '%' => { tokenkind = TokenKind::Syllable;     value = self.chop(1) },
            '*' => { tokenkind = TokenKind::Star;         value = self.chop(1) },
            '∅' => { tokenkind = TokenKind::EmptySet;     value = self.chop(1) },
            '&' => { tokenkind = TokenKind::Ampersand;    value = self.chop(1) },
            '_' => { tokenkind = TokenKind::Underline;    value = self.chop(1) },
            '<' => { tokenkind = TokenKind::LessThan;     value = self.chop(1) },
            '>' => { tokenkind = TokenKind::GreaterThan;  value = self.chop(1) },
            '|' => { tokenkind = TokenKind::Pipe;         value = self.chop(1) },
            '/' => match self.next_char() {
                '/' => { tokenkind = TokenKind::DubSlash; value = self.chop(2) },
                 _  => { tokenkind = TokenKind::Slash;    value = self.chop(1) }
            },
            '=' => match self.next_char() { 
                '>' => { tokenkind = TokenKind::Arrow;    value = self.chop(2); },
                 _  => { tokenkind = TokenKind::Equals;   value = self.chop(1); },
             },
            '-' => match self.next_char() {
                '>' => { tokenkind = TokenKind::Arrow;    value = self.chop(2); },
                 _  => return Err(RuleSyntaxError::ExpectedCharArrow(self.next_char(), self.line, self.pos))
            },
            '…' | '⋯' => { tokenkind = TokenKind::Ellipsis; value = self.chop(1); },
            '.' => match self.next_char() {
                '.' => { tokenkind = TokenKind::Ellipsis; value = self.chop_while(|x| *x == '.'); },
                _ => return Err(RuleSyntaxError::ExpectedCharDot(self.next_char(), self.line, self.pos))
            },
            _ => return Ok(None)
        }
        Ok(Some(Token::new(tokenkind, value, self.line, start, self.pos)))
    }

    // fn get_ipa_old(&mut self) -> Option<Token> {
    //     if self.inside_square { return None }
    //     let start = self.pos;
    //
    //     let mut  buffer = String::new();
    //     let mut last_buffer: String;
    //
    //     let cardinals = self.cardinals_trie;
    //
    //     while self.has_more_chars() {
    //
    //         last_buffer = buffer.clone();
    //         buffer.push(self.curr_char);
    //
    //         if !buffer.is_empty() && cardinals.find(&buffer.as_str()).is_empty() {
    //             if !last_buffer.is_empty() && cardinals.contains(&last_buffer.as_str()) {
    //                 return Some(Token::new(TokenKind::Cardinal, last_buffer, start, self.pos))
    //             }
    //             return None
    //         }
    //        
    //         self.advance();
    //     }
    //
    //     if buffer.is_empty() || !cardinals.contains(&buffer.as_str()) {
    //         return None
    //     }
    //     return Some(Token::new(TokenKind::Cardinal, buffer, start, self.pos))
    // }

    fn get_ipa(&mut self) -> Option<Token> {
        if self.inside_matrix { return None }
        let start = self.pos;

        let mut buffer = self.curr_char().to_string();

        if CARDINALS_TRIE.contains_prefix(buffer.as_str()) {
            self.advance();
            loop {
                let mut tmp = buffer.clone(); tmp.push(self.curr_char());
                if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                    buffer.push(self.curr_char());
                    self.advance();
                    continue;
                }

                if self.curr_char() == '^' {
                    tmp.pop();
                    tmp.push('\u{0361}');
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push('\u{0361}');
                        self.advance();
                        continue;
                    }

                    tmp.pop();
                    tmp.push('\u{035C}');
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push('\u{035C}');
                        self.advance();
                        continue;
                    }
                }

                return Some(Token::new(TokenKind::Cardinal, buffer, self.line, start, self.pos))
            }
        }
        None
    }

    fn get_string(&mut self) -> Result<Option<Token>, RuleSyntaxError> { 

        if !self.curr_char().is_ascii_alphabetic() { return Ok(None) }

        if !self.inside_matrix { 
            return Err(RuleSyntaxError::OutsideBrackets(self.curr_char(), self.line, self.pos))
        }

        let start = self.pos;

        let buffer = self.chop_while(|x| x.is_ascii_alphabetic());

        let tkn_kind: TokenKind = self.string_match(buffer, start)?;

        self.trim_whitespace();

        match self.curr_char() {
            ':' => self.advance(),
            _ => return Err(RuleSyntaxError::ExpectedCharColon(self.curr_char(), self.line, self.pos))
        } 
        
        self.trim_whitespace();

        match self.get_numeric() {
            Some(num) => Ok(Some(Token::new(tkn_kind, num.value, self.line, start, self.pos))),
            _ => Err(RuleSyntaxError::ExpectedNumber(self.curr_char(), self.line, self.pos))

        }
    }

    fn string_match(&mut self, buffer: String, start: usize) -> Result<TokenKind, RuleSyntaxError> {
        use TokenKind::*;
        use FeatType::*;
        use SupraType::*;
        match buffer.to_lowercase().as_str() {
            "tone"   | "ton" | "tn"    => Ok(Feature(Supr(Tone))),
            _ => Err(RuleSyntaxError::UnknownFeature(buffer.clone(), self.line, start, buffer.len()))
        }
    }

    fn feature_match(&mut self, buffer: String, start: usize, end: usize) -> Result<TokenKind, RuleSyntaxError> {
        use TokenKind::*;
        use FeatType::*;
        use NodeType::*;
        use FType::*;
        use SupraType::*;
        match buffer.to_lowercase().as_str() {
            // Root Node Features
            "root"        | "rut"       | "rt"                   => Ok(Feature(Node(Root))),
            "consonantal" | "consonant" | "cons" | "cns"         => Ok(Feature(Feat(Consonantal))),
            "sonorant"    | "sonor"     | "son"  | "snrt" | "sn" => Ok(Feature(Feat(Sonorant))),
            "syllabic"    | "syllab"    | "syll" | "syl"         => Ok(Feature(Feat(Syllabic))),
            // Manner Node Features
            "manner"         | "mann"   | "man"  | "mnr"         => Ok(Feature(Node(Manner))),
            "continuant"     | "contin" | "cont" | "cnt"         => Ok(Feature(Feat(Continuant))),
            "approximant"    | "approx" | "appr" | "app"         => Ok(Feature(Feat(Approximant))),
            "lateral"        | "latrl"  | "ltrl" | "lat"         => Ok(Feature(Feat(Lateral))),
            "nasal"          | "nsl"    | "nas"                  => Ok(Feature(Feat(Nasal))),
            "delayedrelease" | "delrel" | "d.r." | 
            "del.rel."       | "dr"                              => Ok(Feature(Feat(DelayedRelease))),
            "strident"       | "strid"  | "stri"                 => Ok(Feature(Feat(Strident))),
            "rhotic"         | "rhot"   | "rho"  | "rh"          => Ok(Feature(Feat(Rhotic))),
            "click"          | "clik"   | "clk"                  => Ok(Feature(Feat(Click))),
            // Laryngeal Node Features
            "laryngeal"      | "laryng"     | "laryn"  | "lar"   => Ok(Feature(Node(Laryngeal))),
            "voice"          | "voi"        | "vce"    | "vc"    => Ok(Feature(Feat(Voice))),
            "spreadglottis"  | "spreadglot" | 
            "spread"         | "s.g."       | "sg"               => Ok(Feature(Feat(SpreadGlottis))),
            "constrictedglottis"            | "constricted" |
            "constglot"      | "constr"     | "c.g."   | "cg"    => Ok(Feature(Feat(ConstrGlottis))),
            // Place Node Feature
            "place"       | "plce"    | "plc"                    => Ok(Feature(Node(Place))),
            // Labial Place Node Features
            "labial"      | "lbl"     | "lab"                    => Ok(Feature(Node(Labial))),
            // TODO: come up with a better name for this feature
            "bilabial"    | "bilab"   | "blb"                    => Ok(Feature(Feat(Bilabial))),
            "round"       | "rnd"     | "rd"                     => Ok(Feature(Feat(Round))),
            // Coronal Place Node Features
            "coronal"     | "coron"   | "crnl" | "cor"           => Ok(Feature(Node(Coronal))),
            "anterior"    | "anter"   | "antr" | "ant"           => Ok(Feature(Feat(Anterior))),
            "distributed" | "distrib" | "dist" | "dis" | "dst"   => Ok(Feature(Feat(Distributed))),
            // Dorsal Place Node Features
            "dorsal"  | "drsl"  | "dors" | "dor"                 => Ok(Feature(Node(Dorsal))),
            "front"   | "frnt"  | "fnt"  | "fro" | "fr"          => Ok(Feature(Feat(Front))),
            "back"    | "bck"   | "bk"                           => Ok(Feature(Feat(Back))),
            "high"    | "hgh"   | "hi"                           => Ok(Feature(Feat(High))),
            "low"     | "lw"    | "lo"                           => Ok(Feature(Feat(Low))),
            "tense"   | "tens"  | "tns"  | "ten"                 => Ok(Feature(Feat(Tense))),
            "reduced" | "reduc" | "redu" | "rdcd" | "red"        => Ok(Feature(Feat(Reduced))),
            // Pharyngeal Place Node Features
            "pharyngeal" | "pharyng" | "pharyn"  |
            "phar"       | "phr"                                 => Ok(Feature(Node(Pharyngeal))),
            "advancedtongueroot"     | "a.t.r."  | "atr"         => Ok(Feature(Feat(AdvancedTongueRoot))),
            "retractedtongueroot"    | "r.t.r."  | "rtr"         => Ok(Feature(Feat(RetractedTongueRoot))),
            // Suprasegmental Features
            "long"     | "lng"                                   => Ok(Feature(Supr(Long))),
            "overlong" | "overlng" | "ovrlng" | "xlng"           => Ok(Feature(Supr(Overlong))),
            "stress"   | "str"                                   => Ok(Feature(Supr(Stress))),
            "secondarystress"| "sec.stress" | "secstress" |
            "sec.str."       | "secstr" | "sec"                  => Ok(Feature(Supr(SecStress))),
            
            _ => Err(RuleSyntaxError::UnknownFeature(buffer, self.line, start, end))
        }
    }

    pub fn get_next_token(&mut self) -> Result<Token, RuleSyntaxError>{
        
        self.trim_whitespace();
        
        if !self.has_more_chars() { return Ok(Token::new(TokenKind::Eol, String::new(), self.line, self.pos, self.pos+1)) }

        if let Some(bkt_token) = self.get_bracket()?      { return Ok(bkt_token) }
        if let Some(pmt_token) = self.get_primative()     { return Ok(pmt_token) }
        if let Some(num_token) = self.get_numeric()       { return Ok(num_token) }
        if let Some(ftr_token) = self.get_feature()?      { return Ok(ftr_token) }
        if let Some(spc_token) = self.get_special_char()? { return Ok(spc_token) }
        if let Some(ipa_token) = self.get_ipa()           { return Ok(ipa_token) }
        if let Some(str_token) = self.get_string()?       { return Ok(str_token) } 
        
        Err(RuleSyntaxError::UnknownCharacter(self.curr_char(), self.line, self.pos))
    }

    pub fn get_line(&mut self) -> Result<Vec<Token>, RuleSyntaxError> {
        let mut token_list: Vec<Token> =  Vec::new();
        loop {
            let next_token = self.get_next_token()?;
            if let TokenKind::Eol = next_token.kind {
                token_list.push(next_token);
                break
            }
            token_list.push(next_token);
        }
        Ok(token_list)
    }

    // pub fn get_all_lines(&mut self) -> Result<Vec<Vec<Token>>, RuleSyntaxError> {
    //     todo!()
    // }
}

#[cfg(test)]
mod lexer_tests {

    use super::*;

    #[test]
    fn test_syll() {

        let test_input = String::from("%");
        let expected_result = TokenKind::Syllable;

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0).get_next_token().unwrap();

        assert_eq!(result.kind, expected_result);
        assert_eq!(result.value, test_input);
    }

    #[test]
    fn test_ipa_sep() {
        
        let test_input= String::from("t͡ɕ b͡β b a");
        
        let expected_result = vec![
            Token::new(TokenKind::Cardinal, "t͡ɕ".to_owned(), 0,  0,  3),
            Token::new(TokenKind::Cardinal, "b͡β".to_owned(), 0,  4,  7),
            Token::new(TokenKind::Cardinal,  "b".to_owned(), 0,  8,  9),
            Token::new(TokenKind::Cardinal,  "a".to_owned(), 0, 10, 11),
            Token::new(TokenKind::Eol,        String::new(), 0, 11, 12),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_ipa_joined() {
        
        let test_input= String::from("t^ɕb͡βba");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal, "t͡ɕ".to_owned(), 0, 0, 3),
            Token::new(TokenKind::Cardinal, "b͡β".to_owned(), 0, 3, 6),
            Token::new(TokenKind::Cardinal,  "b".to_owned(), 0, 6, 7),
            Token::new(TokenKind::Cardinal,  "a".to_owned(), 0, 7, 8),
            Token::new(TokenKind::Eol,        String::new(), 0, 8, 9),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0).get_line().unwrap();  

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_ipa_tie() {
        
        let test_input= String::from("t^ɕ > b^β");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal,   "t͡ɕ".to_owned(), 0, 0, 3),
            Token::new(TokenKind::GreaterThan, ">".to_owned(), 0, 4, 5),
            Token::new(TokenKind::Cardinal,   "b͡β".to_owned(), 0, 6, 9),
            Token::new(TokenKind::Eol,          String::new(), 0, 9, 10),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0).get_line().unwrap();  

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_metathesis() {
        
        let test_input= String::from("t^ɕ...b͡β > &");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal,   "t͡ɕ".to_owned(), 0,  0,  3),
            Token::new(TokenKind::Ellipsis,  "...".to_owned(), 0,  3,  6),
            Token::new(TokenKind::Cardinal,   "b͡β".to_owned(), 0,  6,  9),
            Token::new(TokenKind::GreaterThan, ">".to_owned(), 0, 10, 11),
            Token::new(TokenKind::Ampersand,   "&".to_owned(), 0, 12, 13),
            Token::new(TokenKind::Eol,          String::new(), 0, 13, 14),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_feature_matrix() {
        use FeatType::*;
        let test_input= String::from("[+voi, -sg, αPLACE]");
        let expected_result = vec![
            Token::new(TokenKind::LeftSquare,                          "[".to_owned(), 0,  0,  1),
            Token::new(TokenKind::Feature(Feat(FType::Voice)),         "+".to_owned(), 0,  1,  5),
            Token::new(TokenKind::Comma,                               ",".to_owned(), 0,  5,  6),
            Token::new(TokenKind::Feature(Feat(FType::SpreadGlottis)), "-".to_owned(), 0,  7, 10),
            Token::new(TokenKind::Comma,                               ",".to_owned(), 0, 10, 11),
            Token::new(TokenKind::Feature(Node(NodeType::Place)),      "α".to_owned(), 0, 12, 18),
            Token::new(TokenKind::RightSquare,                         "]".to_owned(), 0, 18, 19),
            Token::new(TokenKind::Eol,                                  String::new(), 0, 19, 20),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);

        }
    }

    #[test]
    fn test_variables() {
        
        let test_input= String::from("C=1 V=2 > 2 1 / _C // _");
        let expected_result = vec![
            Token::new(TokenKind::Group,       "C".to_owned(), 0,  0,  1),
            Token::new(TokenKind::Equals,      "=".to_owned(), 0,  1,  2),
            Token::new(TokenKind::Number,      "1".to_owned(), 0,  2,  3),
            Token::new(TokenKind::Group,       "V".to_owned(), 0,  4,  5),
            Token::new(TokenKind::Equals,      "=".to_owned(), 0,  5,  6),
            Token::new(TokenKind::Number,      "2".to_owned(), 0,  6,  7),
            Token::new(TokenKind::GreaterThan, ">".to_owned(), 0,  8,  9),
            Token::new(TokenKind::Number,      "2".to_owned(), 0, 10, 11),
            Token::new(TokenKind::Number,      "1".to_owned(), 0, 12, 13),
            Token::new(TokenKind::Slash,       "/".to_owned(), 0, 14, 15),
            Token::new(TokenKind::Underline,   "_".to_owned(), 0, 16, 17),
            Token::new(TokenKind::Group,       "C".to_owned(), 0, 17, 18),
            Token::new(TokenKind::DubSlash,   "//".to_owned(), 0, 19, 21),
            Token::new(TokenKind::Underline,   "_".to_owned(), 0, 22, 23),
            Token::new(TokenKind::Eol,          String::new(), 0, 23, 24),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

}