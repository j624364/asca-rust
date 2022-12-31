use std  ::fmt::{self, Display};
use serde::Deserialize;

use crate::CARDINALS_TRIE;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum NodeType {
    RootNode,      
    MannerNode,
    LaryngealNode,   
    PlaceNode,      
    LabialNode,      
    CoronalNode,     
    DorsalNode,      
    PharyngealNode, 
}

impl NodeType {
    pub const fn count(&self) -> usize { 8 }
}

impl Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeType::RootNode         => write!(f, "ROOT"),
            NodeType::MannerNode       => write!(f, "MAN"),
            NodeType::LaryngealNode    => write!(f, "LAR"),
            NodeType::PlaceNode        => write!(f, "PLACE"),
            NodeType::LabialNode       => write!(f, "LAB"),
            NodeType::CoronalNode      => write!(f, "COR"),
            NodeType::DorsalNode       => write!(f, "DOR"),
            NodeType::PharyngealNode   => write!(f, "PHAR")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum SupraType {
    Long,       // +long || length : 2
    Overlong,   // +overlong || length : 2 
    PrimStress, // +stress || stress : 1
    SecStress,  // +secstress || stress : 2
    // Can only be used with : notation
    Stress,     // stress : 2 == +long, stress : 3 == Overlong
    Length,     // Len : 2 == +long, len : 3 == Overlong
    Tone,       // Tone : 213
}

impl SupraType {
    pub const fn count(&self) -> usize { 7 }
}

impl Display for SupraType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SupraType::Long            => write!(f, "long"),
            SupraType::Overlong        => write!(f, "overlng"),
            SupraType::PrimStress      => write!(f, "prmstr"),
            SupraType::SecStress       => write!(f, "secstr"),
            SupraType::Length          => write!(f, "len"),
            SupraType::Stress          => write!(f, "str"),
            SupraType::Tone            => write!(f, "tone"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum FType {
    // ROOT node 
    Consonantal,   // ± || α.ω 
    Sonorant,      
    Syllabic,      
    // MANNER node 
    Continuant,      
    Approximant,     
    Lateral,         
    Nasal,           
    DelayedRelease,  
    Strident,        
    Rhotic,          
    Click,          
    // LAR node
    Voice,           
    SpreadGlottis,   
    ConstrGlottis,   
    // PLACE Node
    // LABIAL subnode
    Bilabial,      
    Round,          
    // CORONAL subnode
    Anterior,        
    Distributed,     
    // DORSAL subnode
    Front,          
    Back,           
    High,           
    Low,            
    Tense,          
    Reduced,        
    // PHAR subnode
    AdvancedTongueRoot,
    RetractedTongueRoot, 
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
            FType::DelayedRelease      => write!(f, "delrel"),
            FType::Strident            => write!(f, "strid"),
            FType::Rhotic              => write!(f, "rho"),
            FType::Click               => write!(f, "clk"),
            FType::Voice               => write!(f, "voi"),
            FType::SpreadGlottis       => write!(f, "sg"),
            FType::ConstrGlottis       => write!(f, "cg"),
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
            00 => {debug_assert_eq!(value, Consonantal as usize); Consonantal},
            01 => {debug_assert_eq!(value, Sonorant as usize); Sonorant},
            02 => {debug_assert_eq!(value, Syllabic as usize); Syllabic},
            // MANNER node
            03 => {debug_assert_eq!(value, Continuant as usize); Continuant},
            04 => {debug_assert_eq!(value, Approximant as usize); Approximant},
            05 => {debug_assert_eq!(value, Lateral as usize); Lateral},
            06 => {debug_assert_eq!(value, Nasal as usize); Nasal},
            07 => {debug_assert_eq!(value, DelayedRelease as usize); DelayedRelease},
            08 => {debug_assert_eq!(value, Strident as usize); Strident},
            09 => {debug_assert_eq!(value, Rhotic as usize); Rhotic},
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
            _  => panic!("\nOut of Range Error converting `usize` to `FeatType`\nThis is a bug!\n")
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
    Primative,    // Primitive Category i.e. C for Cons, V for Vowel
    Number,       // Number
    Slash,        // /
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
            LeftSquare => write!(f, "LSquare"),
            RightSquare => write!(f, "RSquare"),
            LeftCurly => write!(f, "LCurly"),
            RightCurly => write!(f, "RCurly"),
            // LeftAngle => write!(f, "LAngle"),
            // RightAngle => write!(f, "RAngle"),
            LeftBracket => write!(f, "LBrack"),
            RightBracket => write!(f, "RBrack"),
            LessThan => write!(f, "LT"),
            GreaterThan => write!(f, "GT"),
            Equals => write!(f, "Eq"),
            Underline => write!(f, "UL"),
            Arrow => write!(f, "Arrow"),
            Comma => write!(f, "Comma"),
            Colon => write!(f, "Colon"),
            WordBoundary => write!(f, "WBound"),
            SyllBoundary => write!(f, "SBound"),
            Syllable => write!(f, "Syll"),
            Ampersand => write!(f, "Amper"),
            Primative => write!(f, "Prim"),
            Number => write!(f, "Num"),
            Slash => write!(f, "Slash"),
            Pipe => write!(f, "Pipe"),
            Cardinal => write!(f, "Cardinal"),
            Star => write!(f, "Star"),
            EmptySet => write!(f, "Empty"),
            Ellipsis => write!(f, "Ellipsis"),
            Feature(x) => write!(f, "{}", x),
            Eol => write!(f, "End of Line"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

impl Position {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String, 
    pub position: Position,
}

impl Token {
    pub fn new(kind: TokenKind, value: String, start: usize, end: usize) -> Self {
        Self { 
            kind, 
            value, 
            position: Position::new(start, end)
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let mut spaces = " ".to_string();
        if self.position.end <= 9 {spaces = spaces + " "}
        if self.position.start <= 9 {spaces = spaces + " "}
        match self.kind {
            TokenKind::Feature(x) => write!(f, "{}{}`{}{}`",  self.position, spaces, self.value, x),
            _ => write!(f, "{}{}{} `{}`",  self.position, spaces, self.kind, self.value)
        }
    }
}

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    curr_char: char,
    inside_square: bool
}

impl Lexer {

    pub fn new(input: String) -> Self {
        let mut s = Self { 
            source: input.chars().collect(), 
            pos: 0,
            curr_char: '\0',
            inside_square: false,
        };
        s.curr_char = s.source[s.pos];

        s
    }

    pub fn has_more_chars(&self) -> bool {
        self.pos < self.source.len()
    }

    pub fn advance(&mut self) {
        self.pos += 1;
        self.curr_char = if self.has_more_chars() {
            self.source[self.pos]
        } else {
            '\0'
        };
    }

    pub fn peak_next_char(&self) -> char {
        if self.has_more_chars() {
            self.source[self.pos + 1]
        } else {
            '\0'
        }
    }

    fn get_bracket(&mut self) -> Option<Token> {
        let start = self.pos;

        let tokenkind: TokenKind;
        let value: String;

        match self.curr_char {
            '(' => { tokenkind = TokenKind::LeftBracket;  value = "(".to_string(); },
            ')' => { tokenkind = TokenKind::RightBracket; value = ")".to_string(); },
            ']' => { tokenkind = TokenKind::RightSquare;  value = "]".to_string(); self.inside_square = false},
            '{' => { tokenkind = TokenKind::LeftCurly;    value = "{".to_string(); },
            '}' => { tokenkind = TokenKind::RightCurly;   value = "}".to_string(); },
            '⟨' => unimplemented!(), // { tokenkind = TokenKind::LeftAngle;    value = "⟨".to_string(); },
            '⟩' => unimplemented!(), // { tokenkind = TokenKind::RightAngle;   value = "⟩".to_string(); },
            '[' => { 
                if self.inside_square {panic!("Can't have nested square brackets! Pos: {}", self.pos)}
                tokenkind = TokenKind::LeftSquare;
                value = "[".to_string(); 
                self.inside_square = true},
            _ => return None
        }
        self.advance();

        Some(Token::new(tokenkind, value, start, self.pos))

    }

    fn get_primative(&mut self) -> Option<Token> {
        if !self.curr_char.is_ascii_uppercase() { return None }

        let start = self.pos;
        let c = self.curr_char;
        self.advance();
        
        Some(Token::new(TokenKind::Primative, c.to_string(), start, self.pos))
    }

    fn get_numeric(&mut self) -> Option<Token> {
        if !self.curr_char.is_ascii_digit() { return None }

        let start = self.pos;

        let mut buffer: String = String::new();

        while self.has_more_chars() && self.curr_char.is_ascii_digit() {
            buffer.push(self.curr_char);
            self.advance();
        }

        Some(Token::new(TokenKind::Number, buffer, start, self.pos))

    }

    fn get_feature(&mut self) -> Option<Token> {
        
        if self.curr_char != '+' && self.curr_char != '-' && !matches!(self.curr_char, 'α'..='ω') {
            return None;
        }
        
        let start = self.pos;
        let val = self.curr_char;

        self.advance();
        
        let mod_val: String = if val == '-' && matches!(self.curr_char, 'α'..='ω') {
            self.advance();
            let mut tmp = String::from('-'); tmp.push(self.curr_char);
            tmp
        } else {
            String::from(val)
        };

        let mut buffer = String::new();

        while self.curr_char.is_whitespace() { self.advance(); } 

        while self.curr_char.is_ascii_alphabetic() {
            buffer.push(self.curr_char);
            self.advance();
            
            while self.curr_char.is_whitespace() { self.advance(); }

        }

        if buffer.len() <= 1 { panic!("Expected feature after {}, found \"{}\". Pos: {}", val, buffer, start); }

        let tkn_kind = self.feature_match(buffer, start);
            
        match tkn_kind {
            TokenKind::Feature(FeatType::Node(_)) => if mod_val == "+".to_string() || mod_val == "-".to_string() {
                panic!("Nodes cannot be ±; they can only be used in Alpha Notation expressions.");
            }, 
            TokenKind::Feature(FeatType::Supr(SupraType::Length)) 
            | TokenKind::Feature(FeatType::Supr(SupraType::Stress)) 
            | TokenKind::Feature(FeatType::Supr(SupraType::Tone)) 
            => if mod_val == "+".to_string() || mod_val == "-".to_string() {
                panic!("Tone, Length, and Stress cannot be ±; they can only be used with numeric values.");
            }
            _ => {}
        }

        Some(Token::new(tkn_kind, mod_val, start, self.pos))
    }

    fn get_special_char(&mut self) -> Option<Token> {
        let start = self.pos;

        let tokenkind: TokenKind;
        let value: String;

        match self.curr_char {
            ',' => { tokenkind = TokenKind::Comma;        value = self.curr_char.to_string(); },
            ':' => { tokenkind = TokenKind::Colon;        value = self.curr_char.to_string(); },
            '#' => { tokenkind = TokenKind::WordBoundary; value = self.curr_char.to_string(); },
            '$' => { tokenkind = TokenKind::SyllBoundary; value = self.curr_char.to_string(); },
            '%' => { tokenkind = TokenKind::Syllable;     value = self.curr_char.to_string(); },
            '*' => { tokenkind = TokenKind::Star;         value = self.curr_char.to_string(); },
            '∅' => { tokenkind = TokenKind::EmptySet;     value = self.curr_char.to_string(); },
            '&' => { tokenkind = TokenKind::Ampersand;    value = self.curr_char.to_string(); },
            '_' => { tokenkind = TokenKind::Underline;    value = self.curr_char.to_string(); },
            '<' => { tokenkind = TokenKind::LessThan;     value = self.curr_char.to_string(); },
            '>' => { tokenkind = TokenKind::GreaterThan;  value = self.curr_char.to_string(); },
            '/' => { tokenkind = TokenKind::Slash;        value = self.curr_char.to_string(); },
            '|' => { tokenkind = TokenKind::Pipe;         value = self.curr_char.to_string(); },
            '=' => match self.peak_next_char() { 
                '>' => {
                    let c = self.curr_char;
                    self.advance();
                    tokenkind = TokenKind::Arrow;
                    value = format!("{c}>");
                },
                _ => { tokenkind = TokenKind::Equals;    value = self.curr_char.to_string(); },
             },
            '-' => match self.peak_next_char() {
                '>' => {
                    let c = self.curr_char;
                    self.advance();
                    tokenkind = TokenKind::Arrow;
                    value = format!("{c}>");
                },
                _ => panic!("Expected '->' got '-{}'", self.peak_next_char())
            },
            '…' | '⋯' => { tokenkind = TokenKind::Ellipsis;     value = self.curr_char.to_string(); },
            '.' => match self.peak_next_char() {
                '.' => {
                    self.advance(); 
                    if '.' == self.peak_next_char() { self.advance(); value = "...".to_string()}
                    else { value = "..".to_string(); }
                    tokenkind = TokenKind::Ellipsis;
                },
                _ => panic!("Expected '..' got '.{}'", self.peak_next_char())
            },
            _ => return None
        }
        self.advance();

        Some(Token::new(tokenkind, value, start, self.pos))
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
        if self.inside_square { return None }
        let start = self.pos;

        let mut  buffer = self.curr_char.to_string();

        if CARDINALS_TRIE.contains_partial(&buffer.as_str()) {
            self.advance();
            loop {
                //let tmp: String = buffer.clone() + self.curr_char.to_string().as_str();
                let mut tmp = buffer.clone(); tmp.push(self.curr_char);
                if CARDINALS_TRIE.contains_partial(&tmp.as_str()) {
                    buffer.push(self.curr_char);
                    self.advance();
                    continue;
                }

                return Some(Token::new(TokenKind::Cardinal, buffer, start, self.pos))
            }
        }
        return None
    }

    fn get_string(&mut self) -> Option<Token> { 

        if !self.curr_char.is_ascii_alphabetic() { return None }

        if !self.inside_square { panic!("Features must be inside square brackets") }

        let start = self.pos;

        let mut buffer: String = String::new();

        while self.has_more_chars() && self.curr_char.is_ascii_alphabetic() {
            buffer.push(self.curr_char);
            self.advance();
        }

        let tkn_kind: TokenKind = self.string_match(buffer, start);

        match tkn_kind {
          TokenKind::Feature(FeatType::Supr(SupraType::Length)) 
          | TokenKind::Feature(FeatType::Supr(SupraType::Tone)) 
          | TokenKind::Feature(FeatType::Supr(SupraType::Stress)) => {},
          _ => panic!("This feature must have a binary value (+/-).")
        }

        while self.curr_char.is_whitespace() { self.advance(); } 

        match self.curr_char {
            ':' => self.advance(),
            _ => panic!("This feature must be followed by a colon and then a number (e.g. `len : 2`).")
        } 
        
        while self.curr_char.is_whitespace() { self.advance(); } 

        match self.get_numeric() {
            Some(num) => {
                return Some(Token::new(tkn_kind, num.value, start, self.pos))
            },
            _ => panic!("This feature must be followed by a number (e.g. `len : 2`).")
        }
    }

    fn string_match(&mut self, buffer: String, start: usize) -> TokenKind {
        use TokenKind::*;
        use FeatType::*;
        use SupraType::*;
        match buffer.to_lowercase().as_str() {
            "stress" | "str" | "strss" => Feature(Supr(Stress)),
            "length" | "len" | "ln"    => Feature(Supr(Length)),
            "tone"   | "ton" | "tn"    => Feature(Supr(Tone)),
            _ => panic!("Unknown String Feature at pos: {}", start)
        }
    }

    fn feature_match(&mut self, buffer: String, start: usize) -> TokenKind {
        // apologies for the mess! this may need to be `un-hardcoded` at some stage
        use TokenKind::*;
        use FeatType::*;
        use NodeType::*;
        use FType::*;
        use SupraType::*;
        match buffer.to_lowercase().as_str() {
            // Root Node Features
            "root"        | "rut"       | "rt"                  => return Feature(Node(RootNode)),
            "consonantal" | "consonant" | "cons" | "cns"        => return Feature(Feat(Consonantal)),
            "sonorant"    | "sonor"     | "son"  | "sn"         => return Feature(Feat(Sonorant)),
            "syllabic"    | "syllab"    | "syll" | "sll"        => return Feature(Feat(Syllabic)),
            // Manner Node Features
            "manner"         | "mann"   | "man"  | "mnr" | "mn" => return Feature(Node(NodeType::MannerNode)),
            "continuant"     | "contin" | "cont" | "cnt"        => return Feature(Feat(Continuant)),
            "approximant"    | "approx" | "appr" | "app"        => return Feature(Feat(Approximant)),
            "lateral"        | "latrl"  | "ltrl" | "lat"        => return Feature(Feat(Lateral)),
            "nasal"          | "nsl"    | "nas"                 => return Feature(Feat(Nasal)),
            "delayedrelease" | "delrel" | "dlrl" | "dr"         => return Feature(Feat(DelayedRelease)),
            "strident"       | "strid"  | "stri"                => return Feature(Feat(Strident)),
            "rhotic"         | "rhot"   | "rho"  | "rh"         => return Feature(Feat(Rhotic)),
            "click"          | "clck"   | "clk"                 => return Feature(Feat(Click)),
            // Laryngeal Node Features
            "laryngeal"      | "laryng"     | "laryn"  | "lar"  => return Feature(Node(LaryngealNode)),
            "voice"          | "voi"        | "vce"    | "vc"   => return Feature(Feat(Voice)),
            "spreadglottis"  | "spreadglot" | "spread" | "sg"   => return Feature(Feat(SpreadGlottis)),
            "constrictedglottis"            | "constricted" |
            "constglot"      | "constr"     | "cg"              => return Feature(Feat(ConstrGlottis)),
            // Place Node Feature
            "place"       | "plce"    | "plc"                   => return Feature(Node(PlaceNode)),
            // Labial Place Node Features
            "labial"      | "lab"                               => return Feature(Node(LabialNode)),
            // todo: come up with a better name for this feature
            "bilabial"    | "bilab"   | "blb"                   => return Feature(Feat(Bilabial)),
            "round"       | "rnd"                               => return Feature(Feat(Round)),
            // Coronal Place Node Features
            "coronal"     | "coron"   | "cor"                   => return Feature(Node(CoronalNode)),
            "anterior"    | "anter"   | "ant"                   => return Feature(Feat(Anterior)),
            "distributed" | "distrib" | "dist" | "dst"          => return Feature(Feat(Distributed)),
            // Dorsal Place Node Features
            "dorsal"  | "dors"  | "dor"                         => return Feature(Node(DorsalNode)),
            "front"   | "frnt"  | "fnt"  | "fro" | "fr"         => return Feature(Feat(Front)),
            "back"    | "bck"   | "bk"                          => return Feature(Feat(Back)),
            "high"    | "hgh"   | "hi"                          => return Feature(Feat(High)),
            "low"     | "lo"                                    => return Feature(Feat(Low)),
            "tense"   | "tens"  | "tns"  | "ten"                => return Feature(Feat(Tense)),
            "reduced" | "reduc" | "redu" | "red"                => return Feature(Feat(Reduced)),
            // Pharyngeal Place Node Features
            "pharyngeal" | "pharyng" | "pharyn"  |
            "phar"       | "phr"                                => return Feature(Node(PharyngealNode)),
            "advancedtongueroot"     | "atr"                    => return Feature(Feat(AdvancedTongueRoot)),
            "retractedtongueroot"    | "rtr"                    => return Feature(Feat(RetractedTongueRoot)),
            // Suprasegmental Features
            "long"     | "lng"                                  => return Feature(Supr(Long)),
            "overlong" | "overlng" | "ovrlng" | "xlng"          => return Feature(Supr(Overlong)),
            "stress"   | "primarystress" | "primstress" |
            "primstr"  | "prmstr"  | "str" | "prim"             => return Feature(Supr(PrimStress)),
            "secondarystress"| "secstress" | "secstr" | "sec"   => return Feature(Supr(SecStress)),
        
            _ => panic!("Unknown feature at pos: {}", start)
        }
    }

    pub fn get_next_token(&mut self) -> Token{
        
        while self.curr_char.is_whitespace() { self.advance(); }
        
        if !self.has_more_chars() { return Token::new(TokenKind::Eol, String::new(), self.pos, self.pos+1) }

        if let Some(bkt_token) = self.get_bracket()      { return bkt_token }
        if let Some(pmt_token) = self.get_primative()    { return pmt_token }
        if let Some(num_token) = self.get_numeric()      { return num_token }
        if let Some(ftr_token) = self.get_feature()      { return ftr_token }
        if let Some(spc_token) = self.get_special_char() { return spc_token }
        if let Some(ipa_token) = self.get_ipa()          { return ipa_token }
        if let Some(str_token) = self.get_string()       { return str_token } 
        
        panic!("Unknown character at character {}", self.pos)
        // todo: return error
    }

    pub fn get_all_tokens(&mut self) -> Vec<Token> {
        let mut token_list: Vec<Token> =  Vec::new();
        loop {
            let next_token = self.get_next_token();
            match next_token.kind {
                TokenKind::Eol => {
                    token_list.push(next_token);
                    break
                },
                _ => {token_list.push(next_token);}
            }
        }
        token_list
    }
}

#[cfg(test)]
mod lexer_tests {

    use super::*;

    #[test]
    fn test_syll() {

        let test_input= String::from("%");
        let expected_result = TokenKind::Syllable;

        let result = Lexer::new(test_input.clone()).get_next_token();

        assert_eq!(result.kind, expected_result);
        assert_eq!(result.value, test_input);
    }

    #[test]
    fn test_ipa_sep() {
        
        let test_input= String::from("t͡ɕ b͡β b a");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal, "t͡ɕ".to_owned(),  0,  3),
            Token::new(TokenKind::Cardinal, "b͡β".to_owned(),  4,  7),
            Token::new(TokenKind::Cardinal,  "b".to_owned(),  8,  9),
            Token::new(TokenKind::Cardinal,  "a".to_owned(), 10, 11),
            Token::new(TokenKind::Eol,        String::new(), 11, 12),
        ];

        let result = Lexer::new(test_input.clone()).get_all_tokens();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_ipa_joined() {
        
        let test_input= String::from("t͡ɕb͡βba");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal, "t͡ɕ".to_owned(), 0, 3),
            Token::new(TokenKind::Cardinal, "b͡β".to_owned(), 3, 6),
            Token::new(TokenKind::Cardinal,  "b".to_owned(), 6, 7),
            Token::new(TokenKind::Cardinal,  "a".to_owned(), 7, 8),
            Token::new(TokenKind::Eol,        String::new(), 8, 9),
        ];

        let result = Lexer::new(test_input.clone()).get_all_tokens();  

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_metathesis() {
        
        let test_input= String::from("t͡ɕ...b͡β > &");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal,   "t͡ɕ".to_owned(),  0,  3),
            Token::new(TokenKind::Ellipsis,  "...".to_owned(),  3,  6),
            Token::new(TokenKind::Cardinal,   "b͡β".to_owned(),  6,  9),
            Token::new(TokenKind::GreaterThan, ">".to_owned(), 10, 11),
            Token::new(TokenKind::Ampersand,   "&".to_owned(), 12, 13),
            Token::new(TokenKind::Eol,          String::new(), 13, 14),
        ];

        let result = Lexer::new(test_input.clone()).get_all_tokens();        

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
            Token::new(TokenKind::LeftSquare,                          "[".to_owned(),  0,  1),
            Token::new(TokenKind::Feature(Feat(FType::Voice)),         "+".to_owned(),  1,  5),
            Token::new(TokenKind::Comma,                               ",".to_owned(),  5,  6),
            Token::new(TokenKind::Feature(Feat(FType::SpreadGlottis)), "-".to_owned(),  7, 10),
            Token::new(TokenKind::Comma,                               ",".to_owned(), 10, 11),
            Token::new(TokenKind::Feature(Node(NodeType::PlaceNode)),  "α".to_owned(), 12, 18),
            Token::new(TokenKind::RightSquare,                         "]".to_owned(), 18, 19),
            Token::new(TokenKind::Eol,                                  String::new(), 19, 20),
        ];

        let result = Lexer::new(test_input.clone()).get_all_tokens();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);

        }
    }

    #[test]
    fn test_variables() {
        
        let test_input= String::from("C=1 V=2 > 2 1 / _C");
        let expected_result = vec![
            Token::new(TokenKind::Primative,   "C".to_owned(),  0,  1),
            Token::new(TokenKind::Equals,      "=".to_owned(),  1,  2),
            Token::new(TokenKind::Number,      "1".to_owned(),  2,  3),
            Token::new(TokenKind::Primative,   "V".to_owned(),  4,  5),
            Token::new(TokenKind::Equals,      "=".to_owned(),  5,  6),
            Token::new(TokenKind::Number,      "2".to_owned(),  6,  7),
            Token::new(TokenKind::GreaterThan, ">".to_owned(),  8,  9),
            Token::new(TokenKind::Number,      "2".to_owned(), 10, 11),
            Token::new(TokenKind::Number,      "1".to_owned(), 12, 13),
            Token::new(TokenKind::Slash,       "/".to_owned(), 14, 15),
            Token::new(TokenKind::Underline,   "_".to_owned(), 16, 17),
            Token::new(TokenKind::Primative,   "C".to_owned(), 17, 18),
            Token::new(TokenKind::Eol,          String::new(), 18, 19),
        ];

        let result = Lexer::new(test_input.clone()).get_all_tokens();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

}