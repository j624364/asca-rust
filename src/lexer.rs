use std::fmt::{self, Display};
use crate::CARDINALS;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FeatType {
    // ROOT node 
    RootNode,      
    Consonantal,   
    Sonorant,      
    Syllabic,      
    // MANNER node 
    MannerNode,
    Continuant,      
    Approximant,     
    Lateral,         
    Nasal,           
    DelayedRelease,  
    Strident,        
    Rhotic,          
    Click,          
    // LAR node
    LaryngealNode,   
    Voice,           
    SpreadGlottis,   
    ConstrGlottis,   
    // PLACE Node
    PlaceNode,      
    // LABIAL subnode
    LabialNode,      
    Bilabial,      
    Round,          
    // CORONAL subnode
    CoronalNode,     
    Anterior,        
    Distributed,     
    // DORSAL subnode
    DorsalNode,      
    Front,          
    Back,           
    High,           
    Low,            
    Tense,          
    Reduced,        
    // PHAR subnode
    PharyngealNode, 
    AdvancedTongueRoot,
    RetractedTongueRoot, 
    // Suprasegmental Features
    Long,
    Overlong,
    Stress,
    // Can only be used with : notation
    Length, // Len : 2 == +long
    Tone,
}

impl Display for FeatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use FeatType::*;
        match self {
            RootNode            => write!(f, "ROOT"),
            Consonantal         => write!(f, "cons"),
            Sonorant            => write!(f, "son"),
            Syllabic            => write!(f, "syll"),
            MannerNode          => write!(f, "MAN"),
            Continuant          => write!(f, "cont"),
            Approximant         => write!(f, "appr"),
            Lateral             => write!(f, "lat"),
            Nasal               => write!(f, "nas"),
            DelayedRelease      => write!(f, "delrel"),
            Strident            => write!(f, "strid"),
            Rhotic              => write!(f, "rho"),
            Click               => write!(f, "clk"),
            LaryngealNode       => write!(f, "LAR"),
            Voice               => write!(f, "voi"),
            SpreadGlottis       => write!(f, "sg"),
            ConstrGlottis       => write!(f, "cg"),
            PlaceNode           => write!(f, "PLACE"),
            LabialNode          => write!(f, "LAB"),
            Bilabial            => write!(f, "bilab"),
            Round               => write!(f, "rnd"),
            CoronalNode         => write!(f, "COR"),
            Anterior            => write!(f, "ant"),
            Distributed         => write!(f, "dis"),
            DorsalNode          => write!(f, "DOR"),
            Front               => write!(f, "fr"),
            Back                => write!(f, "bk"),
            High                => write!(f, "hi"),
            Low                 => write!(f, "lo"),
            Tense               => write!(f, "tens"),
            Reduced             => write!(f, "red"),
            PharyngealNode      => write!(f, "PHAR"),
            AdvancedTongueRoot  => write!(f, "atr"),
            RetractedTongueRoot => write!(f, "rtr"),
            Long                => write!(f, "long"),
            Overlong            => write!(f, "overlng"),
            Stress              => write!(f, "str"),
            Length              => write!(f, "len"),
            Tone                => write!(f, "tone"),
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

    fn is_bracket(&mut self) -> Option<Token> {
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

    fn primative(&mut self) -> Option<Token> {
        if !self.curr_char.is_ascii_uppercase() { return None }

        let start = self.pos;
        let c = self.curr_char;
        self.advance();
        
        Some(Token::new(TokenKind::Primative, c.to_string(), start, self.pos))
    }

    fn numeric(&mut self) -> Option<Token> {
        if !self.curr_char.is_ascii_digit() { return None }

        let start = self.pos;

        let mut buffer: String = String::new();

        while self.has_more_chars() && self.curr_char.is_ascii_digit() {
            buffer.push(self.curr_char);
            self.advance();
        }

        Some(Token::new(TokenKind::Number, buffer, start, self.pos))

    }

    fn feature(&mut self) -> Option<Token> {
        
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
              TokenKind::Feature(FeatType::RootNode) 
            | TokenKind::Feature(FeatType::PlaceNode) 
            | TokenKind::Feature(FeatType::DorsalNode) 
            | TokenKind::Feature(FeatType::LabialNode)
            | TokenKind::Feature(FeatType::MannerNode)
            | TokenKind::Feature(FeatType::CoronalNode)
            | TokenKind::Feature(FeatType::LaryngealNode)
            | TokenKind::Feature(FeatType::PharyngealNode) => if mod_val == "+".to_string() || mod_val == "-".to_string() {
                panic!("Nodes cannot be +/-; they can only be used in Alpha Notation expressions.");
            }, 
            TokenKind::Feature(FeatType::Length) | TokenKind::Feature(FeatType::Tone) => if mod_val == "+".to_string() || mod_val == "-".to_string() {
                panic!("Tone and Length cannot be +/-; they can only be used with numeric values.");
            }
            _ => {}
        }

        Some(Token::new(tkn_kind, mod_val, start, self.pos))
    }

    fn special_char(&mut self) -> Option<Token> {
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
                    value = format!("{c}>").to_string();
                },
                _ => { tokenkind = TokenKind::Equals;    value = self.curr_char.to_string(); },
             },
            '-' => match self.peak_next_char() {
                '>' => {
                    let c = self.curr_char;
                    self.advance();
                    tokenkind = TokenKind::Arrow;
                    value = format!("{c}>").to_string();
                },
                _ => panic!("Expected -> got -{}", self.peak_next_char())
            },
            '…' | '⋯' => { tokenkind = TokenKind::Ellipsis;     value = self.curr_char.to_string(); },
            '.' => match self.peak_next_char() {
                '.' => {
                    self.advance(); 
                    if '.' == self.peak_next_char() { self.advance(); value = "...".to_string()}
                    else { value = "..".to_string(); }
                    tokenkind = TokenKind::Ellipsis;
                },
                _ => panic!("Expected .. got .{}", self.peak_next_char())
            },
            _ => return None
        }
        self.advance();

        Some(Token::new(tokenkind, value, start, self.pos))
    }

    // fn ipa_old(&mut self) -> Option<Token> {
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

    fn ipa(&mut self) -> Option<Token> {
        if self.inside_square { return None }
        let start = self.pos;

        let mut  buffer = self.curr_char.to_string();

        if CARDINALS.contains_partial(&buffer.as_str()) {
            self.advance();
            loop {
                //let tmp: String = buffer.clone() + self.curr_char.to_string().as_str();
                let mut tmp = buffer.clone(); tmp.push(self.curr_char);
                if CARDINALS.contains_partial(&tmp.as_str()) {
                    buffer.push(self.curr_char);
                    self.advance();
                    continue;
                }

                return Some(Token::new(TokenKind::Cardinal, buffer, start, self.pos))
            }
        }
        return None
    }

    fn string(&mut self) -> Option<Token> { 

        if !self.curr_char.is_ascii_alphabetic() { return None }

        if !self.inside_square { panic!("Features must be inside square brackets") }

        let start = self.pos;

        let mut buffer: String = String::new();

        while self.has_more_chars() && self.curr_char.is_ascii_alphabetic() {
            buffer.push(self.curr_char);
            self.advance();
        }

        let tkn_kind: TokenKind = self.feature_match(buffer, start);

        match tkn_kind {
          TokenKind::Feature(FeatType::Length) | TokenKind::Feature(FeatType::Tone) | TokenKind::Feature(FeatType::Stress) => {},
          _ => panic!("This feature must have a binary value (+/-).")
        }

        while self.curr_char.is_whitespace() { self.advance(); } 

        match self.curr_char {
            ':' => self.advance(),
            _ => panic!("This feature must be followed by a colon and then a number (e.g. `len : 2`).")
        } 
        
        while self.curr_char.is_whitespace() { self.advance(); } 

        match self.numeric() {
            Some(num) => {
                return Some(Token::new(tkn_kind, num.value, start, self.pos))
            },
            _ => panic!("This feature must be followed by a number (e.g. `len : 2`).")
        }
    }

    fn feature_match(&mut self, buffer: String, start: usize) -> TokenKind {
        // apologies for the mess! this may need to be `un-hardcoded` at some stage
        use TokenKind::*;
        use FeatType::*;
        match buffer.to_lowercase().as_str() {
            // Root Node Features
            "root"        | "rut"       | "rt"                  => { return Feature(RootNode) },
            "consonantal" | "consonant" | "cons" | "cns"        => { return Feature(Consonantal) },
            "sonorant"    | "sonor"     | "son"  | "sn"         => { return Feature(Sonorant) },
            "syllabic"    | "syllab"    | "syll" | "sll"        => { return Feature(Syllabic) },
            // Manner Node Features
            "manner"         | "mann"   | "man"  | "mnr" | "mn" => { return Feature(MannerNode) },
            "continuant"     | "contin" | "cont" | "cnt"        => { return Feature(Continuant) },
            "approximant"    | "approx" | "appr" | "app"        => { return Feature(Approximant) },
            "lateral"        | "latrl"  | "ltrl" | "lat"        => { return Feature(Lateral) },
            "nasal"          | "nsl"    | "nas"                 => { return Feature(Nasal) },
            "delayedrelease" | "delrel" | "dlrl" | "dr"         => { return Feature(DelayedRelease) },
            "strident"       | "strid"  | "stri"                => { return Feature(Strident) },
            "rhotic"         | "rhot"   | "rho"  | "rh"         => { return Feature(Rhotic) },
            "click"          | "clck"   | "clk"                 => { return Feature(Click) },
            // Laryngeal Node Features
            "laryngeal"      | "laryng"     | "laryn"  | "lar"  => { return Feature(LaryngealNode) },
            "voice"          | "voi"        | "vce"    | "vc"   => { return Feature(Voice) },
            "spreadglottis"  | "spreadglot" | "spread" | "sg"   => { return Feature(SpreadGlottis) },
            "constrictedglottis"            | "constricted"     |
            "constglot"      | "constr"     | "cg"              => { return Feature(ConstrGlottis) },
            // Place Node Feature
            "place"       | "plce"    | "plc"                   => { return Feature(PlaceNode) },
            // Labial Place Node Features
            "labial"      | "lab"                               => { return Feature(LabialNode) },
            // todo: come up with a better name for this feature
            "bilabial"    | "bilab"   | "blb"                   => { return Feature(Bilabial) },
            "round"       | "rnd"                               => { return Feature(Round) },
            // Coronal Place Node Features
            "coronal"     | "coron"   | "cor"                   => { return Feature(CoronalNode) },
            "anterior"    | "anter"   | "ant"                   => { return Feature(Anterior) },
            "distributed" | "distrib" | "dist" | "dst"          => { return Feature(Distributed) },
            // Dorsal Place Node Features
            "dorsal"  | "dors"  | "dor"                         => { return Feature(DorsalNode) },
            "front"   | "frnt"  | "fnt"  | "fro" | "fr"         => { return Feature(Front) },
            "back"    | "bck"   | "bk"                          => { return Feature(Back) },
            "high"    | "hgh"   | "hi"                          => { return Feature(High) },
            "low"     | "lo"                                    => { return Feature(Low) },
            "tense"   | "tens"  | "tns"  | "ten"                => { return Feature(Tense) },
            "reduced" | "reduc" | "redu" | "red"                => { return Feature(Reduced) },
            // Pharyngeal Place Node Features
            "pharyngeal" | "pharyng" | "pharyn"  |
            "phar"       | "phr"                                => { return Feature(PharyngealNode) },
            "advancedtongueroot"     | "atr"                    => { return Feature(AdvancedTongueRoot) },
            "retractedtongueroot"    | "rtr"                    => { return Feature(RetractedTongueRoot) },
            // Suprasegmental Features
            "long"     | "lng"                                  => { return Feature(Long) },
            "overlong" | "overlng" | "ovrlng" | "olng"          => { return Feature(Overlong) },
            "stress"   | "str"     | "strss"                    => { return Feature(Stress) },
            "length"   | "len"                                  => { return Feature(Length) },
            "tone"     | "tn"                                   => { return Feature(Tone) },
        
            _ => panic!("Unknown feature at pos: {}", start)
        }
    }

    pub fn get_next_token(&mut self) -> Token{
        
        while self.curr_char.is_whitespace() { self.advance(); }
        
        if !self.has_more_chars() { return Token::new(TokenKind::Eol, "eol".to_string(), self.pos, self.pos+1) }

        if let Some(bkt_token) = self.is_bracket()   { return bkt_token }
        if let Some(pmt_token) = self.primative()    { return pmt_token }
        if let Some(num_token) = self.numeric()      { return num_token }
        if let Some(ftr_token) = self.feature()      { return ftr_token }
        if let Some(spc_token) = self.special_char() { return spc_token }
        if let Some(ipa_token) = self.ipa()          { return ipa_token }
        if let Some(str_token) = self.string()       { return str_token } 
        
        panic!("Unknown character at character {}", self.pos)
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
            Token::new(TokenKind::Eol,     "eol".to_owned(), 11, 12),
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
            Token::new(TokenKind::Eol,     "eol".to_owned(), 8, 9),
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
            Token::new(TokenKind::Eol,       "eol".to_owned(), 13, 14),
        ];

        let result = Lexer::new(test_input.clone()).get_all_tokens();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_feature_matrix() {
        
        let test_input= String::from("[+voi, -sg, αPLACE]");
        let expected_result = vec![
            Token::new(TokenKind::LeftSquare,                       "[".to_owned(),  0,  1),
            Token::new(TokenKind::Feature(FeatType::Voice),         "+".to_owned(),  1,  5),
            Token::new(TokenKind::Comma,                            ",".to_owned(),  5,  6),
            Token::new(TokenKind::Feature(FeatType::SpreadGlottis), "-".to_owned(),  7, 10),
            Token::new(TokenKind::Comma,                            ",".to_owned(), 10, 11),
            Token::new(TokenKind::Feature(FeatType::PlaceNode),     "α".to_owned(), 12, 18),
            Token::new(TokenKind::RightSquare,                      "]".to_owned(), 18, 19),
            Token::new(TokenKind::Eol,                            "eol".to_owned(), 19, 20),
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
            Token::new(TokenKind::Eol,       "eol".to_owned(), 18, 19),
        ];

        let result = Lexer::new(test_input.clone()).get_all_tokens();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

}