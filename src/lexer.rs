use crate::trie::Trie;

#[derive(Debug, Copy, Clone)]
pub enum TokenKind {
    LeftSquare,   // [
    RightSquare,  // ]
    LeftCurly,    // {
    RightCurly,   // }
    LeftAngle,    // <
    RightAngle,   // >
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
    Eol,          // End of Line 

    RootNode,       // ROOT node 
    Consonantal,   
    Sonorant,      
    Syllabic,      
    
    MannerNode,     // MANNER node 
    Continuant,      
    Approximant,     
    Lateral,         
    Nasal,           
    DelayedRelease,  
    Strident,        
    Rhotic,          
    
    LaryngealNode,  // LAR node 
    Voice,           
    SpreadGlottis,   
    ConstrGlottis,   
    
    PlaceNode,      // PLACE Node

    LabialNode,     // LABIAL subnode 
    Round,          
    
    CoronalNode,    // CORONAL subnode 
    Anterior,        
    Distributed,     
    
    DorsalNode,     // DORSAL subnode 
    Front,          
    Back,           
    High,           
    Low,            
    Tense,          
    Reduced,        
    
    PharyngealNode, // PHAR subnode
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

#[derive(Debug, Copy, Clone)]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

impl Position {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone)]
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

pub struct Lexer {
    source: Vec<char>,
    cardinals_trie: Trie,
    pos: usize,
    curr_char: char,
    inside_square: bool
}

impl Lexer {

    pub fn new(input: String, c: &Trie) -> Self {
        let mut s = Self { 
            source: input.chars().collect(), 
            cardinals_trie: c.clone(),
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
            '⟨' => { tokenkind = TokenKind::LeftAngle;    value = "⟨".to_string(); },
            '⟩' => { tokenkind = TokenKind::RightAngle;   value = "⟩".to_string(); },
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
              TokenKind::RootNode 
            | TokenKind::PlaceNode 
            | TokenKind::DorsalNode 
            | TokenKind::LabialNode
            | TokenKind::MannerNode
            | TokenKind::CoronalNode
            | TokenKind::LaryngealNode
            | TokenKind::PharyngealNode => if !matches!(val, 'α'..='ω') {
                panic!("Nodes cannot be +/-; they can only be used in Alpha Notation expressions.");
            }, 
            TokenKind::Length | TokenKind::Tone => if !matches!(val, 'α'..='ω') {
                panic!("Tone and Length cannot be +/-; they can only be used with numeric values.");
            }
            _ => {}
        }

        Some(Token::new(tkn_kind, val.to_string(), start, self.pos))
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
                _ => panic!("Expected {0}> got {0}{1}", self.curr_char, self.peak_next_char())
            },
            '…' | '⋯' => { tokenkind = TokenKind::Ellipsis;     value = self.curr_char.to_string(); },
            '.' => match self.peak_next_char() {
                '.' => {
                    self.advance(); 
                    if '.' == self.peak_next_char() { self.advance(); value = "...".to_string()}
                    else { value = "..".to_string(); }
                    tokenkind = TokenKind::Ellipsis;
                },
                _ => panic!("Expected {0}> got {0}{1}", self.curr_char, self.peak_next_char())
            },
            _ => return None
        }
        self.advance();

        Some(Token::new(tokenkind, value, start, self.pos))
    }

    fn ipa(&mut self) -> Option<Token> {
        if self.inside_square { return None }
        let start = self.pos;

        let mut  buffer = String::new();
        let mut last_buffer: String;

        while self.has_more_chars() {

            last_buffer = buffer.clone();
            buffer.push(self.curr_char);

            if !buffer.is_empty() && self.cardinals_trie.find(&buffer.as_str()).is_empty() {
                if !last_buffer.is_empty() && self.cardinals_trie.contains(&last_buffer.as_str()) {
                    return Some(Token::new(TokenKind::Cardinal, last_buffer, start, self.pos))
                }
                return None
            }
            
            self.advance();
        }

        if buffer.is_empty() || !self.cardinals_trie.contains(&buffer.as_str()) {
            return None
        }
        self.advance();
        return Some(Token::new(TokenKind::Cardinal, buffer, start, self.pos))
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
          TokenKind::Length | TokenKind::Tone | TokenKind::Stress => {},
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
        // apologies for the mess! this needs to be `un-hardcoded` at some stage
        match buffer.to_lowercase().as_str() {
            // Root Node Features
            "root" | "rt"                                       => { return TokenKind::RootNode },
            "consonantal" | "consonant" | "cons" | "cns"        => { return TokenKind::Consonantal },
            "sonorant" | "sonor"| "son"                         => { return TokenKind::Sonorant },
            "syllabic" | "syllab"| "syll"                       => { return TokenKind::Syllabic },
            // Manner Node Features
            "manner" | "mann"| "man" | "mnnr" | "mnr" | "mn"    => { return TokenKind::MannerNode },
            "continuant" | "contin" | "cont" | "cnt"            => { return TokenKind::Continuant },
            "approximant" | "approx" | "appr" | "app"           => { return TokenKind::Approximant },
            "lateral" | "later" | "lat" | "ltrl"                => { return TokenKind::Lateral },
            "nasal" | "nsl" | "nas"                             => { return TokenKind::Nasal },
            "delayedrelease" | "delrel" | "dr"                  => { return TokenKind::DelayedRelease },
            "strident" | "strid" | "stri"                       => { return TokenKind::Strident },
            "rhotic" | "rhot" | "rho" | "rh"                    => { return TokenKind::Rhotic },
            // Laryngeal Node Features
            "laryngeal" | "laryng" | "laryn" | "lar"            => { return TokenKind::LaryngealNode },
            "voice" | "voi" | "vce" | "vc"                      => { return TokenKind::Voice },
            "spreadglottis" | "sprdglot" | "spread" | "sg"      => { return TokenKind::SpreadGlottis },
            "constrictedglottis" | "cnstglot" | "constr" | "cg" => { return TokenKind::ConstrGlottis },
            // Place Node Feature
            "place" | "plc" | "pla"                             => { return TokenKind::PlaceNode },
            // Labial Place Node Features
            "labial" | "lab"                                    => { return TokenKind::LabialNode },
            "round" | "rnd"                                     => { return TokenKind::Round },
            // Coronal Place Node Features
            "coronal" | "coron" | "cor"                         => { return TokenKind::CoronalNode },
            "anterior" | "anter" | "ant"                        => { return TokenKind::Anterior },
            "distributed" | "distrib" | "dist" | "dst"          => { return TokenKind::Distributed },
            // Dorsal Place Node Features
            "dorsal" | "dors" | "dor"                           => { return TokenKind::DorsalNode },
            "front" | "frnt" | "fnt" | "fro" | "fr"             => { return TokenKind::Front },
            "back" | "bck" | "bk"                               => { return TokenKind::Back },
            "high" | "hgh" | "hi"                               => { return TokenKind::High },
            "low" | "lo"                                        => { return TokenKind::Low },
            "tense" | "tens" | "tns" | "ten"                    => { return TokenKind::Tense },
            "reduced" | "reduc" | "redu" | "red"                => { return TokenKind::Reduced },
            // Pharyngeal Place Node Features
            "pharyngeal" | "pharyng" | "pharyn" | "phar"        => { return TokenKind::PharyngealNode },
            "advancedtongueroot" | "atr"                        => { return TokenKind::AdvancedTongueRoot },
            "retractedtongueroot" | "rtr"                       => { return TokenKind::RetractedTongueRoot },
            // Suprasegmental Features
            "long" | "lng"                                      => { return TokenKind::Long },
            "overlong" | "overlng"                              => { return TokenKind::Overlong },
            "stress" | "str" | "strss"                          => { return TokenKind::Stress },
            "length" | "len"                                    => { return TokenKind::Length },
            "tone" | "tn"                                       => { return TokenKind::Tone },
        
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