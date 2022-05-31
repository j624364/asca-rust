#[derive(Debug)]
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
    StringLit,    // 
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

#[derive(Debug)]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

impl Position {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String, 
    pub position: Position,
}

impl Token {
    pub fn new(kind: TokenKind, value: String, start: usize, end: usize) -> Self{
        
        Self { 
            kind, 
            value, 
            position: Position::new(start, end)
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
            
            if self.curr_char.is_whitespace() { self.advance(); }

        }

        if buffer.len() <= 1 { panic!("Expected feature after {}, found \"{}\". Pos: {}", val, buffer, start); }

        let tkn_kind: TokenKind;
        
        // apologies for the mess! this needs to be `un-hardcoded` at some stage
        match buffer.to_lowercase().as_str() {
            "root" | "rt"                                       => {tkn_kind = TokenKind::RootNode},
            "consonantal" | "consonant" | "cons" | "cns"        => {tkn_kind = TokenKind::Consonantal},
            "sonorant" | "sonor"| "son"                         => {tkn_kind = TokenKind::Sonorant},
            "syllabic" | "syllab"| "syll"                       => {tkn_kind = TokenKind::Syllabic},

            "manner" | "mann"| "man" | "mnnr" | "mnr" | "mn"    => {tkn_kind = TokenKind::MannerNode},
            "continuant" | "contin" | "cont" | "cnt"            => {tkn_kind = TokenKind::Continuant},
            "approximant" | "approx" | "appr" | "app"           => {tkn_kind = TokenKind::Approximant},
            "lateral" | "later" | "lat" | "ltrl"                => {tkn_kind = TokenKind::Lateral},
            "nasal" | "nsl" | "nas"                             => {tkn_kind = TokenKind::Nasal},
            "delayedrelease" | "delrel" | "dr"                  => {tkn_kind = TokenKind::DelayedRelease},
            "strident" | "strid" | "stri"                       => {tkn_kind = TokenKind::Strident},
            "rhotic" | "rhot" | "rho" | "rh"                    => {tkn_kind = TokenKind::Rhotic},
            
            "laryngeal" | "laryng" | "laryn" | "lar"            => {tkn_kind = TokenKind::LaryngealNode},
            "voice" | "voi" | "vce" | "vc"                      => {tkn_kind = TokenKind::Voice},
            "spreadglottis" | "sprdglot" | "spread" | "sg"      => {tkn_kind = TokenKind::SpreadGlottis},
            "constrictedglottis" | "cnstglot" | "constr" | "cg" => {tkn_kind = TokenKind::ConstrGlottis},
            
            "place" | "plc" | "pla"                             => {tkn_kind = TokenKind::PlaceNode},
            
            "labial" | "lab"                                    => {tkn_kind = TokenKind::LabialNode},
            "round" | "rnd"                                     => {tkn_kind = TokenKind::Round},
            
            "coronal" | "coron" | "cor"                         => {tkn_kind = TokenKind::CoronalNode},
            "anterior" | "anter" | "ant"                        => {tkn_kind = TokenKind::Anterior},
            "distributed" | "distrib" | "dist" | "dst"          => {tkn_kind = TokenKind::Distributed},
            
            "dorsal" | "dors" | "dor"                           => {tkn_kind = TokenKind::DorsalNode},
            "front" | "frnt" | "fnt" | "fro" | "fr"             => {tkn_kind = TokenKind::Front},
            "back" | "bck" | "bk"                               => {tkn_kind = TokenKind::Back},
            "high" | "hgh" | "hi"                               => {tkn_kind = TokenKind::High},
            "low" | "lo"                                        => {tkn_kind = TokenKind::Low},
            "tense" | "tens" | "tns" | "ten"                    => {tkn_kind = TokenKind::Tense},
            "reduced" | "reduc" | "redu" | "red"                => {tkn_kind = TokenKind::Reduced},
            
            "pharyngeal" | "pharyng" | "pharyn" | "phar"        => {tkn_kind = TokenKind::PharyngealNode},
            "advancedtongueroot" | "atr"                        => {tkn_kind = TokenKind::AdvancedTongueRoot},
            "retractedtongueroot" | "rtr"                       => {tkn_kind = TokenKind::RetractedTongueRoot},
            "long" | "lng"                                      => {tkn_kind = TokenKind::Long},
            "overlong" | "overlng"                              => {tkn_kind = TokenKind::Overlong},
            "stress" | "str" | "strss"                          => {tkn_kind = TokenKind::Stress},
            "length" | "len"                                    => {tkn_kind = TokenKind::Length},
            "tone" | "tn"                                       => {tkn_kind = TokenKind::Tone},
            
            _ => panic!("Unknown feature at pos: {}", start)
        }

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

    fn ipa(&self) -> Option<Token> {
        todo!()
    }

    fn string(&mut self) -> Option<Token> {
        if !self.curr_char.is_ascii_alphabetic() { return None }

        if !self.inside_square { panic!("Strings must be inside square brackets") }

        let start = self.pos;

        let mut buffer: String = String::new();

        while self.has_more_chars() && self.curr_char.is_ascii_alphabetic() {
            buffer.push(self.curr_char);
            self.advance();
        }

        Some(Token::new(TokenKind::StringLit, buffer, start, self.pos))

    }

    pub fn get_next_token(&mut self) -> Token{
        if !self.has_more_chars() {
            return Token::new(TokenKind::Eol, "eol".to_string(), self.pos, self.pos+1)
        } 

        while self.curr_char.is_whitespace() { self.advance(); }

        if let Some(bkt_token) = self.is_bracket()   { return bkt_token }
        if let Some(pmt_token) = self.primative()    { return pmt_token }
        if let Some(num_token) = self.numeric()      { return num_token }
        if let Some(ftr_token) = self.feature()      { return ftr_token }
        if let Some(spc_token) = self.special_char() { return spc_token }
        if let Some(ipa_token) = self.ipa()          { return ipa_token }
        if let Some(str_token) = self.string()       { return str_token } 
        
        panic!("Unknown character at {} at pos: {}", self.curr_char, self.pos)
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

        return token_list;

    }


}