use crate::FeatType;

// pub mod aliasing;
pub mod lexer;
pub mod parser;

#[allow(unused)]
pub enum AliasKind {
    Deromaniser,
    Romaniser
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum AliasTokenKind {
    LeftSquare,       // [
    RightSquare,      // ]




    LessThan,         // <
    GreaterThan,      // >
    Equals,           // =
    Underline,        // _
    Arrow,            // -> or =>
    Comma,            // ,
    Colon,            // :
    WordBoundary,     // #
    SyllBoundary,     // $
    Syllable,         // %

    Group,            // Primitive Category i.e. C for Cons, V for Vowel
    Number,           // Number



    Cardinal,         // IPA character
    Diacritic(u8),    // IPA Diacritic
    Star,             // *
    EmptySet,         // ∅

    Feature(FeatType),
    String, 
    Eol,              // End of Line 
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AliasPosition {
    line: usize,
    start: usize,
    end: usize
}
impl AliasPosition {
    fn new(line: usize, start: usize, end: usize) -> Self {
        Self { line, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasToken {
    kind: AliasTokenKind,
    value: String, 
    position: AliasPosition
}
impl AliasToken {
    fn new(kind: AliasTokenKind, value: String, position: AliasPosition) -> Self {
        Self { kind, value, position }
    }
}