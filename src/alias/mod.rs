use std::fmt;

use parser::AliasItem;

use crate::FeatType;

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Transformation {
    pub(crate) input: AliasItem,
    pub(crate) output: AliasItem
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AliasKind {
    Deromaniser,
    Romaniser
}

impl fmt::Display for AliasKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AliasKind::Deromaniser => write!(f, "deromaniser"),
            AliasKind::Romaniser   => write!(f, "romaniser"),
        }
    }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum AliasTokenKind {
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
impl AliasTokenKind {
    pub(crate) fn as_diacritic(&self) -> Option<&u8> {
        if let Self::Diacritic(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AliasPosition {
    pub(crate) kind: AliasKind,
    pub(crate) line: usize,
    pub(crate) start: usize,
    pub(crate) end: usize
}
impl AliasPosition {
    fn new(kind: AliasKind, line: usize, start: usize, end: usize) -> Self {
        Self { kind, line, start, end }
    }
}

impl fmt::Display for AliasPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}-{}", self.kind, self.line, self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasToken {
    pub(crate) kind: AliasTokenKind,
    pub(crate) value: String, 
    pub(crate) position: AliasPosition
}
impl AliasToken {
    fn new(kind: AliasTokenKind, value: String, position: AliasPosition) -> Self {
        Self { kind, value, position }
    }
}