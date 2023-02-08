use std::fmt;

use crate ::{
    lexer ::Token, 
    parser::Item, 
};

#[derive(Debug, Clone)]
pub enum Error {
    WordSyn(WordSyntaxError),
    RuleSyn(RuleSyntaxError),
    Runtime(RuntimeError),
}

impl From<RuntimeError> for Error {
    fn from(e:RuntimeError) -> Self {
        Error::Runtime(e)
    }
}

impl From<WordSyntaxError> for Error {
    fn from(e:WordSyntaxError) -> Self {
        Error::WordSyn(e)
    }
}

impl From<RuleSyntaxError> for Error {
    fn from(e:RuleSyntaxError) -> Self {
        Error::RuleSyn(e)
    }
}

#[derive(Debug, Clone)]
pub enum WordSyntaxError {
    UnknownChar(String, usize),
    // CharAfterTone(String, usize),
    NoSegmentBeforeColon(usize),
    CouldNotParse
}

#[derive(Debug, Clone)]
pub enum RuntimeError { 
    UnbalancedRule,
    UnknownSegment(String, usize,  usize) // (Segs before, Word Pos in list, Segment Pos in Words)
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum RuleSyntaxError {
    OptMathError(Token, usize, usize),
    UnknownIPA(Token),
    UnknownGrouping(Token),
    UnknownCharacter(char, usize, usize),
    UnknownFeature(String, usize, usize, usize),
    UnknownVariable(Token),
    TooManyUnderlines(Token),
    UnexpectedEol(Token, char),
    ExpectedEndL(Token),
    ExpectedArrow(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ExpectedMatrix(Token),
    ExpectedSegment(Token),
    ExpectedFeature(Token),
    ExpectedVariable(Token),
    ExpectedUnderline(Token),
    ExpectedRightBracket(Token),
    BadSyllableMatrix(Token),
    BadVariableAssignment(Item),
    AlreadyInitialisedVariable(Item, Item, usize),
    InsertErr,
    DeleteErr,
    EmptyInput,
    EmptyOutput,
    EmptyEnv,
}

impl fmt::Display for RuleSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OptMathError(_, l, h)  => write!(f, "An Option's second argument '{}' must be greater than or equal to it's first argument '{}'", h, l),
            Self::UnknownIPA(token)             => write!(f, "Could not get value of IPA '{}'.", token.value),
            Self::UnknownGrouping(token)        => write!(f, "Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::UnknownFeature(feat, l, s, e) => write!(f, "Unknown feature '{} at {}:{}-{}'.", feat, l, s, e),
            Self::UnknownCharacter(c, l, pos)   => write!(f, "Unknown character {} at '{}:{}'.", c, l, pos),
            Self::UnknownVariable(token)        => write!(f, "Unknown variable '{}'", token.value),
            Self::TooManyUnderlines(_) => write!(f, "Cannot have multiple underlines in an environment"),
            Self::UnexpectedEol(_, c)           => write!(f, "Expected `{}`, but received End of Line", c),
            Self::ExpectedEndL(token)           => write!(f, "Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)          => write!(f, "Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)          => write!(f, "Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)          => write!(f, "Expected ':', but received '{}'", token.value),
            Self::ExpectedMatrix(token)         => write!(f, "Expected '[', but received '{}'", token.value),
            Self::ExpectedSegment(token)        => write!(f, "Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedFeature(token)        => write!(f, "{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedVariable(token)       => write!(f, "Expected number, but received {} ", token.value),
            Self::ExpectedUnderline(token)      => write!(f, "Expected '_', but received '{}'", token.value),
            Self::ExpectedRightBracket(token)   => write!(f, "Expected ')', but received '{}'", token.value),
            Self::BadSyllableMatrix(_)          => write!(f, "A syllable can only have parameters stress and tone"),
            Self::BadVariableAssignment(_)      => write!(f, "A variable can only be assigned to a primative (C, V, etc.) or a matrix"),
            Self::AlreadyInitialisedVariable(set_item, _, num) => write!(f, "Variable '{}' is already initialised as {}", num, set_item.kind),
            Self::InsertErr   => write!(f, "The input of an insertion rule must only contain `*` or `∅`"),
            Self::DeleteErr   => write!(f, "The output of a deletion rule must only contain `*` or `∅`"),
            Self::EmptyInput  => write!(f, "Input cannot be empty. Use `*` or '∅' to indicate insertion"),
            Self::EmptyOutput => write!(f, "Output cannot be empty. Use `*` or '∅' to indicate deletion"),
            Self::EmptyEnv    => write!(f, "Environment cannot be empty following a seperator."),
        }
    }
}