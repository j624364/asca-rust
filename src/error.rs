use std::fmt;

use crate ::{
    lexer ::Token, 
};

#[derive(Debug, Clone)]
pub enum Error {
    WordSyn(WordSyntaxError),
    RuleSyn(RuleSyntaxError),
    Runtime(RuntimeError),
}

impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Error::Runtime(e)
    }
}

impl From<WordSyntaxError> for Error {
    fn from(e: WordSyntaxError) -> Self {
        Error::WordSyn(e)
    }
}

impl From<RuleSyntaxError> for Error {
    fn from(e: RuleSyntaxError) -> Self {
        Error::RuleSyn(e)
    }
}

#[derive(Debug, Clone)]
pub enum WordSyntaxError {
    UnknownChar(String, usize),
    NoSegmentBeforeColon(String, usize),
    DiacriticBeforeSegment(String, usize),
    // CharAfterTone(String, usize),
    CouldNotParse(String),
}

impl fmt::Display for WordSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeError { 
    UnbalancedRule,
    UnknownSegment(String, usize,  usize), // (Segs before, Word Pos in list, Segment Pos in Words)
    UnknownVariable(Token),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
        // Self::UnknownVariable(token) => write!(f, "Unknown variable '{}' at {}", token.value, token.position.start),

    }
}

type LineNum = usize;
type StartPos = usize;
type EndPos = usize;
type Pos = usize;

#[derive(Debug, Clone)]
pub enum RuleSyntaxError {
    OptMathError(Token, LineNum, usize),
    UnknownIPA(Token),
    UnknownGrouping(Token),
    UnknownCharacter(char, LineNum, Pos),
    ExpectedCharColon(char, LineNum, Pos),
    ExpectedAlphabetic(char, LineNum, Pos),
    ExpectedCharArrow(char, LineNum, Pos),
    ExpectedCharDot(char, LineNum, Pos),
    ExpectedNumber(char, LineNum, Pos),
    OutsideBrackets(char, LineNum, Pos),
    UnknownFeature(String, LineNum, StartPos, EndPos),
    TooManyUnderlines(Token),
    UnexpectedEol(Token, char),
    ExpectedEndL(Token),
    ExpectedArrow(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ExpectedMatrix(Token),
    ExpectedSegment(Token),
    ExpectedTokenFeature(Token),
    ExpectedVariable(Token),
    ExpectedUnderline(Token),
    ExpectedRightBracket(Token),
    BadSyllableMatrix(Token),
    // BadVariableAssignment(Item),
    // AlreadyInitialisedVariable(Item, Item, usize),
    WrongModNode(LineNum, Pos),
    WrongModTone(LineNum, Pos),
    NestedBrackets(LineNum, Pos),
    InsertErr,
    DeleteErr,
    EmptyInput,
    EmptyOutput,
    EmptyEnv,
}

impl fmt::Display for RuleSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OptMathError(_, l, h)         => write!(f, "An Option's second argument '{h}' must be greater than or equal to it's first argument '{l}'"),
            Self::UnknownIPA(token)             => write!(f, "Could not get value of IPA '{}'.", token.value),
            Self::UnknownGrouping(token)        => write!(f, "Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::UnknownFeature(feat, l, s, e) => write!(f, "Unknown feature '{feat} at {l}:{s}-{e}'."),
            Self::ExpectedAlphabetic(c, l, pos) => write!(f, "Expected ASCII character, but received '{c}' at {l}:{pos}'."),
            Self::ExpectedCharColon(c, l, pos)  => write!(f, "Expected ':', but received '{c}' at {l}:{pos}"),
            Self::ExpectedCharArrow(c, l, pos)  => write!(f, "Expected '->', but received -'{c}' at {l}:{pos}"),
            Self::ExpectedCharDot(c, l, pos)    => write!(f, "Expected '..', but received .'{c}' at {l}:{pos}"),
            Self::ExpectedNumber(c, l, pos)     => write!(f, "Expected a number, but received '{c}' at {l}:{pos}"),
            Self::UnknownCharacter(c, l, pos)   => write!(f, "Unknown character {c} at '{l}:{pos}'."),
            Self::TooManyUnderlines(_)          => write!(f, "Cannot have multiple underlines in an environment"),
            Self::UnexpectedEol(_, c)           => write!(f, "Expected `{c}`, but received End of Line"),
            Self::ExpectedEndL(token)           => write!(f, "Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)          => write!(f, "Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)          => write!(f, "Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)          => write!(f, "Expected ':', but received '{}'", token.value),
            Self::ExpectedUnderline(token)      => write!(f, "Expected '_', but received '{}'", token.value),
            Self::ExpectedRightBracket(token)   => write!(f, "Expected ')', but received '{}'", token.value),
            Self::OutsideBrackets(..)           => write!(f, "Features must be inside square brackets"),
            Self::ExpectedMatrix(token)         => write!(f, "Expected '[', but received '{}'", token.value),
            Self::ExpectedSegment(token)        => write!(f, "Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedTokenFeature(token)   => write!(f, "{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedVariable(token)       => write!(f, "Expected number, but received {} ", token.value),
            Self::BadSyllableMatrix(_)          => write!(f, "A syllable can only have parameters stress and tone"),
            // Self::BadVariableAssignment(_)      => write!(f, "A variable can only be assigned to a primative (C, V, etc.) or a matrix"),
            // Self::AlreadyInitialisedVariable(set_item, _, num) => write!(f, "Variable '{}' is already initialised as {}", num, set_item.kind),
            Self::WrongModNode(..)              => write!(f, "Nodes cannot be ±; they can only be used in Alpha Notation expressions."),
            Self::WrongModTone(..)              => write!(f, "Tones cannot be ±; they can only be used with numeric values."),
            Self::NestedBrackets(..)            => write!(f, "Cannot have nested brackets of the same type"),
            Self::InsertErr                     => write!(f, "The input of an insertion rule must only contain `*` or `∅`"),
            Self::DeleteErr                     => write!(f, "The output of a deletion rule must only contain `*` or `∅`"),
            Self::EmptyInput                    => write!(f, "Input cannot be empty. Use `*` or '∅' to indicate insertion"),
            Self::EmptyOutput                   => write!(f, "Output cannot be empty. Use `*` or '∅' to indicate deletion"),
            Self::EmptyEnv                      => write!(f, "Environment cannot be empty following a seperator."),
        }
    }
}