use std::fmt;
use colored::Colorize;
use crate :: {
    lexer :: Token, 
    Position
};

pub trait ASCAError: Clone {
    fn get_error_message(&self) -> String;
    fn format_error(&self, _: &[String]) -> String;

}

#[derive(Debug, Clone)]
pub enum Error {
    WordSyn(WordSyntaxError),
    RuleSyn(RuleSyntaxError),
    WordRun(WordRuntimeError),
    RuleRun(RuleRuntimeError),
}

impl ASCAError for Error {
    fn get_error_message(&self) -> String {
        match self {
            Error::WordSyn(e) => e.get_error_message(),
            Error::RuleSyn(e) => e.get_error_message(),
            Error::WordRun(e) => e.get_error_message(),
            Error::RuleRun(e) => e.get_error_message(),
        }
    }

    fn format_error(&self, s: &[String]) -> String {
        match self {
            Error::WordSyn(e) => e.format_error(s),
            Error::RuleSyn(e) => e.format_error(s),
            Error::WordRun(e) => e.format_error(s),
            Error::RuleRun(e) => e.format_error(s),
        }
    }
}

impl From<WordRuntimeError> for Error {
    fn from(e: WordRuntimeError) -> Self {
        Error::WordRun(e)
    }
}

impl From<RuleRuntimeError> for Error {
    fn from(e: RuleRuntimeError) -> Self {
        Error::RuleRun(e)
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
    DiacriticDoesNotMeetPreReqs(String, usize),
    // CharAfterTone(String, usize),
    CouldNotParse(String),
}

impl ASCAError for WordSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            WordSyntaxError::UnknownChar(_, _)                 => "Unknown Char".to_owned(),
            WordSyntaxError::NoSegmentBeforeColon(_, _)        => "NoSegmentBeforeColon".to_owned(),
            WordSyntaxError::DiacriticBeforeSegment(_, _)      => "DiacriticBeforeSegment".to_owned(),
            WordSyntaxError::DiacriticDoesNotMeetPreReqs(_, _) => "DiacriticDoesNotMeetPreReqs".to_owned(),
            WordSyntaxError::CouldNotParse(_)                  => "CouldNotParse".to_owned(),
        }
    }

    fn format_error(&self, _rules: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Word Syntax Error".bright_red().bold(), self.get_error_message().bold());
        match self {
            Self::UnknownChar(s, u) => {
                let arrows = " ".repeat(*u) + "^" + "\n";
                result.push_str(&format!("{}{}{}{}",  
                    MARG.bright_cyan().bold(), 
                    s, 
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ));
            },
            WordSyntaxError::NoSegmentBeforeColon(_, _) => todo!(),
            WordSyntaxError::DiacriticBeforeSegment(_, _) => todo!(),
            WordSyntaxError::DiacriticDoesNotMeetPreReqs(_, _) => todo!(),
            WordSyntaxError::CouldNotParse(_) => todo!(),
        }        
        result
    }
}

#[derive(Debug, Clone)]
pub enum WordRuntimeError {
    UnknownSegment(String, usize,  usize), // (Segs before, Word Pos in list, Segment Pos in Words)
}

impl ASCAError for WordRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::UnknownSegment(..) => todo!()
        }
    }

    fn format_error(&self, words: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Runtime Error".bright_red().bold(), self.get_error_message().bold());

        match self {
            Self::UnknownSegment(buffer, word, seg) => {
                let arrows = " ".repeat(words[*word].len() + seg) + "^" + "\n";
                result.push_str(&format!("{}{}{}{} => {}{}{}",  
                    "Runtime Error".bright_red().bold(),
                    self.get_error_message().bold(), 
                    MARG.bright_cyan().bold(), 
                    words[*word], 
                    buffer,
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ));
            },
        }

        result
    }
}


#[derive(Debug, Clone)]
pub enum RuleRuntimeError { 
    UnbalancedRule,
    DeletionOnlySeg,
    DeletionOnlySyll,
    LonelySet(Position),
    UnknownVariable(Token),
}

impl ASCAError for RuleRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::LonelySet(_) => "A Set in output must have a matching Set in input".to_string(),
            Self::UnbalancedRule => todo!(),
            Self::DeletionOnlySeg => "Can't delete a word's only segment".to_string(),
            Self::DeletionOnlySyll => "Can't delete a word's only syllable".to_string(),
            Self::UnknownVariable(token) => format!("Unknown variable '{}' at {}", token.value, token.position.start),
        }
    }

    fn format_error(&self, rules: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Runtime Error".bright_red().bold(), self.get_error_message().bold());
        
        match self {
            Self::DeletionOnlySyll | Self::DeletionOnlySeg => {},
            Self::UnbalancedRule => todo!(),
            Self::UnknownVariable(t) => {
                let line = t.position.line;
                let start = t.position.start;
                let end = t.position.end;

                let arrows = " ".repeat(start) + &"^".repeat(end-start) + "\n";
                result.push_str(&format!("{}{}{}{}",  
                    MARG.bright_cyan().bold(), 
                    rules[line], 
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ));
            },
            Self::LonelySet(pos) => {
                let arrows = " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n";
                result.push_str(&format!("{}{}{}{}",  
                    MARG.bright_cyan().bold(), 
                    rules[pos.line], 
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ));
            }
        }
        result
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
    EmptyInput(LineNum, Pos),
    EmptyOutput(LineNum, Pos),
    EmptyEnv(LineNum, Pos),
}

impl ASCAError for RuleSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            Self::OptMathError(_, low, high)         => format!("An Option's second argument '{high}' must be greater than or equal to it's first argument '{low}'"),
            Self::UnknownIPA(token)             => format!("Could not get value of IPA '{}'.", token.value),
            Self::UnknownGrouping(token)        => format!("Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::UnknownFeature(feat, l, s, e) => format!("Unknown feature '{feat} at {l}:{s}-{e}'."),
            Self::ExpectedAlphabetic(c, l, pos) => format!("Expected ASCII character, but received '{c}' at {l}:{pos}'."),
            Self::ExpectedCharColon(c, l, pos)  => format!("Expected ':', but received '{c}' at {l}:{pos}"),
            Self::ExpectedCharArrow(c, l, pos)  => format!("Expected '->', but received -'{c}' at {l}:{pos}"),
            Self::ExpectedCharDot(c, l, pos)    => format!("Expected '..', but received .'{c}' at {l}:{pos}"),
            Self::ExpectedNumber(c, l, pos)     => format!("Expected a number, but received '{c}' at {l}:{pos}"),
            Self::UnknownCharacter(c, l, pos)   => format!("Unknown character {c} at '{l}:{pos}'."),
            Self::TooManyUnderlines(_)          => "Cannot have multiple underlines in an environment".to_string(),
            Self::UnexpectedEol(_, c)           => format!("Expected `{c}`, but received End of Line"),
            Self::ExpectedEndL(token)           => format!("Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)          => format!("Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)          => format!("Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)          => format!("Expected ':', but received '{}'", token.value),
            Self::ExpectedUnderline(token)      => format!("Expected '_', but received '{}'", token.value),
            Self::ExpectedRightBracket(token)   => format!("Expected ')', but received '{}'", token.value),
            Self::OutsideBrackets(..)           => "Features must be inside square brackets".to_string(),
            Self::ExpectedMatrix(token)         => format!("Expected '[', but received '{}'", if token.kind == crate::TokenKind::Eol {"End Of Line"} else {&token.value}),
            Self::ExpectedSegment(token)        => format!("Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedTokenFeature(token)   => format!("{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedVariable(token)       => format!("Expected number, but received {} ", token.value),
            Self::BadSyllableMatrix(_)          => "A syllable can only have parameters stress and tone".to_string(),
            // Self::BadVariableAssignment(_)      => format!("A variable can only be assigned to a primative (C, V, etc.) or a matrix"),
            // Self::AlreadyInitialisedVariable(set_item, _, num) => format!("Variable '{}' is already initialised as {}", num, set_item.kind),
            Self::WrongModNode(..)              => "Nodes cannot be ±; they can only be used in Alpha Notation expressions.".to_string(),
            Self::WrongModTone(..)              => "Tones cannot be ±; they can only be used with numeric values.".to_string(),
            Self::NestedBrackets(..)            => "Cannot have nested brackets of the same type".to_string(),
            Self::InsertErr                     => "The input of an insertion rule must only contain `*` or `∅`".to_string(),
            Self::DeleteErr                     => "The output of a deletion rule must only contain `*` or `∅`".to_string(),
            Self::EmptyInput(..)                => "Input cannot be empty. Use `*` or '∅' to indicate insertion".to_string(),
            Self::EmptyOutput(..)               => "Output cannot be empty. Use `*` or '∅' to indicate deletion".to_string(),
            Self::EmptyEnv(..)                  => "Environment cannot be empty following a seperator.".to_string(),
        }
    }

    fn format_error(&self, rules: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error".bright_red().bold(), self.get_error_message().bold()); 

        match self {
            Self::OptMathError(t, _, _) | 
            Self::UnknownIPA(t) | 
            Self::UnknownGrouping(t) | 
            Self::TooManyUnderlines(t) | 
            Self::UnexpectedEol(t, _) | 
            Self::ExpectedEndL(t) | 
            Self::ExpectedArrow(t) | 
            Self::ExpectedComma(t) | 
            Self::ExpectedColon(t) | 
            Self::ExpectedMatrix(t) | 
            Self::ExpectedSegment(t) | 
            Self::ExpectedTokenFeature(t) | 
            Self::ExpectedVariable(t) | 
            Self::ExpectedUnderline(t) | 
            Self::ExpectedRightBracket(t) |
            Self::BadSyllableMatrix(t)  => {
                let line = t.position.line;
                let start = t.position.start;
                let end = t.position.end;

                let arrows = " ".repeat(start) + &"^".repeat(end-start) + "\n";

                result.push_str(&format!("{}{}{}{}",  
                    MARG.bright_cyan().bold(), 
                    rules[line], 
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ));
            },
            Self::UnknownFeature(_, line, start, end) => {
                let arrows = " ".repeat(*start) + &"^".repeat(end-start) + "\n";

                result.push_str(&format!("{}{}{}{}",  
                    MARG.bright_cyan().bold(), 
                    rules[*line], 
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ));

            },
            // AlreadyInitialisedVariable(_, _, _) |   
            Self::InsertErr | Self::DeleteErr  => todo!(),

            Self::UnknownCharacter  (_, line, pos) |
            Self::ExpectedCharColon (_, line, pos) |
            Self::ExpectedAlphabetic(_, line, pos) |
            Self::ExpectedCharArrow (_, line, pos) |
            Self::ExpectedCharDot   (_, line, pos) |
            Self::ExpectedNumber    (_, line, pos) |
            Self::OutsideBrackets   (_, line, pos) => {
                let arrows = " ".repeat(*pos) + "^" + "\n";

                result.push_str(&format!("{}{}{}{}", 
                    MARG.bright_cyan().bold(), 
                    rules[*line], 
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ))
            },
            Self::WrongModNode  (line, pos) |
            Self::WrongModTone  (line, pos) |
            Self::NestedBrackets(line, pos) | 
            Self::EmptyInput    (line, pos) | 
            Self::EmptyEnv      (line, pos) |
            Self::EmptyOutput   (line, pos) => {
                let arrows = " ".repeat(*pos) + "^" + "\n";

                result.push_str(&format!("{}{}{}{}",
                    MARG.bright_cyan().bold(), 
                    rules[*line], 
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ))
            },
        }

        result
    }
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
            Self::EmptyInput(..)                => write!(f, "Input cannot be empty. Use `*` or '∅' to indicate insertion"),
            Self::EmptyOutput(..)               => write!(f, "Output cannot be empty. Use `*` or '∅' to indicate deletion"),
            Self::EmptyEnv(..)                  => write!(f, "Environment cannot be empty following a seperator."),
        }
    }
}