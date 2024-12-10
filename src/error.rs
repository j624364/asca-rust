use colored::Colorize;
use crate  :: {
    lexer  :: {Position, Token, TokenKind}, 
    parser :: Item, RuleGroup,
};

pub trait ASCAError: Clone {
    fn get_error_message(&self) -> String;
    // This really isn't the correct solution
    fn format_word_error(&self, _: &[String]) -> String;
    fn format_rule_error(&self, _: &[RuleGroup]) -> String;

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

    fn format_word_error(&self, s: &[String]) -> String {
        match self {
            Error::WordSyn(e) => e.format_word_error(s),
            Error::WordRun(e) => e.format_word_error(s),
            Error::RuleSyn(_) | Error::RuleRun(_) => unreachable!()
        }
    }

    fn format_rule_error(&self, s: &[RuleGroup]) -> String {
        match self {
            Error::WordSyn(_) | Error::WordRun(_) => unreachable!(),
            Error::RuleSyn(e) => e.format_rule_error(s),
            Error::RuleRun(e) => e.format_rule_error(s),
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
    DiacriticDoesNotMeetPreReqsFeat(String, usize, String, bool),
    DiacriticDoesNotMeetPreReqsNode(String, usize, String, bool),
    CouldNotParse(String),
    CouldNotParseEjective(String)
}

impl ASCAError for WordSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            WordSyntaxError::UnknownChar(_, _)                      => "Unknown Char".to_string(),
            WordSyntaxError::NoSegmentBeforeColon(_, _)             => "No Segment Before Colon".to_string(),
            WordSyntaxError::DiacriticBeforeSegment(_, _)           => "Diacritic Before Segment".to_string(),
            WordSyntaxError::CouldNotParse(_)                       => "Unable to parse word".to_string(),
            WordSyntaxError::CouldNotParseEjective(_)               => "Unable to parse word. If you meant to have an ejective, you must use ʼ".to_string(),
            WordSyntaxError::DiacriticDoesNotMeetPreReqsFeat(txt, i, t, pos) |
            WordSyntaxError::DiacriticDoesNotMeetPreReqsNode(txt, i, t, pos) => {
                format!("Segment does not have prerequisite properties to have diacritic `{}`. Must be [{} {}]", txt.chars().nth(*i).unwrap_or_default(), if *pos { '+' } else { '-' },t)
            },
        }
    }

    fn format_word_error(&self, _: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Word Syntax Error:".bright_red().bold(), self.get_error_message().bold());
        let (arrows, text) = match self {
            Self::CouldNotParse(text) => (
                "^".repeat(text.chars().count()) + "\n", 
                text
            ),
            Self::CouldNotParseEjective(text) => (
                " ".repeat(text.chars().count() - 1) + "^\n",
                text
            ),
            Self::UnknownChar(text, i)            |
            Self::NoSegmentBeforeColon(text, i)   |
            Self::DiacriticBeforeSegment(text, i) |
            Self::DiacriticDoesNotMeetPreReqsFeat(text, i, ..) |
            Self::DiacriticDoesNotMeetPreReqsNode(text, i, ..) => (
                " ".repeat(*i) + "^" + "\n", 
                text
            ),
        };
        result.push_str(&format!("{}{}{}{}",  
            MARG.bright_cyan().bold(), 
            text, 
            MARG.bright_cyan().bold(), 
            arrows.bright_red().bold()
        ));

        result
    }

    fn format_rule_error(&self, _: &[RuleGroup]) -> String {
        unreachable!()
    }


}

#[derive(Debug, Clone)]
pub enum WordRuntimeError {
    UnknownSegment(String, usize,  usize), // (Segs before, Word Pos in list, Segment Pos in Words)
}

impl ASCAError for WordRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::UnknownSegment(buf, ..) => format!("Unknown Segment `{}`", buf)
        }
    }

    fn format_word_error(&self, words: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Runtime Error:".bright_red().bold(), self.get_error_message().bold());

        match self {
            Self::UnknownSegment(buffer, word, seg) => {
                let arrows = " ".repeat(words[*word].len() + seg) + "^" + "\n";
                result.push_str(&format!("{}{} => {}{}{}",  
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

    fn format_rule_error(&self, _: &[RuleGroup]) -> String {
        unreachable!()
    }
}

#[derive(Debug, Clone)]
pub enum RuleRuntimeError { 
    DeletionOnlySeg,
    DeletionOnlySyll,
    LonelySet(Position),
    UnevenSet(Position, Position),
    UnknownVariable(Token),
    InsertionNoContextOrException(Position),
    InsertionMatrix(Position),
    AlphaUnknown(Position),
    AlphaUnknownInv(Position),
    AlphaNodeAssignInv(Position),
    AlphaIsNotSameNode(Position),
    AlphaIsNotNode(Position),
    NodeCannotBeSome(String, Position),
    NodeCannotBeNone(String, Position),
    NodeCannotBeSet(String, Position),
    MetathSyllSegment(Position, Position),
    MetathSyllBoundary(Position, Position),
    SubstitutionSyll(Position),
    SubstitutionSyllBound(Position, Position),
    SubstitutionSylltoMatrix(Position, Position),
    SubstitutionSylltoBound(Position, Position),
    SubstitutionSegtoSyll(Position, Position),
    SubstitutionBoundMod(Position, Position),
    WordBoundSetLocError(Position),
}

impl ASCAError for RuleRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::LonelySet(_)                     => "A Set in output must have a matching Set in input".to_string(),
            Self::UnevenSet(..)                    => "Two matched sets must have the same number of elements".to_string(),
            Self::DeletionOnlySeg                  => "Can't delete a word's only segment".to_string(),
            Self::DeletionOnlySyll                 => "Can't delete a word's only syllable".to_string(),
            Self::UnknownVariable(token)           => format!("Unknown variable '{}' at {}", token.value, token.position.start),
            Self::InsertionNoContextOrException(_) => "Insertion rules must have a context".to_string(),
            Self::InsertionMatrix(_)               => "An incomplete matrix cannot be inserted".to_string(),
            Self::AlphaUnknown(_)                  => "Alpha has not be assigned before applying".to_string(),
            Self::AlphaUnknownInv(_)               => "First occurence of a node alpha must not be inverted.".to_string(),
            Self::AlphaNodeAssignInv(_)            => "Node alphas cannot be assigned inverse. First occurrence of a node alpha must be positive.".to_string(),
            Self::AlphaIsNotSameNode(_)            => "Node alphas must only be used on the same node.".to_string(),
            Self::AlphaIsNotNode(_)                => "Node alphas cannot be used on binary features".to_string(),
            Self::NodeCannotBeSome(node, _)        => format!("{} node cannot arbitrarily positive", node),
            Self::NodeCannotBeNone(node, _)        => format!("{} node cannot be removed", node),
            Self::NodeCannotBeSet(node, _)         => format!("{} node cannot be assigned using PLACE alpha", node),
            Self::MetathSyllSegment(..)            => "Cannot swap a syllable with a segment".to_string(),
            Self::MetathSyllBoundary(..)           => "Cannot swap a syllable with a syllable".to_string(),
            Self::SubstitutionSyll(_)              => "Syllables cannot be in substitution output. If you wish to modify a syllable, use a matrix.".to_string(),
            Self::SubstitutionSyllBound(..)        => "Syllable boundaries cannot be substituted.".to_string(),
            Self::SubstitutionSylltoMatrix(..)     => "Syllables and boundaries cannot be substituted by a segment".to_string(),
            Self::SubstitutionSylltoBound(..)      => "Syllables cannot be substituted by a boundary".to_string(),
            Self::SubstitutionSegtoSyll(..)        => "Segments cannot be substituted by a syllable or a boundary".to_string(),
            Self::SubstitutionBoundMod(..)         => "Syllable boundaries cannot be modified by a matrix.".to_string(),
            Self::WordBoundSetLocError(_)          => "Word Boundaries cannot be in the input or output".to_string(),
        }
    }

    fn format_rule_error(&self, rules: &[RuleGroup]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Runtime Error:".bright_red().bold(), self.get_error_message().bold());
        
        let (arrows, line) =  match self {
            Self::DeletionOnlySyll | Self::DeletionOnlySeg => return result,
            Self::UnknownVariable(t) => (
                " ".repeat(t.position.start) + &"^".repeat(t.position.end-t.position.start) + "\n", 
                t.position.line
            ),
            Self::InsertionNoContextOrException(pos) => (
                " ".repeat(pos.end) + "^" + "\n", 
                pos.line
            ),
            Self::WordBoundSetLocError(pos) |
            Self::SubstitutionSyll(pos)     |
            Self::AlphaUnknown(pos)         |
            Self::AlphaUnknownInv(pos)      |
            Self::AlphaNodeAssignInv(pos)   |
            Self::LonelySet(pos)            | 
            Self::NodeCannotBeSome(_, pos)  |
            Self::NodeCannotBeNone(_, pos)  |
            Self::NodeCannotBeSet(_, pos)   |
            Self::AlphaIsNotSameNode(pos)   |
            Self::AlphaIsNotNode(pos)       |
            Self::InsertionMatrix(pos)  =>  (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                pos.line
            ),
            Self::UnevenSet (a, b) |
            Self::MetathSyllSegment (a, b) |
            Self::MetathSyllBoundary(a, b) |
            Self::SubstitutionSegtoSyll (a, b) |
            Self::SubstitutionSyllBound (a, b) |
            Self::SubstitutionSylltoBound (a, b) |
            Self::SubstitutionSylltoMatrix(a, b) |
            Self::SubstitutionBoundMod(a, b) => (
                   " ".repeat(a.start) + &"^".repeat(a.end - a.start) 
                + &" ".repeat(b.start) + &"^".repeat(b.end - b.start) + "\n",
                a.line
            ),
        };

        result.push_str(&format!("{}{}{}{}",  
            MARG.bright_cyan().bold(), 
            rules[line].name, // TODO: FIX
            MARG.bright_cyan().bold(), 
            arrows.bright_red().bold()
        ));

        result
    }

    fn format_word_error(&self, _: &[String]) -> String {
        unreachable!()
    }
}

type LineNum = usize;
type Pos = usize;

#[derive(Debug, Clone)]
pub enum RuleSyntaxError {
    OptLocError(Position),
    OptMathError(Token, LineNum, usize),
    UnknownIPA(Token),
    UnknownGrouping(Token),
    UnknownCharacter(char, LineNum, Pos),
    ExpectedCharColon(char, LineNum, Pos),
    ExpectedAlphabetic(char, LineNum, Pos),
    ExpectedCharArrow(char, LineNum, Pos),
    ExpectedCharDot(char, LineNum, Pos),
    ExpectedNumber(char, LineNum, Pos),
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
    WrongModTone(LineNum, Pos),
    OutsideBrackets(LineNum, Pos),
    NestedBrackets(LineNum, Pos),
    InsertErr(Token),
    DeleteErr(Token),
    MetathErr(Token),
    EmptyInput(LineNum, Pos),
    EmptyOutput(LineNum, Pos),
    EmptyEnv(LineNum, Pos),
    EmptySet(Position),
    InsertMetath(LineNum, Pos, Pos),
    InsertDelete(LineNum, Pos, Pos),
    StuffAfterWordBound(Position),
    StuffBeforeWordBound(Position),
    TooManyWordBoundaries(Position),
    UnknownFeature(String, Position),
    UnknownEnbyFeature(String, Position),
    UnbalancedRuleIO(Vec<Vec<Item>>),
    UnbalancedRuleEnv(Vec<Item>),
    DiacriticDoesNotMeetPreReqsFeat(Position, Position, String, bool),
    DiacriticDoesNotMeetPreReqsNode(Position, Position, String, bool),
    UnexpectedDiacritic(Position, Position),
    WordBoundLoc(Position),
}

impl ASCAError for RuleSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            Self::OptLocError(_)                  => "Optionals can only be used in environments".to_string(),
            Self::OptMathError(_, low, high)      => format!("An Option's second argument '{high}' must be greater than or equal to it's first argument '{low}'"),
            Self::UnknownIPA(token)               => format!("Could not get value of IPA '{}'.", token.value),
            Self::UnknownGrouping(token)          => format!("Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (P)losive, (F)ricative, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::UnknownFeature(feat, pos)       => format!("Unknown feature '{feat}' at {}:{}-{}'. Did you mean {}? ", pos.line, pos.start, pos.end, get_feat_closest(feat)),
            Self::UnknownEnbyFeature(feat, ..)    => format!("Feature '{feat}' has no modifier."),
            Self::ExpectedAlphabetic(c, l, pos)   => format!("Expected ASCII character, but received '{c}' at {l}:{pos}'."),
            Self::ExpectedCharColon(c, l, pos)    => format!("Expected ':', but received '{c}' at {l}:{pos}"),
            Self::ExpectedCharArrow(c, l, pos)    => format!("Expected '->', but received -'{c}' at {l}:{pos}"),
            Self::ExpectedCharDot(c, l, pos)      => format!("Expected '..', but received .'{c}' at {l}:{pos}"),
            Self::ExpectedNumber(c, l, pos)       => format!("Expected a number, but received '{c}' at {l}:{pos}"),
            Self::UnknownCharacter(c, l, pos)     => format!("Unknown character {c} at '{l}:{pos}'."),
            Self::TooManyUnderlines(_)            => "Cannot have multiple underlines in an environment".to_string(),
            Self::StuffAfterWordBound(_)          => "Can't have segments after the end of a word".to_string(),
            Self::StuffBeforeWordBound(_)         => "Can't have segments before the beginning of a word".to_string(),
            Self::TooManyWordBoundaries(_)        => "Cannot have multiple word boundaries on each side of an environment".to_string(),
            Self::UnexpectedEol(_, c)             => format!("Expected `{c}`, but received End of Line"),
            Self::ExpectedEndL(token)             => format!("Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)            => format!("Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)            => format!("Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)            => format!("Expected ':', but received '{}'", token.value),
            Self::ExpectedUnderline(token)        => format!("Expected '_', but received '{}'", token.value),
            Self::ExpectedRightBracket(token)     => format!("Expected ')', but received '{}'", token.value),
            Self::ExpectedMatrix(token)           => format!("Expected '[', but received '{}'", if token.kind == TokenKind::Eol {"End Of Line"} else {&token.value}),
            Self::ExpectedSegment(token)          => format!("Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedTokenFeature(token)     => format!("{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedVariable(token)         => format!("Expected number, but received {} ", token.value),
            Self::BadSyllableMatrix(_)            => "A syllable can only have parameters stress and tone".to_string(),
            Self::WrongModTone(..)                => "Tones cannot be ±; they can only be used with numeric values.".to_string(),
            Self::NestedBrackets(..)              => "Cannot have nested brackets of the same type".to_string(),
            Self::OutsideBrackets(..)             => "Features must be inside square brackets".to_string(),
            Self::InsertErr(_)                    => "The input of an insertion rule must only contain `*` or `∅`".to_string(),
            Self::DeleteErr(_)                    => "The output of a deletion rule must only contain `*` or `∅`".to_string(),
            Self::MetathErr(_)                    => "The output of a methathis rule must only contain `&`".to_string(),
            Self::EmptyInput(..)                  => "Input cannot be empty. Use `*` or '∅' to indicate insertion".to_string(),
            Self::EmptyOutput(..)                 => "Output cannot be empty. Use `*` or '∅' to indicate deletion".to_string(),
            Self::EmptyEnv(..)                    => "Environment cannot be empty following a seperator.".to_string(),
            Self::EmptySet(..)                    => "Sets cannot be empty".to_string(),
            Self::InsertMetath(..)                => "A rule cannot be both an Insertion rule and a Metathesis rule".to_string(),
            Self::InsertDelete(..)                => "A rule cannot be both an Insertion rule and a Deletion rule".to_string(),
            Self::UnbalancedRuleIO(_)             => "Input or Output has too few elements".to_string(),
            Self::UnbalancedRuleEnv(_)            => "Environment has too few elements".to_string(),
            Self::DiacriticDoesNotMeetPreReqsFeat(.., t , pos) |
            Self::DiacriticDoesNotMeetPreReqsNode(.., t , pos) => {
                format!("Segment does not have prerequisite properties to have this diacritic. Must be {}{}", if *pos { '+' } else { '-' },t) 
            },
            Self::UnexpectedDiacritic(..) => "Diacritics can only modify IPA Segments".to_string(),
            Self::WordBoundLoc(_)     => "Wordboundaries are not allowed in the input or output".to_string(),
        }
    }

    fn format_rule_error(&self, rules: &[RuleGroup]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.get_error_message().bold()); 

        let (arrows, line) = match self {
            Self::OptMathError(t, _, _)   | 
            Self::UnknownGrouping(t)      | 
            Self::TooManyUnderlines(t)    | 
            Self::UnexpectedEol(t, _)     | 
            Self::ExpectedEndL(t)         | 
            Self::ExpectedArrow(t)        | 
            Self::ExpectedComma(t)        | 
            Self::ExpectedColon(t)        | 
            Self::ExpectedMatrix(t)       | 
            Self::ExpectedSegment(t)      | 
            Self::ExpectedTokenFeature(t) | 
            Self::ExpectedVariable(t)     | 
            Self::ExpectedUnderline(t)    | 
            Self::ExpectedRightBracket(t) |
            Self::UnknownIPA(t)           | 
            Self::InsertErr(t)            | 
            Self::DeleteErr(t)            |
            Self::MetathErr(t)            |
            Self::BadSyllableMatrix(t)  => (
                " ".repeat(t.position.start) + &"^".repeat(t.position.end-t.position.start) + "\n", 
                t.position.line
            ),
            Self::UnknownFeature(_, pos) | Self::UnknownEnbyFeature(_, pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n", 
                pos.line
            ),
            Self::UnknownCharacter  (_, line, pos) |
            Self::ExpectedCharColon (_, line, pos) |
            Self::ExpectedAlphabetic(_, line, pos) |
            Self::ExpectedCharArrow (_, line, pos) |
            Self::ExpectedCharDot   (_, line, pos) |
            Self::ExpectedNumber    (_, line, pos) => (
                " ".repeat(*pos) + "^" + "\n", 
                *line
            ),
            Self::WrongModTone   (line, pos) |
            Self::EmptyInput     (line, pos) | 
            Self::EmptyEnv       (line, pos) |
            Self::EmptyOutput    (line, pos) |
            Self::NestedBrackets (line, pos) | 
            Self::OutsideBrackets(line, pos) => (
                " ".repeat(*pos) + "^" + "\n", 
                *line
            ),
            Self::InsertDelete(line, pos1, pos2) | 
            Self::InsertMetath(line, pos1, pos2) => (
                " ".repeat(*pos1) + "^" + " ".repeat(pos2 - pos1 - 1).as_str() + "^" + "\n", 
                *line
            ),
            Self::TooManyWordBoundaries(pos) |
            Self::StuffBeforeWordBound(pos)  | 
            Self::StuffAfterWordBound(pos) => (
                " ".repeat(pos.start) + "^" + "\n", 
                pos.line
            ),
            Self::UnbalancedRuleEnv(items) => {
                let first_item = items.first().expect("Env should not be empty");
                let last_item = items.last().expect("Env should not be empty");
                let start = first_item.position.start;
                let end = last_item.position.end;
                (
                    " ".repeat(start) + &"^".repeat(end-start) + "\n", 
                    first_item.position.line
                )
            },
            Self::WordBoundLoc(pos) |
            Self::OptLocError(pos)  |
            Self::EmptySet(pos)  => {
                (
                    " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                    pos.line
                )
            },
            Self::UnbalancedRuleIO(items) => {
                let first_item = items.first().expect("IO should not be empty").first().expect("IO should not be empty");
                let last_item = items.last().expect("IO should not be empty").last().expect("IO should not be empty");
                let start = first_item.position.start;
                let end = last_item.position.end;
                (
                    " ".repeat(start) + &"^".repeat(end-start) + "\n", 
                    first_item.position.line
                )
            },
            Self::UnexpectedDiacritic(elm_pos, dia_pos) | 
            Self::DiacriticDoesNotMeetPreReqsFeat(elm_pos, dia_pos, ..) | 
            Self::DiacriticDoesNotMeetPreReqsNode(elm_pos, dia_pos, ..) => (
                " ".repeat(elm_pos.start) 
                    + &"^".repeat(elm_pos.end - elm_pos.start)
                    + &" ".repeat(dia_pos.start - elm_pos.end)
                    + &"^".repeat(dia_pos.end - dia_pos.start)
                    + "\n", 
                elm_pos.line
            ),
        };

        result.push_str(&format!("{}{}{}{}",  
            MARG.bright_cyan().bold(), 
            rules[line].name, // TODO: fix 
            MARG.bright_cyan().bold(), 
            arrows.bright_red().bold()
        ));

        result
    }

    fn format_word_error(&self, _: &[String]) -> String {
        unreachable!()
    }
}


const FEAT_VARIANTS: [&str; 171] = [ 
    "root", "rut", "rt", 
    "consonantal", "consonant", "cons" , "cns",          
    "sonorant", "sonor", "son" , "snrt", "sn",
    "syllabic", "syllab", "syll" , "syl",          
    // Manner Node Features
    "manner", "mann", "man", "mnnr" , "mnr" ,
    "continuant", "contin", "cont" , "cnt",
    "approximant", "approx", "appr", "app",
    "lateral", "latrl", "ltrl", "lat",
    "nasal", "nsl", "nas",
    "delayedrelease", "delrel", "d.r.", "del.rel.", "delayed", "dl", "dlrl", "dr", "delay", "del.rel", "drel",
    "strident", "strid", "stri", "stridnt",
    "rhotic", "rhot", "rho", "rhtc", "rh",
    "click", "clik", "clk", "clck",
    // Laryngeal Node Features
    "laryngeal", "laryng", "laryn", "lar",
    "voice", "voi", "vce", "vc",
    "spreadglottis", "spreadglot", "spread", "s.g.", "s.g", "sg",
    "constrictedglottis", "constricted", "constglot", "constr", "c.g.", "c.g", "cg",
    // Place Node Feature
    "place", "plce", "plc",   
    // Labial Place Node Features
    "labial", "lbl", "lab",
    "labiodental", "ldental", "labiodent", "labdent", "lbdntl", "ldent", "ldl",
    "round", "rund", "rnd", "rd",
    // Coronal Place Node Features
    "coronal", "coron", "crnl", "cor",
    "anterior", "anter", "antr", "ant",
    "distributed", "distrib", "dist", "dis" , "dst",
    // Dorsal Place Node Features
    "dorsal", "drsl", "dors", "dor",
    "front", "frnt", "fnt", "fro", "frt", "fr",
    "back", "bck", "bk",
    "high", "hgh", "hi",
    "low", "lw", "lo",
    "tense", "tens", "tns", "ten",
    "reduced", "reduc", "redu", "rdcd", "red",
    // Pharyngeal Place Node Features
    "pharyngeal", "pharyng", "pharyn", "phar", "phr",
    "advancedtongueroot", "a.t.r.", "a.t.r", "a.tr", "at.r", "atr",
    "retractedtongueroot", "r.t.r.", "r.t.r", "r.tr", "rt.r", "rtr",
    // Suprasegmental Features
    "long", "lng",
    "overlong", "overlng", "ovrlng", "vlng",
    "stress", "str",
    "secondarystress", "sec.stress", "secstress", "sec.str.", "sec.str", "secstr", "sec"
];

fn get_feat_closest(s: &str) -> &'static str {
    let mut best_lev = usize::MAX;
    let mut best_str= "";

    for var in FEAT_VARIANTS {
        let len = lev(s, var);
        if len < best_lev {
            best_str = var;
            best_lev = len;
        }
    }

    best_str
} 

fn lev(a: &str, b: &str) -> usize {
    let mut dist = 0;

    if a == b { return dist }

    let a_len = a.chars().count();
    let b_len = b.chars().count();

    debug_assert!(a_len > 0);
    debug_assert!(b_len > 0);

    let mut cache: Vec<usize> = (1..).take(a_len).collect();

    for (bi, b_ch) in b.chars().enumerate() {
        dist = bi;
        let mut a_dist = bi;

        for (ai, a_ch) in a.chars().enumerate() {
            let b_dist = a_dist + (a_ch != b_ch) as usize;

            a_dist = cache[ai];

            dist = if a_dist > dist {
                if b_dist > dist {
                    dist + 1
                } else {
                    b_dist
                }
            } else if b_dist > a_dist {
                a_dist + 1
            } else {
                b_dist
            };

            cache[ai] = dist;
        }
    }

    dist
}