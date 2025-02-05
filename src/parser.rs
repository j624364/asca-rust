use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{
    error::*,
    lexer::*,
    rule::{Alpha, Rule},
    seg::Segment,
    syll::Tone,
    CARDINALS_MAP, DIACRITS,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinMod {
    Positive,
    Negative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AlphaMod {
    Alpha(char),
    InvAlpha(char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ModKind {
    Binary(BinMod),
    Alpha(AlphaMod),
}

impl ModKind {
    pub(crate) fn as_bool(
        &self,
        alphas: &RefCell<HashMap<char, Alpha>>,
        err_pos: Position,
    ) -> Result<bool, RuleRuntimeError> {
        match self {
            ModKind::Binary(bin_mod) => Ok(*bin_mod == BinMod::Positive),
            ModKind::Alpha(alpha_mod) => match alpha_mod {
                AlphaMod::Alpha(ch) => {
                    if let Some(alpha) = alphas.borrow().get(ch) {
                        Ok(alpha.as_binary())
                    } else {
                        Err(RuleRuntimeError::AlphaUnknown(err_pos))
                    }
                }
                AlphaMod::InvAlpha(ch) => {
                    if let Some(alpha) = alphas.borrow().get(ch) {
                        Ok(!alpha.as_binary())
                    } else {
                        Err(RuleRuntimeError::AlphaUnknown(err_pos))
                    }
                }
            },
        }
    }

    pub(crate) fn as_bin_mod(&self) -> Option<&BinMod> {
        if let Self::Binary(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Mods {
    Binary(BinMod),
    Number(Tone),
    Alpha(AlphaMod),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SupraSegs {
    pub(crate) stress: [Option<ModKind>; 2], // [Stress, SecStress]
    pub(crate) length: [Option<ModKind>; 2], // [Long, Overlong]
    pub(crate) tone: Option<Tone>,
}

impl SupraSegs {
    pub(crate) fn new() -> Self {
        Self {
            stress: [None, None],
            length: [None, None],
            tone: None,
        }
    }

    pub(crate) fn from(
        stress: [Option<ModKind>; 2],
        length: [Option<ModKind>; 2],
        tone: Option<Tone>,
    ) -> Self {
        Self {
            stress,
            length,
            tone,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Modifiers {
    pub(crate) nodes: [Option<ModKind>; NodeType::count()],
    pub(crate) feats: [Option<ModKind>; FType::count()],
    pub(crate) suprs: SupraSegs,
}

impl Modifiers {
    pub(crate) fn new() -> Self {
        debug_assert_eq!(NodeType::Pharyngeal as usize + 1, NodeType::count());
        debug_assert_eq!(FType::RetractedTongueRoot as usize + 1, FType::count());

        Self {
            nodes: [(); NodeType::count()].map(|_| None),
            feats: [(); FType::count()].map(|_| None),
            suprs: SupraSegs::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ParseElement {
    EmptySet,
    WordBound,
    SyllBound,
    Ellipsis,
    Metathesis,
    Set(Vec<Item>),
    Ipa(Segment, Option<Modifiers>),
    Matrix(Modifiers, Option<usize>),
    Syllable([Option<ModKind>; 2], Option<Tone>, Option<usize>),
    Structure(Vec<Item>, [Option<ModKind>; 2], Option<Tone>, Option<usize>),
    Optional(Vec<Item>, usize, usize),
    Environment(Vec<Item>, Vec<Item>),
    Variable(Token, Option<Modifiers>),
}

impl ParseElement {
    fn as_matrix(&self) -> Option<&Modifiers> {
        if let Self::Matrix(v, _) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl fmt::Display for ParseElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseElement::Variable(tk, p) => {
                // let tt = p.iter()
                // .fold(String::new(), |acc, i| acc + &i.to_string() + ", ");

                // write!(f, "{} = [{}]", t, tt)
                write!(f, "{tk:#?} = {p:#?}")
            }
            ParseElement::EmptySet => write!(f, "∅"),
            ParseElement::WordBound => write!(f, "#"),
            ParseElement::SyllBound => write!(f, "$"),
            ParseElement::Ellipsis => write!(f, "…"),
            ParseElement::Metathesis => write!(f, "&"),

            ParseElement::Ipa(s, m) => write!(f, "{s:?} + {m:?}"),

            ParseElement::Matrix(tokens, var) => {
                write!(f, "{tokens:#?}={var:#?}")
            }
            ParseElement::Syllable(str, tone, var) => {
                write!(f, "SYLL=>{str:?}:{tone:#?}={var:#?}")
            }
            ParseElement::Structure(segs, str, tone, var) => {
                write!(f, "STRUCT=>{str:?}:{tone:#?}={var:#?} <")?;
                for i in segs {
                    write!(f, "{}", i)?;
                    write!(f, ", ")?;
                }
                write!(f, ">")
            }
            ParseElement::Set(s) => {
                write!(f, "{{")?;
                for i in s {
                    write!(f, "{}", i)?;
                    write!(f, ", ")?;
                }
                write!(f, "}}")
            }
            ParseElement::Optional(s, min, max) => {
                write!(f, "(")?;
                for i in s {
                    write!(f, "{}", i)?;
                    write!(f, ", ")?;
                }
                write!(f, " {}:{})", min, max)
            }
            ParseElement::Environment(bef, aft) => {
                let xb = bef
                    .iter()
                    .fold(String::new(), |acc, i| acc + &i.to_string() + ", ");

                let xa = aft
                    .iter()
                    .fold(String::new(), |acc, i| acc + &i.to_string() + ", ");

                if xb.is_empty() && xa.is_empty() {
                    write!(f, "[{xb}] __ [{xa}]")
                } else if xb.is_empty() {
                    write!(f, "[{xb}] __ {xa}")
                } else if xa.is_empty() {
                    write!(f, "{xb} __ [{xa}]")
                } else {
                    write!(f, "{xb} __ {xa}")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item {
    pub(crate) kind: ParseElement,
    pub(crate) position: Position,
}

impl Item {
    pub(crate) fn new(k: ParseElement, p: Position) -> Self {
        Self {
            kind: k,
            position: p,
        }
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub(crate) struct Parser {
    token_list: Vec<Token>,
    group: usize,
    line: usize,
    pos: usize,
    curr_tkn: Token,
}

impl Parser {
    pub(crate) fn new(token_list: Vec<Token>, group: usize, line: usize) -> Self {
        let curr_tkn = token_list[0].clone();
        Self {
            token_list,
            group,
            line,
            pos: 0,
            curr_tkn,
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.curr_tkn = if self.has_more_tokens() {
            self.token_list[self.pos].clone()
        } else {
            Token {
                kind: TokenKind::Eol,
                value: Rc::default(),
                position: Position::new(self.group, self.line, self.pos, self.pos + 1),
            }
        }
    }

    fn has_more_tokens(&self) -> bool {
        self.pos < self.token_list.len()
    }

    fn peek_expect(&self, knd: TokenKind) -> bool {
        self.curr_tkn.kind == knd
    }

    fn expect(&mut self, knd: TokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            self.advance();
            true
        } else {
            false
        }
    }

    fn eat(&mut self) -> Token {
        let token = self.curr_tkn.clone();
        self.advance();
        token
    }

    fn eat_expect(&mut self, knd: TokenKind) -> Option<Token> {
        if self.curr_tkn.kind == knd {
            Some(self.eat())
        } else {
            None
        }
    }

    fn get_bound(&mut self) -> Option<Item> {
        if let Some(token) = self.eat_expect(TokenKind::SyllBoundary) {
            return Some(Item::new(ParseElement::SyllBound, token.position));
        }
        if let Some(token) = self.eat_expect(TokenKind::WordBoundary) {
            return Some(Item::new(ParseElement::WordBound, token.position));
        }
        None
    }

    fn get_word_bound(&mut self) -> Option<Item> {
        if let Some(token) = self.eat_expect(TokenKind::WordBoundary) {
            return Some(Item::new(ParseElement::WordBound, token.position));
        }
        None
    }

    fn get_syll_bound(&mut self) -> Option<Item> {
        if let Some(token) = self.eat_expect(TokenKind::SyllBoundary) {
            return Some(Item::new(ParseElement::SyllBound, token.position));
        }
        None
    }

    fn get_env_elements(&mut self, is_after: bool) -> Result<Vec<Item>, RuleSyntaxError> {
        // returns (('WBOUND')? ( SBOUND / ELLIPSS / OPT / TERM )+) / (( SBOUND / ELLIPSS / OPT / TERM )+ ('WBOUND')?)
        let mut els = Vec::new();
        let mut contains_word_bound = false;
        let mut word_bound_pos = Position::new(0, 0, 0, 0);

        loop {
            if let Some(x) = self.get_word_bound() {
                word_bound_pos = x.position;
                if contains_word_bound {
                    return Err(RuleSyntaxError::TooManyWordBoundaries(word_bound_pos));
                }
                els.push(x);
                contains_word_bound = true;
                continue;
            }
            if let Some(x) = self.get_syll_bound() {
                els.push(x);
                continue;
            }
            if let Some(el) = self.eat_expect(TokenKind::Ellipsis) {
                els.push(Item::new(ParseElement::Ellipsis, el.position));
                continue;
            }
            if let Some(x) = self.get_opt()? {
                // NOTE: This must go above self.get_term() as that func returns an error for options
                els.push(x);
                continue;
            }
            if let Some(x) = self.get_term()? {
                els.push(x);
                continue;
            }
            break;
        }
        if contains_word_bound {
            if !is_after && els.first().expect("contains wbound").kind != ParseElement::WordBound {
                return Err(RuleSyntaxError::StuffBeforeWordBound(word_bound_pos));
            } else if is_after
                && els.last().expect("contains wbound").kind != ParseElement::WordBound
            {
                return Err(RuleSyntaxError::StuffAfterWordBound(word_bound_pos));
            }
        }

        Ok(els)
    }

    fn get_env_term(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns ENV_TRM ← ('WBOUND')? ENV_ELS? '_' ENV_ELS? ('WBOUND')?
        let start = self.curr_tkn.position.start;

        let before = self.get_env_elements(false)?;

        if !self.expect(TokenKind::Underline) {
            return Err(RuleSyntaxError::ExpectedUnderline(self.curr_tkn.clone()));
        }
        let after = self.get_env_elements(true)?;

        if self.peek_expect(TokenKind::Underline) {
            return Err(RuleSyntaxError::TooManyUnderlines(self.curr_tkn.clone()));
        }

        let end = self.token_list[self.pos - 1].position.end;

        Ok(Item::new(
            ParseElement::Environment(before, after),
            Position::new(self.group, self.line, start, end),
        ))
    }

    fn get_spec_env(&mut self) -> Result<Option<Vec<Item>>, RuleSyntaxError> {
        // returns ENV_SPC ← '_' ',' ENV_EL
        let start = self.curr_tkn.position.start;
        let pstn = self.pos;

        if !self.expect(TokenKind::Underline) {
            return Ok(None);
        }

        debug_assert_eq!(pstn, self.pos - 1);
        if !self.expect(TokenKind::Comma) {
            self.pos -= 2;
            self.advance();
            return Ok(None);
        }

        let x = self.get_env_elements(false)?;

        if self.expect(TokenKind::Underline) {
            self.pos = pstn - 1;
            self.advance();
            return Ok(None);
        }

        let end = self.token_list[self.pos - 1].position.end;

        let v = vec![
            Item::new(
                ParseElement::Environment(x.clone(), Vec::new()),
                Position::new(self.group, self.line, start, end),
            ),
            Item::new(
                ParseElement::Environment(Vec::new(), x.into_iter().rev().collect()),
                Position::new(self.group, self.line, start, end),
            ),
        ];

        Ok(Some(v))
    }

    fn get_env(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        // returns ENV ← ENV_SPC / ENV_TRM  (',' ENV_TRM)*
        if let Some(s) = self.get_spec_env()? {
            return Ok(s);
        }

        let mut envs = Vec::new();
        loop {
            let x = self.get_env_term()?;
            envs.push(x);
            if !self.expect(TokenKind::Comma) {
                break;
            }
        }
        if envs.is_empty() {
            return Err(RuleSyntaxError::EmptyEnv(
                self.group,
                self.line,
                self.token_list[self.pos].position.start,
            ));
        }

        Ok(envs)
    }

    fn get_except_block(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        if !self.expect(TokenKind::Pipe) && !self.expect(TokenKind::DubSlash) {
            return Ok(Vec::new());
        }
        self.get_env()
    }

    fn get_context(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        if !self.expect(TokenKind::Slash) {
            return Ok(Vec::new());
        }
        self.get_env()
    }

    fn join_group_with_params(&mut self, character: Item, parameters: Item) -> Item {
        let mut chr = character
            .kind
            .as_matrix()
            .expect("Caller asserts `character` is a matrix")
            .clone();
        let params = parameters
            .kind
            .as_matrix()
            .expect("Caller asserts `parameters` is a matrix")
            .clone();
        for (i, p) in params.nodes.iter().enumerate() {
            if p.is_none() {
                continue;
            }
            chr.nodes[i] = *p
        }
        for (i, p) in params.feats.iter().enumerate() {
            if p.is_none() {
                continue;
            }
            chr.feats[i] = *p
        }
        chr.suprs.stress[0] = if params.suprs.stress[0].is_none() {
            chr.suprs.stress[0]
        } else {
            params.suprs.stress[0]
        };
        chr.suprs.stress[1] = if params.suprs.stress[1].is_none() {
            chr.suprs.stress[1]
        } else {
            params.suprs.stress[1]
        };
        chr.suprs.length[0] = if params.suprs.length[0].is_none() {
            chr.suprs.length[0]
        } else {
            params.suprs.length[0]
        };
        chr.suprs.length[1] = if params.suprs.length[1].is_none() {
            chr.suprs.length[1]
        } else {
            params.suprs.length[1]
        };
        chr.suprs.tone = if params.suprs.tone.is_none() {
            chr.suprs.tone
        } else {
            params.suprs.tone
        };

        Item::new(
            ParseElement::Matrix(chr, None),
            Position::new(
                self.group,
                self.line,
                character.position.start,
                parameters.position.end,
            ),
        )
    }

    fn ipa_to_vals(&self, ipa: Token) -> Result<Segment, RuleSyntaxError> {
        match CARDINALS_MAP.get(ipa.value.as_ref()) {
            Some(z) => Ok(*z),
            None => Err(RuleSyntaxError::UnknownIPA(ipa)),
        }
    }

    fn group_to_matrix(&self, chr: &Token) -> Result<Item, RuleSyntaxError> {
        // returns GROUP ← 'C' / 'O' / 'S' / 'L' / 'N' / 'G' / 'V'
        use FType::*;
        use ModKind::*;

        const SYLL_M: (FType, ModKind) = (Syllabic, Binary(BinMod::Negative)); // -syllabic
        const SYLL_P: (FType, ModKind) = (Syllabic, Binary(BinMod::Positive)); // +syllabic
        const CONS_M: (FType, ModKind) = (Consonantal, Binary(BinMod::Negative)); // -consonantal
        const CONS_P: (FType, ModKind) = (Consonantal, Binary(BinMod::Positive)); // +consonantal
        const SONR_M: (FType, ModKind) = (Sonorant, Binary(BinMod::Negative)); // -sonorant
        const SONR_P: (FType, ModKind) = (Sonorant, Binary(BinMod::Positive)); // +sonorant
        const APPR_M: (FType, ModKind) = (Approximant, Binary(BinMod::Negative)); // -approximant
        const APPR_P: (FType, ModKind) = (Approximant, Binary(BinMod::Positive)); // +approximant
        const CONT_M: (FType, ModKind) = (Continuant, Binary(BinMod::Negative)); // -continuent
        const CONT_P: (FType, ModKind) = (Continuant, Binary(BinMod::Positive)); // +continuent
        const DLRL_M: (FType, ModKind) = (DelayedRelease, Binary(BinMod::Negative)); // -del.rel.
        const NASL_P: (FType, ModKind) = (Nasal, Binary(BinMod::Positive)); // +nasal

        let mut args = Modifiers::new();

        (match chr.value.as_ref() {
            "C" => vec![SYLL_M], // -syll                             // Consonant
            "O" => vec![CONS_P, SONR_M, SYLL_M], // +cons, -son, -syll                // Obstruent
            "S" => vec![CONS_P, SONR_P, SYLL_M], // +cons, +son, -syll                // Sonorant
            "P" => vec![CONS_P, SONR_M, SYLL_M, DLRL_M, CONT_M], // +cons, +son, -syll, -dlrl, -cont  // Plosive
            "F" => vec![CONS_P, SONR_M, SYLL_M, APPR_M, CONT_P], // +cons, +son, -syll, -appr, +cont  // Fricative
            "L" => vec![CONS_P, SONR_P, SYLL_M, APPR_P], // +cons, +son, -syll, +appr         // Liquid
            "N" => vec![CONS_P, SONR_P, SYLL_M, APPR_M, NASL_P], // +cons, +son, -syll, -appr, +nasal // Nasal
            "G" => vec![CONS_M, SONR_P, SYLL_M], // -cons, +son, -syll                // Glide
            "V" => vec![CONS_M, SONR_P, SYLL_P], // -cons, +son, +syll                // Vowel

            // TODO(girv): possible other groups
            // "T"  // Palatal  [+cons, +dist, +fr, -bk, +hi, -lo]
            // "K"  // Velar    [+cons, -fr, +bk, +hi, -lo]
            // "Q"  // Uvular   [+cons, -fr, +bk, -hi, -lo]
            _ => return Err(RuleSyntaxError::UnknownGrouping(chr.clone())),
        })
        .into_iter()
        .for_each(|(feature, value)| args.feats[feature as usize] = Some(value));

        Ok(Item::new(
            ParseElement::Matrix(args, None),
            Position::new(self.group, self.line, chr.position.start, chr.position.end),
        ))
    }

    fn is_feature(&self) -> bool {
        matches!(self.curr_tkn.kind, TokenKind::Feature(_))
    }

    fn curr_token_to_modifier(&self) -> Result<(FeatType, Mods), RuleSyntaxError> {
        // returns ARG ← ARG_MOD [a-zA-Z]+ / TONE
        match self.curr_tkn.kind {
            TokenKind::Feature(feature) => {
                let value = &self.curr_tkn.value;
                match value.as_ref() {
                    "+" => Ok((feature, Mods::Binary(BinMod::Positive))),
                    "-" => Ok((feature, Mods::Binary(BinMod::Negative))),
                    "α" | "β" | "γ" | "δ" | "ε" | "ζ" | "η" | "θ" | "ι" | "κ" | "λ" | "μ" | "ν"
                    | "ξ" | "ο" | "π" | "ρ" | "σ" | "ς" | "τ" | "υ" | "φ" | "χ" | "ψ" | "ω"
                    | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L"
                    | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X"
                    | "Y" | "Z" => Ok((
                        feature,
                        Mods::Alpha(AlphaMod::Alpha(value.chars().next().unwrap())),
                    )),
                    "-α" | "-β" | "-γ" | "-δ" | "-ε" | "-ζ" | "-η" | "-θ" | "-ι" | "-κ" | "-λ"
                    | "-μ" | "-ν" | "-ξ" | "-ο" | "-π" | "-ρ" | "-σ" | "-ς" | "-τ" | "-υ"
                    | "-φ" | "-χ" | "-ψ" | "-ω" | "-A" | "-B" | "-C" | "-D" | "-E" | "-F"
                    | "-G" | "-H" | "-I" | "-J" | "-K" | "-L" | "-M" | "-N" | "-O" | "-P"
                    | "-Q" | "-R" | "-S" | "-T" | "-U" | "-V" | "-W" | "-X" | "-Y" | "-Z" => Ok((
                        feature,
                        Mods::Alpha(AlphaMod::InvAlpha(value.chars().nth(1).unwrap())),
                    )),
                    _ if feature == FeatType::Supr(SupraType::Tone) => {
                        let v = value.replace('0', "");
                        if v.chars().count() > 4 {
                            Err(RuleSyntaxError::ToneTooBig(self.curr_tkn.clone()))
                        } else {
                            Ok((feature, Mods::Number(v.parse().unwrap_or(0))))
                        }
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn get_param_args(&mut self, is_syll: bool) -> Result<Modifiers, RuleSyntaxError> {
        // returns PARAMS ← '[' ARG (',' ARG)* ']'
        let mut args = Modifiers::new();
        while self.has_more_tokens() {
            if self.expect(TokenKind::RightSquare) {
                break;
            }
            if self.expect(TokenKind::Comma) {
                continue;
            }
            if self.is_feature() {
                let (ft, mk) = self.curr_token_to_modifier()?;
                if ft != FeatType::Supr(SupraType::Tone)
                    && ft != FeatType::Supr(SupraType::Stress)
                    && ft != FeatType::Supr(SupraType::SecStress)
                    && is_syll
                {
                    return Err(RuleSyntaxError::BadSyllableMatrix(self.curr_tkn.clone()));
                }
                match ft {
                    FeatType::Node(t) => {
                        args.nodes[t as usize] = match mk {
                            Mods::Binary(b) => Some(ModKind::Binary(b)),
                            Mods::Alpha(a) => Some(ModKind::Alpha(a)),
                            Mods::Number(_) => unreachable!(),
                        }
                    }
                    FeatType::Feat(t) => {
                        args.feats[t as usize] = match mk {
                            Mods::Binary(b) => Some(ModKind::Binary(b)),
                            Mods::Alpha(a) => Some(ModKind::Alpha(a)),
                            Mods::Number(_) => unreachable!(),
                        }
                    }
                    FeatType::Supr(t) => match mk {
                        Mods::Number(n) => args.suprs.tone = Some(n),
                        Mods::Alpha(a) => match t {
                            SupraType::Long => args.suprs.length[0] = Some(ModKind::Alpha(a)),
                            SupraType::Overlong => args.suprs.length[1] = Some(ModKind::Alpha(a)),
                            SupraType::Stress => args.suprs.stress[0] = Some(ModKind::Alpha(a)),
                            SupraType::SecStress => args.suprs.stress[1] = Some(ModKind::Alpha(a)),
                            SupraType::Tone => {
                                unreachable!("Tone cannot be `Alpha'd` (yet anyway)")
                            }
                        },
                        Mods::Binary(b) => match t {
                            SupraType::Long => args.suprs.length[0] = Some(ModKind::Binary(b)),
                            SupraType::Overlong => args.suprs.length[1] = Some(ModKind::Binary(b)),
                            SupraType::Stress => args.suprs.stress[0] = Some(ModKind::Binary(b)),
                            SupraType::SecStress => args.suprs.stress[1] = Some(ModKind::Binary(b)),
                            SupraType::Tone => unreachable!("Tone cannot be `+/-`"),
                        },
                    },
                }
                // args[f as usize] = b;
                self.advance();
                continue;
            }
            if self.curr_tkn.kind == TokenKind::Eol {
                return Err(RuleSyntaxError::UnexpectedEol(self.curr_tkn.clone(), ']'));
            }
            return Err(RuleSyntaxError::ExpectedTokenFeature(self.curr_tkn.clone()));
        }
        Ok(args)
    }

    fn get_params(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns PARAMS ← '[' ARG (',' ARG)* ']'
        let start = self.token_list[self.pos - 1].position.start;
        let args = self.get_param_args(false)?;
        let end = self.token_list[self.pos - 1].position.end;

        Ok(Item::new(
            ParseElement::Matrix(args, None),
            Position::new(self.group, self.line, start, end),
        ))
    }

    fn get_group(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns GROUP (':' PARAMS)?
        let chr = self.group_to_matrix(&self.curr_tkn)?;
        self.advance();

        if !self.expect(TokenKind::Colon) {
            return Ok(chr);
        }

        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()));
        }

        let params = self.get_params()?;

        let joined_matrix = self.join_group_with_params(chr, params);

        Ok(joined_matrix)
    }

    fn get_ipa(&mut self) -> Result<Item, RuleSyntaxError> {
        // returns IPA (':' PARAMS)?
        let mut ipa = self.ipa_to_vals(self.curr_tkn.clone())?;
        let pos = self.curr_tkn.position;
        self.advance();

        while matches!(self.curr_tkn.kind, TokenKind::Diacritic(_)) {
            let dia = self.eat();
            let d = dia.kind.as_diacritic().unwrap();
            if let Err((mod_index, is_node)) = ipa.check_and_apply_diacritic(&DIACRITS[*d as usize])
            {
                if !is_node {
                    let ft = FType::from_usize(mod_index);
                    let positive = match &DIACRITS[*d as usize].prereqs.feats[mod_index].unwrap() {
                        ModKind::Binary(bin_mod) => *bin_mod == BinMod::Positive,
                        _ => unreachable!(),
                    };
                    return Err(RuleSyntaxError::DiacriticDoesNotMeetPreReqsFeat(
                        pos,
                        dia.position,
                        ft.to_string(),
                        positive,
                    ));
                } else {
                    let nt = NodeType::from_usize(mod_index);
                    let positive = match &DIACRITS[*d as usize].prereqs.nodes[mod_index].unwrap() {
                        ModKind::Binary(bin_mod) => *bin_mod == BinMod::Positive,
                        _ => unreachable!(),
                    };
                    return Err(RuleSyntaxError::DiacriticDoesNotMeetPreReqsNode(
                        pos,
                        dia.position,
                        nt.to_string(),
                        positive,
                    ));
                };
            }
        }

        if !self.expect(TokenKind::Colon) {
            return Ok(Item::new(
                ParseElement::Ipa(ipa, None),
                Position::new(
                    self.group,
                    self.line,
                    pos.start,
                    self.token_list[self.pos - 1].position.end,
                ),
            ));
        }
        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()));
        }
        let params = self.get_params()?;
        let joined_kind = ParseElement::Ipa(ipa, Some(params.kind.as_matrix().unwrap().clone()));

        Ok(Item::new(
            joined_kind,
            Position::new(self.group, self.line, pos.start, params.position.end),
        ))
    }

    fn get_var_assign(&mut self, number: Token, char: &Item) -> Item {
        // returns VAR_ASN ← '=' [0-9]+
        let num = number
            .value
            .parse::<usize>()
            .expect("number should be a number as set in `self.get_seg`");
        let mods = char
            .kind
            .as_matrix()
            .expect("char should be matrix as set in `self.get_group`")
            .clone();
        Item::new(
            ParseElement::Matrix(mods, Some(num)),
            Position::new(
                self.group,
                self.line,
                char.position.start,
                char.position.end,
            ),
        )
    }

    fn get_seg(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns SEG ← IPA (':' PARAMS)? / MATRIX VAR_ASN?
        if self.peek_expect(TokenKind::Cardinal) {
            return Ok(Some(self.get_ipa()?));
        }
        if self.peek_expect(TokenKind::Group) {
            let chr = self.get_group()?;
            if self.expect(TokenKind::Equals) {
                let Some(n) = self.eat_expect(TokenKind::Number) else {
                    return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()));
                };
                let res = self.get_var_assign(n, &chr);
                return Ok(Some(res));
            }
            return Ok(Some(chr));
        }
        if self.expect(TokenKind::LeftSquare) {
            let params = self.get_params()?;
            if self.expect(TokenKind::Equals) {
                let Some(n) = self.eat_expect(TokenKind::Number) else {
                    return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()));
                };
                let res = self.get_var_assign(n, &params);
                return Ok(Some(res));
            }
            return Ok(Some(params));
        }
        Ok(None)
    }

    fn get_var(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns VAR ← [0-9]+ (':' PARAMS)?
        let Some(t) = self.eat_expect(TokenKind::Number) else {
            return Ok(None);
        };
        let mut pos = t.position;
        if !self.expect(TokenKind::Colon) {
            let var = Item::new(ParseElement::Variable(t, None), pos);
            return Ok(Some(var));
        }
        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()));
        }
        let params = self.get_params()?;
        let matrix = params
            .kind
            .as_matrix()
            .expect("params should be matrix as set in `self.get_params`")
            .clone();
        pos.end = params.position.end;

        Ok(Some(Item::new(
            ParseElement::Variable(t, Some(matrix)),
            pos,
        )))
    }

    fn get_opt(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns OPT ← '(' OPT_TRM+ (',' [0-9]+ (':' [1-9]+)?)? ')'
        let start_pos = self.curr_tkn.position.start;

        if !self.expect(TokenKind::LeftBracket) {
            return Ok(None);
        }

        let mut segs = Vec::new();
        let mut first_bound: usize = 0;
        let mut second_bound: usize = 0;
        while self.has_more_tokens() {
            if self.peek_expect(TokenKind::RightBracket) {
                break;
            }
            if let Some(x) = self.get_bound() {
                segs.push(x);
                continue;
            }
            if let Some(x) = self.get_syll()? {
                segs.push(x);
                continue;
            }
            if let Some(x) = self.get_set()? {
                segs.push(x);
                continue;
            }
            if let Some(x) = self.get_seg()? {
                segs.push(x);
                continue;
            }
            if let Some(x) = self.get_var()? {
                segs.push(x);
                continue;
            }
            if self.peek_expect(TokenKind::Comma) {
                break;
            }

            return Err(RuleSyntaxError::ExpectedSegment(self.curr_tkn.clone()));
        }
        // FIXME(girv): with this, (C,) and (C,:) are legal alternatives to (C,0) (bug or feature!)
        if self.expect(TokenKind::RightBracket) {
            let end_pos = self.token_list[self.pos - 1].position.end;
            return Ok(Some(Item::new(
                ParseElement::Optional(segs, 0, 1),
                Position::new(self.group, self.line, start_pos, end_pos),
            )));
        }
        if !self.expect(TokenKind::Comma) {
            return Err(RuleSyntaxError::ExpectedComma(self.curr_tkn.clone()));
        }
        if let Some(number) = self.eat_expect(TokenKind::Number) {
            first_bound = number.value.parse().unwrap();
        }
        if self.expect(TokenKind::RightBracket) {
            let end_pos = self.token_list[self.pos - 1].position.end;
            return Ok(Some(Item::new(
                ParseElement::Optional(segs, 0, first_bound),
                Position::new(self.group, self.line, start_pos, end_pos),
            )));
        }
        if !self.expect(TokenKind::Colon) {
            return Err(RuleSyntaxError::ExpectedColon(self.curr_tkn.clone()));
        }
        if let Some(number) = self.eat_expect(TokenKind::Number) {
            second_bound = number.value.parse().unwrap();
            if second_bound < first_bound {
                return Err(RuleSyntaxError::OptMathError(
                    number,
                    first_bound,
                    second_bound,
                ));
            }
        }
        if self.expect(TokenKind::RightBracket) {
            let end_pos = self.token_list[self.pos - 1].position.end;
            return Ok(Some(Item::new(
                ParseElement::Optional(segs, first_bound, second_bound),
                Position::new(self.group, self.line, start_pos, end_pos),
            )));
        }
        Err(RuleSyntaxError::ExpectedRightBracket(self.curr_tkn.clone()))
    }

    fn get_set(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns SET ← '{' SET_TRM (',' SET_TRM)* '}'
        let start_pos = self.curr_tkn.position.start;

        if !self.expect(TokenKind::LeftCurly) {
            return Ok(None);
        }
        let mut terms = Vec::new();
        // NOTE: this while condition may allow  "/ _{A, B, C <eol>" as valid input
        // should probably return SyntaxError::ExpectedRightBracketAtEol
        // bug or feature? ¯\_(ツ)_/¯
        while self.has_more_tokens() {
            if self.expect(TokenKind::RightCurly) {
                break;
            }
            if self.expect(TokenKind::Comma) {
                continue;
            }
            if let Some(x) = self.get_seg()? {
                terms.push(x);
                continue;
            }
            if let Some(x) = self.get_bound() {
                terms.push(x);
                continue;
            }
            if let Some(x) = self.get_syll()? {
                terms.push(x);
                continue;
            }

            return Err(RuleSyntaxError::ExpectedSegment(self.curr_tkn.clone()));
        }

        let end_pos = self.token_list[self.pos - 1].position.end;
        let pos = Position::new(self.group, self.line, start_pos, end_pos);

        if terms.is_empty() {
            Err(RuleSyntaxError::EmptySet(pos))
        } else {
            Ok(Some(Item::new(ParseElement::Set(terms.clone()), pos)))
        }
    }

    fn get_syll(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns SYL ← '%' (':' PARAMS)? VAR_ASN?
        let start_pos = self.curr_tkn.position.start;

        if !self.expect(TokenKind::Syllable) {
            return Ok(None);
        }
        if !self.expect(TokenKind::Colon) {
            let end_pos = self.curr_tkn.position.start - 1;
            if self.expect(TokenKind::Equals) {
                let Some(number) = self.eat_expect(TokenKind::Number) else {
                    return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()));
                };
                let num = number.value.parse::<usize>().unwrap();
                return Ok(Some(Item::new(
                    ParseElement::Syllable([None, None], None, Some(num)),
                    Position::new(self.group, self.line, start_pos, end_pos),
                )));
            }
            return Ok(Some(Item::new(
                ParseElement::Syllable([None, None], None, None),
                Position::new(self.group, self.line, start_pos, end_pos),
            )));
        }
        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()));
        }

        let mods = self.get_param_args(true)?;
        let end_pos = self.token_list[self.pos - 1].position.end;

        if self.expect(TokenKind::Equals) {
            let Some(number) = self.eat_expect(TokenKind::Number) else {
                return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()));
            };
            let num = number.value.parse::<usize>().unwrap();
            return Ok(Some(Item::new(
                ParseElement::Syllable(mods.suprs.stress, mods.suprs.tone, Some(num)),
                Position::new(self.group, self.line, start_pos, end_pos),
            )));
        }
        Ok(Some(Item::new(
            ParseElement::Syllable(mods.suprs.stress, mods.suprs.tone, None),
            Position::new(self.group, self.line, start_pos, end_pos),
        )))
    }

    fn get_struct(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        let start_pos = self.curr_tkn.position.start;

        if !self.expect(TokenKind::LeftAngle) {
            return Ok(None);
        }
        let mut terms = Vec::new();

        while self.has_more_tokens() {
            if self.eat_expect(TokenKind::RightAngle).is_some() {
                break;
            }
            if let Some(x) = self.get_seg()? {
                terms.push(x);
                continue;
            }
            if let Some(el) = self.eat_expect(TokenKind::Ellipsis) {
                terms.push(Item::new(ParseElement::Ellipsis, el.position));
                continue;
            }

            if let Some(x) = self.get_var()? {
                terms.push(x);
                continue;
            }

            return Err(RuleSyntaxError::ExpectedSegment(self.curr_tkn.clone()));
        }

        if !self.expect(TokenKind::Colon) {
            let end_pos = self.curr_tkn.position.start - 1;
            if self.expect(TokenKind::Equals) {
                let Some(number) = self.eat_expect(TokenKind::Number) else {
                    return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()));
                };
                let num = number.value.parse::<usize>().unwrap();
                return Ok(Some(Item::new(
                    ParseElement::Structure(terms, [None, None], None, Some(num)),
                    Position::new(self.group, self.line, start_pos, end_pos),
                )));
            }
            return Ok(Some(Item::new(
                ParseElement::Structure(terms, [None, None], None, None),
                Position::new(self.group, self.line, start_pos, end_pos),
            )));
        }
        if !self.expect(TokenKind::LeftSquare) {
            return Err(RuleSyntaxError::ExpectedMatrix(self.curr_tkn.clone()));
        }

        let mods = self.get_param_args(true)?;
        let end_pos = self.token_list[self.pos - 1].position.end;

        if self.expect(TokenKind::Equals) {
            let Some(number) = self.eat_expect(TokenKind::Number) else {
                return Err(RuleSyntaxError::ExpectedVariable(self.curr_tkn.clone()));
            };
            let num = number.value.parse::<usize>().unwrap();
            return Ok(Some(Item::new(
                ParseElement::Structure(terms, mods.suprs.stress, mods.suprs.tone, Some(num)),
                Position::new(self.group, self.line, start_pos, end_pos),
            )));
        }

        Ok(Some(Item::new(
            ParseElement::Structure(terms, mods.suprs.stress, mods.suprs.tone, None),
            Position::new(self.group, self.line, start_pos, end_pos),
        )))
    }

    fn get_term(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns TERM ← SYL / STRUCT / SET / SEG / OPT / VAR
        if let Some(x) = self.get_syll()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_struct()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_set()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_seg()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_var()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_opt()? {
            return Err(RuleSyntaxError::OptLocError(x.position));
        }

        Ok(None)
    }

    fn get_input_els(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        // returns INP_EL+
        let mut els = Vec::new();
        loop {
            if let Some(el) = self.eat_expect(TokenKind::Ellipsis) {
                els.push(Item::new(ParseElement::Ellipsis, el.position));
            } else if let Some(s_bound) = self.get_syll_bound() {
                els.push(s_bound);
            } else if let Some(trm) = self.get_term()? {
                els.push(trm)
            } else if let Some(w_bound) = self.get_word_bound() {
                return Err(RuleSyntaxError::WordBoundLoc(w_bound.position));
            } else {
                break;
            }
        }
        Ok(els)
    }

    fn get_output_el(&mut self) -> Result<Option<Item>, RuleSyntaxError> {
        // returns OUT_EL ← SYL / SET / SEG / VAR / SBOUND
        // NOTE: a set in the output only makes sense when matched to a set in the input w/ the same # of elements
        // This will be validated when applying
        if let Some(x) = self.get_syll()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_struct()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_set()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_seg()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_var()? {
            return Ok(Some(x));
        }
        if let Some(x) = self.get_syll_bound() {
            return Ok(Some(x));
        }

        Ok(None)
    }

    fn get_output_els(&mut self) -> Result<Vec<Item>, RuleSyntaxError> {
        // returns OUT_EL+
        let mut els = Vec::new();
        while let Some(el) = self.get_output_el()? {
            els.push(el);
        }
        Ok(els)
    }

    fn get_empty(&mut self) -> Option<Item> {
        // EMP ← '*' / '∅'
        if !self.peek_expect(TokenKind::Star) && !self.peek_expect(TokenKind::EmptySet) {
            return None;
        }
        let token = self.eat();
        Some(Item::new(ParseElement::EmptySet, token.position))
    }

    fn get_input(&mut self) -> Result<Vec<Vec<Item>>, RuleSyntaxError> {
        // returns `INP ← INP_TRM  ( ',' INP_TRM )*` where `INP_TRM ← EMP / INP_EL+`
        let mut inputs = Vec::new();
        loop {
            // Insertion
            if let Some(empty) = self.get_empty() {
                inputs.push(vec![empty]);
                if !self.expect(TokenKind::Comma)
                    && (!self.peek_expect(TokenKind::Arrow)
                        && !self.peek_expect(TokenKind::GreaterThan))
                {
                    return Err(RuleSyntaxError::InsertErr(self.curr_tkn.clone()));
                }
                continue;
            }
            // Input elements
            let inp_term = self.get_input_els()?;
            if inp_term.is_empty() && inputs.is_empty() {
                return Err(RuleSyntaxError::UnknownCharacter(
                    self.curr_tkn.value.chars().next().unwrap(),
                    self.group,
                    self.line,
                    self.pos,
                ));
            } else if inp_term.is_empty() && !self.expect(TokenKind::Comma) {
                break;
            }

            if let TokenKind::Diacritic(_) = self.curr_tkn.kind {
                match inp_term.last() {
                    Some(Item { kind: _, position }) => {
                        return Err(RuleSyntaxError::UnexpectedDiacritic(
                            *position,
                            self.curr_tkn.position,
                        ))
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }

            inputs.push(inp_term);

            if !self.expect(TokenKind::Comma) {
                break;
            }
        }
        if inputs.is_empty() {
            return Err(RuleSyntaxError::EmptyInput(
                self.group,
                self.line,
                self.token_list[self.pos].position.start,
            ));
        }
        Ok(inputs)
    }

    fn get_output(&mut self) -> Result<Vec<Vec<Item>>, RuleSyntaxError> {
        // returns `OUT ← OUT_TRM  ( ',' OUT_TRM )*` where `OUT_TRM ← '&' / EMP / OUT_EL+`
        let mut outputs = Vec::new();
        loop {
            // Metathesis
            if let Some(el) = self.eat_expect(TokenKind::Ampersand) {
                outputs.push(vec![Item::new(ParseElement::Metathesis, el.position)]);
                if !self.expect(TokenKind::Comma)
                    && (!self.peek_expect(TokenKind::Slash)
                        && !self.peek_expect(TokenKind::Pipe)
                        && !self.peek_expect(TokenKind::Eol))
                {
                    return Err(RuleSyntaxError::MetathErr(self.curr_tkn.clone()));
                }
                continue;
            }
            // Deletion
            if let Some(empty) = self.get_empty() {
                outputs.push(vec![empty]);
                if !self.expect(TokenKind::Comma)
                    && !self.peek_expect(TokenKind::Slash)
                    && !self.peek_expect(TokenKind::Pipe)
                    && !self.peek_expect(TokenKind::Eol)
                {
                    return Err(RuleSyntaxError::DeleteErr(self.curr_tkn.clone()));
                }
                continue;
            }
            // Output Elements
            let out_term = self.get_output_els()?;
            if out_term.is_empty() && outputs.is_empty() {
                return Err(RuleSyntaxError::EmptyOutput(
                    self.group,
                    self.line,
                    self.token_list[self.pos].position.start,
                ));
            } else if out_term.is_empty() && !self.expect(TokenKind::Comma) {
                break;
            }

            if let TokenKind::Diacritic(_) = self.curr_tkn.kind {
                match out_term.last() {
                    Some(Item { kind: _, position }) => {
                        return Err(RuleSyntaxError::UnexpectedDiacritic(
                            *position,
                            self.curr_tkn.position,
                        ))
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }

            outputs.push(out_term);
            if !self.expect(TokenKind::Comma) {
                break;
            }
        }
        if outputs.is_empty() {
            return Err(RuleSyntaxError::EmptyOutput(
                self.group,
                self.line,
                self.token_list[self.pos].position.start,
            ));
        }
        Ok(outputs)
    }

    fn rule(&mut self) -> Result<Rule, RuleSyntaxError> {
        // returns RULE ← INP ARR OUT ('/' ENV)? (PIPE ENV)? EOL

        // INP
        let input = self.get_input()?;
        // ARR
        if !self.expect(TokenKind::Arrow) && !self.expect(TokenKind::GreaterThan) {
            return Err(RuleSyntaxError::ExpectedArrow(self.curr_tkn.clone()));
        }
        // OUT
        let output = self.get_output()?;

        if self.expect(TokenKind::Eol) {
            return Ok(Rule::new(input, output, Vec::new(), Vec::new()));
        }
        if !self.peek_expect(TokenKind::Slash) && !self.peek_expect(TokenKind::Pipe) {
            return Err(RuleSyntaxError::ExpectedEndLine(self.curr_tkn.clone()));
        }
        // ('/' ENV)
        let context = self.get_context()?;
        // (PIPE ENV)
        let except = self.get_except_block()?;
        // !EOL
        if !self.expect(TokenKind::Eol) {
            return Err(RuleSyntaxError::ExpectedEndLine(self.curr_tkn.clone()));
        }

        Ok(Rule::new(input, output, context, except))
    }

    pub(crate) fn parse(&mut self) -> Result<Option<Rule>, RuleSyntaxError> {
        if self.curr_tkn.kind == TokenKind::Eol {
            Ok(None)
        } else {
            Ok(Some(self.rule()?))
        }
    }
}

#[cfg(test)]
mod parser_tests {

    // macro_rules! map {
    //     ($($k:expr => $v:expr),* $(,)?) => {{
    //         core::convert::From::from([$(($k, $v),)*])
    //     }};
    // }

    use super::*;
    use crate::CARDINALS_MAP;

    fn setup(test_str: &str) -> Vec<Token> {
        Lexer::new(&String::from(test_str).chars().collect::<Vec<_>>(), 0, 0)
            .get_line()
            .unwrap()
    }

    #[test]
    fn test_multi_rule() {
        let maybe_result = Parser::new(
            setup("%:[+stress], % > [-stress], [+stress] / _ , #_"),
            0,
            0,
        )
        .parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap().unwrap();

        assert_eq!(result.input.len(), 2);
        assert_eq!(result.output.len(), 2);
        assert_eq!(result.context.len(), 2);
        assert!(result.except.is_empty());

        let exp_input = vec![
            Item::new(
                ParseElement::Syllable([Some(ModKind::Binary(BinMod::Positive)), None], None, None),
                Position::new(0, 0, 0, 11),
            ),
            Item::new(
                ParseElement::Syllable([None, None], None, None),
                Position::new(0, 0, 13, 14),
            ),
        ];

        let mut x = Modifiers::new();
        let mut y = Modifiers::new();
        x.suprs.stress = [Some(ModKind::Binary(BinMod::Negative)), None];
        y.suprs.stress = [Some(ModKind::Binary(BinMod::Positive)), None];
        let exp_output = vec![
            Item::new(ParseElement::Matrix(x, None), Position::new(0, 0, 17, 26)),
            Item::new(ParseElement::Matrix(y, None), Position::new(0, 0, 28, 37)),
        ];

        let exp_context: Vec<Item> = vec![
            Item::new(
                ParseElement::Environment(vec![], vec![]),
                Position::new(0, 0, 40, 41),
            ),
            Item::new(
                ParseElement::Environment(
                    vec![Item::new(
                        ParseElement::WordBound,
                        Position::new(0, 0, 44, 45),
                    )],
                    vec![],
                ),
                Position::new(0, 0, 44, 46),
            ),
        ];

        assert_eq!(result.input[0][0], exp_input[0], "1");
        assert_eq!(result.input[1][0], exp_input[1], "2");

        assert_eq!(result.output[0][0], exp_output[0], "3");
        assert_eq!(result.output[1][0], exp_output[1], "4");

        assert_eq!(result.context[0], exp_context[0], "5");
        assert_eq!(result.context[1], exp_context[1], "6");
    }

    #[test]
    fn test_metathesis() {
        let maybe_result = Parser::new(setup("t͡ɕ...b͡β > &"), 0, 0).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap().unwrap();

        assert_eq!(result.input.len(), 1);
        assert_eq!(result.output.len(), 1);
        assert!(result.context.is_empty());
        assert!(result.except.is_empty());

        let exp_input_res = vec![
            Item::new(
                ParseElement::Ipa(CARDINALS_MAP.get("t͡ɕ").unwrap().clone(), None),
                Position::new(0, 0, 0, 3),
            ),
            Item::new(ParseElement::Ellipsis, Position::new(0, 0, 3, 6)),
            Item::new(
                ParseElement::Ipa(CARDINALS_MAP.get("b͡β").unwrap().clone(), None),
                Position::new(0, 0, 6, 9),
            ),
        ];

        assert_eq!(result.input[0][0], exp_input_res[0]);
        assert_eq!(result.input[0][1], exp_input_res[1]);
        assert_eq!(result.input[0][2], exp_input_res[2]);
    }

    #[test]
    fn test_variables_plain() {
        let mut x = Modifiers::new();
        x.feats[FType::Syllabic as usize] = Some(ModKind::Binary(BinMod::Negative));

        let _c = Item::new(ParseElement::Matrix(x, Some(1)), Position::new(0, 0, 0, 1));

        let mut y = Modifiers::new();
        y.feats[FType::Consonantal as usize] = Some(ModKind::Binary(BinMod::Negative));
        y.feats[FType::Sonorant as usize] = Some(ModKind::Binary(BinMod::Positive));
        y.feats[FType::Syllabic as usize] = Some(ModKind::Binary(BinMod::Positive));

        let _v = Item::new(ParseElement::Matrix(y, Some(2)), Position::new(0, 0, 4, 5));

        let maybe_result = Parser::new(setup("C=1 V=2 > 2 1 / _C"), 0, 0).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap().unwrap();

        assert_eq!(result.input.len(), 1);
        assert_eq!(result.output.len(), 1);
        assert_eq!(result.context.len(), 1);
        assert!(result.except.is_empty());

        // assert_eq!(result.variables.len(), 2);
        // assert!(result.variables.contains_key(&1));
        // assert!(result.variables.contains_key(&2));
        // assert_eq!(result.variables.get(&1), Some(&c));
        // assert_eq!(result.variables.get(&2), Some(&v));
        // assert_eq!(result.variables.get(&3), None);
    }

    #[test]
    fn test_tone() {
        let maybe_result = Parser::new(setup("%:[tone: 123] > [tone: 321]"), 0, 0).parse();
        assert!(maybe_result.is_ok());
        let result = maybe_result.unwrap().unwrap();

        let exp_input = Item::new(
            ParseElement::Syllable([None, None], Some(123), None),
            Position::new(0, 0, 0, 13),
        );

        let mut out = Modifiers::new();

        out.suprs.tone = Some(321);
        let exp_output = Item::new(ParseElement::Matrix(out, None), Position::new(0, 0, 16, 27));

        assert_eq!(result.input[0][0], exp_input);
        assert_eq!(result.output[0][0], exp_output);
    }

    #[test]
    fn test_exceptions() {
        // Double Slash
        let maybe_res = Parser::new(setup("a > e / _ // _u"), 0, 0).parse();
        assert!(maybe_res.is_ok());
        let result = maybe_res.unwrap().unwrap();

        let itm = Item::new(
            ParseElement::Ipa(CARDINALS_MAP.get("u").unwrap().clone(), None),
            Position::new(0, 0, 14, 15),
        );
        let exp_cont = Item::new(
            ParseElement::Environment(vec![], vec![]),
            Position::new(0, 0, 8, 9),
        );
        let exp_expt = Item::new(
            ParseElement::Environment(vec![], vec![itm]),
            Position::new(0, 0, 13, 15),
        );

        assert_eq!(result.context[0], exp_cont);
        assert_eq!(result.except[0], exp_expt);

        // Pipe
        let maybe_res = Parser::new(setup("a > e / _ | _u"), 0, 0).parse();
        assert!(maybe_res.is_ok());
        let result = maybe_res.unwrap().unwrap();

        let itm = Item::new(
            ParseElement::Ipa(CARDINALS_MAP.get("u").unwrap().clone(), None),
            Position::new(0, 0, 13, 14),
        );
        let exp_cont = Item::new(
            ParseElement::Environment(vec![], vec![]),
            Position::new(0, 0, 8, 9),
        );
        let exp_expt = Item::new(
            ParseElement::Environment(vec![], vec![itm]),
            Position::new(0, 0, 12, 14),
        );

        assert_eq!(result.context[0], exp_cont);
        assert_eq!(result.except[0], exp_expt);

        // No Context
        let maybe_res = Parser::new(setup("a > e | _u"), 0, 0).parse();
        assert!(maybe_res.is_ok());
        let result = maybe_res.unwrap().unwrap();

        let itm = Item::new(
            ParseElement::Ipa(CARDINALS_MAP.get("u").unwrap().clone(), None),
            Position::new(0, 0, 9, 10),
        );
        let exp_expt = Item::new(
            ParseElement::Environment(vec![], vec![itm]),
            Position::new(0, 0, 8, 10),
        );

        assert_eq!(result.except[0], exp_expt);
    }
}
