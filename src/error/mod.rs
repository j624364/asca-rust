mod runtime;
mod syntax;

pub use runtime::*;
pub use syntax::*;

use crate::RuleGroup;

pub trait ASCAError: Clone {
    fn get_error_message(&self) -> String;
    // Fixme: This really isn't the correct solution
    fn format_word_error(&self, _: &[String]) -> String;
    fn format_rule_error(&self, _: &[RuleGroup]) -> String;
    fn format_alias_error(&self, _: &[String], _: &[String]) -> String;
}

#[derive(Debug, Clone)]
pub enum Error {
    WordSyn(WordSyntaxError),
    RuleSyn(RuleSyntaxError),
    WordRun(WordRuntimeError),
    RuleRun(RuleRuntimeError),
    AliasSyn(AliasSyntaxError),
    AliasRun(AliasRuntimeError),
}

impl ASCAError for Error {
    fn get_error_message(&self) -> String {
        match self {
            Self::WordSyn(e) => e.get_error_message(),
            Self::RuleSyn(e) => e.get_error_message(),
            Self::WordRun(e) => e.get_error_message(),
            Self::RuleRun(e) => e.get_error_message(),
            Self::AliasSyn(e) => e.get_error_message(),
            Self::AliasRun(e) => e.get_error_message(),
        }
    }

    fn format_word_error(&self, s: &[String]) -> String {
        match self {
            Self::WordSyn(e) => e.format_word_error(s),
            Self::WordRun(e) => e.format_word_error(s),
            _ => unreachable!(),
        }
    }

    fn format_rule_error(&self, s: &[RuleGroup]) -> String {
        match self {
            Self::RuleSyn(e) => e.format_rule_error(s),
            Self::RuleRun(e) => e.format_rule_error(s),
            _ => unreachable!(),
        }
    }

    fn format_alias_error(&self, into: &[String], from: &[String]) -> String {
        match self {
            Self::AliasSyn(e) => e.format_alias_error(into, from),
            Self::AliasRun(e) => e.format_alias_error(into, from),
            _ => unreachable!(),
        }
    }
}

const FEAT_VARIANTS: [&str; 171] = [
    "root",
    "rut",
    "rt",
    "consonantal",
    "consonant",
    "cons",
    "cns",
    "sonorant",
    "sonor",
    "son",
    "snrt",
    "sn",
    "syllabic",
    "syllab",
    "syll",
    "syl",
    // Manner Node Features
    "manner",
    "mann",
    "man",
    "mnnr",
    "mnr",
    "continuant",
    "contin",
    "cont",
    "cnt",
    "approximant",
    "approx",
    "appr",
    "app",
    "lateral",
    "latrl",
    "ltrl",
    "lat",
    "nasal",
    "nsl",
    "nas",
    "delayedrelease",
    "delrel",
    "d.r.",
    "del.rel.",
    "delayed",
    "dl",
    "dlrl",
    "dr",
    "delay",
    "del.rel",
    "drel",
    "strident",
    "strid",
    "stri",
    "stridnt",
    "rhotic",
    "rhot",
    "rho",
    "rhtc",
    "rh",
    "click",
    "clik",
    "clk",
    "clck",
    // Laryngeal Node Features
    "laryngeal",
    "laryng",
    "laryn",
    "lar",
    "voice",
    "voi",
    "vce",
    "vc",
    "spreadglottis",
    "spreadglot",
    "spread",
    "s.g.",
    "s.g",
    "sg",
    "constrictedglottis",
    "constricted",
    "constglot",
    "constr",
    "c.g.",
    "c.g",
    "cg",
    // Place Node Feature
    "place",
    "plce",
    "plc",
    // Labial Place Node Features
    "labial",
    "lbl",
    "lab",
    "labiodental",
    "ldental",
    "labiodent",
    "labdent",
    "lbdntl",
    "ldent",
    "ldl",
    "round",
    "rund",
    "rnd",
    "rd",
    // Coronal Place Node Features
    "coronal",
    "coron",
    "crnl",
    "cor",
    "anterior",
    "anter",
    "antr",
    "ant",
    "distributed",
    "distrib",
    "dist",
    "dis",
    "dst",
    // Dorsal Place Node Features
    "dorsal",
    "drsl",
    "dors",
    "dor",
    "front",
    "frnt",
    "fnt",
    "fro",
    "frt",
    "fr",
    "back",
    "bck",
    "bk",
    "high",
    "hgh",
    "hi",
    "low",
    "lw",
    "lo",
    "tense",
    "tens",
    "tns",
    "ten",
    "reduced",
    "reduc",
    "redu",
    "rdcd",
    "red",
    // Pharyngeal Place Node Features
    "pharyngeal",
    "pharyng",
    "pharyn",
    "phar",
    "phr",
    "advancedtongueroot",
    "a.t.r.",
    "a.t.r",
    "a.tr",
    "at.r",
    "atr",
    "retractedtongueroot",
    "r.t.r.",
    "r.t.r",
    "r.tr",
    "rt.r",
    "rtr",
    // Suprasegmental Features
    "long",
    "lng",
    "overlong",
    "overlng",
    "ovrlng",
    "vlng",
    "stress",
    "str",
    "secondarystress",
    "sec.stress",
    "secstress",
    "sec.str.",
    "sec.str",
    "secstr",
    "sec",
];

fn get_feat_closest(s: &str) -> &'static str {
    let mut best_lev = usize::MAX;
    let mut best_str = "";

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

    if a == b {
        return dist;
    }

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
