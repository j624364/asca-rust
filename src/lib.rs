mod lexer;
mod trie;
mod parser;
mod word;
mod syll;
mod seg;
mod rule;
pub mod error;
mod subrule;

use serde::Deserialize;
use std::collections::HashMap;
use lazy_static::lazy_static;
use wasm_bindgen::prelude::*;

use lexer ::*;
use parser::*;
use trie  ::*;
use word  ::*;
use seg   ::*;
use rule  ::*;
use error ::*;

const CARDINALS_FILE: &str = include_str!("cardinals.json");
const DIACRITIC_FILE: &str = include_str!("diacritics.json");
lazy_static! {
    static ref CARDINALS_MAP: HashMap<String, Segment> = serde_json::from_str(CARDINALS_FILE).unwrap();
    static ref DIACRITS: Vec<Diacritic> = {
        // this seems very unnecessary, but I don't know enough about serde
        // at least it works
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
        pub enum DiaFeatType {
            Root, Manner, Laryngeal, Place, Labial, Coronal, Dorsal, Pharyngeal, 
            /*RUT*/ Consonantal, Sonorant, Syllabic,      
            /*MAN*/ Continuant, Approximant, Lateral, Nasal, DelayedRelease, Strident, Rhotic, Click,          
            /*LAR*/ Voice, SpreadGlottis, ConstrGlottis,   
            /*LAB*/ Labiodental, Round,          
            /*COR*/ Anterior, Distributed,     
            /*DOR*/ Front, Back, High, Low, Tense, Reduced,        
            /*PHR*/ AdvancedTongueRoot, RetractedTongueRoot, 
        }
        
        #[derive(Deserialize)]
        struct DT {
            pub name: String,
            pub diacrit: char,
            pub prereqs: Option<HashMap<DiaFeatType, bool>>,
            pub payload: Option<HashMap<DiaFeatType, bool>>,
        }

        impl DT {
            pub fn hm_to_mod(&self, hm: &Option<HashMap<DiaFeatType, bool>>) -> DiaMods {
                let mut args = DiaMods::new();
                // if hm.is_none() {return args};
                let Some(s) = hm else {return args};
                for (key, value) in s.iter() {
                    let x = *key as usize;
                    match value {
                        true =>{
                            if x > 7 { args.feats[x - 8] = Some(ModKind::Binary(BinMod::Positive)) }
                            else { args.nodes[x] = Some(ModKind::Binary(BinMod::Positive)) };
                        },
                        false => {
                            if x > 7 { args.feats[x - 8] = Some(ModKind::Binary(BinMod::Negative)) } 
                            else { args.nodes[x] = Some(ModKind::Binary(BinMod::Negative)) };
                        }
                    }
                }
                args
            }

            pub fn to_diacritic(&self) ->  Diacritic {
                Diacritic { 
                    name: self.name.clone(), 
                    diacrit: self.diacrit, 
                    prereqs: self.hm_to_mod(&self.prereqs), 
                    payload: self.hm_to_mod(&self.payload)
                }
            }
        }

        let dt: Vec<DT> = serde_json::from_str(DIACRITIC_FILE).unwrap();

        dt.iter().map(|x| x.to_diacritic()).collect()
    };
    static ref CARDINALS_VEC: Vec<String> = {
        let mut m = Vec::new();
        CARDINALS_MAP.iter().for_each(|(k,_)| {
            m.push(k.clone());
        });

        m
    };
    static ref CARDINALS_TRIE: Trie = {
        let mut m = Trie::new();
        CARDINALS_MAP.iter().for_each(|(k,_)| {
            m.insert(k.as_str());
        });

        m
    };    
}


#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct RuleGroup {
    pub name: String,
    pub rule: Vec<String>,
    pub description: String, 
}

impl RuleGroup {
    pub fn new() -> Self {
        Self { name: String::new(), rule: Vec::new(), description: String::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.name.is_empty() && self.rule.is_empty() && self.description.is_empty()
    }
}

impl Default for RuleGroup {
    fn default() -> Self {
        Self::new()
    }
}


// We are only normalising a few characters as most would be invalid anyway.
pub(crate) fn normalise(s: &str) -> String {
    s.replace('ã', "ã")
     .replace('ẽ', "ẽ")
     .replace('ĩ', "ĩ")
     .replace('õ', "õ")
     .replace('ũ', "ũ")
     .replace('ỹ', "ỹ")
     .replace('ℇ', "ɛ")
     .replace('ꭤ', "ɑ")
     .replace('ǝ', "ə")
     .replace('ɚ', "ə˞")
     .replace('ɝ', "ɜ˞")
     // cause why not?
     .replace('ℎ', "h")
     .replace('ℏ', "ħ")
     .replace('ﬁ', "fi")
     .replace('ﬂ', "fl")
     .replace('ĳ', "ij")
     .replace('ǌ', "nj")
     .replace('ǉ', "lj")
     // .replace('ǳ', "d͡z")
     // .replace('ǆ', "d͡ʒ")
}

fn apply_rule_groups(rules: &[Vec<Rule>], words: &[Word]) -> Result<Vec<Word>, Error> {
    let mut transformed_words: Vec<Word> = vec![];

    for word in words {
        let mut res_word = word.clone();

        for rule_group in rules.iter() {
            for rule in rule_group {
                res_word = rule.apply(res_word)?;
            }
        }
        transformed_words.push(res_word);
    }

    Ok(transformed_words)
}

fn words_to_string(words: &[Word]) -> Result<Vec<String>, WordRuntimeError> {
    let mut wrds_str: Vec<String> = vec![];
    for (i, w) in words.iter().enumerate() {
        match w.render() {
            Ok(r) => wrds_str.push(r),
            Err((b, j)) => return Err(WordRuntimeError::UnknownSegment(b,i,j)),
        }
    }
    Ok(wrds_str)
}

fn parse_words(unparsed_words: &[String]) -> Result<Vec<Word>,WordSyntaxError> {
    let mut words: Vec<Word> = vec![];
    for w in unparsed_words {
        words.push(Word::new(normalise(w))?);
    }
    Ok(words)
}

fn parse_rule_groups(unparsed_rule_groups: &[RuleGroup]) -> Result<Vec<Vec<Rule>>, RuleSyntaxError> {
    let mut rule_groups = vec![];

    for (rgi, rg) in unparsed_rule_groups.iter().enumerate() {
        let mut rule_group = vec![];
        for (ri, r) in rg.rule.iter().enumerate() {
            if let Some(asdf) = Parser::new(Lexer::new(&r.chars().collect::<Vec<_>>(), rgi, ri).get_line()?, rgi, ri).parse()? {
                rule_group.push(asdf);
            }
        }
        rule_groups.push(rule_group);
    }

    Ok(rule_groups)
}

// fn run_web(unparsed_rules: &[RuleGroup], unparsed_words: &[String]) -> Result<Vec<String>, Error> {
//     let words = parse_words(unparsed_words)?;
//     let rules = parse_rule_groups(unparsed_rules)?;

//     let res = apply_rule_groups(&rules, &words)?;
    
//     Ok(words_to_string(&res)?)
// }

pub fn run(unparsed_rules: &[RuleGroup], unparsed_words: &[String]) -> Result<Vec<String>, Error> {
    let words = parse_words(unparsed_words)?;
    let rules = parse_rule_groups(unparsed_rules)?;

    let res = apply_rule_groups(&rules, &words)?;
    
    Ok(words_to_string(&res)?)
}


#[wasm_bindgen]
pub fn run_asca(val: JsValue, unparsed_words: Vec<String>) -> Vec<String> {
    let unparsed_rules: Vec<RuleGroup> = serde_wasm_bindgen::from_value(val).expect("Rules are valid");

    parse_result_web(run(&unparsed_rules, &unparsed_words), &unparsed_rules, &unparsed_words)
}

fn parse_result_web(unparsed_result: Result<Vec<String>, Error>, rules: &[RuleGroup], words: &[String]) -> Vec<String> {
    let mut res = Vec::new();
    match unparsed_result {
        Ok(output) => {
            for o in output {
                res.push(o);
            }
        },
        Err(err) => match err {
            Error::WordSyn(e) => res.push(e.format_word_error(words)),
            Error::WordRun(e) => res.push(e.format_word_error(words)),
            Error::RuleSyn(e) => res.push(e.format_rule_error(rules)),
            Error::RuleRun(e) => res.push(e.format_rule_error(rules)),
        },
    }

    res
}
