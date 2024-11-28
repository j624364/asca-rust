mod lexer;
mod trie;
mod parser;
mod word;
mod syll;
mod seg;
mod rule;
mod error;
mod subrule;

use serde::Deserialize;
use std::collections::HashMap;
use lazy_static::lazy_static;

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

fn parse_rules(unparsed_rules: &[String]) -> Result<Vec<Rule>,RuleSyntaxError> {
    let mut rules: Vec<Rule> = vec![];
    for (l, r) in unparsed_rules.iter().enumerate() {
        // FIXME(James): We are creating/dropping/reallocating the Lexer & Parser every loop
        rules.push(Parser:: new(Lexer::new(&r.chars().collect::<Vec<_>>(), l).get_line()?, l).parse()?);
    }
    Ok(rules)
}

fn parse_words(unparsed_words: &[String]) -> Result<Vec<Word>,WordSyntaxError> {
    let mut words: Vec<Word> = vec![];
    for w in unparsed_words {
        words.push(Word::new(w.clone())?);
    }
    Ok(words)
}

fn apply_rules(rules: &[Rule], words: &[Word], /*is_traced: bool*/) -> Result<Vec<Word>, Error> {
    // TODO: work out tracing
    // We should treat each "rule block" as one instead of treating each intermediate rule as separate
    // This will help with tracing
    let mut transformed_words: Vec<Word> = vec![];
    // let mut traced_words: Vec<Vec<String>> = vec![];

    for (_i, word) in words.iter().enumerate() {
        let mut res_word = word.clone();

        for rule in rules.iter() {
            res_word = rule.apply(res_word)?;
            // if is_traced {
            //     let asdf = traced_word_to_string(&res_word, traced_words[i].last().unwrap_or(&word.render().unwrap()));
            //     traced_words[i].push(asdf);
            // }
        }
        transformed_words.push(res_word);
    }

    // Ok((transformed_words, traced_words))
    Ok(transformed_words)
}

// fn traced_word_to_string(word: &Word, before: &String) -> String {
//     // let word_before = before.unwrap_or(&"()".to_string()).clone();
//     match word.render() {
//         Ok(res) => res,
//         Err((buffer, _)) => format!("Err: {before} => {buffer}")
//     }
// }

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

fn r(unparsed_rules: &[String], unparsed_words: &[String]) -> Result<Vec<String>, Error> {
    let words = parse_words(unparsed_words)?;
    let rules = parse_rules(unparsed_rules)?;

    let res = apply_rules(&rules, &words)?;
    
    Ok(words_to_string(&res)?)
}



pub fn run(unparsed_rules: &[String], unparsed_words: &[String]) -> Vec<String> {
    parse_result(r(unparsed_rules, unparsed_words), unparsed_rules, unparsed_words)
}

fn parse_result(unparsed_result: Result<Vec<String>, Error>, rules: &[String], words: &[String]) -> Vec<String> {
    let mut res = Vec::new();
    match unparsed_result {
        Ok(output) => {
            // for (w, o) in words.iter().zip(output) {
            for o in output {
                res.push(o);
            }
        },
        Err(err) => match err {
            Error::WordSyn(e) => res.push(e.format_error(words)),
            Error::WordRun(e) => res.push(e.format_error(words)),
            Error::RuleSyn(e) => res.push(e.format_error(rules)),
            Error::RuleRun(e) => res.push(e.format_error(rules)),
        },
    }

    res
}

// pub fn deal_with_result(res: Result<(Vec<String>, Vec<Vec<String>>), Error>, rules: &[String], words: &[String]) {
//     match res {
//         Ok((output, _)) => {
//             debug_assert_eq!(output.len(), words.len());
//             println!("\n--- OUTPUT ---");
//             for (w, o) in words.iter().zip(output.iter()) {
//                 println!("{} => {}", w, o);
//             }
//         },
//         Err(err) => match err {
//             Error::WordSyn(e) => println!("{}", e.format_error(words)),
//             Error::WordRun(e) => println!("{}", e.format_error(words)),
//             Error::RuleSyn(e) => println!("{}", e.format_error(rules)),
//             Error::RuleRun(e) => println!("{}", e.format_error(rules)),
//         },
//     }
// }

