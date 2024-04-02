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
// use subrule::*;

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
            /*LAB*/ Bilabial, Round,          
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
        // unless this is optimised away by the compiler, it is a source of potential slowdown
        // TODO(JAMES) Benchmark this
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

fn apply_rules(rules: &[Rule], words: &[Word], is_traced: bool) -> Result<(Vec<Word>, Vec<Vec<String>>), RuleRuntimeError> {
    // TODO: work out tracing
    // We should treat each "rule block" as one instead of treating each intermediate rule as separate
    // This will help with tracing
    let mut transformed_words: Vec<Word> = vec![];
    let mut traced_words: Vec<Vec<String>> = vec![];

    for (i, w) in words.iter().enumerate() {
        let mut wb = w.clone();
        if is_traced {
            traced_words.push(vec![]);
            traced_words[i].push(traced_word_to_string(wb.clone(), None));

        }

        for r in rules.iter() {
            wb = r.apply(wb.clone())?;

            if is_traced {
                let asdf = traced_word_to_string(wb.clone(), traced_words[i].last());
                traced_words[i].push(asdf);
            }
        }
        transformed_words.push(wb);
    }

    Ok((transformed_words, traced_words))
}

fn traced_word_to_string(word: Word, before: Option<&String>) -> String {

    let word_before = before.unwrap_or(&"()".to_string()).clone();
    
    match word.render() {
        Ok(res) => res,
        Err((buffer, _)) => format!("Err: {word_before} => {buffer}")
    }
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

fn run(unparsed_rules: &[String], unparsed_words: &[String], trace: bool) -> Result<(Vec<String>, Vec<Vec<String>>), Error> {
    let words = parse_words(unparsed_words)?;
    let rules = parse_rules(unparsed_rules)?;

    let (res, trace_res) = apply_rules(&rules, &words, trace)?;
    
    Ok((words_to_string(&res)?, trace_res))
}

fn deal_with_result(res: Result<(Vec<String>, Vec<Vec<String>>), Error>, rules: &[String], words: &[String]) {
    match res {
        Ok((output, _)) => {
            debug_assert_eq!(output.len(), words.len());
            for (w, o) in words.iter().zip(output.iter()) {
                println!("{} => {}", w, o);
            }
        },
        Err(err) => match err {
            Error::WordSyn(e) => println!("{}", e.format_error(words)),
            Error::WordRun(e) => println!("{}", e.format_error(words)),
            Error::RuleSyn(e) => println!("{}", e.format_error(rules)),
            Error::RuleRun(e) => println!("{}", e.format_error(rules)),
        },
    }
}

fn main() {
    let unparsed_rules: Vec<String> = vec![
        // String::from("C:[+d.r., -dr, -nas "),
        // String::from("rabol > &"),
        // String::from("oba > &"),
        // String::from("o > *"),
        // String::from("r...l > &"),
        // String::from("sk > &"),
        // String::from("rV > &"),
        // String::from("%% > &"),
        // String::from("% > *"),
        // String::from("* > {s, t}V"),
        // String::from("[+rho]=1 V=2 > 2 1  / _s	"),
        // String::from("%=1 > * / 1_"),
        // String::from("u > *"),
        // String::from("[-voice]$ > * / [-voice]_[-voice]"),
        // String::from("$s > & / #_{p,t,k}"),

        // String::from("t^ʃ > ʃ"),
        // String::from("t > *  / _#"),
        
        // String::from("a > e  / _V:[+hi] | _u"),
        // String::new(""),
        // String::from("* > b / m_r"),
        // String::from("$ > &"),
        // String::from("lm > &"),
        // String::from("V:[tone:32] > * / _#"),
        // String::from("* > *"),

        // String::from("%=1 > * / 1_"),
        // String::from("% > * / _trix"),

        // String::from("V > [+round] / _C[+round]"),

        // String::from("C:[+nasal] > [αplace] / _C:[αplace]"),

        // String::from("d > [αvoice] / _[αvoice]"),
        // String::from("g > [αvoice] / _[-α voice]"),

        // String::from("* > %:[+stress, tone: 213] / a_i"),

        String::from("* > o / _#"),
        String::from("V > [+frt, -back, -rnd] / _#"),
        String::from("V > [-voi] / _#"),
        // String::from("* > $ / _do#"),



    ];

    let unparsed_words: Vec<String> = vec![
        // String::from("pa'ra.bo.la"),
        // String::from("ask"),
        // String::from("hros"),
        // String::from("a.su.ka"),
        // String::from("om.re"),

        // String::from("t^ʃi:.tu"),
        // String::from("t^ʃat"),

        // String::from("samk"),
        // String::from("sang"),
        // String::from("sanp"),
        // String::from("sanf"),
        // String::from("sanq"),

        // String::from("sads"),
        // String::from("sagz"),

        String::from("sed"),

        // String::from("sa˦.ne˦"),

        // String::from("hap.lo.lo.gi"),
        // String::from("nu.tri.tri"),
        // String::from("tra.gi.co.co.mi.co"),
        // String::from("nar.si.si.zm"),
        // String::from("mor.fo.fo.no.lo.gi"),

        // String::from("spa.nja"),
        // String::from("'ga32,da32"),
        // String::from("ganda.en"),
    ];

    let trace = false;

    let res = run(&unparsed_rules, &unparsed_words, trace);

    deal_with_result(res, &unparsed_rules, &unparsed_words);
    
    
}