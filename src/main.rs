mod lexer;
mod trie;
mod parser;
mod word;
mod rule;
mod error;

use serde::Deserialize;
use std::{collections::HashMap, time::Instant};
use colored::Colorize;
use lazy_static::lazy_static;

use lexer ::*;
use parser::*;
use trie  ::*;
use word  ::*;
use rule  ::*;
use error ::*;

const CARDINALS_FILE: &str = include_str!("cardinals.json");
const DIACRITIC_FILE: &str = include_str!("diacritics.json");
lazy_static! {
    static ref CARDINALS_MAP: HashMap<String, Segment> = serde_json::from_str(CARDINALS_FILE).unwrap();
    static ref DIACRITS: Vec<Diacritic> = {
        // this seems unnecessary, but at least it works
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
        pub enum DiaFeatType {
            Root,      
            Manner,
            Laryngeal,   
            Place,      
            Labial,      
            Coronal,     
            Dorsal,      
            Pharyngeal, 
            // RooT Node
            Consonantal,
            Sonorant,      
            Syllabic,      
            // MANNER node 
            Continuant,      
            Approximant,     
            Lateral,         
            Nasal,           
            DelayedRelease,  
            Strident,        
            Rhotic,          
            Click,          
            // LAR node
            Voice,           
            SpreadGlottis,   
            ConstrGlottis,   
            // PLACE Node
            // LABIAL subnode
            Bilabial,      
            Round,          
            // CORONAL subnode
            Anterior,        
            Distributed,     
            // DORSAL subnode
            Front,          
            Back,           
            High,           
            Low,            
            Tense,          
            Reduced,        
            // PHAR subnode
            AdvancedTongueRoot,
            RetractedTongueRoot, 
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

                if hm.is_none() {return args};
                
                let hm = hm.clone().unwrap();

                for (key, value) in hm.iter() {
                    match value {
                        true =>{
                            // DiaFeatType::Root => args.nodes[*n as usize] = Some(SegMKind::Binary(BinMod::Positive)),
                            // DiaFeatType::Feat(f) => args.feats[*f as usize] = Some(SegMKind::Binary(BinMod::Positive)),

                            let x = *key as usize;
                            if x > 7 {
                                args.feats[x - 8] = Some(SegMKind::Binary(BinMod::Positive))
                            } else { 
                                args.nodes[x] = Some(SegMKind::Binary(BinMod::Positive))
                            };
                            

                        },
                        false => {
                            let x = *key as usize;
                            if x > 7 {
                                args.feats[x - 8] = Some(SegMKind::Binary(BinMod::Negative))
                            } else { 
                                args.nodes[x] = Some(SegMKind::Binary(BinMod::Negative))
                            };
                        },
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

fn parse_rules(unparsed_rules: &Vec<String>) -> Result<Vec<Rule>,RuleSyntaxError> {
    
    let mut rules: Vec<Rule> = vec![];
    for (l, r) in unparsed_rules.iter().enumerate() {
        rules.push(Parser:: new(Lexer::new(r, l).get_all_tokens()?, l).parse()?);
    }

    Ok(rules)
}

fn parse_words(unparsed_words: &Vec<String>) -> Result<Vec<Word>,WordSyntaxError> {
    
    let mut words: Vec<Word> = vec![];
    for w in unparsed_words {
        words.push(Word::new(w.clone())?);
    }

    Ok(words)
}

fn apply_rules(rules: Vec<Rule>, words: Vec<Word>, trace: bool) -> Result<(Vec<Word>, Vec<Vec<String>>), RuntimeError> {
    // TODO: work out tracing

    let mut transformed_words: Vec<Word> = vec![];

    let mut traced_words: Vec<Vec<String>> = vec![];

    for (i, w) in words.iter().enumerate() {
        let mut wb = w.clone();

        if trace {
            traced_words.push(vec![]);
            traced_words[i].push(word_to_string(wb.clone())?);

        }

        for r in &rules {
            wb = r.apply(wb.clone())?;

            if trace {
                traced_words[i].push(word_to_string(wb.clone())?);
            }
        }
        transformed_words.push(wb);
    }

    Ok((transformed_words, traced_words))
}

fn word_to_string(word: Word) -> Result<String, RuntimeError> {
    todo!();
}

fn words_to_string(words: Vec<Word>) -> Result<Vec<String>, RuntimeError> {

    let mut wrds_str: Vec<String> = vec![];

    for w in words {
        wrds_str.push(word_to_string(w)?);
    }

    Ok(wrds_str)
}

fn run(unparsed_rules: &Vec<String>, unparsed_words: &Vec<String>, trace: bool) -> Result<(Vec<String>, Vec<Vec<String>>), Error> {
    let words = parse_words(unparsed_words)?;
    let rules = parse_rules(unparsed_rules)?;

    let (res, trace_res) = apply_rules(rules, words, trace)?;

    Ok((words_to_string(res)?, trace_res))

}

fn deal_with_result(res: Result<(Vec<String>, Vec<Vec<String>>), Error>, rules: &Vec<String>, words: &Vec<String>) {
    match res {
        Ok(_) => todo!(),
        Err(err) => match err {
            Error::WordSyn(_) => todo!(),
            Error::RuleSyn(e) => {
                use RuleSyntaxError::*;
                match e.clone() {
                    OptMathError(t, _, _) | 
                    UnknownIPA(t) | 
                    UnknownGrouping(t) | 
                    UnknownVariable(t) | 
                    ExpectedEndL(t) | 
                    ExpectedArrow(t) | 
                    ExpectedComma(t) | 
                    ExpectedColon(t) | 
                    ExpectedMatrix(t) | 
                    ExpectedSegment(t) | 
                    ExpectedFeature(t) | 
                    ExpectedVariable(t) | 
                    ExpectedUnderline(t) | 
                    ExpectedRightBracket(t) |
                    BadSyllableMatrix(t)  => {
                        let line = t.position.line;
                        let start = t.position.start;
                        let end = t.position.end;

                        let marg = "\n    |     ";
                        let arrs = " ".repeat(start) + &"^".repeat(end-start) + "\n";

                        println!("{}{}{}{}{}{}",  
                            "Syntax Error".bright_red().bold(),
                            format!(": {}", e).bold(), 
                            marg.bright_cyan().bold(), 
                            rules[line], 
                            marg.bright_cyan().bold(), 
                            arrs.bright_red().bold()
                        )
                    },
                    UnknownFeature(_, line, start, end) => {
                        let marg = "\n    |     ";
                        let arrs = " ".repeat(start) + &"^".repeat(end-start) + "\n";

                        println!("{}{}{}{}{}{}",  
                            "Syntax Error".bright_red().bold(),
                            format!(": {}", e).bold(), 
                            marg.bright_cyan().bold(), 
                            rules[line], 
                            marg.bright_cyan().bold(), 
                            arrs.bright_red().bold()
                        )

                    },
                    UnknownCharacter(_, line, pos) => {
                        let marg = "\n    |     ";
                        let arrs = " ".repeat(pos) + "^" + "\n";

                        println!("{}{}{}{}{}{}",  
                            "Syntax Error".bright_red().bold(),
                            format!(": {}", e).bold(), 
                            marg.bright_cyan().bold(), 
                            rules[line], 
                            marg.bright_cyan().bold(), 
                            arrs.bright_red().bold()
                        )

                    },
                    _ => println!("{}{}", "Error".to_string().red().bold(), format!(": {}", e).bold())
                }
            },
            Error::Runtime(_) => todo!(),
        },
    }

}

fn main() {
    let unparsed_rules: Vec<String> = vec![
        String::from("X:[+long, +setr, -nas]"),
        String::from("r > l"),
    ];

    let unparsed_words: Vec<String> = vec![
        String::from("a.ri"),
    ];

    let res = run(&unparsed_rules, &unparsed_words, false);

    deal_with_result(res, &unparsed_rules, &unparsed_words);
    
    
}

fn main2() {
    // let test = String::from("[]...[] > &");
    // let test = String::from("[+voi, -sg, αPLACE]...C > &");
    // let test = String::from("V > [+long] / _C#");
    // let test = String::from("%:[tone:214] > [tone:35] / _%:[tone:214] ");
    // let test = String::from("t͡ɕ...b͡β > &");
    // let test = String::from("r...V > &");
    // let test = String::from("V:[+syll]...l > & / _,C");
    // let test = String::from("C=1 V=2 > 2 1  / _C");
    // let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");
    
    // let w = Word::new("ˌna.kiˈsa".to_owned()).unwrap();
    let w = Word::new("a.ki.ra".to_owned()).unwrap();
    // let mut w = Word::new("ˌna.kiˈ:a".to_owned()).unwrap();
    println!("{}", w);
    
    let mut tokens;
    const ITERS: u32 = 1;
    
    let start  = Instant::now();
    // let test= String::from("t͡ɕ...b͡β > &");
    // let test= String::from("r > l");
    let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");
    let test = String::from("%:[+setr]");
    let mut maybe_rule: Result<Rule, RuleSyntaxError> = Err(RuleSyntaxError::EmptyInput);

    for _ in 0..ITERS {
        
        let mut lex = Lexer::new(&test,0);
        tokens = lex.get_all_tokens().unwrap();
    
        // tokens.clone().into_iter().for_each(|t| {
        //         println!("{}", t);
        //     });
        let mut parser = Parser:: new(tokens,0);

        maybe_rule = parser.parse();
    }

    let dur = start.elapsed();
    println!("\nTotal Time: {:?}", dur);
    println!("Average Time per Iteration: {:?}\n", dur/ITERS);

}