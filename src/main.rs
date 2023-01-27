mod lexer;
mod trie;
mod parser;
mod word;
mod rule;
mod error;

use serde::Deserialize;
use std::collections::HashMap;
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
                            if x > 7 { args.feats[x - 8] = Some(SegMKind::Binary(BinMod::Positive)) }
                            else { args.nodes[x] = Some(SegMKind::Binary(BinMod::Positive)) };
                        },
                        false => {
                            if x > 7 { args.feats[x - 8] = Some(SegMKind::Binary(BinMod::Negative)) } 
                            else { args.nodes[x] = Some(SegMKind::Binary(BinMod::Negative)) };
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
        rules.push(Parser:: new(Lexer::new(r, l).get_all_tokens()?, l).parse()?);
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

fn apply_rules(rules: &[Rule], words: &[Word], is_traced: bool) -> Result<(Vec<Word>, Vec<Vec<String>>), RuntimeError> {
    // TODO: work out tracing
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
        Err((buffer, _)) => format!("Err: {} => {}", word_before, buffer)
    }
}

fn words_to_string(words: &[Word]) -> Result<Vec<String>, RuntimeError> {

    let mut wrds_str: Vec<String> = vec![];

    for (i, w) in words.iter().enumerate() {
        match w.render() {
            Ok(r) => wrds_str.push(r),
            Err((b, j)) => return Err(RuntimeError::UnknownSegment(b,i,j)),
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

    const MARG: &str = "\n    |     ";

    match res {
        Ok(_) => todo!(),
        Err(err) => match err {
            Error::WordSyn(_) => todo!(),
            Error::RuleSyn(e) => {
                use RuleSyntaxError::*;
                print!("{}{}", "Syntax Error".bright_red().bold(), format!(": {}", e).bold());
                match e {
                    OptMathError(t, _, _) | 
                    UnknownIPA(t) | 
                    UnknownGrouping(t) | 
                    UnknownVariable(t) | 
                    UnexpectedEol(t, _) | 
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

                        let arrows = " ".repeat(start) + &"^".repeat(end-start) + "\n";

                        println!("{}{}{}{}",  
                            MARG.bright_cyan().bold(), 
                            rules[line], 
                            MARG.bright_cyan().bold(), 
                            arrows.bright_red().bold()
                        )
                    },
                    UnknownFeature(_, line, start, end) => {
                        let arrows = " ".repeat(start) + &"^".repeat(end-start) + "\n";

                        println!("{}{}{}{}",  
                            MARG.bright_cyan().bold(), 
                            rules[line], 
                            MARG.bright_cyan().bold(), 
                            arrows.bright_red().bold()
                        )

                    },
                    UnknownCharacter(_, line, pos) => {
                        let arrows = " ".repeat(pos) + "^" + "\n";

                        println!("{}{}{}{}",  
                            MARG.bright_cyan().bold(), 
                            rules[line], 
                            MARG.bright_cyan().bold(), 
                            arrows.bright_red().bold()
                        )

                    },
                    BadVariableAssignment(_) |              // TODO: these should print itself in the line
                    AlreadyInitialisedVariable(_, _, _) |   // 
                    InsertErr | DeleteErr | EmptyInput | 
                    EmptyOutput | EmptyEnv => {},
                }
            },
            Error::Runtime(re) => match re.clone() {
                RuntimeError::UnbalancedRule => todo!(),
                RuntimeError::UnknownSegment(buffer, word, seg) => {
                    let arrows = " ".repeat(words[word].len() + seg) + "^" + "\n";

                    println!("{}{}{}{} => {}{}{}",  
                            "Runtime Error".bright_red().bold(),
                            format!(": {}", re).bold(), 
                            MARG.bright_cyan().bold(), 
                            words[word], 
                            buffer,
                            MARG.bright_cyan().bold(), 
                            arrows.bright_red().bold()
                        )
                },
            }
        },
    }

}

fn main() {
    let unparsed_rules: Vec<String> = vec![
        String::from("C:[+d.r., -dr, -nas "),
        String::from("r > l"),
    ];

    let unparsed_words: Vec<String> = vec![
        String::from("a.ri"),
    ];

    let res = run(&unparsed_rules, &unparsed_words, false);

    deal_with_result(res, &unparsed_rules, &unparsed_words);
    
    
}

// fn main2() {
//     // let test = String::from("[]...[] > &");
//     // let test = String::from("[+voi, -sg, αPLACE]...C > &");
//     // let test = String::from("V > [+long] / _C#");
//     // let test = String::from("%:[tone:214] > [tone:35] / _%:[tone:214] ");
//     // let test = String::from("t͡ɕ...b͡β > &");
//     // let test = String::from("r...V > &");
//     // let test = String::from("V:[+syll]...l > & / _,C");
//     // let test = String::from("C=1 V=2 > 2 1  / _C");
//     // let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");
    
//     // let w = Word::new("ˌna.kiˈsa".to_owned()).unwrap();
//     let w = Word::new("a.ki.ra".to_owned()).unwrap();
//     // let mut w = Word::new("ˌna.kiˈ:a".to_owned()).unwrap();
//     println!("{}", w);
    
//     let mut tokens;
//     const ITERS: u32 = 1;
    
//     let start  = Instant::now();
//     // let test= String::from("t͡ɕ...b͡β > &");
//     // let test= String::from("r > l");
//     let test = String::from("%:[+stress], % > [-stress], [+stress] / _ , #_ ");
//     let test = String::from("%:[+setr]");
//     let mut maybe_rule: Result<Rule, RuleSyntaxError> = Err(RuleSyntaxError::EmptyInput);

//     for _ in 0..ITERS {
        
//         let mut lex = Lexer::new(&test,0);
//         tokens = lex.get_all_tokens().unwrap();
    
//         // tokens.clone().into_iter().for_each(|t| {
//         //         println!("{}", t);
//         //     });
//         let mut parser = Parser:: new(tokens,0);

//         maybe_rule = parser.parse();
//     }

//     let dur = start.elapsed();
//     println!("\nTotal Time: {:?}", dur);
//     println!("Average Time per Iteration: {:?}\n", dur/ITERS);

// }