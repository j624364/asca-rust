use std::{
    collections::HashMap, 
    fmt
};

use crate ::{
    parser::{Item, ParseKind}, 
    error ::RuntimeError, 
    word  ::Word
};


#[derive(Debug)]
pub struct Rule {
    pub input:     Vec<Vec<Item>>,    // to support multi-rules
    pub output:    Vec<Vec<Item>>,    // these need to be Vec<Vec<Item>>
    pub context:   Vec<Item>,         
    pub except:    Vec<Item>,    
    pub rule_type: u8, // bitmap 8 = insert_rule, 4 = redup_rule, 2 = del_rule, 1 = metath_rule, 0 = substi_rule
    pub variables: HashMap<usize,Item>,    
}                                   

impl Rule {
    pub fn new(i: Vec<Vec<Item>>, o: Vec<Vec<Item>>, c :Vec<Item>, e :Vec<Item>, r: u8, v: HashMap<usize, Item>) -> Self {
        Self { input: i, output: o, context: c, except: e , rule_type: r, variables: v}
    }

    pub fn apply(&self, word: Word /*, trace: bool*/) -> Result<String, RuntimeError> /* return Word */{
        // todo!();
        let out_word = word.clone(); 

        match self.rule_type {
            0 => {},
            1 => {},
            2 => {},
            4 => {},
            8 => {},
            _ => unreachable!("Malformed Rule Type: {}", self.rule_type)
        }

        match self.find_input_initial(out_word) {
            Some((m, n)) => {
                if m == n { println!("Match at {}", m); } 
                else { println!("Match between {}:{}", m, n); }
            },
            None => println!("No Match")
        }

        Ok("test".to_string())

        // find input 
        // match except left/right
        // match context left/right
        // apply


    }

    fn find_input_initial(&self, word: Word) -> Option<(usize, usize)> {

        
        for x in &self.input[0] {
            println!("{}", x);

            // syllable will have to be dealt with separately up here

            
            let mut i = 0;
            for seg in &word.segments {

                if let ParseKind::IPA(s, params) = &x.kind {
                    // todo: deal with modifiers
                    if seg.matrix == *s {
                        return Some((i, i));
                    }
                } else if let ParseKind::Matrix(params) = &x.kind {
                    // come up with way to match matrix with ipa
                    // self.match_matrix_ipa(params, seg)
                } else if let ParseKind::Set(set) = &x.kind {

                } else if let ParseKind::Optional(opts, l, h) = &x.kind {

                } else if let ParseKind::Variable(ident, params) = &x.kind {

                }

                i+=1;

            }
        }
    

        None
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        match self.rule_type {
            8 => writeln!(f, "Insertion Rule ->")?,
            4 => writeln!(f, "Reduplication Rule ->")?,
            2 => writeln!(f, "Deletion Rule ->")?,
            1 => writeln!(f, "Metathesis Rule ->")?,
            _ => writeln!(f, "Rule ->")?
        }

        writeln!(f, "    Input = [")?;
        self.input.iter().for_each(|i| {
            println!("        {:?}", i);
        });
        writeln!(f, "    ]")?;

        writeln!(f, "    Output = [")?;
        self.output.iter().for_each(|o| {
            println!("        {:?}", o);
        });
        writeln!(f, "    ]")?;

        writeln!(f, "    Context = [")?;
        self.context.iter().for_each(|c| {
            println!("        {}", c);
        });
        writeln!(f, "    ]")?;

        writeln!(f, "    Exception = [")?;
        self.except.iter().for_each(|e| {
            println!("        {}", e);
        });
        writeln!(f, "    ]")?;


        Ok(())
    }
}