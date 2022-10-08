use std::{
    collections::HashMap, 
    fmt, cmp::max
};

use crate ::{
    parser::{Item, ParseKind}, 
    error ::RuntimeError, 
    word  ::Word
};

pub struct SubRule {
    input:   Vec<Item>,
    output:  Vec<Item>,
    context: Item,         
    except:  Item,
    rule_type: u8,
    variables: HashMap<usize, Item>,
}


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

    pub fn split_into_subrules(&self) -> Result<Vec<SubRule>, RuntimeError> {
        // check that input, output, context, except are the same length
        // and if any are not, that they are len == 1

        let max = max(self.input.len(), max(self.output.len(), max(self.context.len(), self.except.len())));

        if self.input.len()   != max || self.input.len()   != 1 { return Err(RuntimeError::UnbalancedRule) }
        if self.output.len()  != max || self.output.len()  != 1 { return Err(RuntimeError::UnbalancedRule) }
        if self.context.len() != max || self.context.len() != 1 { return Err(RuntimeError::UnbalancedRule) }
        if self.except.len()  != max || self.except.len()  != 1 { return Err(RuntimeError::UnbalancedRule) }

        let mut sr_vec = Vec::new();

        for i in 0..max {
            let input   = if self.input.len()  == 1 { self.input[0].clone() }  else { self.input[i].clone() };
            let output  = if self.output.len() == 1 { self.output[0].clone() } else { self.output[i].clone() };
            let context = if self.context.len() == 1 { self.context[0].clone() } else { self.context[i].clone() };
            let except  = if self.except.len()  == 1 { self.except[0].clone() }  else { self.except[i].clone() };
            let rule_type = self.rule_type.clone();
            let variables = self.variables.clone();

            sr_vec.push(SubRule {input, output, context, except, rule_type, variables});
        }

        Ok(sr_vec)
    }

    pub fn apply(&self, word: Word /*, trace: bool*/) -> Result<String, RuntimeError> /* return Word */{
        // todo!();
        let out_word = word.clone(); 

        let sub_rules = self.split_into_subrules()?;

        for i in sub_rules {
            // find input 
            // match except left/right
            // match context left/right
            // apply
        }

        // match self.rule_type {
        //     0 => {},
        //     1 => {},
        //     2 => {},
        //     4 => {},
        //     8 => {},
        //     _ => unreachable!("Malformed Rule Type: {}", self.rule_type)
        // }

        match self.find_input_initial(out_word) {
            Some((m, n)) => {
                if m == n { println!("Match at {}", m); } 
                else { println!("Match between {}:{}", m, n); }
            },
            None => println!("No Match")
        }

        Ok("test".to_string())

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