use std::{
    collections::{HashMap, VecDeque}, 
    fmt, cmp::max
};

use crate ::{
    parser::{Item, ParseKind, Modifiers}, 
    error ::RuntimeError, 
    word  ::{Word, Segment}
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RuleType {
    Substitution,
    Metathesis,
    Deletion,
    // Reduplication,
    Insertion,

}

// struct BacktrackState {
//     can_backtrack: bool,
//     state: Item,
//     consumptions: Option<usize>
// }

#[derive(Debug)]
pub struct SubRule {
    input:   Vec<Item>,
    output:  Vec<Item>,
    context: Option<Item>,         
    except:  Option<Item>,
    rule_type: RuleType, // RuleType,
    variables: HashMap<usize, Item>,
}

impl SubRule {
    fn apply(&mut self, word: Word) -> Result<Word, RuntimeError> {
        // find input, if no match early return
        // match except left/right, if no match early return
        // match context left/right, if no match early return
        // apply
        // syllable re-match/repair
        
        // let Some((match_pos, match_len)) = self.match_input(&word)? else {
        //     return Ok(word)
        // };


        todo!()
    }

    // fn match_input(&mut self, word: &Word) -> Result<Option<(usize, usize)>, RuntimeError> {
    //     assert!(!self.input.is_empty());
    //     assert!(!word.segments.is_empty());

    //     let mut match_pos = 0usize;
    //     let mut matched = false;
    //     let max_match_pos = word.syllables.len() - 1;
        
    //     while !matched && match_pos < max_match_pos {
    //         let (matched, match_length, _) = self.match_x(word, match_pos, None)?;
    //         if matched {
    //             return Ok(Some((match_pos, match_length)))
    //         }
    //         match_pos += 1;
            
    //     }

    //     Ok(None)
    // }

    // fn match_x(&mut self, word: &Word, match_pos: usize, match_length: usize) -> Result<(bool, usize, usize), RuntimeError> {
    //     todo!();
    //     if match_pos == word.segments.len() - 1 {
    //         return Ok(Some((true, match_length, None)))
    //     }

    //     for 
    // }

    fn match_seg_at_pos(&mut self, item: Item, seg: Segment) -> Result<bool, RuntimeError> {
        match item.kind {
            ParseKind::EmptySet => todo!(),
            ParseKind::WordBound => todo!(),
            ParseKind::SyllBound => todo!(),
            ParseKind::Ellipsis => todo!(),
            ParseKind::Metathesis => todo!(),
            ParseKind::Variable(_, _) => todo!(),
            ParseKind::Ipa(s, m) => if m.is_none(){
                if s == seg {
                    return Ok(true)
                }
            } else {
                // TODO: compare modifiers
            },
            ParseKind::Matrix(_) => todo!(),
            ParseKind::Syllable(_, _) => todo!(),
            ParseKind::Set(_) => todo!(),
            ParseKind::Optional(_, _, _) => todo!(),
            ParseKind::Environment(_, _) => todo!(),
        }

        Ok(false)
    }
}

pub struct Rule {
    pub input:     Vec<Vec<Item>>,    // to support multirules
    pub output:    Vec<Vec<Item>>,    // these need to be Vec<Vec<Item>>
    pub context:   Vec<Item>,         
    pub except:    Vec<Item>,    
    pub rule_type: RuleType, // bitmap 8 = insert_rule, 4 = redup_rule, 2 = del_rule, 1 = metath_rule, 0 = substi_rule
    pub variables: HashMap<usize,Item>,    
}   // todo: if we move rule_type to SubRule, that would allow us to have multirules with insert/delete/metath

impl Rule {
    pub fn new(i: Vec<Vec<Item>>, o: Vec<Vec<Item>>, c :Vec<Item>, e :Vec<Item>, r: RuleType, v: HashMap<usize, Item>) -> Self {
        Self { input: i, output: o, context: c, except: e , rule_type: r, variables: v}
    }

    pub fn split_into_subrules(&self) -> Result<Vec<SubRule>, RuntimeError> {
        // check that input, output, context, except are the same length
        // and if any are not, that they are length == 1
        // context and except can be length == 0
        let max = max(self.input.len(), max(self.output.len(), max(self.context.len(), self.except.len())));

        if self.input.len()   != max && self.input.len()   != 1 { return Err(RuntimeError::UnbalancedRule) }
        if self.output.len()  != max && self.output.len()  != 1 { return Err(RuntimeError::UnbalancedRule) }
        if self.context.len() != max && self.context.len() != 1 && !self.context.is_empty() { return Err(RuntimeError::UnbalancedRule) }
        if self.except.len()  != max && self.except.len()  != 1 && !self.except.is_empty() { return Err(RuntimeError::UnbalancedRule) }

        // populate subrules, if one if length==1 then it's value is duplicated to rest of subrules
        let mut sub_vec = Vec::new();
        for i in 0..max {
            let input   = if self.input.len()  == 1 { self.input[0].clone() }  else { self.input[i].clone() };
            let output  = if self.output.len() == 1 { self.output[0].clone() } else { self.output[i].clone() };
            let context = if self.context.is_empty() { None } else if self.context.len() == 1 { Some(self.context[0].clone()) } else { Some(self.context[i].clone()) };
            let except  = if self.except.is_empty()  { None } else if self.except.len()  == 1 { Some(self.except[0].clone()) }  else { Some(self.except[i].clone()) };
            let rule_type = self.rule_type;  // TODO: calc rule_type here instead of in parser
            let variables = self.variables.clone();

            sub_vec.push(SubRule {input, output, context, except, rule_type, variables});
        }

        Ok(sub_vec)
    }

    pub fn apply(&self, word: Word /*, trace: bool*/) -> Result<Word, RuntimeError> /* return Word */{
        
        let sub_rules = self.split_into_subrules()?;
        
        let mut res_word = word; 
        for mut i in sub_rules {
            res_word = i.apply(res_word)?;

            println!("{:#?} ---> {:#?}", i, res_word);
        }

        Ok(res_word) // TODO: return new word

    }

    // fn state_matches_ipa_at_index(&self, seg: &Segment, mods: &Modifiers, word: Word, i: usize) -> (bool, usize) {
    //     todo!()
    // }

    // fn find_input(&self, states: Vec<Item>, word: Word) -> (bool, usize, usize) {
    //     let mut queue = VecDeque::from(states);
    //     let mut i = 0;
    //     let mut backtrack_stack: Vec<BacktrackState> = Vec::new();
    //     let mut curr_state = queue.pop_front();
    //
    //     fn backtrack() -> bool {
    //         todo!()
    //     }
    //
    //     while curr_state.is_some() {
    //         match curr_state.clone().unwrap().kind {
    //             ParseKind::Ellipsis => { // NOTE: this will probably not work
    //                 let (is_match, consumed) = if i >= word.segments.len() {
    //                     (false, 0)
    //                 } else {
    //                     (true, 1)
    //                 };
    //
    //                 if !is_match {
    //                     backtrack_stack.push(BacktrackState {
    //                         can_backtrack: true,
    //                         state: curr_state.clone().unwrap(),
    //                         consumptions: Some(consumed),
    //                     });
    //                     curr_state = queue.pop_front();
    //                     continue;
    //                 }
    //
    //                 backtrack_stack.push(BacktrackState {
    //                     can_backtrack: true,
    //                     state: curr_state.clone().unwrap(),
    //                     consumptions: Some(consumed),
    //                 });
    //
    //                 i += consumed;
    //
    //             },
    //             ParseKind::Variable(_, _) => todo!(),
    //             ParseKind::Ipa(ref s, ref m) => {
    //                 let (is_match, consumed) = self.state_matches_ipa_at_index(s, m, word.clone(), i);
    //
    //                 if !is_match {
    //                     let index_before_backtrack = i;
    //                     if !backtrack() {
    //                         return (false, index_before_backtrack, index_before_backtrack)
    //                     }
    //                     continue;
    //                 }
    //
    //                 backtrack_stack.push(BacktrackState {
    //                     can_backtrack: false,
    //                     state: curr_state.clone().unwrap(),
    //                     consumptions: Some(consumed),
    //                 });
    //
    //                 continue;
    //             },
    //             ParseKind::Matrix(_) => todo!(),
    //             ParseKind::Syllable(_,_) => todo!(),
    //             ParseKind::Set(_) => todo!(),
    //             ParseKind::Optional(_, _, _) => todo!(),
    //
    //             ParseKind::EmptySet => unreachable!("Insert rule check should not be done here"), // probably has to be done in apply() to skip straight to env check
    //             ParseKind::Metathesis => unreachable!("'&' not allowed in input"),
    //             ParseKind::WordBound => { unreachable!("Word boundaries not allowed in input")
    //                 // if b.kind == TokenKind::WordBoundary {
    //                 //     if i != 0 && i != word.segments.len()-1 {
    //                 //         panic!()
    //                 //     }
    //                 // }
    //             },
    //             ParseKind::SyllBound => todo!(),
    //             ParseKind::Environment(_, _) => unreachable!("Env. not allowed in input"),
    //         }
    //     }
    //
    //     (false, 0, 0)
    // }

    // fn find_input_initial(&self, word: Word) -> Option<(usize, usize)> {
    //     for x in &self.input[0] {
    //         // println!("ffdfvsdf {}", x);
    //         // syllable will have to be dealt with separately up here
    //         for (i, seg) in word.segments.iter().enumerate() {
    //             if let ParseKind::Ipa(s, params) = &x.kind {
    //                 // todo: deal with modifiers
    //                 if *seg == *s {
    //                     return Some((i, i));
    //                 }
    //             } else if let ParseKind::Matrix(params) = &x.kind {
    //                 // come up with way to match matrix with ipa
    //                 // self.match_matrix_ipa(params, seg)
    //             } else if let ParseKind::Set(set) = &x.kind {
    //                 //
    //             } else if let ParseKind::Optional(opts, l, h) = &x.kind {
    //                 //
    //             } else if let ParseKind::Variable(ident, params) = &x.kind {
    //                 //
    //             }
    //         }
    //     }
    //     None
    // }
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        match self.rule_type {
            RuleType::Insertion     => writeln!(f, "Insertion Rule ->")?,
            // RuleType::Reduplication => writeln!(f, "Reduplication Rule ->")?,
            RuleType::Deletion      => writeln!(f, "Deletion Rule ->")?,
            RuleType::Metathesis    => writeln!(f, "Metathesis Rule ->")?,
            RuleType::Substitution  => writeln!(f, "Rule ->")?,
        }

        writeln!(f, "    Input = [")?;
        for i in self.input.iter() {
            writeln!(f, "        {:?}", i)?;
        }
        writeln!(f, "    ]")?;

        writeln!(f, "    Output = [")?;
        for o in self.output.iter() {
            writeln!(f, "        {:?}", o)?;
        }
        writeln!(f, "    ]")?;

        writeln!(f, "    Context = [")?;
        for c in self.context.iter() {
            writeln!(f, "        {}", c)?;
        }
        writeln!(f, "    ]")?;

        writeln!(f, "    Exception = [")?;
        for e in self.except.iter() {
            writeln!(f, "        {}", e)?;
        }
        writeln!(f, "    ]")?;


        Ok(())
    }
}


#[cfg(test)]
mod rule_tests {
    use super::*;
    
    fn setup_rule(test_str: &str) -> Rule {
        use crate::{Lexer, Parser};
        Parser:: new(Lexer::new(test_str,0).get_all_tokens().unwrap(), 0).parse().unwrap()
    }

    fn setup_word(test_str: &str) -> Word {
        Word::new(String::from(test_str)).unwrap()
    }

    // #[test]
    // fn test_match() {
    //     let test_rule = setup_rule("r > l");
    //     let test_word = setup_word("la.ri.sa");

    //     assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "la.li.sa");


    // }
}