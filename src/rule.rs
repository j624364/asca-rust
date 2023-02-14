use std::{
    collections::HashMap, 
    fmt, cmp::max, cell::RefCell
};

use crate ::{
    parser::{Item, ParseKind, Modifiers, Supr}, 
    error ::RuntimeError, 
    word  ::{Word, Segment}, lexer::Token
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
    variables: RefCell<HashMap<usize, Segment>>,
    // alphas: Vec<_>
}

impl SubRule {
    fn apply(&self, word: Word) -> Result<Word, RuntimeError> {
        // find input, if no match early return
        // match except left/right, if no match early return
        // match context left/right, if no match early return
        // apply
        // syllable re-match/repair
        //
        // let Some((match_pos, match_len)) = self.match_input(&word)? else {
        //     return Ok(word)
        // };

        match self.rule_type {
            RuleType::Substitution  => {/* input>env>output */},
            RuleType::Metathesis    => {/* skip calc output */},
            RuleType::Deletion      => {/* skip calc output */},
            RuleType::Insertion     => {/* skip match input */},
        }

        let res = self.match_input(&word, &self.input)?;

        if let Some((s, e)) = res {
            println!("{}", word.render().unwrap());
            println!("Match! {s}:{e}")
        } else {
            println!("No match")
        }

        todo!()
    }

    // fn match_input(&mut self, word: &Word) -> Result<Option<(usize, usize)>, RuntimeError> {
    //     assert!(!self.input.is_empty());
    //     assert!(!word.segments.is_empty());
    //
    //     let mut match_pos = 0usize;
    //     let mut matched = false;
    //     let max_match_pos = word.syllables.len() - 1;
    //     
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
    //
    //     for 
    // }

    // NOTE: ONly returns first match
    // may have to return `end` and then after applying, start word from end instead of 0
    fn match_input(&self, word: &Word, states: &[Item]) -> Result<Option<(usize, usize)>, RuntimeError> {
        let mut i = 0usize;
        let mut begin = None;
        // let mut end = None;
        let mut states = states.iter();
        let mut state = states.next().unwrap();

        while i < word.segments.len() {
            if self.match_input_item(state, word, &i)? {
                if begin.is_none() {
                    begin = Some(i);
                }
                let Some(s) = states.next() else {
                    return Ok(Some((begin.unwrap(), i)))
                };
                state = s;      // TODO: if ellipsis we need to NOT advance until we no longer match, at which point we backtrack
                i+=1;           // `...` is the same os OptionalSeg: (AnySegment, 0)
                continue;       // or we could check next state -> if no match apply ... else apply next states
            }                   

            if begin.is_none() {
                i+=1;
                continue;
            }
            return Ok(None)
        }

        if begin.is_none() {
            return Ok(None)
        }

        if let ParseKind::WordBound | ParseKind::SyllBound = state.kind {
            Ok(Some((begin.unwrap(), word.segments.len() - 1)))
        } else {
            Ok(None)
        }        
    }

    fn match_input_item(&self, item: &Item, word: &Word, seg_index: &usize) -> Result<bool, RuntimeError> {
        // let seg = word.segments[seg_index];
        match &item.kind {
            ParseKind::Variable(vt, m) => self.match_var(vt, m, word, *seg_index),
            ParseKind::Ipa(s, m) => self.match_ipa(s, m, word, *seg_index),
            ParseKind::Matrix(m, v) => self.match_matrix(m, v, word, *seg_index),
            ParseKind::Set(s) => self.match_set(s, word, *seg_index),
            ParseKind::SyllBound => Ok(self.match_syll_bound(word, *seg_index)), 
            ParseKind::Ellipsis => Ok(true),            // TODO: backtracking
            ParseKind::Syllable(s, t) => self.match_syll(s, t, word, seg_index),       // if we match syllable we need to somehow jump to the next boundary
            ParseKind::Optional(_, _, _) => todo!(),    // this prob should be recursive to self.asdf() and will have to backtrack like `...`            
            _ => unreachable!(),                        // NOTE: if insertion rule, we should jump straight to matching environment
        }
    }

    fn match_syll(&self, _stress: &Option<Supr>, _tone: &Option<String>, _word: &Word, _seg_index: &usize) -> Result<bool, RuntimeError> {
        // check current segment is at start of syll
        // match stress and tone
        // somehow jump to next syllalbe
        todo!();
    }

    fn match_syll_bound(&self, word: &Word, seg_index: usize) -> bool {
        // NOTE: matches for boundary BEFORE segment
        word.is_syll_initial(seg_index)
    }

    fn match_ipa(&self, s: &Segment, mods: &Option<Modifiers>, word:& Word, seg_index: usize) -> Result<bool, RuntimeError>{
        let seg = word.get_seg_at(seg_index).unwrap();
        if mods.is_none() {
            Ok(*s == seg)
        } else {
            todo!("Need to compare modifiers")  // TODO: compare modifiers
        }
    }

    fn match_set(&self, set: &[Item], word:& Word, seg_index: usize) -> Result<bool, RuntimeError> {
        for s in set {
            match &s.kind {
                ParseKind::Variable(vt, m) => return self.match_var(vt, m, word, seg_index),
                ParseKind::Ipa(s, m) => return self.match_ipa(s, m, word, seg_index),
                ParseKind::Matrix(m, v) => return self.match_matrix(m, v, word, seg_index),
                _ => unreachable!(),
            }
        }
        Ok(false)
    }

    fn match_var(&self, vt: &Token, mods: &Option<Modifiers>, word:& Word, seg_index: usize) -> Result<bool, RuntimeError> {
        // let seg = word.segments[seg_index];
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().expect("")) {
            self.match_ipa(var, mods, word, seg_index)
        } else {
            Err(RuntimeError::UnknownVariable(vt.clone()))
        }

    }

    fn match_matrix(&self, mods: &Modifiers, var:&Option<usize>, word:& Word, seg_index: usize) -> Result<bool, RuntimeError> {
        // TODO: do var
        if var.is_none() {
            Ok(word.match_modifiers_at(mods, seg_index))
        } else if word.match_modifiers_at(mods, seg_index) {
            self.variables.borrow_mut().insert(var.unwrap(), word.get_seg_at(seg_index).unwrap());
            Ok(true)
        } else {
            Ok(false)
        }
        
    }
}

pub struct Rule {
    pub input:     Vec<Vec<Item>>,    // to support multirules
    pub output:    Vec<Vec<Item>>,    // these need to be Vec<Vec<Item>>
    pub context:   Vec<Item>,
    pub except:    Vec<Item>,
    pub rule_type: RuleType,
}   // todo: if we move rule_type calc to SubRule, that would allow us to have multirules with insert/delete/metath

impl Rule {
    pub fn new(i: Vec<Vec<Item>>, o: Vec<Vec<Item>>, c :Vec<Item>, e :Vec<Item>, r: RuleType) -> Self {
        Self { input: i, output: o, context: c, except: e , rule_type: r}
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

            sub_vec.push(SubRule {input, output, context, except, rule_type, variables: RefCell::new(HashMap::new())});
        }

        Ok(sub_vec)
    }

    pub fn apply(&self, word: Word /*, trace: bool*/) -> Result<Word, RuntimeError> /* return Word */{
        
        let sub_rules = self.split_into_subrules()?;
        
        let mut res_word = word; 
        for i in sub_rules {
            res_word = i.apply(res_word)?;

            println!("{i:#?} ---> {res_word:#?}");
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
            writeln!(f, "        {i:?}")?;
        }
        writeln!(f, "    ]")?;

        writeln!(f, "    Output = [")?;
        for o in self.output.iter() {
            writeln!(f, "        {o:?}")?;
        }
        writeln!(f, "    ]")?;

        writeln!(f, "    Context = [")?;
        for c in self.context.iter() {
            writeln!(f, "        {c}")?;
        }
        writeln!(f, "    ]")?;

        writeln!(f, "    Exception = [")?;
        for e in self.except.iter() {
            writeln!(f, "        {e}")?;
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