use std::{
    collections::HashMap, 
    fmt, cmp::max, cell::RefCell
};

use crate ::{
    parser::{Item, ParseKind, Modifiers, Supr, SegMKind, BinMod, AlphaMod}, 
    error ::RuntimeError, 
    word  ::{Word, Segment, Syllable, StressKind, feature_to_node_mask, NodeKind}, 
    lexer ::{Token, SupraType, FType}
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RuleType {
    Substitution,
    Metathesis,
    Deletion,
    // Reduplication,
    Insertion,
}

pub struct Match {
    pub start: usize,
    pub end  : usize
}

impl Match {
    pub fn new(start: usize, end: usize) -> Self {
        Self {start, end}
    }
}

// struct BacktrackState {
//     can_backtrack: bool,
//     state: Item,
//     consumptions: Option<usize>
// }

#[derive(Debug)]
pub struct SubRule {
    input    : Vec<Item>,
    output   : Vec<Item>,
    context  : Option<Item>,         
    except   : Option<Item>,
    rule_type: RuleType, // RuleType,
    variables: RefCell<HashMap<usize, Segment>>,
    alphas: RefCell<HashMap<char, SegMKind>> // TODO: Supras should be alpha-able as well
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

        let res = self.match_input_at(&word, &self.input, 0)?;

        if let Some(m) = res {
            println!("{}", word.render().unwrap());
            println!("Match! {}:{}", m.start, m.end)
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

    // NOTE: Only returns first match from start index
    // may have to check `match.end` > 'segs.length' and then, after applying, start word from end instead of 0
    fn match_input_at(&self, word: &Word, states: &[Item], start_index: usize) -> Result<Option<Match>, RuntimeError> {
        let mut cur_index = start_index;
        let mut begin = None;
        // let mut end = None;
        // let mut states = states.iter();
        let mut state_index = 0;
        let mut state = states[state_index].clone();

        while cur_index <= word.seg_count() {
            if self.match_input_item(&state, word, &mut cur_index)? {
                if begin.is_none() {
                    begin = Some(cur_index); // TODO: This won't work if we jump i.e. if we match syllable
                }
                
                if state_index >= states.len() - 1 {
                    return Ok(Some(Match::new(begin.unwrap(), cur_index)))
                }
                // let Some(s) = states.next() else {
                //     return Ok(Some(Match::new(begin.unwrap(), cur_index)))
                // };
                state_index += 1;
                state = states[state_index].clone();
                                // TODO: if ellipsis we need to NOT advance state until we no longer match, at which point we backtrack
                                // `...` is the same os OptionalSeg: (AnySegment, 0)
                                // or we could check next state -> if no match apply ... else apply next states

                cur_index+=1;   // TODO: This does work for matching syll_bound    
                continue;           
            }                   

            if begin.is_none() {
                cur_index+=1;
                continue;
            } else {
                cur_index = begin.unwrap() + 1; // TODO: won't work for backtracking
                state_index = 0;
                state = states[start_index].clone();
                begin = None;
            }
        }

        if begin.is_none() {
            return Ok(None)
        }

        if let ParseKind::WordBound | ParseKind::SyllBound = state.kind {
            Ok(Some(Match::new(begin.unwrap(), word.seg_count())))
        } else {
            Ok(None)
        }        
    }

    fn match_input_item(&self, item: &Item, word: &Word, seg_index: &mut usize) -> Result<bool, RuntimeError> {
        // let seg = word.segments[seg_index];
        match &item.kind {
            ParseKind::Variable(vt, m) => self.match_var(vt, m, word, *seg_index),
            ParseKind::Ipa(s, m) => self.match_ipa(s, m, word, *seg_index),
            ParseKind::Matrix(m, v) => self.match_matrix(m, v, word, *seg_index),
            ParseKind::Set(s) => self.match_set(s, word, *seg_index),
            ParseKind::SyllBound => Ok(self.match_syll_bound(word, *seg_index)), // TODO: we dont want to advance cur_index after this
            ParseKind::Syllable(s, t) => self.match_syll(s, t, word, seg_index),
            ParseKind::Ellipsis => Ok(true),            // TODO: backtracking
            ParseKind::Optional(_, _, _) => todo!(),    // this prob should be recursive to self.asdf() and will have to backtrack like `...`            
            _ => unreachable!(),                        
        }
    }

    fn match_syll(&self, stress: &Option<Supr>, tone: &Option<String>, word: &Word, seg_index: &mut usize) -> Result<bool, RuntimeError> {
        // checks current segment is at start of syll
        // matches stress and tone
        // jumps to next syllable
        if !word.is_syll_initial(*seg_index) {
            let curr_syll_index = word.get_syll_index_from_seg_index(*seg_index);
            let curr_syll = word.get_syll_at(curr_syll_index).unwrap();

            // TODO: match syll Modifiers
            if let Some(s) = stress.as_ref() {
                if !self.match_stress(s, &curr_syll) {
                    return Ok(false)
                }
            }

            if let Some(t) = tone.as_ref() {
                if !self.match_tone(t, &curr_syll) {
                    return Ok(false)
                }
            }

            // match word.get_syll_at(curr_syll_index+1) { 
            //     Some(s) => *seg_index = s.start,
            //     None => *seg_index = curr_syll.start,
            // }

            *seg_index = curr_syll.start; // NOTE: this is only correct if we +=1 index after match in `match_input_at`

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_stress(&self, stress: &Supr, syll: &Syllable) -> bool {
        match stress.kind {
            // ±stress    (+ matches prim and sec, - matches unstressed)
            SupraType::Stress => match stress.modifier {
                SegMKind::Binary(b) => match b {
                    BinMod::Negative => syll.stress == StressKind::Unstressed,
                    BinMod::Positive => syll.stress != StressKind::Unstressed,
                },
                SegMKind::Alpha(_) => todo!(), // TODO: God know how this is gonna work
            },
            // ±secstress (+ matches sec, - matches prim and unstressed)
            SupraType::SecStress => match stress.modifier {
                SegMKind::Binary(b) => match b {
                    BinMod::Negative => syll.stress != StressKind::Secondary,
                    BinMod::Positive => syll.stress == StressKind::Secondary,
                },
                SegMKind::Alpha(_) => todo!(),
            },
            _ => unreachable!(),
        }
    }

    fn match_tone(&self, tone: &str, syll: &Syllable) -> bool {        
        tone == syll.tone
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

    fn match_var(&self, vt: &Token, mods: &Option<Modifiers>, word: &Word, seg_index: usize) -> Result<bool, RuntimeError> {
        // let seg = word.segments[seg_index];
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().expect("")) {
            self.match_ipa(var, mods, word, seg_index)
        } else {
            Err(RuntimeError::UnknownVariable(vt.clone()))
        }

    }

    fn match_matrix(&self, mods: &Modifiers, var: &Option<usize>, word: &Word, seg_index: usize) -> Result<bool, RuntimeError> {
        // TODO: do var
        if var.is_none() {
            self.match_modifiers(mods, word, seg_index)
        } else if self.match_modifiers(mods, word, seg_index)? {
            self.variables.borrow_mut().insert(var.unwrap(), word.get_seg_at(seg_index).unwrap());
            Ok(true)
        } else {
            Ok(false)
        }
        
    }

    fn match_modifiers(&self, mods: &Modifiers, word: &Word, seg_index: usize) -> Result<bool, RuntimeError> {
        //loop through mods
        // if alpha, check alphaMap
            // if not there, insert and return true
            // else, match against Map
        // if binary
            // match

        let seg = word.segments[seg_index];

        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_mod(m, i, seg) {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn match_mod(&self, md: &Option<SegMKind>, feat_index: usize, seg: Segment) -> bool {
        if let Some(kind) = md { 
            let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
            return self.match_seg_kind(kind, seg, node, mask) 

        }
        true
    }

    fn match_seg_kind(&self, kind: &SegMKind, seg: Segment, node: NodeKind, mask: u8) -> bool {
        match kind {
            SegMKind::Binary(b) => { 
                if !(match b {
                    BinMod::Negative => seg.feat_match(node, mask, false),
                    BinMod::Positive => seg.feat_match(node, mask, true),
                }) {
                    return false
                }
                true
            },
            SegMKind::Alpha(am) => match am {
                AlphaMod::Alpha(a) => {
                    if let Some(x) = self.alphas.borrow().get(a) {
                        self.match_seg_kind(x, seg, node, mask)

                    } else {
                        self.alphas.borrow_mut().insert(*a, *kind);  // NOTE: This might not work as intended, probably shouldn't be `kind` here
                        true
                    }
                },
                AlphaMod::InversAlpha(ia) => {
                    if let Some(x) = self.alphas.borrow().get(ia) {
                        !self.match_seg_kind(x, seg, node, mask)

                    } else {
                        self.alphas.borrow_mut().insert(*ia, *kind); // NOTE: This might not work as intended, probably shouldn't be `kind` here
                        true
                    }
                },
            },
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

            sub_vec.push(SubRule {input, output, context, except, rule_type, variables: RefCell::new(HashMap::new()), alphas: RefCell::new(HashMap::new())});
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
        Parser:: new(Lexer::new(&test_str.chars().collect::<Vec<_>>(),0).get_all_tokens().unwrap(), 0).parse().unwrap()
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