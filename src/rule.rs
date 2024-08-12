use std::{
    collections::HashMap, 
    cell::RefCell,
    cmp ::max, 
    fmt, 
};

use crate   :: {
    parser  ::{Item, Supr}, 
    error   :: RuleRuntimeError, 
    word    :: Word, SegPos,
    seg     :: NodeKind, 
    subrule :: SubRule, 
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RuleType {
    Substitution,
    Metathesis,
    Deletion,
    // Reduplication,
    Insertion,
}

#[derive(Debug)]
pub struct PlaceMod {
    pub lab: Option<u8>,
    pub cor: Option<u8>,
    pub dor: Option<u8>,
    pub phr: Option<u8>,
}

impl PlaceMod {
    pub fn new(lab: Option<u8>, cor: Option<u8>, dor: Option<u8>, phr: Option<u8>) -> Self {
        Self { lab, cor, dor, phr }
    }
}

#[derive(Debug)]
pub enum Alpha {
    Node(NodeKind, Option<u8>),
    Place(NodeKind, PlaceMod),
    Feature(NodeKind, u8, bool),
    Supra(Supr), // TODO: Replace Supr with something else
}

impl Alpha {
    /// Returns `true` if the alpha is `Feature`.
    pub fn is_feature(&self) -> bool {
        matches!(self, Self::Feature(..))
    }

    pub fn as_node(&self) -> Option<(&NodeKind, &Option<u8>)> {
        if let Self::Node(n, m) = self {
            Some((n, m))
        } else {
            None
        }
    }

    pub fn as_feature(&self) -> Option<(&NodeKind, &u8, &bool)> {
        if let Self::Feature(nk, msk, pos) = self {
            Some((nk, msk, pos))
        } else {
            None
        }
    }

    pub fn as_place(&self) -> Option<(&NodeKind, &PlaceMod)> {
        if let Self::Place(nk, place) = self {
            Some((nk, place))
        } else {
            None
        }
    }
}

// pub struct Capture {
//     input: Vec<Item>,
//     state: ,
// }
//
// #[derive(Debug)]
// pub struct SubRule {
//     input    : Vec<Item>,
//     output   : Vec<Item>,
//     context  : Option<Item>,         
//     except   : Option<Item>,
//     rule_type: RuleType,
//     variables: RefCell<HashMap<usize, Segment>>,
//     alphas   : RefCell<HashMap<char, Alpha>> 
// }
//
// impl SubRule {
//     fn apply(&self, word: Word) -> Result<Word, RuleRuntimeError> {
//         // find input, if no match early return
//         // match except left/right, if no match early return
//         // match context left/right, if no match early return
//         // apply
//         // syllable re-match/repair
//         //
//         // let Some((match_pos, match_len)) = self.match_input(&word)? else {
//         //     return Ok(word)
//         // };
//
//         match self.rule_type {
//             RuleType::Substitution  => {/* input>env>output */},
//             RuleType::Metathesis    => {/* skip calc output */},
//             RuleType::Deletion      => {/* skip calc output */},
//             RuleType::Insertion     => {/* skip match input */},
//         }
//
//         let res = self.match_input_at(&word, 0)?;
//
//         if !res.is_empty() {
//             println!("{}", word.render().unwrap());
//             println!("Match! {:?}", res);
//         } else {
//             println!("{}", word.render().unwrap());
//             println!("No match");
//         }
//
//         todo!()
//     }
//
//     // fn match_input(&mut self, word: &Word) -> Result<Option<(usize, usize)>, RuntimeError> {
//     //     assert!(!self.input.is_empty());
//     //     assert!(!word.segments.is_empty());
//     //
//     //     let mut match_pos = 0usize;
//     //     let mut matched = false;
//     //     let max_match_pos = word.syllables.len() - 1;
//     //     
//     //     while !matched && match_pos < max_match_pos {
//     //         let (matched, match_length, _) = self.match_x(word, match_pos, None)?;
//     //         if matched {
//     //             return Ok(Some((match_pos, match_length)))
//     //         }
//     //         match_pos += 1;
//     //     }
//     //     Ok(None)
//     // }
//
//     // fn match_x(&mut self, word: &Word, match_pos: usize, match_length: usize) -> Result<(bool, usize, usize), RuntimeError> {
//     //     todo!();
//     //     if match_pos == word.segments.len() - 1 {
//     //         return Ok(Some((true, match_length, None)))
//     //     }
//     //
//     //     for 
//     // }
//
//     // NOTE: Only returns first match from start index
//     // may have to check `match.end` > 'segs.length' and then, after applying, start word from end instead of 0
//     fn match_input_at(&self, word: &Word, start_index: usize) -> Result<Vec<MatchElement>, RuleRuntimeError> {
//         let mut cur_index = start_index;
//         let mut begin = None;
//         // let mut end = None;
//         // let mut states = states.iter();
//         let mut state_index = 0;
//         // let mut state = self.input[state_index].clone();
//
//         let mut captures: Vec<_> = Vec::new();
//
//         while cur_index <= word.seg_count() {
//             let mtch = self.match_input_item(word, &mut cur_index, &self.input, &mut state_index)?;
//             // println!("{} : {} {} {}", word.segments[cur_index].get_as_grapheme().unwrap(),  mtch, seg_adv, stt_adv);
//             if let Some(m) = mtch {
//                 if begin.is_none() {
//                     begin = Some(cur_index); // TODO: This won't work if we jump i.e. if we match syllable
//                 }
//
//                 captures.push(m);
//             
//                 if state_index > self.input.len() - 1 {
//                     return Ok(captures) // Ok((Match::new(begin.unwrap(), cur_index)))
//                 }
//                 // let Some(s) = states.next() else {
//                 //     return Ok(Some(Match::new(begin.unwrap(), cur_index)))
//                 // };
//                 // if stt_adv {
//                 //     state_index += 1;
//                 //     state = self.input[state_index].clone();
//                 // }
//                 // TODO: if Ellipsis or Optional we need to NOT advance state until we no longer match, at which point we backtrack
//                 // `...` is the same os Optional: (AnySegment, 0)
//                 // or we could check next state -> if no match apply ... else apply next states
//
//                 // if seg_adv {
//                 //     cur_index+=1; 
//                 // }
//                 continue;           
//             }                   
//
//             if captures.is_empty() {
//                 cur_index+=1;
//                 continue;
//             } else {
//                 cur_index = begin.unwrap() + 1;
//                 state_index = 0;
//                 // state = self.input[start_index].clone();
//                 begin = None;
//             }
//         }
//
//         if begin.is_none() {
//             return Ok(vec![])
//         } if let ParseKind::WordBound | ParseKind::SyllBound = self.input.last().expect("Input is empty").kind {
//             // Ok(Some(Match::new(begin.unwrap(), word.seg_count())))
//             todo!()
//         } else {
//             Ok(vec![])
//         }        
//     }
//
//     // TODO: These could be compressed
//     fn match_input_item(&self, word: &Word, seg_index: &mut usize, states: &[Item], state_index: &mut usize) -> Result<Option<MatchElement>, RuleRuntimeError> {
//         // let seg = word.segments[seg_index];
//         let state = states[*state_index].clone();
//         match &state.kind {
//             ParseKind::Variable(vt, m) => if self.match_var(vt, m, word, *seg_index)? {
//                 *seg_index += 1;
//                 *state_index += 1;
//                 Ok(Some(MatchElement::Segment(seg_index.clone() - 1))) // TODO(girv): maybe have a `variable` varient
//             } else  {
//                 Ok(None)
//             },
//             ParseKind::Ipa(s, m) => if self.match_ipa(s, m, word, *seg_index)? {
//                 *seg_index += 1;
//                 *state_index += 1;
//                 Ok(Some(MatchElement::Segment(seg_index.clone() - 1)))
//             } else {
//                 Ok(None)
//             },
//             ParseKind::Matrix(m, v) => if self.match_matrix(m, v, word, *seg_index)? {
//                 *seg_index += 1;
//                 *state_index += 1;
//                 Ok(Some(MatchElement::Segment(seg_index.clone() - 1)))
//             } else {
//                 Ok(None)
//             },
//             ParseKind::Set(s) => if self.match_set(s, word, *seg_index)? {
//                 *seg_index += 1;
//                 *state_index += 1;
//                 Ok(Some(todo!())) // TODO(girv): Set item can be segment, syll, or boundary
//             } else {
//                 Ok(None)
//             },
//             ParseKind::SyllBound => if self.match_syll_bound(word, *seg_index) {
//                 // NOTE: we don't want to advance the segment here
//                 *state_index += 1;
//                 Ok(Some(MatchElement::SyllBound(seg_index.clone())))
//             } else {
//                 Ok(None)
//             },  
//             ParseKind::Syllable(s, t) => if self.match_syll(s, t, word, seg_index)? {
//                 // NOTE(girv): currently match_syll() increments moves seg_index to end of the current syllable
//                 // we must increment here to begin at the start of the next syllable
//
//                 let asdf = word.get_syll_index_from_seg_index(seg_index.clone());
//
//                 *seg_index += 1;
//                 *state_index += 1;
//                 Ok(Some(MatchElement::Syllable(asdf)))
//             } else {
//                 Ok(None)
//             },
//             ParseKind::Ellipsis =>  if self.match_ellipsis(word, seg_index, states, state_index)? {
//            
//                 *seg_index += 1;
//                 *state_index += 1;
//                 Ok(Some(vec![]))
//             } else {
//                 Ok(None)
//             },            
//             ParseKind::Optional(opt_states, match_min, match_max) => if self.match_optionals(word, seg_index, opt_states, *match_min, *match_max)? {
//                 todo!()
//             } else {
//                 Ok(false)
//             }, 
//             _ => unreachable!()
//         }
//     }
//
//     fn match_optionals(&self, word: &Word, seg_index: &mut usize, opt_states: &[Item], match_min: usize, match_max: usize) -> Result<bool, RuleRuntimeError> {
//         // should work like regex (...){min, max}? 
//         let max = if match_max == 0 {None} else{ Some(match_max)};
//         self.match_multiple(word, seg_index,  opt_states, &mut 0, match_min, max)
//     }
//
//     fn match_ellipsis(&self, word: &Word, seg_index: &mut usize, states: &[Item], state_index: &mut usize) -> Result<bool, RuleRuntimeError> {
//         // should work akin to '.+?' in Regex, that is, a lazy-match of one-or-more elements
//         self.match_multiple(word, seg_index, states, state_index, 1, None)
//     }
//
//     #[allow(unused)]
//     fn match_multiple(&self, word: &Word, seg_index: &mut usize,  states: &[Item], state_index: &mut usize, match_min: usize, match_max: Option<usize>) -> Result<bool, RuleRuntimeError> {
//    
//         let back_state = *state_index;
//         let back_seg = *seg_index;
//    
//         // match `min` times
//         let mut i = 0;
//         // TODO: Check for possible off-by-one error
//         while i < match_min {
//             // TODO: Test for off-by-one errors
//             if *state_index >= states.len() || *seg_index >= word.segments.len() || !self.match_input_item(word, seg_index, states, state_index)?.is_some() {
//                 *seg_index = back_seg; // this might not be necessary if the value is never read after this
//                 return Ok(false)
//             }
//             i += 1;
//         }
//
//         if *state_index < self.input.len() && *seg_index < word.segments.len() {
//             todo!()
//
//             // try match rest
//             // if no match 
//             //     if less than match_min then return false
//
//             //     state = back_state
//             //     index = back_index+1
//             //     continue until match_max ?
//             // else
//             // return true
//         } 
//         Ok(false)
//     }
//
//     fn match_syll(&self, stress: &Option<Supr>, tone: &Option<String>, word: &Word, seg_index: &mut usize) -> Result<bool, RuleRuntimeError> {
//         // checks current segment is at start of syll
//         // matches stress and tone
//         // jumps to end of syllable if match
//         if word.seg_is_syll_initial(*seg_index) {
//             let curr_syll_index = word.get_syll_index_from_seg_index(*seg_index);
//             let curr_syll = word.get_syll_at(curr_syll_index).unwrap();
//
//             if let Some(s) = stress.as_ref() {
//                 if !self.match_stress(s, &curr_syll) {
//                     return Ok(false)
//                 }
//             }
//
//             if let Some(t) = tone.as_ref() {
//                 if !self.match_tone(t, &curr_syll) {
//                     return Ok(false)
//                 }
//             }
//             *seg_index = curr_syll.end; // NOTE: this is only correct if we advance seg_index after match in `match_input_at`
//
//             Ok(true)
//         } else {
//             Ok(false) 
//         }
//     }
//
//     fn match_stress(&self, stress: &Supr, syll: &Syllable) -> bool {
//         match stress.kind {
//             // ±stress (+ matches prim and sec, - matches unstressed)
//             SupraType::Stress => match stress.modifier {
//                 SegMKind::Binary(b) => match b {
//                     BinMod::Negative => syll.stress == StressKind::Unstressed,
//                     BinMod::Positive => syll.stress != StressKind::Unstressed,
//                 },
//                 SegMKind::Alpha(_) => todo!(),
//             },
//             // ±secstress (+ matches sec, - matches prim and unstressed)
//             SupraType::SecStress => match stress.modifier {
//                 SegMKind::Binary(b) => match b {
//                     BinMod::Negative => syll.stress != StressKind::Secondary,
//                     BinMod::Positive => syll.stress == StressKind::Secondary,
//                 },
//                 SegMKind::Alpha(_) => todo!(),
//             },
//             _ => unreachable!(),
//         }
//     }
//
//     fn match_tone(&self, tone: &str, syll: &Syllable) -> bool {        
//         tone == syll.tone
//     }
//
//     fn match_syll_bound(&self, word: &Word, seg_index: usize) -> bool {
//         // NOTE: matches for boundary BEFORE current segment
//         word.seg_is_syll_initial(seg_index)
//     }
//
//     fn match_ipa(&self, s: &Segment, mods: &Option<Modifiers>, word:&Word, seg_index: usize) -> Result<bool, RuleRuntimeError>{
//         let seg = word.get_seg_at(seg_index).unwrap();
//         if mods.is_none() {
//             Ok(*s == seg)
//         } else {
//             todo!("Need to compare modifiers")  // TODO: compare modifiers
//         }
//     }
//
//     fn match_set(&self, set: &[Item], word:& Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
//         for s in set {
//             match &s.kind {
//                 ParseKind::Variable(vt, m) => return self.match_var(vt, m, word, seg_index),
//                 ParseKind::Ipa(s, m) => return self.match_ipa(s, m, word, seg_index),
//                 ParseKind::Matrix(m, v) => return self.match_matrix(m, v, word, seg_index),
//                 _ => unreachable!(),
//             }
//         }
//         Ok(false)
//     }
//
//     fn match_var(&self, vt: &Token, mods: &Option<Modifiers>, word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
//         // let seg = word.segments[seg_index];
//         if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().expect("")) {
//             self.match_ipa(var, mods, word, seg_index)
//         } else {
//             Err(RuleRuntimeError::UnknownVariable(vt.clone()))
//         }
//     }
//
//     fn match_matrix(&self, mods: &Modifiers, var: &Option<usize>, word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
//         if var.is_none() {
//             self.match_modifiers(mods, word, seg_index)
//         } else if self.match_modifiers(mods, word, seg_index)? {
//             self.variables.borrow_mut().insert(var.unwrap(), word.get_seg_at(seg_index).unwrap());
//             Ok(true)
//         } else {
//             Ok(false)
//         } 
//     }
//
//     fn match_modifiers(&self, mods: &Modifiers, word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
//         //loop through mods
//         // if alpha, check alphaMap
//             // if not there, insert and return true
//             // else, match against Map
//         // if binary
//             // match
//         //TODO: do mods.nodes and mods.supr
//
//         let seg = word.segments[seg_index];
//
//         for (i, m) in mods.feats.iter().enumerate() {
//             if !self.match_feat_mod(m, i, seg)? {
//                 return Ok(false);
//             }
//         }
//         Ok(true)
//     }
//
//     fn match_feat_mod(&self, md: &Option<SegMKind>, feat_index: usize, seg: Segment) -> Result<bool, RuleRuntimeError> {
//         if let Some(kind) = md { 
//             let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
//             return self.match_seg_kind(kind, seg, node, mask)
//         }
//         Ok(true)
//     }
//
//     fn match_seg_kind(&self, kind: &SegMKind, seg: Segment, node: NodeKind, mask: u8) -> Result<bool, RuleRuntimeError> {
//         match kind {
//             SegMKind::Binary(bt) => match bt {
//                 BinMod::Negative => Ok(seg.feat_match(node, mask, false)),
//                 BinMod::Positive => Ok(seg.feat_match(node, mask, true)),
//             },
//             SegMKind::Alpha(am) => match am {
//                 AlphaMod::Alpha(a) => {
//                     let x = self.alphas.borrow().get(a);
//                     if let Some(alph) = self.alphas.borrow().get(a) {
//                         if let Some((n, m, pos)) = alph.as_feature() {
//                             return Ok(seg.feat_match(*n, *m, *pos))
//                         } else {
//                             todo!("Err")
//                         }
//                     } 
//                     self.alphas.borrow_mut().insert(*a, Alpha::Feature(node, mask, true)); 
//                     Ok(true)             
//                 },
//                 AlphaMod::InversAlpha(ia) => {
//                     if let Some(alph) = self.alphas.borrow().get(ia) {
//                         if let Some((n, m, pos)) = alph.as_feature() {
//                             Ok(seg.feat_match(*n, *m, !pos)) // TODO: test this
//                         } else {
//                             todo!("Err")
//                         }
//                     } else if let Some(f) = seg.get_feat(node, mask) {
//                         self.alphas.borrow_mut().insert(*ia, Alpha::Feature(node, mask, f != 0));
//                         Ok(true)
//                     } else {
//                         Err(todo!())
//                         // return err
//                     }            
//                 },
//             },
//         }
//     }
// }

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

    pub fn split_into_subrules(&self) -> Result<Vec<SubRule>, RuleRuntimeError> {
        // check that input, output, context, except are the same length
        // and if any are not, that they are length == 1
        // context and except can be length == 0
        let max = max(self.input.len(), max(self.output.len(), max(self.context.len(), self.except.len())));

        if self.input.len()   != max && self.input.len()   != 1 { return Err(RuleRuntimeError::UnbalancedRuleIO(self.input.clone()))  }
        if self.output.len()  != max && self.output.len()  != 1 { return Err(RuleRuntimeError::UnbalancedRuleIO(self.output.clone())) }
        if self.context.len() != max && self.context.len() != 1 && !self.context.is_empty() { return Err(RuleRuntimeError::UnbalancedRuleEnv(self.context.clone())) }
        if self.except.len()  != max && self.except.len()  != 1 && !self.except.is_empty()  { return Err(RuleRuntimeError::UnbalancedRuleEnv(self.except.clone()))  }

        // populate subrules, if one if length==1 then it's value is duplicated to rest of subrules
        let mut sub_vec = Vec::new();
        for i in 0..max {
            let input   = if self.input.len()  == 1 { self.input[0].clone() }  else { self.input[i].clone() };
            let output  = if self.output.len() == 1 { self.output[0].clone() } else { self.output[i].clone() };
            let context = if self.context.is_empty() { None } else if self.context.len() == 1 { Some(self.context[0].clone()) } else { Some(self.context[i].clone()) };
            let except  = if self.except.is_empty()  { None } else if self.except.len()  == 1 { Some(self.except[0].clone()) }  else { Some(self.except[i].clone()) };
            let rule_type = self.rule_type;  // TODO: calc rule_type here instead of in parser

            sub_vec.push(
                SubRule {
                    input, 
                    output, 
                    context, 
                    except, 
                    rule_type, 
                    variables: RefCell::new(HashMap::new()), 
                    alphas: RefCell::new(HashMap::new()), 
                    // pos: SegPos::new(0, 0),
                    // state_index: SegPos::new(0, 0),
                }
            );
        }

        Ok(sub_vec)
    }

    pub fn apply(&self, word: Word /*, trace: bool*/) -> Result<Word, RuleRuntimeError> /* return Word */{
        
        let sub_rules = self.split_into_subrules()?;
        
        let mut res_word = word; 
        for i in sub_rules {
            res_word = i.apply(res_word)?;

            // println!("{i:#?} ---> {res_word:#?}");
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
        Parser:: new(Lexer::new(&test_str.chars().collect::<Vec<_>>(),0).get_line().unwrap(), 0).parse().unwrap()
    }

    fn setup_word(test_str: &str) -> Word {
        Word::new(String::from(test_str)).unwrap()
    }

    #[test]
    fn test_sub_simple_ipa() {
        let test_rule = setup_rule("r > l");
        let test_word = setup_word("la.ri.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "la.li.sa");
    }

    #[test]
    fn test_met_simple_ipa() {
        let test_rule = setup_rule("lVr > &");
        let test_word = setup_word("la.ri");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ra.li");
    }

    #[test]
    fn test_del_ipa_before_bound() {
        let test_rule = setup_rule("t > *  / _#");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kat.ka");
    }
    
    #[test]
    fn test_del_vowel_after_vowel() {
        let test_rule = setup_rule("V > * / V_");
        let test_word = setup_word("kai.lua");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ka.lu");
    }

    #[test]
    fn test_del_matrix_after_matrix() {
        let test_rule = setup_rule("[+syll, +high] > * / [+syll, -high]_");
        let test_word = setup_word("kai.lua");
        // from Assamese, "a high vowel gets deleted following a non-high vowel"
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ka.lua");
    }

    #[test]
    fn test_except_before_simple_ipa() {
        let test_rule = setup_rule(" i > e | c_");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.ci");
    }

    #[test]
    fn test_except_before_ipa() {
        let test_rule = setup_rule(" i > e | cc _");
        let test_word = setup_word("ki.cːi");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.cːi");
    }

    #[test]
    fn test_except_before_ipa_bound() {
        let test_rule = setup_rule(" i > e | cc_");
        let test_word = setup_word("kic.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kec.ci");
    }

    #[test]
    fn test_except_after_simple_ipa() {
        let test_rule = setup_rule(" i > e | _c");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ki.ce");
    }

    #[test]
    fn test_except_after_ipa() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("ki.cːi");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ki.cːe");
    }

    #[test]
    fn test_except_after_ipa_bound() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("kic.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kic.ce");
    }

    #[test]
    fn test_except_before_ipa_bound_false() {
        let test_rule = setup_rule(" i > e | cc_");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.ce");
    }

    #[test]
    fn test_except_after_ipa_bound_false() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.ce");
    }

    #[test]
    fn test_context_set() {
        let test_rule = setup_rule("i > ɛ / _{r,h,ʍ}");
        let test_word = setup_word("si.sir");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "si.sɛr");
        
        let test_word = setup_word("si.si.haz");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "si.sɛ.haz");

        let test_word = setup_word("ri.hi.ʍaz");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "rɛ.hɛ.ʍaz");
    } 

    #[test]
    fn test_portuguese() {
        let test_rules = [
            setup_rule("[+rho] > [-cont] / C_, _$"),
            setup_rule("s, m > * / _#"),
            setup_rule("k > t^s / _[+front]"),
            setup_rule("i > j / _V"),
            setup_rule("V:[+long] > [-long]"),
            setup_rule("e > * / Vr_#"),
            setup_rule("$ > * / _r#"),
            setup_rule("$w > * / V_V"),
            setup_rule("u > o / _#"),
            setup_rule("gn > nj"),  // ŋn > ɲ
            setup_rule("p,t,k > [+voice] / V_V"),
            setup_rule("k > i / i_t, e_t"), 
            setup_rule("k > u / u_t, o_t"), 
            setup_rule("p > t / V_t"), // C > 1 / V_C=1
            setup_rule("i:[+long] > [-long]"),
            setup_rule("e > * / C_rV"),
            setup_rule("t^s > s"),
            setup_rule("l > ʎ / _j"),
            setup_rule("j > * / ʎ_"),
            setup_rule("s > ʃ / i_"),
            setup_rule("j > ʒ"),
            setup_rule("a:[-str], e:[-str], o:[-str] > ɐ, ɨ, u | _CC"),
            setup_rule("C=1 > * / _1"),
            setup_rule("b, d, g > β, ð, ɣ | #_"),
            setup_rule("C$ > & / $_"),
            setup_rule("$C > & / _$"),
            setup_rule("V:[+str] > [+nasal] / _[+cons, +nasal]C"),
            setup_rule("[+cons, +nasal] > * / V:[+nasal]_"),
        ];
        let test_words = [
            setup_word("'fo.kus"),
            setup_word("'jo.kus"),
            setup_word("dis'trik.tus"),
            setup_word("ki:.wi'ta:.tem"),
            setup_word("a.dop'ta.re"),
            setup_word("'o.pe.ra"),
            setup_word("se'kun.dus"),
            setup_word("'fi:.liam"),
            setup_word("'po:n.tem"),
        ];
        let output_matchs = [
            setup_word("ˈfo.ɣu"),
            setup_word("ˈʒo.ɣu"),
            setup_word("diʃˈtɾi.tu"),
            setup_word("siˈða.ðɨ"),
            setup_word("ɐ.ðoˈtar"),
            setup_word("ˈo.βrɐ"),   // ˈɔ.βɾɐ
            setup_word("sɨˈɣũ.ðu"),
            setup_word("ˈfi.ʎɐ"),
            setup_word("ˈpõ.tɨ"),
        ];

        let mut output_words: Vec<Word> = vec![];

        for word in &test_words {
            let mut w = word.clone();
            for rule in &test_rules {
                w = rule.apply(w).unwrap();
            }
            output_words.push(w)
        }

        for (w, m) in output_words.iter().zip(output_matchs) {
            assert_eq!(w.render().unwrap(), m.render().unwrap());
        }

    }
}