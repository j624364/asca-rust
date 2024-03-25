// TODO(girv): lots of duplication here atm, focusing on getting things done before optimising


use std ::{
    cell::RefCell, 
    collections::{HashMap, VecDeque}
};

use crate ::{
    error :: RuleRuntimeError, 
    lexer ::{Token, FType}, 
    parser::{Item, ParseKind, Modifiers, ModKind, BinMod, AlphaMod, SupraSegs}, 
    rule  ::{RuleType, Alpha}, 
    seg   ::{Segment, NodeKind, feature_to_node_mask}, 
    syll  ::{Syllable, StressKind}, 
    word  ::{Word, SegPos}
};

type SylPos = usize;  // the index of the syllable in the word.syllables array
type BndPos = usize;  // the index of the syllable that has the boundary as it's start

#[derive(Debug, Clone, Copy)]
pub enum MatchElement {
    Segment  (SegPos),
    Syllable (SylPos),
    SyllBound(BndPos)
}

#[derive(Debug)]
pub enum VarKind {
    Segment(Segment),
    Syllable(Syllable)
}

#[derive(Debug)]
pub struct SubRule {
    pub input    : Vec<Item>,
    pub output   : Vec<Item>,
    pub context  : Option<Item>,         
    pub except   : Option<Item>,
    pub rule_type: RuleType,
    pub variables: RefCell<HashMap<usize, VarKind>>,
    pub alphas   : RefCell<HashMap<char, Alpha>> 
}

impl SubRule {
    pub fn apply(&self, word: Word) -> Result<Word, RuleRuntimeError> {
        // RuleType::Substitution  => {/* input>env>output */},
        // RuleType::Metathesis    => {/* skip calc output */},
        // RuleType::Deletion      => {/* skip calc output */},
        // RuleType::Insertion     => {/* skip match input */},

        if let RuleType::Insertion = self.rule_type {
            return self.transform(&word, vec![], &mut None)
        } 
        
        let mut word = word;
        let mut cur_index = SegPos::new(0, 0);
        // TODO(girv): `$ > *` or any broad deletion rule without context/exception should  give a warning to the user
        loop {
            let (res, mut next_index) = self.input_match_at(&word, cur_index)?;
            if !res.is_empty() {
                let start = match res[0] {
                    MatchElement::Segment(sp)  => sp,
                    MatchElement::Syllable(s)  |
                    MatchElement::SyllBound(s) => SegPos::new(s, 0),
                };
                let end = match *res.last().unwrap() {
                    MatchElement::Segment(mut sp)  => {
                        // So that long vowels work
                        let mut seg_len = word.seg_length_in_syll(sp);
                        while seg_len > 1 {
                            sp.increment(&word);
                            seg_len -= 1;
                        }
                        sp
                    },
                    MatchElement::SyllBound(s) => SegPos::new(s, 0),
                    MatchElement::Syllable(s)  => SegPos::new(s, word.syllables[s].segments.len()-1),
                };
                if !self.match_before_context_and_exception(&word, start)? || !self.match_after_context_and_exception(&word, end)? {
                    if let Some(ci) = next_index { 
                        cur_index = ci;
                        continue;
                    }
                    // end of word
                    break;
                }
                println!("{}", word.render().unwrap());
                println!("Match! {:?}", res);
                word = self.transform(&word, res, &mut next_index)?;
                if let Some(ci) = next_index { 
                    cur_index = ci;
                } else {
                    // end of word
                    break;
                }
            } else {
                // no match
                break
            }
        }
        Ok(word)
    }

    fn match_before_context_and_exception(&self, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let empty_env = Item::new(ParseKind::Environment(vec![], vec![]), crate::Position { line: 0, start: 0, end: 0 });
        let ParseKind::Environment(states, _) = &match &self.context {
            Some(s) => s,
            None => match &self.except {
                Some(_) => &empty_env,
                None => return Ok(true),
            },
        }.kind else { unreachable!() };
        let ParseKind::Environment(except_states, _) = &match &self.except {
            Some(ex) => ex,
            None => match &self.context {
                Some(_) => &empty_env,
                None => unreachable!(),
            },
        }.kind else { unreachable!() };

        let mut start_pos = pos;
        let mut is_context_match = true;
        for (i, state) in states.iter().rev().enumerate() {
            if !self.context_match(word, state, &mut start_pos, false, i)? {
                is_context_match = false;
                break;
            }
        }
        let mut start_pos = pos;
        let mut is_not_excepted = true;
        for (i, state) in except_states.iter().rev().enumerate() {
            if !self.context_match(word, state, &mut start_pos, false, i)? {
                is_not_excepted = false;
                break;
            }
        }
        Ok(is_not_excepted && is_context_match)
    }

    fn match_after_context_and_exception(&self, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let empty_env = Item::new(ParseKind::Environment(vec![], vec![]), crate::Position { line: 0, start: 0, end: 0 });
        let ParseKind::Environment(_, states) = &match &self.context {
            Some(s) => s,
            None => match &self.except {
                Some(_) => &empty_env,
                None => return Ok(true),
            },
        }.kind else { unreachable!() };
        let ParseKind::Environment(_, except_states) = &match &self.except {
            Some(ex) => ex,
            None => match &self.context {
                Some(_) => &empty_env,
                None => unreachable!(),
            },
        }.kind else { unreachable!() };

        let mut start_pos = pos;
        let mut is_context_match = true;
        for (si, state) in states.iter().enumerate() {
            if !self.context_match(word, state, &mut start_pos, true, si)? {
                is_context_match = false;
                break;
            }
        }
        let mut start_pos = pos;
        let mut is_excepted = false;
        for (si, state) in except_states.iter().enumerate() {
            if self.context_match(word, state, &mut start_pos, true, si)? {
                is_excepted = true;
                break;
            }
        }
        Ok(!is_excepted && is_context_match)
    }

    fn context_match(&self, word: &Word, state: &Item, pos: &mut SegPos, forwards: bool, state_index: usize) -> Result<bool, RuleRuntimeError> {
        // So that $_ and #_ works
        if state_index == 0 {
            if forwards {
                pos.increment(word);
            } else if let ParseKind::WordBound | ParseKind::SyllBound = state.kind {
                // Do Nothing
            } else {
                pos.decrement(word);
            }
        }
        // So that things like V:[+long]_ will work 
        if !forwards {
            while pos.seg_index > 0 && word.syllables[pos.syll_index].segments[pos.seg_index] == word.syllables[pos.syll_index].segments[pos.seg_index - 1] {
                pos.decrement(word);
            } 
        }
        match &state.kind {
            ParseKind::WordBound => if (!forwards && pos.at_word_start()) || (forwards && word.out_of_bounds(*pos)) {
                Ok(true)
            } else { Ok(false) },
            ParseKind::SyllBound => if pos.at_syll_start() {
                Ok(true)
            } else { Ok(false) },
            ParseKind::Ipa(s, m) => if self.context_match_ipa(s, m, word, *pos)? {
                if forwards { pos.increment(word); } else { pos.decrement(word); }
                Ok(true)
            } else { Ok(false) },
            ParseKind::Matrix(m, v) => self.context_match_matrix(m, v, word, pos, forwards),
            ParseKind::Syllable(s, t, v) => self.context_match_syll(s, t, v, word, pos, forwards),
            ParseKind::Variable(vt, mods) => self.context_match_var(vt, mods, word, pos, forwards),
            ParseKind::Set(s) => self.context_match_set(s, word, pos, forwards),
            ParseKind::Optional(_, _, _) => todo!(),
            ParseKind::Ellipsis => todo!(),
            
            
            ParseKind::EmptySet | ParseKind::Metathesis |
            ParseKind::Environment(_, _) => unreachable!(),
        }
    }

    fn context_match_set(&self, set: &[Item], word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {

        // for s in set {
        //     let res = match &s.kind {
        //         ParseKind::Variable(vt, m) => self.input_match_var(captures, state_index, vt, m, word, seg_index),
        //         ParseKind::Ipa(s, m)       => self.input_match_ipa(captures, s, m, word, *seg_index),
        //         ParseKind::Matrix(m, v)    => self.input_match_matrix(captures, m, v, word, seg_index),
        //         ParseKind::Syllable(..) => todo!(),
        //         _ => unimplemented!(),
        //     };
        //     if res? {
        //         return Ok(true)
        //     } else { // NOTE(girv): not needed, but it's more explicit
        //         continue;
        //     }
        // }
        // Ok(false)
        todo!()
    }

    fn context_match_var(&self, vt: &Token, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            match var {
                VarKind::Segment(s) => if self.context_match_ipa(s, mods, word, *pos)? {
                    if forwards { pos.increment(word); } else { pos.decrement(word); }
                    Ok(true)
                } else { Ok(false) },
                VarKind::Syllable(s) => self.context_match_syll_var(s, mods, word, pos, forwards),
            }            
        } else {
            Err(RuleRuntimeError::UnknownVariable(vt.clone()))
        }
    }

    fn context_match_syll_var(&self, syll_to_match: &Syllable, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        
        if (forwards && !pos.at_syll_start()) || (!forwards && !pos.at_syll_end(word)) {
            return Ok(false)
        }

        let cur_syll = if word.in_bounds(*pos) {
             &word.syllables[pos.syll_index]
        } else {
            return Ok(false)
        };
        
        if mods.is_none() {
            if *cur_syll != *syll_to_match {
                return Ok(false)
            }
        } else {
            todo!("Need to Compare Mods")
        }

        if forwards {
            pos.syll_index += 1;
            pos.seg_index = 0;
        } else {
            pos.seg_index = 0;
            pos.decrement(word);
        }
        
        Ok(true)
    }

    fn context_match_syll(&self, stress: &[Option<ModKind>; 2], tone: &Option<String>, var: &Option<usize>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        if (forwards && pos.at_syll_start()) || (!forwards && pos.at_syll_end(word)) {
            let cur_syll = if word.in_bounds(*pos){ 
                &word.syllables[pos.syll_index] 
            } else { 
                return Ok(false)
            };

            if !self.match_stress(stress, cur_syll) {
                return Ok(false)
            }
            if let Some(t) = tone.as_ref() {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
            }
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[pos.syll_index].clone()));
            }

            if forwards {
                pos.syll_index += 1;
                pos.seg_index = 0;
            } else {
                pos.seg_index = 0;
                pos.decrement(word);
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn context_match_matrix(&self, mods: &Modifiers, var: &Option<usize>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {        
        if word.out_of_bounds(*pos) {
            return Ok(false)
        }
        if self.match_modifiers(mods, word, pos)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Segment(word.get_seg_at(*pos).unwrap()));
            }
            if forwards {
                pos.increment(word);
            } else {
                pos.decrement(word);
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn transform(&self, word: &Word, input: Vec<MatchElement>, current_pos: &mut Option<SegPos>) -> Result<Word, RuleRuntimeError> {
        match self.rule_type {
            RuleType::Metathesis => {
                let mut res_word = word.clone();
                for z in 0..(input.len() / 2) {
                    match (input[z], input[input.len()-1-z]) {
                        (MatchElement::Segment(i), MatchElement::Segment(j)) => {
                            // FIXME: If we swap syllables or boundaries then do this, these SegPos may not be correct
                            let si = res_word.get_seg_at(i).unwrap();
                            let sj = res_word.get_seg_at(j).unwrap();
                            let tmp = si;
                            res_word.syllables[i.syll_index].segments[i.seg_index] = sj;
                            res_word.syllables[j.syll_index].segments[j.seg_index] = tmp;
                        },
                        (MatchElement::Syllable(i), MatchElement::Syllable(j)) => {
                            res_word.swap_syll(i, j);
                        },
                        (MatchElement::SyllBound(_), MatchElement::SyllBound(_)) => {/* Do nothing */},
                        (MatchElement::Segment(si), MatchElement::SyllBound(bi)) => {
                            // TODO(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules                            
                            let seg = res_word.syllables[si.syll_index].segments[si.seg_index];
                            if bi < res_word.syllables.len() {
                                res_word.syllables[bi].segments.push_front(seg);
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_back()                                
                            } else {
                                res_word.syllables.push(Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: String::new() });
                                res_word.syllables.last_mut().unwrap().segments.push_front(seg);
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_back()
                            }                 

                            if res_word.syllables[si.syll_index].segments.is_empty() {
                                res_word.syllables.remove(si.syll_index);
                            }
                        },
                        (MatchElement::SyllBound(bi), MatchElement::Segment(si)) => {
                            // TODO(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules
                            let seg = res_word.syllables[si.syll_index].segments[si.seg_index];
                            if bi > 0 {
                                res_word.syllables[bi-1].segments.push_back(seg);
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()                                
                            } else {
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()
                                res_word.syllables.insert(0, Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: String::new() });
                                res_word.syllables.first_mut().unwrap().segments.push_front(seg);
                            }
                            if res_word.syllables[si.syll_index].segments.is_empty() {
                                res_word.syllables.remove(si.syll_index);
                            }
                        },
                        // TODO(girv): I think we're just gonna disallow these, I can't think of a valid rule where these make sense
                        (MatchElement::Segment(_), MatchElement::Syllable(_)) => todo!(),
                        (MatchElement::Syllable(_), MatchElement::Segment(_)) => todo!(),
                        (MatchElement::Syllable(_), MatchElement::SyllBound(_)) => todo!(),
                        (MatchElement::SyllBound(_), MatchElement::Syllable(_)) => todo!(),
                    }
                }

                Ok(res_word)
            },
            RuleType::Deletion => {
                let mut res_word = word.clone();
                for z in input.into_iter().rev() {
                    match z {
                        MatchElement::Segment(i) => {
                            // remove segment                             
                            if res_word.syllables.len() <= 1 && word.syllables[i.syll_index].segments.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySeg)
                            }
                            res_word.syllables[i.syll_index].segments.remove(i.seg_index);
                            // if that was the only segment in that syllable, remove the syllable
                            if res_word.syllables[i.syll_index].segments.is_empty() {
                                res_word.syllables.remove(i.syll_index);
                                                           
                                if let Some(pos) = current_pos {
                                    debug_assert!(pos.syll_index > 0);
                                    pos.syll_index -= 1;
                                }
                            }
                            if let Some(pos) = current_pos { 
                                if pos.seg_index > 0 {
                                    pos.seg_index -= 1; 
                                }
                            }
                        },
                        MatchElement::Syllable(i) => {
                            // remove syllable
                            if res_word.syllables.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySyll)
                            }
                            res_word.remove_syll(i);

                            if let Some(pos) = current_pos {
                                debug_assert!(pos.syll_index > 0);
                                pos.syll_index -= 1;
                            }
                        },
                        MatchElement::SyllBound(i) => {
                            // join the two neighbouring syllables
                            // if one has stress and/or tone, joined syll gets them
                            // if they both have stress, highest wins
                            // if they both have tone, join them i.e. ma5a1 > ma:51
                            if res_word.syllables.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySyll)
                            }
                            
                            if i == 0 || i >= res_word.syllables.len() {
                                // can't delete a word boundary
                                continue;
                            }
                            let mut syll_segs = res_word.syllables[i].segments.clone();
                            res_word.syllables[i-1].segments.append(&mut syll_segs);

                            res_word.syllables[i-1].stress = match (res_word.syllables[i-1].stress, res_word.syllables[i].stress) {
                                (StressKind::Primary, _) | (_, StressKind::Primary) => StressKind::Primary,
                                (StressKind::Secondary, StressKind::Secondary)  | 
                                (StressKind::Secondary, StressKind::Unstressed) | 
                                (StressKind::Unstressed, StressKind::Secondary)  => StressKind::Secondary,
                                (StressKind::Unstressed, StressKind::Unstressed) => StressKind::Unstressed,
                            };
                            let syll_tone = res_word.syllables[i].tone.clone();
                            res_word.syllables[i-1].tone.push_str(&syll_tone);
                            res_word.syllables.remove(i);

                            if let Some(pos) = current_pos {
                                debug_assert!(pos.syll_index > 0);
                                pos.syll_index -= 1;
                            }
                        },
                    }
                }
                Ok(res_word)
            },
            RuleType::Insertion => {
                // find insertion position/range using context
                // if inserting syllable, assert a range not a position
                // "Parse" and insert output
                let mut res_word = word.clone();
                let mut start_index = SegPos::new(0, 0);
                let mut after = false;

                while let Some(insert_position) = self.insertion_context_match(&res_word, &mut start_index, &mut after)? {
                    println!("Match! {} at {:?}", word.render().unwrap(),insert_position);
                    res_word = self.insertion(&res_word, insert_position, after)?;
                    after = false;
                }
                Ok(res_word)
            },
            RuleType::Substitution => {
                self.substitution(word, input)
            },
        }
    }

    fn insertion_context_match(&self, word: &Word, start_pos: &mut SegPos, after: &mut bool) -> Result<Option<SegPos>, RuleRuntimeError> {
        // match before
        // match after
        // return the index after before 

        let mut cur_pos = *start_pos;
        let mut state_index = 0;
        let mut match_begin = None;

        let empty_env = Item::new(ParseKind::Environment(vec![], vec![]), crate::Position { line: 0, start: 0, end: 0 });

        let ParseKind::Environment(before_states, after_states) = &match &self.context {
            Some(s) => s,
            None => match &self.except {
                Some(_) => &empty_env,
                None => return  Err(RuleRuntimeError::InsertionNoContextOrException(self.output.last().unwrap().position)),
            },
        }.kind else { unreachable!() };

        let ParseKind::Environment(before_except_states, after_except_states) = &match &self.except {
            Some(ex) => ex,
            None => match &self.context {
                Some(_) => &empty_env,
                None => todo!(),
            },
        }.kind else { unreachable!() };

        if before_states.is_empty() && after_states.is_empty() && before_except_states.is_empty() && after_except_states.is_empty() {
            return Err(RuleRuntimeError::InsertionNoContextOrException(self.output.last().unwrap().position))
        }

        if !before_states.is_empty() {
            while word.in_bounds(cur_pos) {
                if self.insertion_context_match_item(&mut cur_pos, &mut state_index, word, before_states, false)? {
                    if match_begin.is_none() { 
                        // if we haven't started matching, we have now
                        match_begin = Some(cur_pos)
                    }
                    if state_index > before_states.len() - 1 { 
                        break; // match
                    }
                    // else continue 
                } else if match_begin.is_none() { 
                    // if we weren't in the middle of matching and didn't no match, move on
                    cur_pos.increment(word);
                    // NOTE(girv): Should be unnecessary, but safety first!:
                    state_index = 0;
                } else { 
                    // if we were in the middle of matching but now don't match, go back to when we started matching +1 and start again
                    cur_pos = match_begin.unwrap();
                    cur_pos.increment(word);
                    state_index = 0;
                    match_begin = None;
                }
            }
            // if we've got to the end of the word and we haven't began matching
            if match_begin.is_none() { 
                return Ok(None)
            }
            match before_states.last().unwrap().kind {
                // if we've reached the end of the word and the last state is a word boundary
                ParseKind::WordBound | ParseKind::SyllBound => {},
                // No Match
                _ => { return Ok(None) }
            }
        }

        let mut insertion_position = cur_pos;

        // To avoid an infinite loop
        if let Some(st) = before_states.last() {
            if let ParseKind::WordBound | ParseKind::SyllBound = st.kind {
                start_pos.increment(word)
            }
        }

        state_index = 0;
        match_begin = None;

        if !after_states.is_empty() {
            if before_states.is_empty() {
                while word.in_bounds(cur_pos) {
                    if self.insertion_context_match_item(&mut cur_pos, &mut state_index, word, after_states, true)? {
                        if match_begin.is_none() {
                            match_begin = Some(cur_pos)
                        }
                        if state_index > after_states.len() - 1 {
                            break;
                        }
                    } else if match_begin.is_none() {
                        cur_pos.increment(word);
                        state_index = 0;
                    } else {
                        cur_pos = match_begin.unwrap();
                        cur_pos.increment(word);
                        state_index = 0;
                        match_begin = None;
                    }
                }
                if match_begin.is_none() { 
                    return Ok(None)
                }
                match after_states.last().unwrap().kind {
                    ParseKind::WordBound | ParseKind::SyllBound => {*start_pos = cur_pos},
                    _ => { return Ok(None) }
                }
                insertion_position = cur_pos;
                *after = true;
            } else {
                while word.in_bounds(cur_pos) {
                    if !self.insertion_context_match_item(&mut cur_pos, &mut state_index, word, after_states, true)? {
                        return Ok(None)
                    }
                }
            } 
        }

        // To avoid an infinite loop
        if let Some(st) = after_states.last() {
            if let ParseKind::WordBound | ParseKind::SyllBound = st.kind {
                start_pos.increment(word)
            }
        }
        
        //TODO(girv): match before and after exceptions

        Ok(Some(insertion_position))
    }

    fn insertion_context_match_item(&self, cur_pos: &mut SegPos, state_index: &mut usize, word: &Word, states: &[Item], is_context_after: bool) -> Result<bool, RuleRuntimeError> {
        match &states[*state_index].kind {
            ParseKind::WordBound => if (!is_context_after && cur_pos.at_word_start()) || (is_context_after && cur_pos.at_word_end(word)) {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Ipa(s, m) => if self.context_match_ipa(s, m, word, *cur_pos)? {
                    cur_pos.increment(word);
                    *state_index += 1;
                    Ok(true)
            } else { Ok(false) },
            // ParseKind::SyllBound => if (!is_context_after && cur_pos.at_syll_start()) || (is_context_after && cur_pos.at_syll_end(word)) {
            ParseKind::SyllBound => if cur_pos.at_syll_start() {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Ellipsis => todo!(),
            ParseKind::Syllable(_, _, _) => todo!(),
            ParseKind::Set(_) => todo!(),
            ParseKind::Matrix(_, _) => todo!(),
            ParseKind::Optional(_, _, _) => todo!(),
            ParseKind::Variable(_, _) => todo!(),


            ParseKind::EmptySet | ParseKind::Metathesis |
            ParseKind::Environment(_, _) => unreachable!(),
        }
    }

    fn context_match_ipa(&self, s: &Segment, mods: &Option<Modifiers>, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {

        if word.out_of_bounds(pos) {
            return Ok(false)
        }
        let seg = word.get_seg_at(pos).unwrap();
        if mods.is_none() {
            Ok(*s == seg)
        } else {
            todo!("Need to Compare Modifiers")
        }
    }

    fn insertion(&self, word: &Word, insert_pos: SegPos, after: bool) -> Result<Word, RuleRuntimeError> {
        let mut res_word = word.clone();
        let mut insert_pos = insert_pos;
        
        for (state_index, state) in self.output.iter().enumerate() {
            match &state.kind {
                ParseKind::Syllable(_stress, _tone, _var) => {
                    if insert_pos.seg_index == 0 {
                        // Will have to error as a syllable must have segments
                        todo!("ERR")
                    } else {
                        // split current syll into two at insert_pos
                        todo!("Split")
                    }
                },
                ParseKind::Ipa(seg, mods) => {
                    if after {
                        res_word.syllables[insert_pos.syll_index].segments.insert(insert_pos.seg_index+1, *seg)
                    } else {
                        res_word.syllables[insert_pos.syll_index].segments.insert(insert_pos.seg_index, *seg)
                    }
                    if let Some(_m) = mods {
                        todo!("Apply mods");
                    }
                    insert_pos.increment(&res_word);
                },
                ParseKind::Variable(_num, _mods) => todo!(),
                
                
                ParseKind::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(state.position)),
                ParseKind::Set(_) => return Err(RuleRuntimeError::LonelySet(state.position)),
                ParseKind::EmptySet      | ParseKind::Metathesis | 
                ParseKind::SyllBound     | ParseKind::Ellipsis   | 
                ParseKind::Optional(..)  | ParseKind::WordBound  | 
                ParseKind::Environment(..) => unreachable!(),
            }
        }

        Ok(res_word)
    }

    fn apply_syll_mods(&self, word: &mut Word, syll_index: usize, mods: &Modifiers, var: &Option<usize>) -> Result<(), RuleRuntimeError> {
        // NOTE(girv): Maybe we should error or give warnings if we have non-syllable features 
        if let Some(_v) = var {
            todo!("Deal with variable")
        } else {
            word.syllables.get_mut(syll_index).unwrap().apply_mods(&self.variables, &mods.suprs)
        }
    }
    
    fn apply_seg_mods(&self, word: &mut Word, pos: SegPos, mods: &Modifiers, var: &Option<usize>) -> Result<(), RuleRuntimeError>{
        if let Some(_v) = var {
            todo!("Deal with variable")
        } else {
            word.apply_mods(&self.alphas, mods, pos)
        }
    }
    
    fn substitution(&self, word: &Word, input: Vec<MatchElement>) -> Result<Word, RuleRuntimeError> {
        let mut res_word = word.clone();
        for (si, (in_state, out_state)) in self.input.iter().zip(&self.output).enumerate() {
            match &out_state.kind {
                ParseKind::Syllable(_, _, _) => todo!(),
                ParseKind::Matrix(m, v) => {
                    // get match at index and check it's a segment/or syllable and not a boundary
                    // if a syllable, make sure only do Syllable Suprs
                    // apply changes
                    match input[si] {
                        MatchElement::Segment(sp)   => self.apply_seg_mods(&mut res_word, sp, m, v)?,
                        MatchElement::Syllable(sp)  => self.apply_syll_mods(&mut res_word, sp, m, v)?,
                        MatchElement::SyllBound(_)  => todo!("Err: Can't apply matrix to syllable boundary"),
                    }
                },
                ParseKind::Ipa(seg, mods) => match input[si] {
                    MatchElement::Segment(sp) => {
                        // "Replace with output IPA.
                        res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                        // Apply Mods
                        if let Some(m) = mods {
                            res_word.apply_mods(&self.alphas, m, sp)?;
                        }
                    },    
                    MatchElement::Syllable(_) => todo!("Probably Err"),
                    MatchElement::SyllBound(_) => todo!("Err"),
                },
                ParseKind::Variable(_, _) => todo!(),
                ParseKind::Set(_) => {
                    // Check that self.input[si] is a set, if not throw RuleRuntimeError::LonelySet(state.position)
                    // Check both sets have the same number of elements 
                    // See which one of the input set matched and use the corresponding in output to substitute
                    todo!()
                },
                
                ParseKind::EmptySet      | ParseKind::Metathesis | 
                ParseKind::SyllBound     | ParseKind::Ellipsis   | 
                ParseKind::Optional(..)  | ParseKind::WordBound  | 
                ParseKind::Environment(..) => unreachable!(),
            }
        }

        if self.output.len() > self.input.len() {
            todo!("insert remaining outputs");
        } else if self.input.len() > self.output.len() {
            todo!("remove remaining inputs");
        }


        Ok(res_word)
    }

    fn input_match_at(&self, word: &Word, start_index: SegPos) -> Result<(Vec<MatchElement>, Option<SegPos>), RuleRuntimeError> {
        // TODO(girv): match context and exceptions
        let mut cur_index = start_index;
        let mut match_begin = None;
        let mut state_index = 0;
        let mut captures: Vec<_> = Vec::new();

        while word.in_bounds(cur_index) {
            if self.input_match_item(&mut captures, &mut cur_index, &mut state_index, word, &self.input)? {
                if state_index > self.input.len() - 1 { 
                    // if we have a full match             

                    // To avoid an infinite loop
                    if self.input.last().unwrap().kind == ParseKind::SyllBound {
                        cur_index.increment(word);
                    }
                    return Ok((captures, Some(cur_index)));
                }
                if match_begin.is_none() { 
                    // if we haven't started matching, we have now
                    match_begin = Some(cur_index)
                }
                // else continue 
            } else if match_begin.is_none() { 
                // if we weren't in the middle of matching and didn't no match, move on
                cur_index.increment(word);
                // NOTE(girv): Should be unnecessary, but safety first!:
                state_index = 0;
                captures = vec![];  
            } else { 
                // if we were in the middle of matching but now don't match, go back to when we started matching +1 and start again
                cur_index = match_begin.unwrap();
                cur_index.increment(word);
                state_index = 0;
                captures = vec![];
                match_begin = None;
            }
        }

        if match_begin.is_none() { // if we've got to the end of the word and we haven't began matching
            Ok((vec![], None))
        } else if let ParseKind::WordBound | ParseKind::SyllBound = self.input.last().unwrap().kind {
            // if we've reached the end of the word and the last state is a word boundary
            captures.push(MatchElement::SyllBound(word.syllables.len()));
            Ok((captures, None))
        } else { // No Match
            Ok((vec![], None))
        }
    }


    fn input_match_item(
        &self, 
        captures: &mut Vec<MatchElement>, 
        seg_pos: &mut SegPos, 
        state_index: &mut usize,
        word: &Word, 
        states: &[Item], 
    ) -> Result<bool, RuleRuntimeError> {
        match &states[*state_index].kind {
            ParseKind::Variable(vt, m) => if self.input_match_var(captures, state_index, vt, m, word, seg_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Ipa(s, m) => if self.input_match_ipa(captures, s, m, word, *seg_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Matrix(m, v) => if self.input_match_matrix(captures, m, v, word, seg_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true) 
            } else { Ok(false) },
            ParseKind::Set(s) => if self.input_match_set(captures, state_index, s, word, seg_pos)? {
                seg_pos.increment(word); // TODO(girv): when we allow boundaries within sets, this will have to be incremented within the match_set function
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::SyllBound => if self.input_match_syll_bound(captures, *seg_pos) {
                // NOTE(girv): Boundaries do not advance seg_index 
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Syllable(s, t, v) => self.input_match_syll(captures, state_index, s, t, v, word, seg_pos),
            ParseKind::Ellipsis => self.input_match_ellipsis(captures, word, seg_pos, states, state_index),
            ParseKind::Optional(opt_states, match_min, match_max) => self.input_match_optionals(captures, word, seg_pos, states, opt_states, *match_min, *match_max),            
            ParseKind::EmptySet | ParseKind::WordBound | ParseKind::Metathesis | ParseKind::Environment(_, _) => unreachable!(),
        }
    }

    fn input_match_optionals(&self, captures: &mut [MatchElement], word: &Word, pos: &mut SegPos, states: &[Item], opt_states: &[Item], match_min: usize, match_max: usize) -> Result<bool, RuleRuntimeError> {
        // should work like regex (...){min, max}? 
        let max = if match_max == 0 {None} else{ Some(match_max)};
        self.input_match_multiple(captures, word, pos,  states, &mut 0, match_min, max, opt_states, true)
    }
    
    fn input_match_ellipsis(&self, captures: &mut [MatchElement], word: &Word, pos: &mut SegPos, states: &[Item], state_index: &mut usize) -> Result<bool, RuleRuntimeError> {
        // should work akin to '.+?' in Regex, that is, a lazy-match of one-or-more elements
        // this should not capture, however
        self.input_match_multiple(captures, word, pos, states, state_index, 1, None, &[], false)
    }

    fn input_match_multiple(
        &self, captures: &mut [MatchElement], 
        word: &Word, seg_index: &mut SegPos, 
        states: &[Item], state_index: &mut usize, 
        match_min: usize, match_max: Option<usize>,
        inner_states: &[Item], capture_wanted: bool    
    ) -> Result<bool, RuleRuntimeError> {

        let back_state = *state_index;
        let back_seg = *seg_index;
        let mut caps: Vec<MatchElement> = vec![];

        let mut i = 0;
        while i < match_min {
            if inner_states.is_empty() {
                // TODO(girv): test for OB1 error

                if word.out_of_bounds(*seg_index) {
                // if *seg_index >= word.seg_count() {
                    return Ok(false)
                }
                
                if capture_wanted {
                    // add to caps
                    caps.push(MatchElement::Segment(*seg_index))
                }

                seg_index.increment(word);
                i += 1;
            } else {
                // NOTE: segs are captured regardless of the `capture_wanted` check
                let mut inner_state_index = 0;

                while word.in_bounds(*seg_index) && inner_state_index < inner_states.len() {
                // while *seg_index <= word.seg_count() && inner_state_index < inner_states.len(){
                    if !self.input_match_item(&mut caps, seg_index, &mut inner_state_index, word, inner_states)? {
                        *seg_index = back_seg;
                        *state_index = back_state;
                        return Ok(false)
                    }
                }
                if word.out_of_bounds(*seg_index) {
                // if *seg_index >= word.seg_count() {
                    return Ok(false)
                }
                i += 1;
            }
        }

        todo!()
        
        // for i < match_max {
        // 
        //
        //
        // 
        // }
        
        
        // add caps to captures
        // return Ok(true)
    }

    fn input_match_syll(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, stress: &[Option<ModKind>;2], tone: &Option<String>, var: &Option<usize>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        // checks current segment is at start of syll
        // matches stress and tone
        // jumps to end of syllable if match
        if word.in_bounds(*pos) && pos.seg_index == 0 {
        // if word.seg_is_syll_initial(*seg_index) {
            let cur_syll_index = pos.syll_index;
            let cur_syll = &word.syllables[cur_syll_index];

            if !self.match_stress(stress, cur_syll) {
                return Ok(false)
            }
            if let Some(t) = tone.as_ref() {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
            }
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[pos.syll_index].clone()));
            }
            captures.push(MatchElement::Syllable(cur_syll_index));
            *state_index += 1;
            pos.syll_index += 1;
            pos.seg_index = 0;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_stress(&self, stress: &[Option<ModKind>; 2], syll: &Syllable) -> bool {
        // ±stress (+ matches prim and sec, - matches unstressed)
        if let Some(str) = stress[0] {
            match str {
                ModKind::Binary(b) => match b {
                    BinMod::Negative => if syll.stress != StressKind::Unstressed { return false },
                    BinMod::Positive => if syll.stress == StressKind::Unstressed { return false },
                },
                ModKind::Alpha(_) => todo!(),
            }
        }
        // ±secstress (+ matches sec, - matches prim and unstressed)
        if let Some(str) = stress[1] {
            match str {
                ModKind::Binary(b) => match b {
                    BinMod::Negative => if syll.stress == StressKind::Secondary { return false },
                    BinMod::Positive => if syll.stress != StressKind::Secondary { return false },
                },
                ModKind::Alpha(_) => todo!(),
            }
        }
        true
    }

    fn match_tone(&self, tone: &str, syll: &Syllable) -> bool {        
        tone == syll.tone
    }

    fn input_match_syll_bound(&self, captures: &mut Vec<MatchElement>, pos: SegPos) -> bool {
        if pos.seg_index == 0 {
            captures.push(MatchElement::SyllBound(pos.syll_index));
            true
        } else {
            false
        }
    }

    fn input_match_set(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, set: &[Item], word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        for s in set {
            let res = match &s.kind {
                ParseKind::Variable(vt, m) => self.input_match_var(captures, state_index, vt, m, word, pos),
                ParseKind::Ipa(s, m)       => self.input_match_ipa(captures, s, m, word, *pos),
                ParseKind::Matrix(m, v)    => self.input_match_matrix(captures, m, v, word, pos),
                ParseKind::Syllable(..) => todo!(),
                _ => unimplemented!(),
            };
            if res? {
                return Ok(true)
            } else { // NOTE(girv): not needed, but it's more explicit
                continue;
            }
        }
        Ok(false)
    }

    fn input_match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(pos).unwrap();
        if mods.is_none() {
            if *s == seg {
                captures.push(MatchElement::Segment(pos));
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            todo!("Need to Compare Modifiers")
        }
    }

    // fn match_syll_var(&self, captures: &mut Vec<MatchElement>, s: &Syllable, mods: &Option<Modifiers>, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
    fn input_match_syll_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, syll_to_match: &Syllable, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        if pos.seg_index != 0 || word.out_of_bounds(*pos){
            return Ok(false)
        }
        let csi = pos.syll_index;
        let cur_syll = &word.syllables[csi];
        
        if mods.is_none() {
            if *cur_syll != *syll_to_match {
                return Ok(false)
            }
            captures.push(MatchElement::Syllable(csi));
    
            *state_index += 1;
            pos.syll_index += 1;
            pos.seg_index = 0;

            // Because we are incrementing in the parent function in the case of a match
            // We must jump to the next syllable but not skip the segment at index 0
            if pos.syll_index < word.syllables.len() {
                pos.decrement(word);
            }
            
            Ok(true)
        } else {
            todo!("Need to Compare Mods")
        }

    }

    fn input_match_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, vt: &Token, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            match var {
                VarKind::Segment(s)  => self.input_match_ipa(captures, s, mods, word, *pos),
                VarKind::Syllable(s) => self.input_match_syll_var(captures, state_index , s, mods, word, pos),
            }
            // NOTE(girv): we should not push here
        } else {
            Err(RuleRuntimeError::UnknownVariable(vt.clone()))
        }
    }

    fn input_match_matrix(&self, captures: &mut Vec<MatchElement>, mods: &Modifiers, var: &Option<usize>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> { 
        if self.match_modifiers(mods, word, pos)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Segment(word.get_seg_at(*pos).unwrap()));
            }
            captures.push(MatchElement::Segment(*pos));
            // the way we implement `long` vowels means we need to do this
            let mut seg_length = word.seg_length_in_syll(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(true)
        } else {
            // the way we implement `long` vowels means we need to do this
            let mut seg_length = word.seg_length_in_syll(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(false)
        }
    }

    fn match_modifiers(&self, mods: &Modifiers, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(*pos).expect("Segment Position should be within bounds");
        
        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i, seg)? {
                return Ok(false);
            }
        }
        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.match_node_mod(m, i, seg)? {
                return Ok(false);
            }
        }
        self.match_supr_mod_seg(word, &mods.suprs, pos)
    }

    fn match_supr_mod_seg(&self, word: &Word, mods: &SupraSegs, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {

        let syll = &word.syllables[pos.syll_index];

        if !self.match_stress(&mods.stress, syll) { return Ok(false) }
        if !self.match_seg_length(word, &mods.length, pos) { return Ok(false) }

        if let Some(t) = mods.tone.as_ref() {
            return Ok(self.match_tone(t, syll))
        }

        Ok(true)
    }

    fn match_seg_length(&self, word: &Word, length: &[Option<ModKind>; 2], pos: &mut SegPos) -> bool {
        let seg_length = word.seg_length_in_syll(*pos);
        // +/- long
        if let Some(len) = length[0] {
            match len {
                ModKind::Binary(b) => match b {
                    BinMod::Negative => if seg_length > 1 { return false },
                    BinMod::Positive => if seg_length < 2 { return false },
                },
                ModKind::Alpha(_) => todo!(),
            }
        }
        // +/- overlong
        if let Some(len) = length[1] {
            match len {
                ModKind::Binary(b) => match b {
                    BinMod::Negative => if seg_length > 2 { return false },
                    BinMod::Positive => if seg_length < 3 { return false },
                },
                ModKind::Alpha(_) => todo!(),
            }
        }
        true
    }

    fn match_node_mod(&self, md:&Option<ModKind>, node_index: usize, seg: Segment) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md {
            let node = NodeKind::from_usize(node_index);
            return self.match_node(seg, node, kind)
        }
        Ok(true)
    }

    fn match_feat_mod(&self, md: &Option<ModKind>, feat_index: usize, seg: Segment) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md { 
            let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
            return self.match_seg_kind(kind, seg, node, mask)
        }
        Ok(true)
    }

    fn match_node(&self, seg: Segment, node: NodeKind, val: &ModKind) -> Result<bool, RuleRuntimeError> {
        match val {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => Ok(seg.get_node(node).is_none()),
                BinMod::Positive => Ok(seg.get_node(node).is_some()),
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(a) => {
                    if let Some(alph) = self.alphas.borrow().get(a) {
                        if let Some((n, m)) = alph.as_node() {
                            return Ok(seg.node_match(*n, *m))
                        } else if let Some((n, l, c, d, p)) = alph.as_place() {
                            return Ok(
                                seg.node_match(*n, *l) &&
                                seg.node_match(*n, *c) &&
                                seg.node_match(*n, *d) &&
                                seg.node_match(*n, *p)
                            )
                        } else {
                            todo!("err: Alpha is not node");
                        }
                    }
                    if node == NodeKind::Place {
                         let l = seg.get_node(NodeKind::Labial);
                         let c = seg.get_node(NodeKind::Coronal);
                         let d = seg.get_node(NodeKind::Dorsal);
                         let p = seg.get_node(NodeKind::Pharyngeal);

                        self.alphas.borrow_mut().insert(*a, Alpha::Place(node, (l, c, d, p))); 
                    } else {
                        self.alphas.borrow_mut().insert(*a, Alpha::Node(node, seg.get_node(node))); 
                    }
                    Ok(true)
                },
                AlphaMod::InvAlpha(ia) => {
                    if let Some(alph) = self.alphas.borrow().get(ia) {
                        if let Some((n, m)) = alph.as_node() {
                            return Ok(!seg.node_match(*n, *m))
                        } else if let Some((n, l, c, d, p)) = alph.as_place() {
                            return Ok(
                                !seg.node_match(*n, *l) &&
                                !seg.node_match(*n, *c) &&
                                !seg.node_match(*n, *d) &&
                                !seg.node_match(*n, *p)
                            )
                        } else {
                            todo!("err: Alpha is not node");
                        }
                    }
                    if node == NodeKind::Place {
                        let l = seg.get_node(NodeKind::Labial);
                        let c = seg.get_node(NodeKind::Coronal);
                        let d = seg.get_node(NodeKind::Dorsal);
                        let p = seg.get_node(NodeKind::Pharyngeal);

                        self.alphas.borrow_mut().insert(*ia, Alpha::Place(node, (l, c, d, p))); 
                    } else {
                        self.alphas.borrow_mut().insert(*ia, Alpha::Node(node, seg.get_node(node)));
                    }
                    Ok(true)
                },
            },
        }
    }

    fn match_seg_kind(&self, kind: &ModKind, seg: Segment, node: NodeKind, mask: u8) -> Result<bool, RuleRuntimeError> {
        match kind {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => Ok(seg.feat_match(node, mask, false)),
                BinMod::Positive => Ok(seg.feat_match(node, mask, true)),
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(a) => {
                    if let Some(alph) = self.alphas.borrow().get(a) {
                        if let Some((&n, &m, &pos)) = alph.as_feature() {
                            return Ok(seg.feat_match(n, m, pos))
                        } else {
                            todo!("Err: Alpha is not feature")
                        }
                    } 
                    if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*a, Alpha::Feature(node, mask, f != 0)); 
                        Ok(true)
                    } else {
                        // Maybe err?
                        Ok(false)
                    }
                },
                AlphaMod::InvAlpha(ia) => {
                    if let Some(alph) = self.alphas.borrow().get(ia) {
                        if let Some((&n, &m, &pos)) = alph.as_feature() {
                            return Ok(seg.feat_match(n, m, !pos))
                        } else {
                            todo!("Err: Alpha is not feature")
                        }
                    } 
                    if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*ia, Alpha::Feature(node, mask, f == 0));
                        Ok(true)
                    } else {
                        // Maybe err?
                        Ok(false)
                    } 
                },
            },
        }
    }
}