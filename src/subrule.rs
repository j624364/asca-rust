// NOTE(girv): lots of duplication here atm, focusing on getting things done before optimising
// TODO

use std ::{
    cell::RefCell, 
    collections::{HashMap, VecDeque}
};

use crate ::{
    error :: RuleRuntimeError, 
    lexer ::{FType, Position, Token}, 
    parser::{AlphaMod, BinMod, Item, ModKind, Modifiers, ParseElement, SupraSegs}, 
    rule  ::{Alpha, PlaceMod, RuleType}, 
    seg   ::{feature_to_node_mask, NodeKind, Segment}, 
    syll  ::{StressKind, Syllable}, 
    word  ::{SegPos, Word},
};

type SylPos = usize;            // the index of the syllable in the word.syllables array
type BndPos = usize;            // the index of the syllable that has the boundary as it's start
type SetInd = Option<usize>;    // if matched in a set, the index within that set that was matched

#[derive(Debug, Clone, Copy)]
pub enum MatchElement {
    Segment  (SegPos, SetInd),
    Syllable (SylPos, SetInd),
    SyllBound(BndPos, SetInd)
}

impl MatchElement {
    pub fn set_ind(&mut self, si: SetInd) {
        *self = match self {
            MatchElement::Segment(sp, _) => MatchElement::Segment(*sp, si),
            MatchElement::Syllable(sp, _) => MatchElement::Syllable(*sp, si),
            MatchElement::SyllBound(bp, _) => MatchElement::SyllBound(*bp, si),
        }
    }
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

        if self.rule_type == RuleType::Insertion {
            return self.transform(&word, vec![], &mut None)
        } 
        
        let mut word = word;
        let mut cur_index = SegPos::new(0, 0);
        // TODO(girv): `$ > *` or any broad deletion rule without context/exception should  give a warning to the user
        loop {
            let (res, mut next_index) = self.input_match_at(&word, cur_index)?;
            if !res.is_empty() {
                let start = match res[0] {
                    MatchElement::Segment(sp, _)  => sp,
                    MatchElement::Syllable(s, _)  |
                    MatchElement::SyllBound(s, _) => SegPos::new(s, 0),
                };
                let end = match *res.last().unwrap() {
                    MatchElement::Segment(mut sp, _)  => {
                        // So that long vowels work
                        let mut seg_len = word.seg_length_in_syll(sp);
                        while seg_len > 1 {
                            sp.increment(&word);
                            seg_len -= 1;
                        }
                        sp
                    },
                    MatchElement::SyllBound(s, _) => {
                        let mut sp = SegPos::new(s, 0);
                        if s != 0 {
                            sp.decrement(&word);
                        } // else {
                            // FIXME(girv): this may lead to bugs 
                        // }
                        sp
                    },
                    MatchElement::Syllable(s, _)  => SegPos::new(s, word.syllables[s].segments.len()-1),
                };
                if !self.match_before_context_and_exception(&word, start)? || !self.match_after_context_and_exception(&word, end)? {
                    if let Some(ci) = next_index { 
                        cur_index = ci;
                        continue;
                    }
                    // end of word
                    break;
                }
                let prev = word.render().unwrap();
                println!("Match! {:?}", res);
                word = self.transform(&word, res, &mut next_index)?;
                println!("{} => {}", prev, word.render().unwrap());
                
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
        let empty_env = Item::new(ParseElement::Environment(vec![], vec![]), crate::Position { line: 0, start: 0, end: 0 });
        let ParseElement::Environment(states, _) = &match &self.context {
            Some(s) => s,
            None => match &self.except {
                Some(_) => &empty_env,
                None => return Ok(true),
            },
        }.kind else { unreachable!() };
        let ParseElement::Environment(except_states, _) = &match &self.except {
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
        let mut is_except_match = !except_states.is_empty();
        for (si, state) in except_states.iter().rev().enumerate() {
            if !self.context_match(word, state, &mut start_pos, false, si)? {
                println!("{} {}", word.render().unwrap(), state);
                is_except_match = false;
            }
        }
        Ok(!is_except_match && is_context_match)
    }

    fn match_after_context_and_exception(&self, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
        // TODO(girv): Don't know if this is actually better doing what I'm doing in before context match, so I'll leave them as different until I can bench them
        const EMPTY_ENV: Item = Item{ kind: ParseElement::Environment(vec![], vec![]), position: crate::Position { line: 0, start: 0, end: 0 }};
        let binding = EMPTY_ENV;
        let ParseElement::Environment(_, states) = &match &self.context {
            Some(s) => s,
            None => match &self.except {
                Some(_) => &binding,
                None => return Ok(true),
            },
        }.kind else { unreachable!() };
        let ParseElement::Environment(_, except_states) = &match &self.except {
            Some(ex) => ex,
            None => match &self.context {
                Some(_) => &binding,
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
        let mut is_except_match = !except_states.is_empty();
        for (si, state) in except_states.iter().enumerate() {
            if !self.context_match(word, state, &mut start_pos, true, si)? {
                println!("{} {}", word.render().unwrap(), state);
                is_except_match = false;
            }
        }

        Ok(!is_except_match && is_context_match)
    }

    fn context_match(&self, word: &Word, state: &Item, pos: &mut SegPos, forwards: bool, state_index: usize) -> Result<bool, RuleRuntimeError> {
        // So that $_ and #_ works
        if state_index == 0 {
            if forwards {
                pos.increment(word);
            } else if let ParseElement::WordBound | ParseElement::SyllBound = state.kind {
                // Do Nothing
            } else {
                pos.decrement(word);
            }
        }
        // // So that things like V:[+long]_ will work 
        // if !forwards {
        //     while pos.seg_index > 0 && word.syllables[pos.syll_index].segments[pos.seg_index] == word.syllables[pos.syll_index].segments[pos.seg_index - 1] {
        //         pos.decrement(word);
        //     } 
        // }
        match &state.kind {
            ParseElement::WordBound => if (!forwards && pos.at_word_start()) || (forwards && word.out_of_bounds(*pos)) {
                Ok(true)
            } else { Ok(false) },
            ParseElement::SyllBound => if pos.at_syll_start() {
                Ok(true)
            } else { Ok(false) },
            ParseElement::Ipa(s, m) => if self.context_match_ipa(s, m, word, *pos)? {
                if forwards { pos.increment(word); } else { pos.decrement(word); }
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => self.context_match_matrix(m, v, word, pos, forwards, state.position),
            ParseElement::Syllable(s, t, v) => self.context_match_syll(s, t, v, word, pos, forwards),
            ParseElement::Variable(vt, mods) => self.context_match_var(vt, mods, word, pos, forwards),
            ParseElement::Set(s) => self.context_match_set(s, word, pos, forwards),
            ParseElement::Optional(_, _, _) => todo!(),
            ParseElement::Ellipsis => todo!(),
            
            
            ParseElement::EmptySet | ParseElement::Metathesis |
            ParseElement::Environment(_, _) => unreachable!(),
        }
    }

    fn context_match_set(&self, set: &[Item], word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        for s in set {
            let res = match &s.kind {
                ParseElement::Variable(vt, m) => self.context_match_var(vt, m, word, pos, forwards),
                ParseElement::Ipa(s, m) => if self.context_match_ipa(s, m, word, *pos)? {
                    if forwards {
                        pos.increment(word);
                    } else {
                        pos.decrement(word);
                    }
                    Ok(true)
                } else {Ok(false)},
                ParseElement::Matrix(m, v) => self.context_match_matrix(m, v, word, pos, forwards, s.position),
                ParseElement::Syllable(..) => todo!(),
                ParseElement::WordBound => todo!(),
                ParseElement::SyllBound => todo!(),
                _ => unimplemented!(),
            };
            if res? {
                return Ok(true)
            }
        }
        Ok(false)
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
        } else { return Ok(false) };
        
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
            } else { return Ok(false) };

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

    fn context_match_matrix(&self, mods: &Modifiers, var: &Option<usize>, word: &Word, pos: &mut SegPos, forwards: bool, err_pos: Position) -> Result<bool, RuleRuntimeError> {        
        if word.out_of_bounds(*pos) {
            return Ok(false)
        }
        if self.match_modifiers(mods, word, pos, err_pos)? {
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
                        (MatchElement::Segment(i, _), MatchElement::Segment(j, _)) => {
                            // FIXME: If we swap syllables or boundaries then do this, these SegPos may not be correct
                            let si = res_word.get_seg_at(i).unwrap();
                            let sj = res_word.get_seg_at(j).unwrap();
                            let tmp = si;
                            res_word.syllables[i.syll_index].segments[i.seg_index] = sj;
                            res_word.syllables[j.syll_index].segments[j.seg_index] = tmp;
                        },
                        (MatchElement::Syllable(i, _), MatchElement::Syllable(j, _)) => {
                            res_word.swap_syll(i, j);
                        },
                        (MatchElement::SyllBound(..), MatchElement::SyllBound(..)) => {/* Do nothing */},
                        (MatchElement::Segment(si, _), MatchElement::SyllBound(bi, _)) => {
                            // FIXME(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules                            
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
                        (MatchElement::SyllBound(bi, _), MatchElement::Segment(si, _)) => {
                            // FIXME(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules
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
                        (MatchElement::Segment(..), MatchElement::Syllable(..)) => todo!(),
                        (MatchElement::Syllable(..), MatchElement::Segment(..)) => todo!(),
                        (MatchElement::Syllable(..), MatchElement::SyllBound(..)) => todo!(),
                        (MatchElement::SyllBound(..), MatchElement::Syllable(..)) => todo!(),
                    }
                }
                // TODO: Update current position
                Ok(res_word)
            },
            RuleType::Deletion => {
                let mut res_word = word.clone();
                for z in input.into_iter().rev() {
                    match z {
                        MatchElement::Segment(i, _) => {
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
                        MatchElement::Syllable(i, _) => {
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
                        MatchElement::SyllBound(i, _) => {
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
                // find insertion position using context
                // "Parse" and insert output
                let mut res_word = word.clone();
                let mut start_index = SegPos::new(0, 0);
                let mut after = false;

                while let Some(mut insert_position) = self.insertion_context_match(&res_word, &start_index, &mut after)? {
                    print!("Match! {} at {:?}", word.render().unwrap(), insert_position);
                    res_word = self.insert(&res_word, &mut insert_position, after)?;
                    println!(" => {}", res_word.render().unwrap());
                    if !word.in_bounds(insert_position) { break; }
                    insert_position.increment(word);
                    start_index = insert_position;
                    after = false;
                }
                Ok(res_word)
            },
            RuleType::Substitution => self.substitution(word, input, current_pos),
        }
    }

    fn insertion_context_match(&self, word: &Word, start_pos: &SegPos, after: &mut bool) -> Result<Option<SegPos>, RuleRuntimeError> {
        // match before
        // match after
        // return the index after before 

        let mut cur_pos = *start_pos;
        let mut state_index = 0;
        let mut match_begin = None;

        let empty_env = Item::new(ParseElement::Environment(vec![], vec![]), crate::Position { line: 0, start: 0, end: 0 });

        let ParseElement::Environment(before_states, after_states) = &match &self.context {
            Some(s) => s,
            None => match &self.except {
                Some(_) => &empty_env,
                None => return Err(RuleRuntimeError::InsertionNoContextOrException(self.output.last().unwrap().position)),
            },
        }.kind else { unreachable!() };

        let ParseElement::Environment(before_except_states, after_except_states) = &match &self.except {
            Some(ex) => ex,
            None => match &self.context {
                Some(_) => &empty_env,
                None => unreachable!(),
            },
        }.kind else { unreachable!() };

        if before_states.is_empty() && after_states.is_empty() && before_except_states.is_empty() && after_except_states.is_empty() {
            return Err(RuleRuntimeError::InsertionNoContextOrException(self.output.last().unwrap().position))
        }

        if !before_states.is_empty() {
            while word.in_bounds(cur_pos) {
                if self.insertion_context_match_item(&mut cur_pos, &mut state_index, word, before_states, false)?.is_some() {
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

            match before_states.last().unwrap().kind {
                // if we've reached the end of the word and the last state is a word boundary
                // ParseKind::WordBound | ParseKind::SyllBound => {},
                // No Match
                _ if match_begin.is_none() => { return Ok(None) },
                _ => {}
            }
        }

        let mut insertion_position = cur_pos;
        
        // // To avoid an infinite loop
        // if let Some(st) = before_states.last() {
        //     if let ParseKind::WordBound | ParseKind::SyllBound = st.kind {
        //         start_pos.increment(word);
        //     }
        // }

        state_index = 0;
        match_begin = None;

        if !after_states.is_empty() {
            if before_states.is_empty() {
                while word.in_bounds(cur_pos) {
                    if let Some(match_pos) = self.insertion_context_match_item(&mut cur_pos, &mut state_index, word, after_states, true)? {
                        if match_begin.is_none() {
                            match_begin = Some(match_pos);
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
                
                insertion_position = match match_begin {
                    Some(ip) => ip,
                    None => match after_states.last().unwrap().kind {
                        ParseElement::WordBound | ParseElement::SyllBound => {
                            SegPos::new(word.syllables.len()-1, word.syllables.last().unwrap().segments.len())
                        },
                        _ => return Ok(None) 
                    },
                };                
                *after = true;
            } else {
                let mut cp = cur_pos;
                while word.in_bounds(cp) && state_index < after_states.len() {
                    if self.insertion_context_match_item(&mut cp, &mut state_index, word, after_states, true)?.is_none() {
                        return Ok(None)
                    }
                }
            } 
        }

        
        // To avoid an infinite loop
        // if let Some(st) = after_states.last() {
        //     if let ParseKind::WordBound | ParseKind::SyllBound = st.kind {
        //         start_pos.increment(word)
        //     }
        // }
        
        //TODO(girv): match before and after exceptions

        Ok(Some(insertion_position))
    }

    fn insertion_context_match_item(&self, cur_pos: &mut SegPos, state_index: &mut usize, word: &Word, states: &[Item], is_context_after: bool) -> Result<Option<SegPos>, RuleRuntimeError> {
        let err_pos = states[*state_index].position;
        match &states[*state_index].kind {
            ParseElement::WordBound => if (!is_context_after && cur_pos.at_word_start()) || (is_context_after && word.out_of_bounds(*cur_pos)) {
                *state_index += 1;
                Ok(Some(*cur_pos))
            } else { Ok(None) },
            ParseElement::Ipa(s, m) => if self.context_match_ipa(s, m, word, *cur_pos)? {
                *state_index += 1;
                let pos = *cur_pos;
                cur_pos.increment(word);
                Ok(Some(pos))
            } else { Ok(None) },
            ParseElement::SyllBound => if (!is_context_after && cur_pos.at_syll_start()) || (is_context_after && cur_pos.at_syll_end(word)) {
            // ParseKind::SyllBound => if cur_pos.at_syll_start() {
                *state_index += 1;
                Ok(Some(*cur_pos))
            } else { Ok(None) },
            ParseElement::Ellipsis => todo!(),
            ParseElement::Syllable(s, t, v) => if self.context_match_syll(s, t, v, word, cur_pos, true)? {
                *state_index += 1;
                cur_pos.decrement(word);
                Ok(Some(*cur_pos))
            } else { Ok(None) },
            ParseElement::Set(set) => if self.context_match_set(set, word, cur_pos, true)? {
                *state_index += 1;
                // I hate this, but it works for now
                // cur_pos.decrement(word);
                println!("{cur_pos:?}");
                Ok(Some(*cur_pos))
            } else { Ok(None)},
            ParseElement::Matrix(m, v) => if self.context_match_matrix(m, v, word, cur_pos, true, err_pos)? {
                *state_index += 1;
                // I hate this, but it works for now
                cur_pos.decrement(word);
                Ok(Some(*cur_pos))
            } else { Ok(None)},
            ParseElement::Variable(vt, mods) => if self.context_match_var(vt, mods, word, cur_pos, true)? {
                *state_index += 1;
                Ok(Some(*cur_pos))
            } else { Ok(None)},
            ParseElement::Optional(_, _, _) => todo!(),


            ParseElement::EmptySet | ParseElement::Metathesis |
            ParseElement::Environment(_, _) => unreachable!(),
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

    // fn input_match_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, vt: &Token, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
    //     if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
    //         match var {
    //             VarKind::Segment(s)  => self.input_match_ipa(captures, s, mods, word, *pos),
    //             VarKind::Syllable(s) => self.input_match_syll_var(captures, state_index , s, mods, word, pos),
    //         }
    //         // NOTE(girv): we should not push here
    //     } else {
    //         Err(RuleRuntimeError::UnknownVariable(vt.clone()))
    //     }
    // }

    fn insert_variable(&self, word: &mut Word, insert_pos: &mut SegPos, num: &Token, mods: &Option<Modifiers>, after: bool) -> Result<(), RuleRuntimeError> {
        if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
            match var {
                VarKind::Segment(_) => todo!(),
                VarKind::Syllable(syll) => {
                    if insert_pos.at_word_start() {
                        if after {
                            word.syllables.insert(0, syll.clone());
                            insert_pos.syll_index += 2;
                        } else {
                            word.syllables.insert(0, syll.clone());
                            insert_pos.syll_index += 1;
                        }
                    } else if insert_pos.at_syll_start() {
                        if after {
                            word.syllables.insert(insert_pos.syll_index+1, syll.clone());
                            insert_pos.syll_index += 2;
                        } else {
                            word.syllables.insert(insert_pos.syll_index, syll.clone());
                            insert_pos.syll_index += 1;
                        }
                    } else {
                        todo!("Err: Can't insert syllable here")
                    }
                },
            }
            Ok(())
        } else {
            Err(RuleRuntimeError::UnknownVariable(num.clone()))
        }
    }

    fn insert_syllable(&self, word: &mut Word, insert_pos: &mut SegPos, stress: &[Option<ModKind>; 2], tone: &Option<String>, var: &Option<usize>) -> Result<(), RuleRuntimeError> {
        // split current syll into two at insert_pos
        // Apply stress etc. to second syll
        if insert_pos.at_syll_start() || insert_pos.at_syll_end(word) {
            // Will have to error as one of the the resulting syllables would be empty
            todo!("Err: Can't insert syllable here")
        } 

        let mut new_syll = Syllable::new();
        new_syll.apply_mods(&self.alphas, /*&self.variables,*/ &SupraSegs { stress: *stress, length: [None, None], tone: tone.clone() })?;

        let syll = word.syllables.get_mut(insert_pos.syll_index).unwrap();

        while syll.segments.len() > insert_pos.seg_index {
            new_syll.segments.push_front(syll.segments.pop_back().unwrap());
        }
        word.syllables.insert(insert_pos.syll_index+1, new_syll);

        insert_pos.syll_index += 2;
        insert_pos.seg_index = 0;

        if let Some(v) = var {
            self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[insert_pos.syll_index -1].clone()));
        }

        Ok(())
    }

    fn insert(&self, word: &Word, insert_pos: &mut SegPos, after: bool) -> Result<Word, RuleRuntimeError> {
        let mut res_word = word.clone();
        
        for state in &self.output {
            match &state.kind {
                ParseElement::Syllable(stress, tone, var) => self.insert_syllable(&mut res_word, insert_pos, stress, tone, var)?,
                ParseElement::Ipa(seg, mods) => {        
                    if res_word.in_bounds(*insert_pos) {
                        res_word.syllables[insert_pos.syll_index].segments.insert(insert_pos.seg_index, *seg);
                    } else {
                        res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
                    }          
                    if let Some(m) = mods {
                        println!("{:?}", insert_pos);
                        res_word.apply_mods(&self.alphas, m, *insert_pos)?;
                    }          
                    
                    if res_word.in_bounds(*insert_pos) {
                        insert_pos.increment(&res_word);
                    }
                },
                ParseElement::Variable(num, mods) => self.insert_variable(&mut res_word, insert_pos, num, mods, after)?,
                ParseElement::SyllBound => {
                    if insert_pos.at_syll_start() || insert_pos.at_syll_end(&res_word) {
                        continue; // do nothing
                    }
                    // split current syll into two at insert_pos
                    // initialise stress & tone as default

                    let mut new_syll = Syllable::new();
                    let syll = res_word.syllables.get_mut(insert_pos.syll_index).unwrap();

                    while syll.segments.len() > insert_pos.seg_index {
                        new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                    }
                    res_word.syllables.insert(insert_pos.syll_index+1, new_syll);

                    insert_pos.syll_index += 1;
                    insert_pos.seg_index = 0;
                },
                
                
                ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(state.position)),
                ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(state.position)),
                ParseElement::EmptySet  | ParseElement::Metathesis | 
                ParseElement::Ellipsis  | ParseElement::Optional(..) | 
                ParseElement::WordBound | ParseElement::Environment(..) => unreachable!(),
            }
        }

        Ok(res_word)
    }

    fn apply_syll_mods(&self, word: &mut Word, syll_index: usize, mods: &Modifiers, var: &Option<usize>) -> Result<(), RuleRuntimeError> {
        // NOTE(girv): Maybe we should error or give warnings if we have non-syllable features 
        if let Some(_v) = var {
            todo!("Deal with variable")
        } else {
            word.syllables.get_mut(syll_index).unwrap().apply_mods(&self.alphas, /*&self.variables,*/ &mods.suprs)
        }
    }
    
    fn apply_seg_mods(&self, word: &mut Word, pos: SegPos, mods: &Modifiers, var: &Option<usize>) -> Result<i8, RuleRuntimeError>{
        if let Some(_v) = var {
            todo!("Deal with variable")
        } else {
            word.apply_mods(&self.alphas, mods, pos)
        }
    }
    
    fn substitution(&self, word: &Word, input: Vec<MatchElement>, current_pos: &mut Option<SegPos>) -> Result<Word, RuleRuntimeError> {
        // the SegPositions captured in input will not be correct if we change the length of a segment
        // therefore we must keep track of a change in a syllable's length and update the SegPositions accordingly
        let mut total_len_change: Vec<i8> = vec![0; word.syllables.len()];
        
        let mut res_word = word.clone();
        for (state_index, (in_state, out_state)) in self.input.iter().zip(&self.output).enumerate().rev() {
            match &out_state.kind {
                ParseElement::Syllable(_, _, _) => todo!(), // error??
                ParseElement::Matrix(m, v) => {
                    // get match at index and check it's a segment/or syllable and not a boundary and apply changes
                    // if a syllable, make sure to only do Syllable Suprs
                    match input[state_index] {
                        MatchElement::Segment(mut sp, _)   => {
                            // TODO: since it's reversed syll index should only change on insertion ???
                            // inserting a syll_bound will complicate this
                            match total_len_change[sp.syll_index].cmp(&0) {
                                std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                                std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize, // Should not underflow
                                _ => {},
                            }
                            debug_assert!(res_word.in_bounds(sp));
                            let lc = self.apply_seg_mods(&mut res_word, sp, m, v)?;
                            total_len_change[sp.syll_index] += lc;
                            if let Some(pos) = current_pos { 
                                match lc.cmp(&0) {
                                    std::cmp::Ordering::Greater => for _ in 0..lc { pos.increment(&res_word); },
                                    std::cmp::Ordering::Less    => for _ in lc..0 { pos.decrement(&res_word); },
                                    _ => {},
                                }
                            }
                        },
                        MatchElement::Syllable(sp, _)  => self.apply_syll_mods(&mut res_word, sp, m, v)?,
                        MatchElement::SyllBound(..)  => todo!("Err: Can't apply matrix to syllable boundary"),
                    }
                },
                ParseElement::Ipa(seg, mods) => match input[state_index] {
                    MatchElement::Segment(mut sp, _) => {
                        match total_len_change[sp.syll_index].cmp(&0) {
                            std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                            std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                            _ => {},
                        }
                        debug_assert!(res_word.in_bounds(sp));
                        // "Replace with output IPA.
                        res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                        // Apply Mods
                        if let Some(m) = mods {
                            let lc = res_word.apply_mods(&self.alphas, m, sp)?;
                            total_len_change[sp.syll_index] += lc;
                            if let Some(pos) = current_pos { 
                                match lc.cmp(&0) {
                                    std::cmp::Ordering::Greater => for _ in 0..lc { pos.increment(&res_word); },
                                    std::cmp::Ordering::Less    => for _ in lc..0 { pos.decrement(&res_word); },
                                    _ => {},
                                }
                            }
                        }
                    },    
                    MatchElement::Syllable(..) | MatchElement::SyllBound(..) => todo!("Err"),
                },
                ParseElement::Variable(num, mods) => {
                    if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
                        match (var, input[state_index]) {
                            (VarKind::Segment(seg), MatchElement::Segment(mut sp, _)) => {
                                match total_len_change[sp.syll_index].cmp(&0) {
                                    std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                                    std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                                    _ => {},
                                }
                                debug_assert!(res_word.in_bounds(sp));
                                res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                if let Some(m) = mods {
                                    let lc = res_word.apply_mods(&self.alphas, m, sp)?;
                                    total_len_change[sp.syll_index] += lc;
                                    if let Some(pos) = current_pos { 
                                        match lc.cmp(&0) {
                                            std::cmp::Ordering::Greater => for _ in 0..lc { pos.increment(&res_word); },
                                            std::cmp::Ordering::Less    => for _ in lc..0 { pos.decrement(&res_word); },
                                            _ => {},
                                        }
                                    }
                                }
                            },
                            (VarKind::Syllable(syll), MatchElement::Syllable(sp, _)) => {
                                res_word.syllables[sp] = syll.clone();
                                if let Some(m) = mods {
                                    res_word.syllables[sp].apply_mods(&self.alphas, &m.suprs)?;
                                }
                            },
                            (VarKind::Segment(_), MatchElement::Syllable(..)) |
                            (VarKind::Segment(_), MatchElement::SyllBound(..)) |
                            (VarKind::Syllable(_), MatchElement::Segment(..)) |
                            (VarKind::Syllable(_), MatchElement::SyllBound(..)) => todo!("Err"),
                        }
                    } else {
                        return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                    }
                },
                ParseElement::Set(set_output) => {
                    // Check that self.input[si] is a set, if not throw RuleRuntimeError::LonelySet(state.position)
                    // Check both sets have the same number of elements 
                    // See which one of the input set matched and use the corresponding in output to substitute
                    match &in_state.kind {
                        ParseElement::Set(set_input) => if set_input.len() == set_output.len() {
                            if let MatchElement::Segment(mut sp, Some(i)) = input[state_index] {
                                match total_len_change[sp.syll_index].cmp(&0) {
                                    std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                                    std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                                    _ => {},
                                }
                                debug_assert!(res_word.in_bounds(sp));
                                match &set_output[i].kind {
                                    ParseElement::Ipa(seg, mods) => {
                                        res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                        if let Some(m) = mods {
                                            let lc = res_word.apply_mods(&self.alphas, m, sp)?;
                                            total_len_change[sp.syll_index] += lc;
                                            if let Some(pos) = current_pos { 
                                                match lc.cmp(&0) {
                                                    std::cmp::Ordering::Greater => for _ in 0..lc { pos.increment(&res_word); },
                                                    std::cmp::Ordering::Less    => for _ in lc..0 { pos.decrement(&res_word); },
                                                    _ => {},
                                                }
                                            }
                                        }
                                    }
                                    _ => todo!("`Syllable` is not implemented in `Sets` yet")
                                }
                            } else {
                                unimplemented!()
                            }
                        } else { todo!("Err: Matched sets must be the same size") },
                        _ => return Err(RuleRuntimeError::LonelySet(out_state.position))
                    }
                },
                ParseElement::SyllBound => {
                    // if !pos.at_syll_start() && !pos.at_syll_end(word) {
                    //     // split current syll into two at insert_pos
                    //     todo!("Split")
                    // }
                    if let MatchElement::SyllBound(..) = input[state_index] {
                        continue
                    } else {
                        todo!("ERR")
                    }
                },
                ParseElement::EmptySet   | ParseElement::Metathesis    | 
                ParseElement::Ellipsis   | ParseElement::Optional(..)  | 
                ParseElement::WordBound  | ParseElement::Environment(..) => unreachable!(),
            }
        }

        if self.output.len() > self.input.len() {
            println!("{}", self.output[self.output.len() - self.input.len()]);
            todo!("insert remaining outputs");
        } else if self.input.len() > self.output.len() {
            // TODO(girv): factor this out
            let start_index = self.input.len() - self.output.len();
            for &z in input.iter().skip(start_index).rev() {
                match z {
                    MatchElement::Segment(mut sp, _) => {
                        match total_len_change[sp.syll_index].cmp(&0) {
                            std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                            std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                            _ => {},
                        }
                        debug_assert!(res_word.in_bounds(sp));
                        // remove segment                             
                        if res_word.syllables.len() <= 1 && word.syllables[sp.syll_index].segments.len() <= 1 {
                            return Err(RuleRuntimeError::DeletionOnlySeg)
                        }
                        res_word.syllables[sp.syll_index].segments.remove(sp.seg_index);
                        // if that was the only segment in that syllable, remove the syllable
                        if res_word.syllables[sp.syll_index].segments.is_empty() {
                            res_word.syllables.remove(sp.syll_index);
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
                    MatchElement::Syllable(i, _) => {
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
                    MatchElement::SyllBound(i, _) => {
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
                            (StressKind::Secondary,  StressKind::Unstressed) | 
                            (StressKind::Secondary,  StressKind::Secondary)  | 
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
        };
        Ok(res_word)
    }

    fn input_match_at(&self, word: &Word, start_index: SegPos) -> Result<(Vec<MatchElement>, Option<SegPos>), RuleRuntimeError> {
        let mut cur_index = start_index;
        let mut match_begin = None;
        let mut state_index = 0;
        let mut captures: Vec<_> = Vec::new();

        while word.in_bounds(cur_index) {
            if self.input_match_item(&mut captures, &mut cur_index, &mut state_index, word, &self.input)? {
                if state_index > self.input.len() - 1 { 
                    // if we have a full match

                    // As matching a syllbound doesn't increment, this is to avoid an infinite loop
                    if self.input.last().unwrap().kind == ParseElement::SyllBound {
                        cur_index.increment(word);
                    }
                    return Ok((captures, Some(cur_index)));
                }
                if match_begin.is_none() { 
                    // if we haven't started matching, we have now
                    match_begin = Some(cur_index)
                }
                // else continue 
            } else if let Some (x) = match_begin { 
                // if we were in the middle of matching but now don't match, go back to when we started matching +1 and start again
                cur_index = x;
                cur_index.increment(word);
                state_index = 0;
                captures = vec![];
                match_begin = None;
            } else {
                // if we weren't in the middle of matching, move on
                cur_index.increment(word);
                // NOTE(girv): Should be unnecessary, but safety first!
                state_index = 0;
                captures = vec![];  
            }
        }

        if match_begin.is_none() { // if we've got to the end of the word and we haven't began matching
            Ok((vec![], None))
        } else if self.input.last().unwrap().kind == ParseElement::SyllBound {
            // if we've reached the end of the word and the last state is a word boundary
            captures.push(MatchElement::SyllBound(word.syllables.len(), None));
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
        let err_pos = states[*state_index].position;
        match &states[*state_index].kind {
            ParseElement::Variable(vt, m) => if self.input_match_var(captures, state_index, vt, m, word, seg_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Ipa(s, m) => if self.input_match_ipa(captures, s, m, word, *seg_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => if self.input_match_matrix(captures, m, v, word, seg_pos, err_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true) 
            } else { Ok(false) },
            ParseElement::Set(s) => if self.input_match_set(captures, state_index, s, word, seg_pos)? {
                seg_pos.increment(word); // TODO(girv): when we allow boundaries within sets, this will have to be incremented within the match_set function
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::SyllBound => if self.input_match_syll_bound(captures, *seg_pos) {
                // NOTE(girv): Boundaries do not advance seg_index 
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Syllable(s, t, v) => self.input_match_syll(captures, state_index, s, t, v, word, seg_pos),
            ParseElement::Ellipsis => self.input_match_ellipsis(captures, word, seg_pos, states, state_index),
            ParseElement::Optional(opt_states, match_min, match_max) => self.input_match_optionals(captures, word, seg_pos, states, opt_states, *match_min, *match_max),            
            ParseElement::EmptySet | ParseElement::WordBound | ParseElement::Metathesis | ParseElement::Environment(_, _) => unreachable!(),
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
                    caps.push(MatchElement::Segment(*seg_index, None))
                }

                seg_index.increment(word);
                i += 1;
            } else {
                // NOTE: segs are captured here regardless of the `capture_wanted` check
                let mut inner_state_index = 0;

                while word.in_bounds(*seg_index) && inner_state_index < inner_states.len() {
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
            captures.push(MatchElement::Syllable(cur_syll_index, None));
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
            captures.push(MatchElement::SyllBound(pos.syll_index, None));
            true
        } else {
            false
        }
    }

    fn input_match_set(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, set: &[Item], word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        for (i,s) in set.iter().enumerate() {
            let res = match &s.kind {
                ParseElement::Variable(vt, m) => self.input_match_var(captures, state_index, vt, m, word, pos),
                ParseElement::Ipa(s, m)       => self.input_match_ipa(captures, s, m, word, *pos),
                ParseElement::Matrix(m, v)    => self.input_match_matrix(captures, m, v, word, pos, s.position),
                ParseElement::Syllable(..) => todo!(),
                ParseElement::WordBound => todo!(),
                ParseElement::SyllBound => todo!(),
                _ => unimplemented!(),
            };
            if res? {
                captures.last_mut().unwrap().set_ind(Some(i));
                return Ok(true)
            }
        }
        Ok(false)
    }

    fn input_match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(pos).unwrap();
        if mods.is_none() {
            if *s == seg {
                captures.push(MatchElement::Segment(pos, None));
                Ok(true)
            } else {
                Ok(false)
            }
        } else if self.match_ipa_with_modifiers(s, mods.as_ref().unwrap(), word, &pos)? {
            captures.push(MatchElement::Segment(pos, None));
            Ok(true)
        } else {
            Ok(false)
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
            captures.push(MatchElement::Syllable(csi, None));
    
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
        match self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            Some(var) => match var {
                VarKind::Segment(s)  => self.input_match_ipa(captures, s, mods, word, *pos),
                VarKind::Syllable(s) => self.input_match_syll_var(captures, state_index , s, mods, word, pos),
            },
            None => Err(RuleRuntimeError::UnknownVariable(vt.clone())),
        }
    }

    fn input_match_matrix(&self, captures: &mut Vec<MatchElement>, mods: &Modifiers, var: &Option<usize>, word: &Word, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> { 
        if self.match_modifiers(mods, word, pos, err_pos)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Segment(word.get_seg_at(*pos).unwrap()));
            }
            captures.push(MatchElement::Segment(*pos, None));
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

    fn match_ipa_with_modifiers(&self, s: &Segment, mods: &Modifiers, word: &Word, pos: &SegPos) -> Result<bool, RuleRuntimeError> {
        let mut seg = word.get_seg_at(*pos).expect("Segment Position should be within bounds");

        seg.apply_seg_mods(&self.alphas, mods.nodes, mods.feats)?;

        if *s == seg {
            Ok(self.match_supr_mod_seg(word, &mods.suprs, pos))
        } else {
            Ok(false)
        }
    }

    fn match_modifiers(&self, mods: &Modifiers, word: &Word, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(*pos).expect("Segment Position should be within bounds");
        
        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i, seg, err_pos)? {
                return Ok(false);
            }
        }
        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.match_node_mod(m, i, seg, err_pos)? {
                return Ok(false);
            }
        }
        Ok(self.match_supr_mod_seg(word, &mods.suprs, pos))
    }

    fn match_supr_mod_seg(&self, word: &Word, mods: &SupraSegs, pos: &SegPos) -> bool {

        let syll = &word.syllables[pos.syll_index];

        if !self.match_stress(&mods.stress, syll) { return false }
        if !self.match_seg_length(word, &mods.length, pos) { return false }

        if let Some(t) = mods.tone.as_ref() {
            return self.match_tone(t, syll)
        }

        true
    }

    fn match_seg_length(&self, word: &Word, length: &[Option<ModKind>; 2], pos: &SegPos) -> bool {
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

    fn match_node_mod(&self, md:&Option<ModKind>, node_index: usize, seg: Segment, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md {
            let node = NodeKind::from_usize(node_index);
            return self.match_node(seg, node, kind, err_pos)
        }
        Ok(true)
    }

    fn match_feat_mod(&self, md: &Option<ModKind>, feat_index: usize, seg: Segment, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md { 
            let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
            return self.match_seg_kind(kind, seg, node, mask, err_pos)
        }
        Ok(true)
    }

    fn match_node(&self, seg: Segment, node: NodeKind, val: &ModKind, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match val {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => Ok(seg.get_node(node).is_none()),
                BinMod::Positive => Ok(seg.get_node(node).is_some()),
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        if let Some((n, m)) = alph.as_node() {
                            return Ok(seg.node_match(*n, *m))
                        } else if let Some((_, place)) = alph.as_place() {
                            return Ok(
                                seg.node_match(NodeKind::Labial, place.lab)  &&
                                seg.node_match(NodeKind::Coronal, place.cor) &&
                                seg.node_match(NodeKind::Dorsal, place.dor)  &&
                                seg.node_match(NodeKind::Pharyngeal, place.phr)
                            )
                        } else {
                            return Err(RuleRuntimeError::AlphaIsNotNode(err_pos))
                        }
                    }
                    if node == NodeKind::Place {
                        let place = PlaceMod::new(
                            seg.get_node(NodeKind::Labial),
                            seg.get_node(NodeKind::Coronal),
                            seg.get_node(NodeKind::Dorsal),
                            seg.get_node(NodeKind::Pharyngeal),
                        );
                        self.alphas.borrow_mut().insert(*ch, Alpha::Place(node, place)); 
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Node(node, seg.get_node(node))); 
                    }
                    Ok(true)
                },
                AlphaMod::InvAlpha(inv_ch) => {
                    if let Some(alph) = self.alphas.borrow().get(inv_ch) {
                        if let Some((n, m)) = alph.as_node() {
                            Ok(!seg.node_match(*n, *m))
                        } else if let Some((_, place)) = alph.as_place() {
                            Ok(
                                !seg.node_match(NodeKind::Labial, place.lab)  ||
                                !seg.node_match(NodeKind::Coronal, place.cor) ||
                                !seg.node_match(NodeKind::Dorsal, place.dor)  ||
                                !seg.node_match(NodeKind::Pharyngeal, place.phr)
                            )
                        } else {
                            Err(RuleRuntimeError::AlphaIsNotNode(err_pos))
                        }
                    } else {
                        Err(RuleRuntimeError::AlphaUnknown(err_pos))
                    }
                },
            },
        }
    }

    fn match_seg_kind(&self, kind: &ModKind, seg: Segment, node: NodeKind, mask: u8, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match kind {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => Ok(seg.feat_match(node, mask, false)),
                BinMod::Positive => Ok(seg.feat_match(node, mask, true)),
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        if let Some((&n, &m, &pos)) = alph.as_feature() {
                            return Ok(seg.feat_match(n, m, pos))
                        } else {
                            return Err(RuleRuntimeError::AlphaIsNotFeature(err_pos))
                        }
                    } 
                    if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Feature(node, mask, f != 0)); 
                        Ok(true)
                    } else {
                        // Maybe err?
                        Ok(false)
                    }
                },
                AlphaMod::InvAlpha(inv_ch) => {
                    if let Some(alph) = self.alphas.borrow().get(inv_ch) {
                        if let Some((&n, &m, &pos)) = alph.as_feature() {
                            return Ok(seg.feat_match(n, m, !pos))
                        } else {
                            return Err(RuleRuntimeError::AlphaIsNotFeature(err_pos))
                        }
                    } 
                    if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*inv_ch, Alpha::Feature(node, mask, f == 0));
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