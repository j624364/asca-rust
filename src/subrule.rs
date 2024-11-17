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
            self.alphas.borrow_mut().clear();
            self.variables.borrow_mut().clear();
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
                        let mut seg_len = word.seg_length_at(sp);
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
                    println!("EOW");
                    break;
                }
            } else {
                println!("No match at {:?}", cur_index);
                break
            }
        }
        Ok(word)
    }

    fn match_before_context_and_exception(&self, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let empty_env = Item::new(ParseElement::Environment(vec![], vec![]), Position { line: 0, start: 0, end: 0 });
        let ParseElement::Environment(context_states, _) = &match &self.context {
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
        let word_rev = word.reverse();
        let mut context_states = context_states.clone();
        let mut except_states = except_states.clone();
        context_states.reverse();
        except_states.reverse();



        let mut start_pos = pos.reversed(word);
        start_pos.increment(&word_rev);
        let mut is_context_match = true;
        let mut si = 0;
        while si < context_states.len() {
            if !self.context_match(&context_states, &mut si, &word_rev, &mut start_pos, false)? {
                is_context_match = false;
                break;
            }
            si += 1;
        }
        let mut start_pos = pos.reversed(word);
        start_pos.increment(&word_rev);
        let mut is_except_match = !except_states.is_empty();
        let mut si = 0;
        while si < except_states.len() {
            if !self.context_match(&except_states, &mut si, &word_rev, &mut start_pos, false)? {
                is_except_match = false;
            }
            si += 1;
        }
        Ok(!is_except_match && is_context_match)
    }

    fn match_after_context_and_exception(&self, word: &Word, pos: SegPos) -> Result<bool, RuleRuntimeError> {
        const EMPTY_ENV: Item = Item{ kind: ParseElement::Environment(vec![], vec![]), position: Position { line: 0, start: 0, end: 0 }};
        let binding = EMPTY_ENV;
        let ParseElement::Environment(_, context_states) = &match &self.context {
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
        start_pos.increment(word);
        let mut is_context_match = true;
        let mut si = 0;
        while si < context_states.len() {
            if !self.context_match(context_states, &mut si, word, &mut start_pos, true)? {
                is_context_match = false;
                break;
            }
            si += 1;
        }
        let mut start_pos = pos;
        start_pos.increment(word);
        let mut is_except_match = !except_states.is_empty();
        let mut si = 0;
        while si < except_states.len() {
            if !self.context_match(except_states, &mut si, word, &mut start_pos, true)? {
                // println!("{} {}", word.render().unwrap(), except_states[si]);
                is_except_match = false;
            }
            si += 1;
        }

        Ok(!is_except_match && is_context_match)
    }

    fn context_match(&self, states: &[Item], state_index: &mut usize, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        let state = &states[*state_index];
        match &state.kind {
            ParseElement::WordBound => Ok(word.out_of_bounds(*pos)),
            ParseElement::SyllBound => Ok(pos.at_syll_start()),
            ParseElement::Ipa(s, m) => if self.context_match_ipa(s, m, word, *pos, state.position)? {
                pos.increment(word); 
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => self.context_match_matrix(m, v, word, pos, state.position),
            ParseElement::Syllable(s, t, v) => self.context_match_syll(s, t, v, word, pos, forwards),
            ParseElement::Variable(vt, mods) => self.context_match_var(vt, mods, word, pos, forwards, state.position),
            ParseElement::Set(s) => self.context_match_set(s, word, pos, forwards),
            ParseElement::Optional(opt_states, min, max) => self.context_match_option(states, state_index, word, pos, forwards, opt_states, *min, *max),
            ParseElement::Ellipsis => self.context_match_ellipsis(states, state_index, word, pos, forwards),
            
            
            ParseElement::EmptySet | ParseElement::Metathesis |
            ParseElement::Environment(_, _) => unreachable!(),
        }
    }

    fn context_match_ellipsis(&self, states: &[Item], state_index: &mut usize, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        if *state_index >= states.len() {
            return Ok(true)
        }

        *state_index += 1;
        pos.increment(word);

        while word.in_bounds(*pos) {
            let back_pos = *pos;
            let back_state = *state_index;

            let mut m = true;
            while *state_index < states.len() {
                if !self.context_match(states, state_index, word, pos, forwards)? {
                    m = false;
                    break;
                }
                *state_index += 1;
            }
            if m {
                return Ok(true)
            }
            *state_index = back_state;
            *pos = back_pos;
            pos.increment(word);
        }
        
        Ok(false)
    }

    fn match_opt_states(&self, opt_states: &[Item], word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        let mut si = 0;
        while si < opt_states.len() {
            if !self.context_match(opt_states, &mut si, word, pos, forwards)? {
                return Ok(false)
            }
            si += 1;
        }
        Ok(true)
    }

    fn context_match_option(&self, states: &[Item], state_index: &mut usize, word: &Word, pos: &mut SegPos, forwards: bool, opt_states: &[Item], match_min: usize, match_max: usize) -> Result<bool, RuleRuntimeError> {
        // should work like regex (...){min, max}? 
        let match_max = if match_max == 0 { None } else { Some(match_max) };
        let back_pos = *pos;
        
        let mut index = 0;
        while index < match_min {
            if !self.match_opt_states(opt_states, word, pos, forwards)? {
                *pos = back_pos;
                return Ok(false)

            }
            // if word.out_of_bounds(*pos) {
            //     *pos = back_pos;
            //     return Ok(false)
            // }
            index += 1;
        }

        *state_index +=1;
        let back_state = *state_index;
        let back_pos = *pos;

        let mut m = true;
        while *state_index < states.len() {
            if !self.context_match(states, state_index, word, pos, forwards)? {
                m = false;
                break;
            }
            *state_index += 1;
        }
        if m {
            return Ok(true)
        }

        // println!("Up to here!");
        *pos = back_pos;
        *state_index = back_state;
        

        // (X, 0,2)
        // _ , _X, _XX
        
        // opt  states
        // opt opt states
        // opt opt opt states
        // opt opt opt opt states
        // if let Some(max) = match_max {
        //     let num_iters = max-match_min;
        //     let arr: Vec<bool> = Vec::with_capacity(num_iters);
        //     println!("min: {}", match_min); println!("max: {}", max);
        //     println!("cap: {}", arr.capacity());
        // }


        if let Some(max) = match_max {
            while index < max {
                *state_index = back_state;
                if self.match_opt_states(opt_states, word, pos, forwards)? {
                    let mut m = true;
                    while *state_index < states.len() {
                        if !self.context_match(states, state_index, word, pos, forwards)? {
                            m = false;
                            break;
                        }
                        *state_index += 1;
                    }
                    if m {
                        return Ok(true)
                    } else {
                        index +=1;
                        continue;
                    }
                } else {
                    // println!("dfgsgnsdgfkj;kj");
                    // *pos = back_pos;
                    // *state_index = back_state;
                    return Ok(true)
                }
            }
            // println!("state ind: {}", state_index);
            Ok(false)
        } else {
            
            todo!()
        }
    }

    fn context_match_set(&self, set: &[Item], word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        for s in set {
            let res = match &s.kind {
                ParseElement::Variable(vt, mods) => self.context_match_var(vt, mods, word, pos, forwards, s.position),
                ParseElement::Ipa(seg, mods) => if self.context_match_ipa(seg, mods, word, *pos, s.position)? {
                    pos.increment(word);
                    Ok(true)
                } else {Ok(false)},
                ParseElement::Matrix(mods, var) => self.context_match_matrix(mods, var, word, pos, s.position),
                ParseElement::Syllable(stress, tone, var) => self.context_match_syll(stress, tone, var, word, pos, forwards),
                ParseElement::WordBound => Ok(word.out_of_bounds(*pos)),
                ParseElement::SyllBound => Ok(pos.at_syll_start()),
                _ => unimplemented!(),
            };
            if res? {
                return Ok(true)
            }
        }
        Ok(false)
    }

    fn context_match_var(&self, vt: &Token, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, forwards: bool, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            match var {
                VarKind::Segment(s) => if self.context_match_ipa(s, mods, word, *pos, err_pos)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
                VarKind::Syllable(s) => self.context_match_syll_var(s, mods, word, pos, forwards),
            }            
        } else {
            Err(RuleRuntimeError::UnknownVariable(vt.clone()))
        }
    }

    fn context_match_syll_var(&self, syll_to_match: &Syllable, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        if !pos.at_syll_start()  {
            return Ok(false)
        }
        let cur_syll = if word.in_bounds(*pos) {
             &word.syllables[pos.syll_index]
        } else { return Ok(false) };

        let segs_to_match = if forwards {
            syll_to_match.segments.clone() // i hate this
        } else {
            let mut segs = syll_to_match.segments.clone();
            segs.make_contiguous().reverse();
            segs
        };
        
        if let Some(Modifiers { nodes: _, feats: _, suprs }) = mods {
            if !self.match_stress(&suprs.stress, cur_syll)? {
                return Ok(false)
            }
            if let Some(t) = suprs.tone.as_ref()  {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
            }
            if cur_syll.segments != syll_to_match.segments {
                return Ok(false)
            }
        } else if cur_syll.segments != segs_to_match || cur_syll.stress != syll_to_match.stress || cur_syll.tone != syll_to_match.tone {
            return Ok(false)
        }
        pos.syll_index += 1;
        pos.seg_index = 0;
        Ok(true)
    }

    fn context_match_syll(&self, stress: &[Option<ModKind>; 2], tone: &Option<String>, var: &Option<usize>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {        
        if !pos.at_syll_start() {
            return Ok(false)
        }
        let cur_syll = if word.in_bounds(*pos){ 
            &word.syllables[pos.syll_index] 
        } else { return Ok(false) };

        if !self.match_stress(stress, cur_syll)? {
            return Ok(false)
        }
        if let Some(t) = tone.as_ref() {
            if !self.match_tone(t, cur_syll) {
                return Ok(false)
            }
        }
        if let Some(v) = var {
            if forwards {
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[pos.syll_index].clone()));
            } else {
                let mut syll = word.syllables[pos.syll_index].clone();
                syll.segments.make_contiguous().reverse();
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(syll));
            }
        }
        pos.syll_index += 1;
        pos.seg_index = 0;
        Ok(true)
    }

    fn context_match_matrix(&self, mods: &Modifiers, var: &Option<usize>, word: &Word, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {        
        if word.out_of_bounds(*pos) {
            return Ok(false)
        }
        if self.match_modifiers(mods, word, pos, err_pos)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Segment(word.get_seg_at(*pos).unwrap()));
            }
            pos.increment(word); // TODO: Probably have to do what we do for input and account for long segments
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn transform(&self, word: &Word, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Word, RuleRuntimeError> {
        match self.rule_type {
            RuleType::Metathesis => {
                let mut res_word = word.clone();
                for z in 0..(input.len() / 2) {
                    match (input[z], input[input.len()-1-z]) {
                        (MatchElement::Segment(li, _), MatchElement::Segment(ri, _)) => {
                            // FIXME: If we swap syllables or boundaries then do this, these SegPos may not be correct
                            let sl = res_word.get_seg_at(li).unwrap();
                            let sr = res_word.get_seg_at(ri).unwrap();
                            let tmp = sl;
                            res_word.syllables[li.syll_index].segments[li.seg_index] = sr;
                            res_word.syllables[ri.syll_index].segments[ri.seg_index] = tmp;
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
                let mut pos = SegPos::new(0, 0);
                let mut res_word = word.clone();
                for z in input.into_iter().rev() {
                    match z {
                        MatchElement::Segment(i, _) => {
                            pos = i;
                            // remove segment                             
                            if res_word.syllables.len() <= 1 && word.syllables[i.syll_index].segments.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySeg)
                            }
                            res_word.syllables[i.syll_index].segments.remove(i.seg_index);
                            // if that was the only segment in that syllable, remove the syllable
                            if res_word.syllables[i.syll_index].segments.is_empty() {
                                res_word.syllables.remove(i.syll_index);
                            }
                        },
                        MatchElement::Syllable(i, _) => {
                            // remove syllable
                            if res_word.syllables.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySyll)
                            }
                            pos.syll_index = i;
                            pos.seg_index = 0;
                            pos.decrement(&res_word);
                            res_word.remove_syll(i);
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
                            pos.syll_index = i;
                            pos.seg_index = 0;
                            pos.decrement(&res_word);

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
                        },
                    }
                }
                if let Some(next) = next_pos {
                    pos.increment(&res_word);
                    *next = pos;
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
            ParseElement::Syllable(s, t, v) => if self.context_match_syll(s, t, v, word, cur_pos, true)? {
                *state_index += 1;
                cur_pos.decrement(word);
                Ok(Some(*cur_pos))
            } else { Ok(None) },
            ParseElement::Set(set) => if self.context_match_set(set, word, cur_pos, true)? {
                *state_index += 1;
                // I hate this, but it works for now
                cur_pos.decrement(word);
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
            ParseElement::Ellipsis => todo!(),
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

        if let Some(m) = mods {
            Ok(self.match_ipa_with_modifiers(s, m, word, &pos)?)
        } else {
            Ok(*s == seg)
        }
    }

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

    // fn insert(&self, word: &Word, insert_pos: &mut SegPos, insert_after: bool) -> Result<Word, RuleRuntimeError> {
    //     let mut res_word = word.clone();
    //     for state in &self.output {
    //         match &state.kind {
    //             ParseElement::Syllable(stress, tone, var) => self.insert_syllable(&mut res_word, insert_pos, stress, tone, var)?,
    //             ParseElement::Ipa(seg, mods) => {       
    //                 println!("ins_pos: {:?}", insert_pos);
    //
    //                 let mut pos = if insert_after { SegPos::new(insert_pos.syll_index, insert_pos.seg_index )
    //                 } else { *insert_pos };
    //                 println!("after: {}" , insert_after);
    //                 // if let Some(Item { kind: ParseElement::Environment(vb, va), position: _ }) = &self.context { 
    //                 //     if vb.is_empty() && va[0].kind == ParseElement::WordBound || va[0].kind == ParseElement::SyllBound {
    //                 //         println!("DJAHBDJAHBSDJHb");
    //                 //         if res_word.in_bounds(pos) {
    //                 //             res_word.syllables[pos.syll_index].segments.insert(pos.seg_index, *seg);
    //                 //         } else {
    //                 //             res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
    //                 //         if let Some(m) = mods {
    //                 //             println!("{:?}", pos);
    //                 //             res_word.apply_mods(&self.alphas, m, pos)?;
    //                 //         }          
    //                 //         if res_word.in_bounds(pos) {
    //                 //             pos.increment(&res_word);
    //                 //         }
    //                 //         continue;
    //                 //     }
    //                 // }
    //
    //                 if res_word.in_bounds(pos) {
    //                     res_word.syllables[pos.syll_index].segments.insert(pos.seg_index, *seg);
    //                 } else if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) { 
    //                     if pos.seg_index >= syll.segments.len() {
    //                         syll.segments.push_back(*seg);
    //                     } else {
    //                         syll.segments.insert(pos.seg_index, *seg);
    //                     }
    //                 } else {
    //                     res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
    //                 }          
    //                 if let Some(m) = mods {
    //                     println!("{:?}", pos);
    //                     res_word.apply_mods(&self.alphas, m, pos)?;
    //                 }          
    //              
    //                 if res_word.in_bounds(pos) {
    //                     pos.increment(&res_word);
    //                 }
    //                 *insert_pos = pos;
    //             },
    //             ParseElement::Variable(num, mods) => self.insert_variable(&mut res_word, insert_pos, num, mods, insert_after)?,
    //             ParseElement::SyllBound => {
    //                 if insert_pos.at_syll_start() || insert_pos.at_syll_end(&res_word) {
    //                     continue; // do nothing
    //                 }
    //                 // split current syll into two at insert_pos
    //                 // initialise stress & tone as default
    //                 let mut new_syll = Syllable::new();
    //                 let syll = res_word.syllables.get_mut(insert_pos.syll_index).unwrap();
    //                 while syll.segments.len() > insert_pos.seg_index {
    //                     new_syll.segments.push_front(syll.segments.pop_back().unwrap());
    //                 }
    //                 res_word.syllables.insert(insert_pos.syll_index+1, new_syll);
    //                 insert_pos.syll_index += 1;
    //                 insert_pos.seg_index = 0;
    //             },     
    //             ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(state.position)),
    //             ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(state.position)),
    //             ParseElement::EmptySet  | ParseElement::Metathesis | 
    //             ParseElement::Ellipsis  | ParseElement::Optional(..) | 
    //             ParseElement::WordBound | ParseElement::Environment(..) => unreachable!(),
    //         }
    //     }
    //     Ok(res_word)
    // }

    fn context_match_ipa(&self, s: &Segment, mods: &Option<Modifiers>, word: &Word, pos: SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if word.out_of_bounds(pos) {
            return Ok(false)
        }
        let seg = word.get_seg_at(pos).unwrap();
        if let Some(m) = mods {
            Ok(self.match_ipa_with_modifiers(s, m, word, &pos, err_pos)?)
        } else {
            Ok(*s == seg)
        }
    }

    fn apply_syll_mods(&self, word: &mut Word, syll_index: usize, mods: &SupraSegs, var: &Option<usize>, err_pos: Position) -> Result<(), RuleRuntimeError> {
        word.syllables.get_mut(syll_index).unwrap().apply_syll_mods(&self.alphas, mods, err_pos)?;
        
        if let Some(v) = var {
            self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[syll_index].clone()));
        }
        Ok(())
    }
    
    fn apply_seg_mods(&self, word: &mut Word, pos: SegPos, mods: &Modifiers, var: &Option<usize>, err_pos: Position) -> Result<i8, RuleRuntimeError>{
        let lc = word.apply_seg_mods(&self.alphas, mods, pos, err_pos)?;

        if let Some(v) = var {
            self.variables.borrow_mut().insert(*v, VarKind::Segment(word.syllables[pos.syll_index].segments[pos.seg_index]));
        }

        Ok(lc)
    }
    
    // TODO: break this up
    fn substitution(&self, word: &Word, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Word, RuleRuntimeError> {
        // the SegPositions captured in input will not be correct if we change the length of a segment
        // therefore we must keep track of a change in a syllable's length and update the SegPositions accordingly
        let mut total_len_change: Vec<i8> = vec![0; word.syllables.len()];
        let mut last_pos = SegPos::new(0, 0);
        
        let mut res_word = word.clone();
        for (state_index, (in_state, out_state)) in self.input.iter().zip(&self.output).enumerate() {
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
                            last_pos = sp;
                            debug_assert!(res_word.in_bounds(sp));
                            let lc = self.apply_seg_mods(&mut res_word, sp, m, v, out_state.position)?;
                            total_len_change[sp.syll_index] += lc;
                            if lc > 0 {
                                last_pos.seg_index += lc.unsigned_abs() as usize;
                            }
                        },
                        MatchElement::Syllable(sp, _)  => {
                            last_pos.syll_index = sp;
                            last_pos.seg_index = 0;
                            self.apply_syll_mods(&mut res_word, sp, &m.suprs, v, out_state.position)?;                            
                        },
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
                        last_pos = sp;
                        debug_assert!(res_word.in_bounds(sp));
                        // "Replace with output IPA.
                        let lc = res_word.syllables[sp.syll_index].replace_segment(sp.seg_index, seg, mods, &self.alphas, out_state.position)?;
                        total_len_change[sp.syll_index] += lc;
                        if lc > 0 {
                            last_pos.seg_index += lc.unsigned_abs() as usize;
                        }
                        last_pos.seg_index +=1;
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
                                last_pos = sp;
                                debug_assert!(res_word.in_bounds(sp));
                                res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                if let Some(m) = mods {
                                    let lc = res_word.apply_seg_mods(&self.alphas, m, sp, num.position)?;
                                    total_len_change[sp.syll_index] += lc;
                                    if lc > 0 {
                                        last_pos.seg_index += lc.unsigned_abs() as usize;
                                    }
                                }
                                last_pos.seg_index +=1;
                            },
                            (VarKind::Syllable(syll), MatchElement::Syllable(sp, _)) => {
                                res_word.syllables[sp] = syll.clone();
                                last_pos.syll_index = sp;
                                last_pos.seg_index = 0;
                                if let Some(m) = mods {
                                    res_word.syllables[sp].apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                }
                                // total_len_change.insert(sp, 0);
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
                            match input[state_index] {
                                MatchElement::Segment(mut sp, set_index) => {
                                    let Some(i) = set_index else {todo!("Err: no matching set")};
                                    match total_len_change[sp.syll_index].cmp(&0) {
                                        std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                                        std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                                        _ => {},
                                    }
                                    last_pos = sp;
                                    match &set_output[i].kind {
                                        ParseElement::Ipa(seg, mods) => {
                                            res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                            if let Some(m) = mods {
                                                let lc = res_word.apply_seg_mods(&self.alphas, m, sp, set_output[i].position)?;
                                                total_len_change[sp.syll_index] += lc;
                                                if lc > 0 {
                                                    last_pos.seg_index += lc.unsigned_abs() as usize;
                                                }
                                            }
                                            last_pos.seg_index +=1;
                                        }
                                        ParseElement::Matrix(mods, var) => {
                                            let lc = self.apply_seg_mods(&mut res_word, sp, mods, var, out_state.position)?;
                                            total_len_change[sp.syll_index] += lc;
                                            if lc > 0 {
                                                last_pos.seg_index += lc.unsigned_abs() as usize;
                                            }
                                            last_pos.seg_index +=1;
                                        },
                                        ParseElement::Variable(num, mods) => { 
                                            if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
                                                match var {
                                                    VarKind::Segment(seg) => {
                                                        res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                                        if let Some(m) = mods {
                                                            let lc = res_word.apply_seg_mods(&self.alphas, m, sp, num.position)?;
                                                            total_len_change[sp.syll_index] += lc;
                                                            if lc > 0 {
                                                                last_pos.seg_index += lc.unsigned_abs() as usize;
                                                            }
                                                        }
                                                        last_pos.seg_index +=1;
                                                    },
                                                    VarKind::Syllable(_) => todo!("Err"),
                                                }
                                            } else {
                                                return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                                            }
                                        },
                                        ParseElement::SyllBound => {todo!("Either remove the segment and insert syllbound at that position, or error")},
                                        ParseElement::WordBound => todo!("Err"),
                                        ParseElement::Syllable(..) => todo!("Err"),
                                        _ => unreachable!(),
                                    }
                                },
                                MatchElement::Syllable(sp, set_index) => {
                                    let Some(i) = set_index else {todo!("Err: no matching set")};
                                    last_pos.syll_index = sp;
                                    last_pos.seg_index = 0;

                                    match &set_output[i].kind {
                                        ParseElement::Matrix(mods, var) => {
                                            self.apply_syll_mods(&mut res_word, sp, &mods.suprs, var, out_state.position)?;
                                        },
                                        ParseElement::Syllable(stress, tone, var) => {
                                            let sups = SupraSegs::new(*stress, [None, None], tone.clone());
                                            self.apply_syll_mods(&mut res_word, sp, &sups, var, out_state.position)?;
                                        },
                                        ParseElement::Variable(num, mods) => {
                                            if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
                                                match var {
                                                    VarKind::Syllable(syll) => {
                                                        res_word.syllables[sp] = syll.clone();
                                                        if let Some(m) = mods {
                                                            res_word.syllables[sp].apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                                        }
                                                    },
                                                    VarKind::Segment(_) => todo!("Err"),
                                                }
                                            } else {
                                                return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                                            }
                                        },

                                        ParseElement::Ipa(..) => todo!("Err"),
                                        ParseElement::SyllBound => todo!("Err"),
                                        ParseElement::WordBound => todo!("Err"),
                                        _ => unreachable!(),
                                    }
                                },
                                MatchElement::SyllBound(sp, set_index) => {
                                    let Some(i) = set_index else {todo!("Err: no matching set")};
                                    last_pos.syll_index = sp;
                                    last_pos.seg_index = 0;

                                    match &set_output[i].kind {
                                        ParseElement::SyllBound => continue,
                                        _ => todo!("Err: Syllbound can only match to syllbound")
                                    }
                                },
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

        let mut pos = last_pos;
        if self.output.len() > self.input.len() {
            for z in self.output.iter().skip(self.input.len()) {
                match &z.kind {
                    ParseElement::Ipa(seg, mods) => {
                        if res_word.in_bounds(pos) {
                            res_word.syllables[pos.syll_index].segments.insert(pos.seg_index, *seg);
                        } else if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) { 
                            if pos.seg_index >= syll.segments.len() {
                                syll.segments.push_back(*seg);
                            } else {
                                syll.segments.insert(pos.seg_index, *seg);
                            }
                        } else {
                            res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
                        }          
                        if let Some(m) = mods {
                            let lc = res_word.apply_seg_mods(&self.alphas, m, pos, z.position)?;
                            if lc > 0 {
                                pos.seg_index += lc.unsigned_abs() as usize;
                            }
                        }          
                        
                        if res_word.in_bounds(pos) {
                            pos.increment(&res_word);
                        }
                    },
                    ParseElement::SyllBound => {
                        if pos.at_syll_start() {
                            // if res_word.out_of_bounds(*pos) {
                            // } else {
                            //     continue;
                            // }
                            // TODO: Possibly err if out of bounds, as it leads to unexpected behaviour (see rule::test_sub_insert_syll())
                            continue;
                        } 
                        // else if pos.at_syll_end(&res_word) {
                        //     pos.syll_index += 1;
                        //     pos.seg_index = 0;
                        //     continue;
                        // }
                        // split current syll into two at pos
                        // initialise stress & tone as default
                        let mut new_syll = Syllable::new();
                        let syll = res_word.syllables.get_mut(pos.syll_index).unwrap();
                        // new_syll.stress = syll.stress;
                        // new_syll.tone = syll.tone.clone();
                        // syll.stress = StressKind::Unstressed;
                        // syll.tone = String::new();
    
                        while syll.segments.len() > pos.seg_index {
                            new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                        }
                        res_word.syllables.insert(pos.syll_index+1, new_syll);
                        total_len_change.insert(pos.syll_index+1, 0);
                        pos.syll_index += 1;
                        pos.seg_index = 0;

                    },
                    ParseElement::Syllable(stress, tone, var) => {
                        if pos.at_syll_start() {
                            // apply mods to current syllable
                            if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) {
                                syll.apply_syll_mods(&self.alphas, &SupraSegs { stress: *stress, length: [None, None], tone: tone.clone() }, z.position)?;
                                if let Some(v) = var {
                                    self.variables.borrow_mut().insert(*v, VarKind::Syllable(res_word.syllables[pos.syll_index].clone()));
                                }
                                continue;
                            } else {
                                // TODO: Possibly err as out of bounds leads to unexpected behaviour (see rule::test_sub_insert_syll())
                                continue;
                            }
                        } 
                        // split current syll into two at insert_pos
                        // Apply mods to second syll
                        let mut new_syll = Syllable::new();
                        new_syll.apply_syll_mods(&self.alphas, /*&self.variables,*/ &SupraSegs { stress: *stress, length: [None, None], tone: tone.clone() }, z.position)?;

                        let syll = res_word.syllables.get_mut(pos.syll_index).expect("pos should not be out of bounds");

                        while syll.segments.len() > pos.seg_index {
                            new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                        }
                        res_word.syllables.insert(pos.syll_index+1, new_syll);

                        pos.syll_index += 2;
                        pos.seg_index = 0;

                        if let Some(v) = var {
                            self.variables.borrow_mut().insert(*v, VarKind::Syllable(res_word.syllables[pos.syll_index -1].clone()));
                        }
                    },
                    ParseElement::Variable(num, mods) => {
                        if let Some(var) = self.variables.borrow().get(&num.value.parse().unwrap()) {
                            match var {
                                VarKind::Segment(seg) => {
                                    if res_word.in_bounds(pos) {
                                        res_word.syllables[pos.syll_index].segments.insert(pos.seg_index, *seg);
                                    } else if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) { 
                                        if pos.seg_index >= syll.segments.len() {
                                            syll.segments.push_back(*seg);
                                        } else {
                                            syll.segments.insert(pos.seg_index, *seg);
                                        }
                                    } else {
                                        res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
                                    }
                                    if let Some(m) = mods {
                                        let lc = res_word.apply_seg_mods(&self.alphas, m, pos, num.position)?;
                
                                        match lc.cmp(&0) {
                                            std::cmp::Ordering::Greater => pos.seg_index += lc.unsigned_abs() as usize,
                                            std::cmp::Ordering::Less    => pos.seg_index -= lc.unsigned_abs() as usize,
                                            _ => {}
                                        }
                                    }          
                                    
                                    if res_word.in_bounds(pos) {
                                        pos.increment(&res_word);
                                    }
                                },
                                VarKind::Syllable(syll) => {
                                    let mut new_syll = syll.clone();
                                    if let Some(m) = mods {
                                        new_syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                    }
                                    if pos.at_syll_start() {
                                        res_word.syllables.insert(pos.syll_index, new_syll.clone());
                                        pos.syll_index += 1;

                                    } else {
                                        // split current syllable in two, insert var_syll in between them
                                        let before_syll = res_word.syllables.get_mut(pos.syll_index).unwrap();
                                        let mut after_syll = Syllable::new();
                                        while before_syll.segments.len() > pos.seg_index {
                                            after_syll.segments.push_front(before_syll.segments.pop_back().unwrap());
                                        }
                                        res_word.syllables.insert(pos.syll_index+1, after_syll);
                                        res_word.syllables.insert(pos.syll_index+1, new_syll);

                                        pos.syll_index += 3;
                                        pos.seg_index = 0;
                                    }
                                },
                            }
                        } else {
                            return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                        }
                    },


                    ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(z.position)),
                    ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(z.position)),
                    
                    ParseElement::EmptySet   | ParseElement::Metathesis    | 
                    ParseElement::Ellipsis   | ParseElement::Optional(..)  | 
                    ParseElement::WordBound  | ParseElement::Environment(..) => unreachable!(),
                }
            }
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
                        pos = sp;
                        debug_assert!(res_word.in_bounds(sp));
                        // remove segment                             
                        if res_word.syllables.len() <= 1 && word.syllables[sp.syll_index].segments.len() <= 1 {
                            return Err(RuleRuntimeError::DeletionOnlySeg)
                        }
                        res_word.syllables[sp.syll_index].segments.remove(sp.seg_index);
                        // if that was the only segment in that syllable, remove the syllable
                        if res_word.syllables[sp.syll_index].segments.is_empty() {
                            res_word.syllables.remove(sp.syll_index);
                        }
                        if pos.seg_index > 0 {
                            pos.seg_index -= 1; 
                        }
                    },
                    MatchElement::Syllable(i, _) => {
                        // remove syllable
                        if res_word.syllables.len() <= 1 {
                            return Err(RuleRuntimeError::DeletionOnlySyll)
                        }
                        pos.syll_index = i;
                        pos.seg_index = 0;
                        pos.decrement(&res_word);
                        res_word.remove_syll(i);
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
                        pos.syll_index = i;
                        pos.seg_index = 0;
                        pos.decrement(&res_word);

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
                    },
                }
            }
        };
        if let Some(next) = next_pos {
            pos.increment(&res_word);
            // pos.increment(&res_word);
            *next = pos;
        }

        if res_word.syllables.last_mut().unwrap().segments.is_empty() {
            res_word.syllables.pop();
        }
        Ok(res_word)
    }

    // fn deletion(&self, word: &Word, els: Vec<MatchElement>) -> Result<Word, RuleRuntimeError> {
    //     todo!()
    // }

    fn input_match_at(&self, word: &Word, start_index: SegPos) -> Result<(Vec<MatchElement>, Option<SegPos>), RuleRuntimeError> {
        let mut cur_index = start_index;
        let mut match_begin = None;
        let mut state_index = 0;
        let mut captures: Vec<_> = Vec::new();

        while word.in_bounds(cur_index) {
            if self.input_match_item(&mut captures, &mut cur_index, &mut state_index, word, &self.input)? {
                // if we have a full match
                if state_index > self.input.len() - 1 { 

                    // As matching a syllbound doesn't increment, this is to avoid an infinite loop
                    if self.input.last().unwrap().kind == ParseElement::SyllBound {
                        cur_index.increment(word);
                    }
                    return Ok((captures, Some(cur_index)));
                }
                if match_begin.is_none() { 
                    // if we haven't started matching, we have now
                    match captures.last().expect("") {
                        MatchElement::Segment(sp, _) => match_begin = Some(*sp),
                        MatchElement::Syllable(sp, _) |
                        MatchElement::SyllBound(sp, _) => match_begin = Some(SegPos { syll_index: *sp, seg_index: 0 }),
                    }
                }
                // else continue 
            } else if let Some (x) = match_begin { 
                // if we were in the middle of matching but now don't match, go back to when we started matching +1 and start again
                cur_index = x;
                cur_index.increment(word);
                state_index = 0;
                captures = vec![];
                match_begin = None;
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
            } else {
                // if we weren't in the middle of matching, move on
                cur_index.increment(word);
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
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
            ParseElement::Variable(vt, m) => if self.input_match_var(captures, state_index, vt, m, word, seg_pos, err_pos)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Ipa(s, m) => if self.input_match_ipa(captures, s, m, word, *seg_pos, err_pos)? {
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

            ParseElement::Optional(..) | ParseElement::Environment(_, _) |
            ParseElement::EmptySet | ParseElement::WordBound | ParseElement::Metathesis  => unreachable!(),
        }
    }

    // fn input_match_optionals(&self, captures: &mut [MatchElement], word: &Word, pos: &mut SegPos, states: &[Item], opt_states: &[Item], match_min: usize, match_max: usize) -> Result<bool, RuleRuntimeError> {
    //     // should work like regex (...){min, max}?
    //     let max = if match_max == 0 {None} else{ Some(match_max)};
    //     self.input_match_multiple(captures, word, pos,  states, &mut 0, match_min, max, opt_states, true)
    // }
    
    fn input_match_ellipsis(&self, captures: &mut Vec<MatchElement>, word: &Word, pos: &mut SegPos, states: &[Item], state_index: &mut usize) -> Result<bool, RuleRuntimeError> {
        // should work akin to '.+?' in Regex, that is, a lazy-match of one-or-more elements
        // increment seg_pos
        // save position
        // try to match rest of states
        // if match, return true
        // else return to saved position
        // increment seg_pos
        // repeat until end of word
        
        if *state_index >= states.len() {
            return Ok(true)
        }

        *state_index += 1;
        pos.increment(word);

        while word.in_bounds(*pos) {
            let back_pos = *pos;
            let back_state = *state_index;

            let mut m = true;
            while *state_index < states.len() {
                if !self.input_match_item(captures, pos, state_index, word, states)? {
                    m = false;
                    break;
                }
                *state_index += 1;
            }
            if m {
                return Ok(true)
            }
            *state_index = back_state;
            *pos = back_pos;
            pos.increment(word);
        }
        
        Ok(false)
    }

    fn input_match_syll(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, stress: &[Option<ModKind>;2], tone: &Option<String>, var: &Option<usize>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        // checks current segment is at start of syll
        // matches stress and tone
        // jumps to end of syllable if match
        if word.in_bounds(*pos) && pos.seg_index == 0 {
        // if word.seg_is_syll_initial(*seg_index) {
            let cur_syll_index = pos.syll_index;
            let cur_syll = &word.syllables[cur_syll_index];

            if !self.match_stress(stress, cur_syll)? {
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

    fn match_stress(&self, stress: &[Option<ModKind>; 2], syll: &Syllable) -> Result<bool, RuleRuntimeError> {
        // ±stress (+ matches prim and sec, - matches unstressed)
        if let Some(str) = stress[0] {
            match str {
                ModKind::Binary(bm) => match bm {
                    BinMod::Negative => if syll.stress != StressKind::Unstressed { return Ok(false) },
                    BinMod::Positive => if syll.stress == StressKind::Unstressed { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Primary |
                                StressKind::Secondary  => if !pos { return Ok(false) },
                                StressKind::Unstressed => if  pos { return Ok(false) },
                            }
                            alpha_assigned = true;
                        } 
                        if !alpha_assigned {
                            let stress = syll.stress != StressKind::Unstressed;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                    },
                    AlphaMod::InvAlpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Primary |
                                StressKind::Secondary  => if  pos { return Ok(false) },
                                StressKind::Unstressed => if !pos { return Ok(false) },
                            }
                            alpha_assigned = true;
                        } // else {
                        //     return Err(RuleRuntimeError::AlphaUnknownInvStress(err_pos))
                        // } 
                        if !alpha_assigned {
                            let stress = syll.stress == StressKind::Unstressed;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                        
                    },
                },
            }
        }
        // ±secstress (+ matches sec, - matches prim and unstressed)
        if let Some(str) = stress[1] {
            match str {
                ModKind::Binary(bm) => match bm {
                    BinMod::Negative => if syll.stress == StressKind::Secondary { return Ok(false) },
                    BinMod::Positive => if syll.stress != StressKind::Secondary { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Secondary  => if !pos { return Ok(false) },
                                StressKind::Primary |
                                StressKind::Unstressed => if  pos { return Ok(false) },
                            }
                            alpha_assigned = true;
                        } 
                        if !alpha_assigned {
                            let stress = syll.stress == StressKind::Secondary;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                    },
                    AlphaMod::InvAlpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Secondary  => if  pos { return Ok(false) },
                                StressKind::Primary |
                                StressKind::Unstressed => if !pos { return Ok(false) },
                            }
                            alpha_assigned = true;
                        } // else {
                        //     return Err(RuleRuntimeError::AlphaUnknownInvStress(err_pos))
                        // }
                        if !alpha_assigned {
                            let stress = syll.stress != StressKind::Secondary;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                    },
                },
            }
        }

        Ok(true)
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
                ParseElement::Variable(vt, mods) => self.input_match_var(captures, state_index, vt, mods, word, pos, s.position),
                ParseElement::Ipa(seg, mods) => if self.input_match_ipa(captures, seg, mods, word, *pos, s.position)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Matrix(mods, var) => if self.input_match_matrix(captures, mods, var, word, pos, s.position)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Syllable(stress, tone, var) => self.input_match_syll(captures, state_index, stress, tone, var, word, pos),
                ParseElement::SyllBound => if pos.at_syll_start() {
                    captures.push(MatchElement::SyllBound(pos.syll_index, Some(i))); // FIXME: `i` is being unnecessarily reassigned
                    Ok(true)
                } else { Ok(false) },
                ParseElement::WordBound => todo!("Err: WordBound not valid in input"),
                _ => unreachable!(),
            };
            if res? {
                captures.last_mut().unwrap().set_ind(Some(i));
                return Ok(true)
            }
        }
        Ok(false)
    }

    fn input_match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, word: &Word, pos: SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(pos).unwrap();

        if let Some(m) = mods {
            if self.match_ipa_with_modifiers(s, m, word, &pos, err_pos)? {
                captures.push(MatchElement::Segment(pos, None));
                Ok(true)
            } else {
                Ok(false)
            }
        } else if *s == seg {
            captures.push(MatchElement::Segment(pos, None));
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn input_match_syll_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, syll_to_match: &Syllable, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        if pos.seg_index != 0 || word.out_of_bounds(*pos){
            return Ok(false)
        }
        let csi = pos.syll_index;
        let cur_syll = &word.syllables[csi];

        if let Some(m) = mods {
            if !self.match_stress(&m.suprs.stress, cur_syll)? {
                return Ok(false)
            } 
            if let Some(t) = &m.suprs.tone.as_ref() {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
            }
            if cur_syll.segments != syll_to_match.segments {
                return Ok(false)
            }

        } else if *cur_syll != *syll_to_match {
            return Ok(false)
        }
        captures.push(MatchElement::Syllable(csi, None));

        *state_index += 1;
        pos.syll_index += 1;
        pos.seg_index = 0;

        // Because we are incrementing in the parent function in the case of a match
        // We must jump to the next syllable but not skip the segment at index 0
        // if pos.syll_index < word.syllables.len() {
        //     pos.decrement(word);
        // }
        
        Ok(true)
        
    }

    fn input_match_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, vt: &Token, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            Some(var) => match var {
                VarKind::Segment(s)  => if self.input_match_ipa(captures, s, mods, word, *pos, err_pos)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
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
            let mut seg_length = word.seg_length_at(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(true)
        } else {
            // the way we implement `long` vowels means we need to do this
            let mut seg_length = word.seg_length_at(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(false)
        }
    }

    fn match_ipa_with_modifiers(&self, s: &Segment, mods: &Modifiers, word: &Word, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let mut seg = word.get_seg_at(*pos).expect("Segment Position should be within bounds");

        seg.apply_seg_mods(&self.alphas, mods.nodes, mods.feats, err_pos, true)?;

        if *s == seg {
            Ok(self.match_supr_mod_seg(word, &mods.suprs, pos)?)
        } else {
            Ok(false)
        }
    }

    fn match_modifiers(&self, mods: &Modifiers, word: &Word, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(*pos).expect("Segment Position should be within bounds");
        
        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i, seg)? {
                return Ok(false);
            }
        }
        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.match_node_mod(m, i, seg, err_pos)? {
                return Ok(false);
            }
        }
        self.match_supr_mod_seg(word, &mods.suprs, pos)
    }

    fn match_supr_mod_seg(&self, word: &Word, mods: &SupraSegs, pos: &SegPos) -> Result<bool, RuleRuntimeError> {

        let syll = &word.syllables[pos.syll_index];

        if !self.match_stress(&mods.stress, syll)? { return Ok(false) }
        if !self.match_seg_length(word, &mods.length, pos)? { return Ok(false) }

        if let Some(t) = mods.tone.as_ref() {
            return Ok(self.match_tone(t, syll))
        }

        Ok(true)
    }

    fn match_seg_length(&self, word: &Word, length: &[Option<ModKind>; 2], pos: &SegPos) -> Result<bool, RuleRuntimeError> {
        let seg_length = word.seg_length_at(*pos);
        // +/- long
        if let Some(len) = length[0] {
            match len {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => if seg_length < 2 { return Ok(false) },
                    BinMod::Negative => if seg_length > 1 { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match alph.as_binary() {
                                true  => if seg_length < 2 { return Ok(false) },
                                false => if seg_length > 1 { return Ok(false) },
                            }
                            alpha_assigned = true;
                        }
                        if !alpha_assigned {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length > 1));
                        }
                    },
                    AlphaMod::InvAlpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match !alph.as_binary() {
                                true  => if seg_length < 2 { return Ok(false) },
                                false => if seg_length > 1 { return Ok(false) }
                            }
                            alpha_assigned = true;
                        }
                        if !alpha_assigned {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length <= 1));
                        }
                    },
                },
            }
        }
        // +/- overlong
        if let Some(len) = length[1] {
            match len {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => if seg_length < 3 { return Ok(false) },
                    BinMod::Negative => if seg_length > 2 { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match alph.as_binary() {
                                true  => if seg_length < 3 { return Ok(false) },
                                false => if seg_length > 2 { return Ok(false) },
                            }
                            alpha_assigned = true;
                        } // else {
                        //     return Err(RuleRuntimeError::AlphaIsNotSupra(err_pos))
                        // }
                        if !alpha_assigned  {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length > 2));
                        }

                    },
                    AlphaMod::InvAlpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match !alph.as_binary() {
                                true  => if seg_length < 3 { return Ok(false) },
                                false => if seg_length > 2 { return Ok(false) },
                            }
                            alpha_assigned = true;
                        }
                        if !alpha_assigned {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length <= 2));
                        }
                    },
                },
            }
        }
        Ok(true)
    }

    fn match_node_mod(&self, md:&Option<ModKind>, node_index: usize, seg: Segment, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md {
            let node = NodeKind::from_usize(node_index);
            return self.match_node(seg, node, kind, err_pos)
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

    fn match_node(&self, seg: Segment, node: NodeKind, val: &ModKind, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match val {
            ModKind::Binary(bt) => if node == NodeKind::Place {
                let x = seg.get_node(NodeKind::Labial).is_some() 
                || seg.get_node(NodeKind::Coronal).is_some()
                || seg.get_node(NodeKind::Dorsal).is_some()
                || seg.get_node(NodeKind::Pharyngeal).is_some();
                match bt {
                    BinMod::Positive => Ok(x),
                    BinMod::Negative => Ok(!x),
                }
            } else {
                match bt {
                    BinMod::Positive => Ok(seg.get_node(node).is_some()),
                    BinMod::Negative => Ok(seg.get_node(node).is_none()),
                }
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        if let Some((n, m)) = alph.as_node() {
                            if n == node {
                                return Ok(seg.node_match(n, m))
                            } else {
                                todo!("Err: node alpha must be matched to same node")
                            }
                        } else if let Some(place) = alph.as_place() {
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
                        self.alphas.borrow_mut().insert(*ch, Alpha::Place(place)); 
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Node(node, seg.get_node(node))); 
                    }
                    Ok(true)
                },
                AlphaMod::InvAlpha(inv_ch) => {
                    if let Some(alph) = self.alphas.borrow().get(inv_ch) {
                        if let Some((n, m)) = alph.as_node() {
                            if n == node {
                                Ok(!seg.node_match(n, m))
                            } else {
                                todo!("Err: node alpha must be matched to same node")
                            }
                        } else if let Some(place) = alph.as_place() {
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
                        Err(RuleRuntimeError::AlphaUnknownInv(err_pos))
                    }
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
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        return Ok(seg.feat_match(node, mask, alph.as_binary()))
                    } // NOTE: cannot `be else if` because alphas.borrow is not dropped. See: https://github.com/rust-lang/rust/issues/113792
                    if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Feature(f != 0)); 
                        Ok(true)
                    } else {
                        // Maybe err?
                        Ok(false)
                    }
                },
                AlphaMod::InvAlpha(inv_ch) => {
                    if let Some(alph) = self.alphas.borrow().get(inv_ch) {
                        return Ok(seg.feat_match(node, mask, !alph.as_binary()))
                    } // NOTE: cannot `be else if` because alphas.borrow is not dropped. See: https://github.com/rust-lang/rust/issues/113792
                    if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*inv_ch, Alpha::Feature(f == 0));
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