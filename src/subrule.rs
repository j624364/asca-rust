use std ::{
    cell::RefCell, 
    collections::{HashMap, VecDeque}
};

use crate ::{
    error :: RuleRuntimeError, 
    lexer ::{Token, SupraType, FType}, 
    parser::{Item, ParseKind, Modifiers, Supr, SegModKind, BinMod, AlphaMod}, 
    rule  ::{RuleType, Alpha}, 
    seg   ::{Segment, NodeKind, feature_to_node_mask}, 
    syll  ::{Syllable, StressKind}, 
    word  ::{Word, SegPos},
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
        // match self.rule_type {
        //     RuleType::Substitution  => {/* input>env>output */},
        //     RuleType::Metathesis    => {/* skip calc output */},
        //     RuleType::Deletion      => {/* skip calc output */},
        //     RuleType::Insertion     => {/* skip match input */},
        // }

        if let RuleType::Insertion = self.rule_type {
            // TODO(girv): match context and exceptions
            return self.transform(&word, vec![], &mut None)
        } 
        
        let mut word = word;
        let mut cur_index = SegPos::new(0, 0);
        // FIXME(girv): a rule of `$ > &` gives an infinite loop
        // Additionally, `$ > *` or any broad deletion rule without context/exception should error or give a warning to the user
        loop {
            let (res, mut next_index) = self.match_input_at(&word, cur_index)?;
            if !res.is_empty() {
                println!("{}", word.render().unwrap());
                println!("Match! {:?}", res);
                // FIXME(girv): if the layout of the word changes (i.e. through substitution, insertion or deletion)
                //             then the returned index is not in the correct position
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

    fn transform(&self, word: &Word, input: Vec<MatchElement>, current_pos: &mut Option<SegPos>) -> Result<Word, RuleRuntimeError> {
        match self.rule_type {
            RuleType::Metathesis => {
                let mut res_word = word.clone();

                for z in 0..(input.len() / 2) {
                    match (input[z], input[input.len()-1-z]) {
                        (MatchElement::Segment(i), MatchElement::Segment(j)) => {
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
                                println!("{:?}", res_word.syllables);

                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()                                
                                println!("{:?}", res_word.syllables);
                            } else {
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()
                                res_word.syllables.insert(0, Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: String::new() });
                                res_word.syllables.first_mut().unwrap().segments.push_front(seg);
                            }
                            if res_word.syllables[si.syll_index].segments.is_empty() {
                                res_word.syllables.remove(si.syll_index);
                            }
                            println!("{:?}", res_word.syllables);
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
                            println!("{:?}", word.syllables);
                            println!("i: {:?}", i);
                            res_word.syllables[i.syll_index].segments.remove(i.seg_index);
                            // if that was the only segment in that syllable, remove the syllable
                            if res_word.syllables[i.syll_index].segments.is_empty() {
                                res_word.syllables.remove(i.syll_index);
                                
                                println!("{:?}", current_pos);
                            
                                if let Some(pos) = current_pos {
                                    debug_assert!(pos.syll_index > 0);
                                    pos.syll_index -= 1;
                                }
                            }
                            println!("{:?}", current_pos);
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
                let _ = self.analyse_output_insertion(word)?;
                todo!();
            },
            RuleType::Substitution => {
                let _ = self.analyse_output_substitution(word, input)?;
                todo!();
            },
        }
    }

    fn analyse_output_insertion(&self, word: &Word) -> Result<Vec<Segment>, RuleRuntimeError> {
        //                                                      ^ This will definitely not output Vec<Segment>, but this is a stand-in until we understand out datastructure
        
        for state in &self.output {
            match state.kind {
                ParseKind::Syllable(_, _, _) => todo!(),
                ParseKind::Matrix(_, _) => todo!(),
                ParseKind::Ipa(_, _) => todo!(),
                ParseKind::Variable(_, _) => todo!(),


                ParseKind::Set(_) => return Err(RuleRuntimeError::LonelySet(state.position)),
                ParseKind::EmptySet      | ParseKind::Metathesis | 
                ParseKind::SyllBound     | ParseKind::Ellipsis   | 
                ParseKind::Optional(..)  | ParseKind::WordBound  | 
                ParseKind::Environment(..) => unreachable!(),
            }
        }

        todo!()
    }
    
    fn analyse_output_substitution(&self, word: &Word, input: Vec<MatchElement>) -> Result<Vec<Segment>, RuleRuntimeError> {
        //                                                                                    ^ This will definitely not output Vec<Segment>, but this is a stand-in until we understand out datastructure
        for (si, state) in self.output.iter().enumerate() {
            match state.kind {
                ParseKind::Syllable(_, _, _) => todo!(),
                ParseKind::Matrix(_, _) => todo!(),
                ParseKind::Ipa(_, _) => todo!(),
                ParseKind::Variable(_, _) => todo!(),
                ParseKind::Set(_) => {
                    // Check that self.input[si] is a set, if not throw RuleRuntimeError::LonelySet(state.position)
                    // See which one of the input set matched and use the corresponding in output to substitute
                    todo!()
                },
                
                ParseKind::EmptySet      | ParseKind::Metathesis | 
                ParseKind::SyllBound     | ParseKind::Ellipsis   | 
                ParseKind::Optional(..)  | ParseKind::WordBound  | 
                ParseKind::Environment(..) => unreachable!(),
            }
        }
        todo!()
    }

    fn match_input_at(&self, word: &Word, start_index: SegPos) -> Result<(Vec<MatchElement>, Option<SegPos>), RuleRuntimeError> {
        // TODO(girv): match context and exceptions
        let mut cur_index = start_index;
        let mut match_begin = None;
        let mut state_index = 0;
        let mut captures: Vec<_> = Vec::new();

        while word.in_bounds(cur_index) {
            if self.match_input_item(&mut captures, &mut cur_index, &mut state_index, word, &self.input)? {
                if state_index > self.input.len() - 1 { 
                    // if we have a full match             
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
        } else if let ParseKind::WordBound | ParseKind::SyllBound = self.input.last().expect("Input is empty").kind {
            // if we've reached the end of the word and the last state is a word boundary
            captures.push(MatchElement::SyllBound(word.syllables.len()));
            Ok((captures, None))
        } else { // No Match
            Ok((vec![], None))
        }
    }


    fn match_input_item(
        &self, 
        captures: &mut Vec<MatchElement>, 
        seg_index: &mut SegPos, 
        state_index: &mut usize,
        word: &Word, 
        states: &[Item], 
    ) -> Result<bool, RuleRuntimeError> {
        match &states[*state_index].kind {
            ParseKind::Variable(vt, m) => if self.match_var(captures, state_index, vt, m, word, seg_index)? {
                seg_index.increment(word);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Ipa(s, m) => if self.match_ipa(captures, s, m, word, *seg_index)? {
                seg_index.increment(word);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Matrix(m, v) => if self.match_matrix(captures, m, v, word, *seg_index)? {
                seg_index.increment(word);
                *state_index += 1;
                Ok(true) 
            } else { Ok(false) },
            ParseKind::Set(s) => if self.match_set(captures, state_index, s, word, seg_index)? {
                seg_index.increment(word); // TODO(girv): when we allow boundaries within sets, this will have to be incremented within the match_set function
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::SyllBound => if self.match_syll_bound(captures, word, *seg_index) {
                // NOTE(girv): Boundaries do not advance seg_index 
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Syllable(s, t, v) => self.match_syll(captures, state_index, s, t, v, word, seg_index),
            ParseKind::Ellipsis => self.match_ellipsis(captures, word, seg_index, states, state_index),
            ParseKind::Optional(opt_states, match_min, match_max) => self.match_optionals(captures, word, seg_index, states, opt_states, *match_min, *match_max),            
            ParseKind::EmptySet | ParseKind::WordBound | ParseKind::Metathesis | ParseKind::Environment(_, _) => unreachable!(),
        }
    }

    fn match_optionals(&self, captures: &mut [MatchElement], word: &Word, seg_index: &mut SegPos, states: &[Item], opt_states: &[Item], match_min: usize, match_max: usize) -> Result<bool, RuleRuntimeError> {
        // should work like regex (...){min, max}? 
        let max = if match_max == 0 {None} else{ Some(match_max)};
        self.match_multiple(captures, word, seg_index,  states, &mut 0, match_min, max, opt_states, true)
    }
    
    fn match_ellipsis(&self, captures: &mut [MatchElement], word: &Word, seg_index: &mut SegPos, states: &[Item], state_index: &mut usize) -> Result<bool, RuleRuntimeError> {
        // should work akin to '.+?' in Regex, that is, a lazy-match of one-or-more elements
        // this should not capture, however
        self.match_multiple(captures, word, seg_index, states, state_index, 1, None, &[], false)
    }

    fn match_multiple(
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
                    if !self.match_input_item(&mut caps, seg_index, &mut inner_state_index, word, inner_states)? {
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

    fn match_syll(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, stress: &Option<Supr>, tone: &Option<String>, var: &Option<usize>, word: &Word, seg_index: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        // checks current segment is at start of syll
        // matches stress and tone
        // jumps to end of syllable if match
        if seg_index.seg_index == 0 {
        // if word.seg_is_syll_initial(*seg_index) {
            let cur_syll_index = seg_index.syll_index; //word.get_syll_index_from_seg_index(*seg_index);
            let cur_syll = &word.syllables[cur_syll_index];

            if let Some(s) = stress.as_ref() {
                if !self.match_stress(s, cur_syll) {
                    return Ok(false)
                }
            }

            if let Some(t) = tone.as_ref() {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
            }

            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[seg_index.syll_index].clone()));
            }
            captures.push(MatchElement::Syllable(cur_syll_index));
            
            *state_index += 1;
            // NOTE(girv): this is only correct if we DON'T advance seg_index after match in `match_input_at`
            seg_index.syll_index += 1;
            seg_index.seg_index = 0;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_stress(&self, stress: &Supr, syll: &Syllable) -> bool {
        match stress.kind {
            // ±stress (+ matches prim and sec, - matches unstressed)
            SupraType::Stress => match stress.modifier {
                SegModKind::Binary(b) => match b {
                    BinMod::Negative => syll.stress == StressKind::Unstressed,
                    BinMod::Positive => syll.stress != StressKind::Unstressed,
                },
                SegModKind::Alpha(_) => todo!(),
            },
            // ±secstress (+ matches sec, - matches prim and unstressed)
            SupraType::SecStress => match stress.modifier {
                SegModKind::Binary(b) => match b {
                    BinMod::Negative => syll.stress != StressKind::Secondary,
                    BinMod::Positive => syll.stress == StressKind::Secondary,
                },
                SegModKind::Alpha(_) => todo!(),
            },
            _ => unreachable!(),
        }
    }

    fn match_tone(&self, tone: &str, syll: &Syllable) -> bool {        
        tone == syll.tone
    }

    fn match_syll_bound(&self, captures: &mut Vec<MatchElement>, word: &Word, seg_index: SegPos) -> bool {
        if seg_index.seg_index == 0 {
        // if word.seg_is_syll_initial(seg_index) {
            captures.push(MatchElement::SyllBound(seg_index.syll_index));
            true
        } else {
            false
        }
    }

    fn match_set(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, set: &[Item], word: &Word, seg_index: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        for s in set {
            let res = match &s.kind {
                ParseKind::Variable(vt, m) => self.match_var(captures, state_index, vt, m, word, seg_index),
                ParseKind::Ipa(s, m)       => self.match_ipa(captures, s, m, word, *seg_index),
                ParseKind::Matrix(m, v)    => self.match_matrix(captures, m, v, word, *seg_index),
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

    fn match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, word: &Word, seg_index: SegPos) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(seg_index).unwrap();
        if mods.is_none() {
            if *s == seg {
                captures.push(MatchElement::Segment(seg_index));
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            todo!("Need to Compare Modifiers")
        }
    }

    // fn match_syll_var(&self, captures: &mut Vec<MatchElement>, s: &Syllable, mods: &Option<Modifiers>, word: &Word, seg_index: SegPos) -> Result<bool, RuleRuntimeError> {
    fn match_syll_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, syll_to_match: &Syllable, mods: &Option<Modifiers>, word: &Word, seg_index: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        if seg_index.seg_index != 0 {
            return Ok(false)
        }
        let csi = seg_index.syll_index;
        let cur_syll = &word.syllables[csi];
        
        if mods.is_none() {
            if *cur_syll != *syll_to_match {
                return Ok(false)
            }
            captures.push(MatchElement::Syllable(csi));
    
            *state_index += 1;
            seg_index.syll_index += 1;
            seg_index.seg_index = 0;
            
            Ok(true)
        } else {
            todo!("Need to Compare Mods")
        }

    }


    fn match_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, vt: &Token, mods: &Option<Modifiers>, word: &Word, seg_index: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            match var {
                VarKind::Segment(s) => self.match_ipa(captures, s, mods, word, *seg_index),
                VarKind::Syllable(s) => self.match_syll_var(captures, state_index , s, mods, word, seg_index),
            }
            
            // NOTE(girv): we should not push here
        } else {
            Err(RuleRuntimeError::UnknownVariable(vt.clone()))
        }
    }

    fn match_matrix(&self, captures: &mut Vec<MatchElement>, mods: &Modifiers, var: &Option<usize>, word: &Word, seg_index: SegPos) -> Result<bool, RuleRuntimeError> { 
        if self.match_modifiers(mods, word, seg_index)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Segment(word.get_seg_at(seg_index).unwrap()));
            }
            captures.push(MatchElement::Segment(seg_index));
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_modifiers(&self, mods: &Modifiers, word: &Word, seg_index: SegPos) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(seg_index).expect("Segment Position Out of Bounds");

        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i, seg)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn match_feat_mod(&self, md: &Option<SegModKind>, feat_index: usize, seg: Segment) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md { 
            let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
            return self.match_seg_kind(kind, seg, node, mask)

        }
        Ok(true)
    }

    fn match_seg_kind(&self, kind: &SegModKind, seg: Segment, node: NodeKind, mask: u8) -> Result<bool, RuleRuntimeError> {
        match kind {
            SegModKind::Binary(bt) => match bt {
                BinMod::Negative => Ok(seg.feat_match(node, mask, false)),
                BinMod::Positive => Ok(seg.feat_match(node, mask, true)),
            },
            SegModKind::Alpha(am) => match am {
                AlphaMod::Alpha(a) => {
                    // let x = self.alphas.borrow().get(a);
                    if let Some(alph) = self.alphas.borrow().get(a) {
                        if let Some((n, m, pos)) = alph.as_feature() {
                            return Ok(seg.feat_match(*n, *m, *pos))
                        } else {
                            todo!("Err")
                        }
                    } 
                    self.alphas.borrow_mut().insert(*a, Alpha::Feature(node, mask, true)); 
                    Ok(true)
                    
                },
                AlphaMod::InvAlpha(ia) => {
                    if let Some(alph) = self.alphas.borrow().get(ia) {
                        if let Some((n, m, pos)) = alph.as_feature() {
                            Ok(seg.feat_match(*n, *m, !pos)) // TODO: test this
                        } else {
                            todo!("Err")
                        }
                    } else if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*ia, Alpha::Feature(node, mask, f != 0));
                        Ok(true)
                    } else {
                        // Err(todo!())
                        todo!() // return err
                    }
                    
                },
            },
        }
    }

}