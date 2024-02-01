use std ::{
    cell::RefCell, 
    collections::HashMap
};

use crate ::{
    parser::{Item, ParseKind, Modifiers, Supr, SegMKind, BinMod, AlphaMod}, 
    error :: RuleRuntimeError, 
    word  :: Word, 
    syll  ::{Syllable, StressKind},
    seg   ::{Segment, NodeKind, feature_to_node_mask},
    lexer ::{Token, SupraType, FType}, 
    rule  ::{RuleType, Alpha}
};

type SegIndex = usize; // the index of the segment in the word.segments array
type SylIndex = usize; // the index of the syllable in the word.syllables array
type BndIndex = usize; // the index of the segment immediately after the boundary

#[derive(Debug, Clone, Copy)]
pub enum MatchElement {
    Segment  (SegIndex),
    Syllable (SylIndex),
    SyllBound(BndIndex)
}

impl MatchElement {
    pub fn get_val(&self) -> usize {
        match self {
            Self::Segment(v)  |
            Self::Syllable(v) |
            Self::SyllBound(v) => *v,
            
        }
    }
}


#[derive(Debug)]
pub struct SubRule {
    pub input    : Vec<Item>,
    pub output   : Vec<Item>,
    pub context  : Option<Item>,         
    pub except   : Option<Item>,
    pub rule_type: RuleType,
    pub variables: RefCell<HashMap<usize, Segment>>,
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

        let input_result = if let RuleType::Insertion = self.rule_type {
            vec![]
        } else {
            let result = self.match_input_at(&word, 0)?;
            if result.is_empty() {
                // println!("{}", word.render().unwrap());
                // println!("No match");
                return Ok(word)
            }
            result
        };


        println!("{}", word.render().unwrap());
        println!("Match! {:?}", input_result);

        // Match context
        
        // Match exceptions

        // Apply Output
        self.transform(&word, input_result)
        
    }

    fn transform(&self, word: &Word, input: Vec<MatchElement>) -> Result<Word, RuleRuntimeError> {
        match self.rule_type {
            RuleType::Metathesis => {
                let mut res_word = word.clone();

                for z in 0..(input.len() / 2) {
                    
                    match (input[z], input[input.len()-1-z]) {
                        (MatchElement::Segment(i), MatchElement::Segment(j)) => {
                            // res_word.segments[i] = word.segments[j];
                            // res_word.segments[j] = word.segments[i];
                            res_word.segments.swap(i, j);
                            
                        },
                        (MatchElement::Segment(_), MatchElement::Syllable(_)) => todo!(),
                        (MatchElement::Segment(_), MatchElement::SyllBound(_)) => todo!(),
                        (MatchElement::Syllable(_), MatchElement::Segment(_)) => todo!(),
                        (MatchElement::Syllable(i), MatchElement::Syllable(j)) => {
                            res_word.swap_syll(i, j);
                        },
                        (MatchElement::Syllable(_), MatchElement::SyllBound(_)) => todo!(),
                        (MatchElement::SyllBound(_), MatchElement::Segment(_)) => todo!(),
                        (MatchElement::SyllBound(_), MatchElement::Syllable(_)) => todo!(),
                        (MatchElement::SyllBound(_), MatchElement::SyllBound(_)) => todo!(),
                    }
                }

                Ok(res_word)
            },
            RuleType::Deletion => todo!(),
            RuleType::Insertion => todo!(),
            RuleType::Substitution => todo!(),
        }
    }

    fn match_input_at(&self, word: &Word, start_index: usize) -> Result<Vec<MatchElement>, RuleRuntimeError> {
        let mut cur_index = start_index;
        let mut begin = None;
        let mut state_index = 0;
        let mut captures: Vec<_> = Vec::new();

        while cur_index <= word.seg_count() {
            if self.match_input_item(&mut captures, &mut cur_index, &mut state_index, word, &self.input)? {
                if state_index > self.input.len() - 1 {
                    return Ok(captures);
                }

                if begin.is_none() {
                    begin = Some(cur_index); // TODO: This won't work if we jump i.e. if we match syllable
                }
            } else if begin.is_none() {
                cur_index += 1;
                captures = vec![]; // TODO(girv): Should be unnecessary, but safety first!: 
            } else {
                cur_index = begin.unwrap() + 1;
                state_index = 0;
                captures = vec![];
                begin = None;
            }
        }
        if begin.is_none() {
            Ok(vec![])
        } else if let ParseKind::WordBound | ParseKind::SyllBound = self.input.last().expect("Input is empty").kind {
            // Ok(Some(Match::new(begin.unwrap(), word.seg_count())))
            todo!()
        } else {
            Ok(vec![])
        }
    }


    fn match_input_item(
        &self, 
        captures: &mut Vec<MatchElement>, 
        seg_index: &mut usize, 
        state_index: &mut usize,
        word: &Word, 
        states: &[Item], 
    ) -> Result<bool, RuleRuntimeError> {
        match &states[*state_index].kind {
            ParseKind::Variable(vt, m) => if self.match_var(captures, vt, m, word, *seg_index)? {
                *seg_index += 1;
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Ipa(s, m) => if self.match_ipa(captures, s, m, word, *seg_index)? {
                *seg_index += 1;
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Matrix(m, v) => if self.match_matrix(captures, m, v, word, *seg_index)? {
                *seg_index += 1;
                *state_index += 1;
                Ok(true) 
            } else { Ok(false) },
            ParseKind::Set(s) => if self.match_set(captures, s, word, *seg_index)? {
                *seg_index += 1; // TODO(girv): when we allow boundaries within sets, this will have to be incremented within the match_set function
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::SyllBound => if self.match_syll_bound(captures, word, *seg_index) {
                // NOTE(girv): Boundaries do not advance seg_index 
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseKind::Syllable(s, t) => self.match_syll(captures, state_index, s, t, word, seg_index),
            ParseKind::Ellipsis => self.match_ellipsis(captures, word, seg_index, states, state_index),
            ParseKind::Optional(opt_states, match_min, match_max) => self.match_optionals(captures, word, seg_index, states, opt_states, *match_min, *match_max),
            _ => unreachable!()
        }
    }

    fn match_optionals(&self, captures: &mut [MatchElement], word: &Word, seg_index: &mut usize, states: &[Item], opt_states: &[Item], match_min: usize, match_max: usize) -> Result<bool, RuleRuntimeError> {
        // should work like regex (...){min, max}? 
        let max = if match_max == 0 {None} else{ Some(match_max)};
        self.match_multiple(captures, word, seg_index,  states, &mut 0, match_min, max, opt_states, true)
    }
    
    fn match_ellipsis(&self, captures: &mut [MatchElement], word: &Word, seg_index: &mut usize, states: &[Item], state_index: &mut usize) -> Result<bool, RuleRuntimeError> {
        // should work akin to '.+?' in Regex, that is, a lazy-match of one-or-more elements
        // this should not capture, however
        self.match_multiple(captures, word, seg_index, states, state_index, 1, None, &[], false)
    }

    fn match_multiple(
        &self, captures: &mut [MatchElement], 
        word: &Word, seg_index: &mut usize, 
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
                if *seg_index >= word.seg_count() {
                    return Ok(false)
                }
                
                if capture_wanted {
                    // add to caps
                    caps.push(MatchElement::Segment(*seg_index))
                }

                *seg_index += 1;
                i += 1;
            } else {
                // NOTE: segs are captured regardless of the `capture_wanted` check
                let mut inner_state_index = 0;
                while *seg_index <= word.seg_count() && inner_state_index < inner_states.len(){
                    if !self.match_input_item(&mut caps, seg_index, &mut inner_state_index, word, inner_states)? {
                        *seg_index = back_seg;
                        *state_index = back_state;
                        return Ok(false)
                    }
                }
                if *seg_index >= word.seg_count() {
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


    fn match_syll(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, stress: &Option<Supr>, tone: &Option<String>, word: &Word, seg_index: &mut usize) -> Result<bool, RuleRuntimeError> {
        // checks current segment is at start of syll
        // matches stress and tone
        // jumps to end of syllable if match
        if word.seg_is_syll_initial(*seg_index) {
            let cur_syll_index = word.get_syll_index_from_seg_index(*seg_index);
            let cur_syll = word.get_syll_at(cur_syll_index).unwrap();

            if let Some(s) = stress.as_ref() {
                if !self.match_stress(s, &cur_syll) {
                    return Ok(false)
                }
            }

            if let Some(t) = tone.as_ref() {
                if !self.match_tone(t, &cur_syll) {
                    return Ok(false)
                }
            }
            captures.push(MatchElement::Syllable(cur_syll_index));
            
            *state_index += 1;
            *seg_index = cur_syll.end + 1; // NOTE(girv): this is only correct if we DON'T advance seg_index after match in `match_input_at`

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_stress(&self, stress: &Supr, syll: &Syllable) -> bool {
        match stress.kind {
            // ±stress (+ matches prim and sec, - matches unstressed)
            SupraType::Stress => match stress.modifier {
                SegMKind::Binary(b) => match b {
                    BinMod::Negative => syll.stress == StressKind::Unstressed,
                    BinMod::Positive => syll.stress != StressKind::Unstressed,
                },
                SegMKind::Alpha(_) => todo!(),
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

    fn match_syll_bound(&self, captures: &mut Vec<MatchElement>, word: &Word, seg_index: usize) -> bool {
        if word.seg_is_syll_initial(seg_index) {
            captures.push(MatchElement::SyllBound(seg_index));
            true
        } else {
            false
        }
    }

    fn match_set(&self, captures: &mut Vec<MatchElement>, set: &[Item], word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
        for s in set {
            let res = match &s.kind {
                ParseKind::Variable(vt, m) => self.match_var(captures, vt, m, word, seg_index),
                ParseKind::Ipa(s, m)       => self.match_ipa(captures, s, m, word, seg_index),
                ParseKind::Matrix(m, v)    => self.match_matrix(captures, m, v, word, seg_index),
                _ => unreachable!(),
            };
            if res? {
                return Ok(true)
            } else {
                continue;
            }
        }
        Ok(false)
    }

    fn match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
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

    fn match_var(&self, captures: &mut Vec<MatchElement>, vt: &Token, mods: &Option<Modifiers>, word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().expect("TODO ERROR MESSAGE")) {
            self.match_ipa(captures, var, mods, word, seg_index)
            // NOTE(girv): we should not push here
        } else {
            Err(RuleRuntimeError::UnknownVariable(vt.clone()))
        }
    }

    fn match_matrix(&self, captures: &mut Vec<MatchElement>, mods: &Modifiers, var: &Option<usize>, word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> { 
        if self.match_modifiers(mods, word, seg_index)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, word.get_seg_at(seg_index).unwrap());
            }
            captures.push(MatchElement::Segment(seg_index));
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn match_modifiers(&self, mods: &Modifiers, word: &Word, seg_index: usize) -> Result<bool, RuleRuntimeError> {
        let seg = word.segments[seg_index];

        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i, seg)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn match_feat_mod(&self, md: &Option<SegMKind>, feat_index: usize, seg: Segment) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md { 
            let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
            return self.match_seg_kind(kind, seg, node, mask)

        }
        Ok(true)
    }

    fn match_seg_kind(&self, kind: &SegMKind, seg: Segment, node: NodeKind, mask: u8) -> Result<bool, RuleRuntimeError> {
        match kind {
            SegMKind::Binary(bt) => match bt {
                BinMod::Negative => Ok(seg.feat_match(node, mask, false)),
                BinMod::Positive => Ok(seg.feat_match(node, mask, true)),
            },
            SegMKind::Alpha(am) => match am {
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
                AlphaMod::InversAlpha(ia) => {
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