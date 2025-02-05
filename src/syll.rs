use crate::{
    error::RuleRuntimeError,
    parser::{Modifiers, SupraSegs},
    rule::Alpha,
    seg::Segment,
    Position,
};
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StressKind {
    Primary,
    Secondary,
    Unstressed,
}

impl Default for StressKind {
    fn default() -> Self {
        Self::Unstressed
    }
}

impl fmt::Display for StressKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StressKind::Primary => write!(f, "P"),
            StressKind::Secondary => write!(f, "S"),
            StressKind::Unstressed => write!(f, "-"),
        }
    }
}

pub(crate) type Tone = u16;

#[derive(Debug, Clone)]
pub(crate) struct Syllable {
    pub(crate) segments: VecDeque<Segment>,
    pub(crate) stress: StressKind,
    pub(crate) tone: Tone,
}

impl Syllable {
    pub(crate) fn new() -> Self {
        Self {
            segments: VecDeque::new(),
            stress: StressKind::default(),
            tone: 0,
        }
    }

    pub(crate) fn replace_segment(
        &mut self,
        pos: usize,
        seg: &Segment,
        mods: &Option<Modifiers>,
        alphas: &RefCell<HashMap<char, Alpha>>,
        err_pos: Position,
    ) -> Result<i8, RuleRuntimeError> {
        let mut seg_len = self.get_seg_length_at(pos);
        let mut lc = 1 - seg_len as i8;

        while seg_len > 1 {
            self.segments.remove(pos + 1);
            // TODO: we should really do this instead of removing, but it brakes inserting after subbing
            // self.segments[pos+1] = *seg;
            seg_len -= 1;
        }
        self.segments[pos] = *seg;

        if let Some(m) = mods {
            lc += self.apply_seg_mods(alphas, m, pos, err_pos)?;
        }

        Ok(lc)
    }

    pub fn get_seg_indices(&self) -> Vec<usize> {
        let mut segments = self.segments.iter();

        let mut vec = Vec::new();

        let mut i = 0;

        while i < self.segments.len() {
            let index = i;
            let len = self.get_seg_length_at(i);
            segments.nth(len);
            i += len;
            vec.push(index);
        }
        vec
    }

    pub(crate) fn insert_segment(
        &mut self,
        pos: usize,
        seg: &Segment,
        mods: &Option<Modifiers>,
        alphas: &RefCell<HashMap<char, Alpha>>,
        err_pos: Position,
    ) -> Result<i8, RuleRuntimeError> {
        let mut lc = 0;
        if pos > self.segments.len() {
            self.segments.push_back(*seg);
        } else {
            self.segments.insert(pos, *seg);
        }

        if let Some(m) = mods {
            lc += self.apply_seg_mods(alphas, m, pos, err_pos)?;
        }

        Ok(lc)
    }

    pub(crate) fn get_seg_length_at(&self, pos: usize) -> usize {
        debug_assert!(pos < self.segments.len());
        let mut s_i = pos + 1;
        let mut len = 1;
        while s_i < self.segments.len() && self.segments[pos] == self.segments[s_i] {
            len += 1;
            s_i += 1;
        }
        len
    }

    pub(crate) fn apply_seg_mods(
        &mut self,
        alphas: &RefCell<HashMap<char, Alpha>>,
        mods: &Modifiers,
        start_pos: usize,
        err_pos: Position,
    ) -> Result<i8, RuleRuntimeError> {
        // check seg length, if long then we must apply mods to all occurences (we assume that we are at the start)
        // debug_assert!(self.in_bounds(start_pos));
        let mut pos = start_pos;
        let mut seg_len = self.get_seg_length_at(pos);
        while seg_len > 0 {
            let seg = self.segments.get_mut(pos).expect("position is in bounds");
            seg.apply_seg_mods(alphas, mods.nodes, mods.feats, err_pos, false)?;
            seg_len -= 1;
            pos += 1;
        }
        // Really, this should be first so that we don't have to needlessly apply mods if we apply -long
        self.apply_supras(alphas, &mods.suprs, start_pos, err_pos)
    }

    pub(crate) fn apply_supras(
        &mut self,
        alphas: &RefCell<HashMap<char, Alpha>>,
        mods: &SupraSegs,
        pos: usize,
        err_pos: Position,
    ) -> Result<i8, RuleRuntimeError> {
        let seg = self.segments[pos];
        let mut seg_len = self.get_seg_length_at(pos);
        let mut len_change = 0;
        match mods.length {
            // [long, Overlong]
            [None, None] => {}
            [None, Some(v)] => {
                if v.as_bool(alphas, err_pos)? {
                    while seg_len < 3 {
                        self.segments.insert(pos, seg);
                        seg_len += 1;
                        len_change += 1;
                    }
                } else {
                    while seg_len > 2 {
                        self.segments.remove(pos);
                        seg_len -= 1;
                        len_change -= 1;
                    }
                }
            }
            [Some(long), None] => {
                if long.as_bool(alphas, err_pos)? {
                    while seg_len < 2 {
                        self.segments.insert(pos, seg);
                        seg_len += 1;
                        len_change += 1;
                    }
                } else {
                    while seg_len > 1 {
                        self.segments.remove(pos);
                        seg_len -= 1;
                        len_change -= 1;
                    }
                }
            }
            [Some(long), Some(vlong)] => match (
                long.as_bool(alphas, err_pos)?,
                vlong.as_bool(alphas, err_pos)?,
            ) {
                (true, true) => {
                    while seg_len < 3 {
                        self.segments.insert(pos, seg);
                        seg_len += 1;
                        len_change += 1;
                    }
                }
                (true, false) => {
                    while seg_len > 2 {
                        self.segments.remove(pos);
                        seg_len -= 1;
                        len_change -= 1;
                    }
                    while seg_len < 2 {
                        self.segments.insert(pos, seg);
                        seg_len += 1;
                        len_change += 1;
                    }
                }
                (false, false) => {
                    while seg_len > 1 {
                        self.segments.remove(pos);
                        seg_len -= 1;
                        len_change -= 1;
                    }
                }
                (false, true) => return Err(RuleRuntimeError::OverlongPosLongNeg(err_pos)),
            },
        }

        self.apply_syll_mods(alphas, mods, err_pos)?;

        Ok(len_change)
    }

    pub(crate) fn apply_syll_mods(
        &mut self,
        alphas: &RefCell<HashMap<char, Alpha>>,
        mods: &SupraSegs,
        err_pos: Position,
    ) -> Result<(), RuleRuntimeError> {
        match mods.stress {
            // [stress, secstress]
            [None, None] => {}
            [None, Some(sec)] => {
                if sec.as_bool(alphas, err_pos)? {
                    self.stress = StressKind::Secondary;
                } else if self.stress == StressKind::Secondary {
                    self.stress = StressKind::Unstressed;
                }
            }
            [Some(prim), None] => {
                if prim.as_bool(alphas, err_pos)? {
                    self.stress = StressKind::Primary;
                } else {
                    self.stress = StressKind::Unstressed;
                }
            }
            [Some(prim), Some(sec)] => match (
                prim.as_bool(alphas, err_pos)?,
                sec.as_bool(alphas, err_pos)?,
            ) {
                (true, true) => self.stress = StressKind::Secondary,
                (true, false) => self.stress = StressKind::Primary,
                (false, false) => self.stress = StressKind::Unstressed,
                (false, true) => return Err(RuleRuntimeError::SecStrPosStrNeg(err_pos)),
            },
        }

        if let Some(t) = &mods.tone {
            self.tone = *t;
        }

        Ok(())
    }
}

impl PartialEq for Syllable {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments && self.stress == other.stress && self.tone == other.tone
    }
}

impl fmt::Display for Syllable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({},{},'{}')",
            self.segments.len(),
            self.stress,
            self.tone
        )
    }
}

#[test]
fn test_seg_indices() {
    use super::*;
    let indices = (&Word::new("ka:r".to_owned(), &[]).unwrap().syllables[0]).get_seg_indices();
    assert_eq!(indices.len(), 3);
    assert_eq!(indices[0], 0);
    assert_eq!(indices[1], 1);
    assert_eq!(indices[2], 3);

    let indices = (&Word::new("ka::r".to_owned(), &[]).unwrap().syllables[0]).get_seg_indices();
    assert_eq!(indices.len(), 3);
    assert_eq!(indices[0], 0);
    assert_eq!(indices[1], 1);
    assert_eq!(indices[2], 4);

    let indices = (&Word::new("k:ar:".to_owned(), &[]).unwrap().syllables[0]).get_seg_indices();
    assert_eq!(indices.len(), 3);
    assert_eq!(indices[0], 0);
    assert_eq!(indices[1], 2);
    assert_eq!(indices[2], 3);
}
