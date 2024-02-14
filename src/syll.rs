use std::{collections::VecDeque, fmt};

use crate::Segment;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StressKind {
    Primary,
    Secondary,
    Unstressed
}

impl Default for StressKind {
    fn default() -> Self {
        Self::Unstressed
    }
}

impl fmt::Display for StressKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StressKind::Primary    => write!(f, "P"),
            StressKind::Secondary  => write!(f, "S"),
            StressKind::Unstressed => write!(f, "-"),
        }
    }
}


#[derive(Debug, Clone)]
pub struct Syllable {
    pub segments: VecDeque<Segment>,
    pub stress: StressKind,
    pub tone: String
}

impl Syllable {
    pub fn new() -> Self {
        Self {segments: VecDeque::new(), stress: StressKind::default(), tone: String::new()}
    }
}

impl PartialEq for Syllable {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments && self.stress == other.stress && self.tone == other.tone
    }
}

impl fmt::Display for Syllable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{},'{}')", self.segments.len(), self.stress, self.tone)
    }
}