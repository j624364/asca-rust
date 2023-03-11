use std::fmt;


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
    pub start: usize,
    pub end: usize,
    pub stress: StressKind,
    pub tone: String
}

impl Syllable {
    pub fn new() -> Self {
        Self {start: 0, end: 0, stress: StressKind::default(), tone: String::new()}
    }
}

impl fmt::Display for Syllable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}:{},{},'{}')", self.start, self.end, self.stress, self.tone)
    }
}