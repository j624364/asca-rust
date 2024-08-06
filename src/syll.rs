use std     :: { cell::RefCell, collections::{ HashMap, VecDeque }, fmt };
use crate   :: {
    rule    :: Alpha,
    seg     :: Segment, 
    error   :: RuleRuntimeError, 
    parser  :: { BinMod, ModKind, SupraSegs }, 
    subrule :: VarKind, 
};

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

    pub fn apply_mods(&mut self, _alphas: &RefCell<HashMap<char, Alpha>> , /*_vars: &RefCell<HashMap<usize, VarKind>>,*/ mods: &SupraSegs) -> Result<(), RuleRuntimeError>{
        // NOTE: this function ignores mods.length 
        match mods.stress {
            // [stress, secstress]
            [None, None] => {},
            [None, Some(v)] => match v {
                ModKind::Binary(b) => match b {
                    BinMod::Negative => if let StressKind::Secondary = self.stress {
                        self.stress = StressKind::Unstressed
                    },
                    BinMod::Positive => self.stress = StressKind::Secondary,
                },
                ModKind::Alpha(_) => todo!(),
            },
            [Some(v), None] => match v {
                ModKind::Binary(b) => match b {
                    BinMod::Negative => self.stress = StressKind::Unstressed,
                    BinMod::Positive => self.stress = StressKind::Primary,
                },
                ModKind::Alpha(_) => todo!(),
            },
            [Some(p), Some(s)] => match (p,s) {
                (ModKind::Binary(a), ModKind::Binary(b)) => match (a,b) {
                    (BinMod::Negative, BinMod::Negative) => self.stress = StressKind::Unstressed,
                    (BinMod::Negative, BinMod::Positive) => self.stress = StressKind::Secondary, //FIXME: This could be seen as an error? (+secstress is inherently +stress)
                    (BinMod::Positive, BinMod::Negative) => self.stress = StressKind::Primary,
                    (BinMod::Positive, BinMod::Positive) => self.stress = StressKind::Secondary,
                },
                (ModKind::Binary(_), ModKind::Alpha(_)) => todo!(),
                (ModKind::Alpha(_), ModKind::Binary(_)) => todo!(),
                (ModKind::Alpha(_), ModKind::Alpha(_)) => todo!(),
            },
        }

        if let Some(t) = &mods.tone {
            self.tone = t.clone();
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
        write!(f, "({},{},'{}')", self.segments.len(), self.stress, self.tone)
    }
}