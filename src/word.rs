use   std::fmt;
use serde::Deserialize;

use crate :: {
    lexer ::{FType, NodeType}, 
    error ::WordSyntaxError, 
    parser::{SegMKind, BinMod, Modifiers, Supr}, 
    CARDINALS_MAP, CARDINALS_TRIE, 
    CARDINALS_VEC, DIACRITS
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

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum LengthKind {
//     Short,
//     Long,
//     Overlong
// }

// impl Default for LengthKind {
//     fn default() -> Self {
//         Self::Short
//     }
// }

// impl fmt::Display for LengthKind {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             LengthKind::Short    => write!(f, "-"),
//             LengthKind::Long     => write!(f, ":"),
//             LengthKind::Overlong => write!(f, "::"),
//         }
//     }
// }

#[derive(Clone, PartialEq, Eq)]
pub struct DiaMods {
    pub nodes: [Option<SegMKind>; NodeType::Pharyngeal as usize + 1],
    pub feats: [Option<SegMKind>; FType::RetractedTongueRoot as usize + 1],
}

impl DiaMods {
    pub fn new() -> Self {
        Self { 
            nodes: [();NodeType::Pharyngeal as usize + 1].map(|_| None), 
            feats: [();FType::RetractedTongueRoot as usize + 1].map(|_| None), 
        }
    }
}

impl fmt::Debug for DiaMods {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let mut nodes = String::new();
        for val in self.nodes.iter() {
            match val {
                Some(v) => match v {
                    SegMKind::Binary(b) => match b {
                        BinMod::Negative => nodes.push('-'),
                        BinMod::Positive => nodes.push('+'),
                    },
                    SegMKind::Alpha(am) => match am {
                        crate::parser::AlphaMod::Alpha(a) => nodes.push(*a),
                        crate::parser::AlphaMod::InversAlpha(ia) => nodes.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => nodes.push('0'),
            }
        }

        let mut feats = String::new();
        for val in self.feats.iter() {
            match val {
                Some(v) => match v {
                    SegMKind::Binary(b) => match b {
                        BinMod::Negative => feats.push('-'),
                        BinMod::Positive => feats.push('+'),
                    },
                    SegMKind::Alpha(am) => match am {
                        crate::parser::AlphaMod::Alpha(a) => feats.push(*a),
                        crate::parser::AlphaMod::InversAlpha(ia) => feats.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => feats.push('0'),
            }
        }

        f.debug_struct("DiaMods").field("nodes", &nodes).field("feats", &feats).finish()
    }
}

#[derive(Debug, Clone)]
pub struct Diacritic {
    pub name: String,
    pub diacrit: char,
    pub prereqs: DiaMods,
    pub payload: DiaMods,
}

#[derive(Debug, Copy, Clone)]
pub enum NodeKind {
    Root,
    Manner,
    Laryngeal,
    Place,
    Labial,
    Coronal,
    Dorsal,
    Pharyngeal
}

impl NodeKind {
    pub const fn count() -> usize { 8 }

    pub fn from_usize(value: usize) -> Self {
        use NodeKind::*;

        match value {
            0 => {debug_assert_eq!(value, Root as usize); Root}
            1 => {debug_assert_eq!(value, Manner as usize); Manner}
            2 => {debug_assert_eq!(value, Laryngeal as usize); Laryngeal}
            3 => {debug_assert_eq!(value, Place as usize); Place}
            4 => {debug_assert_eq!(value, Labial as usize); Labial}
            5 => {debug_assert_eq!(value, Coronal as usize); Coronal}
            6 => {debug_assert_eq!(value, Dorsal as usize); Dorsal}
            7 => {debug_assert_eq!(value, Pharyngeal as usize); Pharyngeal},
            _ => unreachable!("\nOut of Range converting `usize` to `NodeType`\nThis is a bug!\n")
        }
    }
}

pub const fn feature_to_node_mask(feat: FType) -> (NodeKind, u8) {
    use FType::*;
    match feat {
        Consonantal         => (NodeKind::Root, 0b100),
        Sonorant            => (NodeKind::Root, 0b010),
        Syllabic            => (NodeKind::Root, 0b001),
        
        Continuant          => (NodeKind::Manner, 0b10000000),
        Approximant         => (NodeKind::Manner, 0b01000000),
        Lateral             => (NodeKind::Manner, 0b00100000),
        Nasal               => (NodeKind::Manner, 0b00010000),
        DelayedRelease      => (NodeKind::Manner, 0b00001000),
        Strident            => (NodeKind::Manner, 0b00000100),
        Rhotic              => (NodeKind::Manner, 0b00000010),
        Click               => (NodeKind::Manner, 0b00000001),
        
        Voice               => (NodeKind::Laryngeal, 0b100),
        SpreadGlottis       => (NodeKind::Laryngeal, 0b010),
        ConstrGlottis       => (NodeKind::Laryngeal, 0b001),
        
        Bilabial            => (NodeKind::Labial, 0b10),
        Round               => (NodeKind::Labial, 0b01),

        Anterior            => (NodeKind::Coronal, 0b10),
        Distributed         => (NodeKind::Coronal, 0b01),

        Front               => (NodeKind::Dorsal, 0b100000),
        Back                => (NodeKind::Dorsal, 0b010000),
        High                => (NodeKind::Dorsal, 0b001000),
        Low                 => (NodeKind::Dorsal, 0b000100),
        Tense               => (NodeKind::Dorsal, 0b000010),
        Reduced             => (NodeKind::Dorsal, 0b000001),

        AdvancedTongueRoot  => (NodeKind::Pharyngeal, 0b10),
        RetractedTongueRoot => (NodeKind::Pharyngeal, 0b01),
    }
}

fn modifier_index_to_node_mask(i: usize) -> (NodeKind, u8) {
    assert!(i < FType::count());
    feature_to_node_mask(FType::from_usize(i))
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Segment {
    pub root      : u8,
    pub manner    : u8,
    pub laryngeal : u8,
    pub labial    : Option<u8>,
    pub coronal   : Option<u8>,
    pub dorsal    : Option<u8>,
    pub pharyngeal: Option<u8>,
}

impl Segment {
    pub fn get_as_grapheme(&self) -> Option<String> {
        // fn match_from_modifiers(seg: &Segment, mods:&DiaMods) -> bool {
        //     for (i, md) in mods.feats.iter().enumerate() {
        //         let positive = match md {
        //             Some(SegMKind::Binary(b)) => match b {
        //                 BinMod::Negative => false,
        //                 BinMod::Positive => true,
        //             }
        //             Some(SegMKind::Alpha(_)) => todo!(),
        //             None => continue,
        //         };
        //         let (node, mask) = modifier_index_to_node_mask(i);
        //         if seg.feat_match(node, mask, positive) {
        //             continue;
        //         }
        //         return false
        //     }
        //     for (i, md) in mods.nodes.iter().enumerate() {
        //         todo!()
        //     }
        //     true
        // }

        // test against all cardinals for a match
        for c_grapheme in CARDINALS_VEC.iter() {
            let x = CARDINALS_MAP.get(c_grapheme).unwrap();
            if *x == *self { return Some(c_grapheme.to_string()) }
        }

        // if no match is found, 
        // loop through again, but this time test cardinal + diacritics

        // Sort by difference (maybe also filter diff > 8 to cut on size)
        // iterate and match starting from smallest difference

        let mut candidates = Vec::new();

        for c_grapheme in CARDINALS_VEC.iter() {
            let seg: Segment = *CARDINALS_MAP.get(c_grapheme).unwrap();
            let diff_count = self.diff_count(&seg);
            if diff_count < 8 {
                candidates.push((seg, c_grapheme, diff_count))
            }
        }

        candidates.sort_by(|(.., a), (.., b) | a.cmp(b));
        
        // for (a,_, b) in &candidates {
        //     eprintln!("{} {}", a.get_as_grapheme().unwrap(), b);
        // }
        // eprintln!("{}", candidates.len());
        

        for (cand_seg , cand_graph, _) in candidates {
            let mut buf_seg = cand_seg;
            let mut buf_str = cand_graph.clone();

            for d in DIACRITS.iter() {
                if self.match_modifiers(&d.prereqs) && self.match_modifiers(&d.payload) {
                    // if c_grapheme == "r" {
                    //     eprintln!("--------");
                    //     eprintln!("{} {} {:?} {:?}", buf_str,  d.diacrit, d.payload, d.payload);
                    //     eprintln!("--------");
                    // }
                    buf_seg.apply_diacritic_payload(&d.payload);

                    if buf_seg == cand_seg {
                        continue;
                    } else {
                        buf_str.push(d.diacrit);
                    }
                }
                if buf_seg == *self { 
                    return Some(buf_str);
                }
            }
        }

        todo!()

        // for c_grapheme in CARDINALS_VEC.iter() {
        //     let x = CARDINALS_MAP.get(c_grapheme).unwrap();
        //     let mut buf_str = c_grapheme.clone();
        //     let mut buf_seg = x.clone();

        //     for d in DIACRITS.iter() {
            
        //         // let mut buf_seg = buffer.1;
        //         if self.match_modifiers(&d.prereqs) && self.match_modifiers(&d.payload) {
        //             // todo!("add diacritic to buffer")

        //             // if c_grapheme == "r" {
        //             //     eprintln!("--------");
        //             //     eprintln!("{} {} {:?} {:?}", buf_str,  d.diacrit, d.payload, d.payload);
        //             //     eprintln!("--------");
        //             // }
        //             buf_seg.apply_diacritic_payload(&d.payload);

        //             if buf_seg == *x {
        //                 continue;
        //             } else {
        //                 buf_str.push(d.diacrit);
        //             }
        //         }

        //         // if c_grapheme == "r" {
        //         //     eprintln!("{} {} {:?} {:?}", buf_str,  d.diacrit, d.payload, d.payload);
        //         // }

                

        //         if buf_seg == *self { 
        //             candidates.push(buf_str.clone());
        //         }
        //     }
            
        // }

        // eprintln!("{:?}\n", candidates);

        // if candidates.is_empty() {
        //     return None
        // } else {
        //     let mut cand_index = 0;
        //     let mut cand_len = 1024; 
        //     for (i, c) in candidates.iter().enumerate() {
        //         let len = c.chars().count();
        //         if len < cand_len {
        //             cand_index = i;
        //             cand_len = len;
        //         }
        //     }

        //     return Some(candidates[cand_index].clone())
        // }


    }

    #[allow(unused)]
    pub fn match_modifiers(&self, mods: &DiaMods) -> bool {
        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i) {
                return false
            }
        }

        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.match_node_mod(m, i) {
                return false
            }
        }

        true
    }

    pub fn match_node_mod(&self, md: &Option<SegMKind>, node_index: usize) -> bool {
        if let Some(kind) = md {
            let node = NodeKind::from_usize(node_index);
            return self.match_node_mod_kind(kind, node)
        }
        true
    }

    pub fn match_node_mod_kind(&self, kind: &SegMKind, node: NodeKind) -> bool {
        match kind {
            SegMKind::Binary(bt) => match bt {
                BinMod::Negative => self.is_node_none(node),
                BinMod::Positive => self.is_node_some(node),
            },
            // NOTE: Alpha's don't make sense here
            // this is a consequence of using SegMKind for everything
            SegMKind::Alpha(_) => unreachable!(),
        }
    }

    pub fn match_feat_mod(&self, md: &Option<SegMKind>, feat_index: usize) -> bool {
        if let Some(kind) = md {
            let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
            return self.match_feat_mod_kind(kind, node, mask)
        }

        true
    }

    pub fn match_feat_mod_kind(&self, kind: &SegMKind, node: NodeKind, mask: u8) -> bool {
        match kind {
            SegMKind::Binary(bt) => match bt {
                BinMod::Negative => self.feat_match(node, mask, false),
                BinMod::Positive => self.feat_match(node, mask, true),
            },
            // NOTE: Alpha's don't make sense here
            // this is a consequence of using SegMKind for everything
            SegMKind::Alpha(_) => unreachable!(),
        }
    }

    fn diff(&self, other: &Segment) -> Segment {

        fn asdf(first: Option<u8>, sec: Option<u8>) -> Option<u8> {
            let Some(a) = first else { return sec};
            let Some(b) = sec else {return first};
            Some(a ^ b)
        }

        Segment { 
            root: self.root ^ other.root,
            manner: self.manner ^ other.manner, 
            laryngeal: self.laryngeal ^ other.laryngeal, 
            labial:  asdf(self.labial, other.labial),
            coronal: asdf(self.coronal, other.coronal), 
            dorsal:  asdf(self.dorsal, other.dorsal), 
            pharyngeal: asdf(self.pharyngeal, other.pharyngeal), 
        }
    }

    fn diff_count(&self, other: &Segment) -> usize {
        let diff = self.diff(other);

          diff.root.count_ones() as usize
        + diff.manner.count_ones() as usize
        + diff.laryngeal.count_ones() as usize
        + diff.labial.unwrap_or(0).count_ones() as usize
        + diff.coronal.unwrap_or(0).count_ones() as usize
        + diff.dorsal.unwrap_or(0).count_ones() as usize
        + diff.pharyngeal.unwrap_or(0).count_ones() as usize
    }

    pub fn get_node(&self, node: NodeKind) -> Option<u8> {
        match node {
            NodeKind::Root       => Some(self.root),
            NodeKind::Manner     => Some(self.manner),
            NodeKind::Laryngeal  => Some(self.laryngeal),
            NodeKind::Labial     => self.labial,
            NodeKind::Coronal    => self.coronal,
            NodeKind::Dorsal     => self.dorsal,
            NodeKind::Pharyngeal => self.pharyngeal,
            NodeKind::Place      => unreachable!(),
        }
    }

    pub fn set_place_nodes(&mut self, vals: [Option<u8>; 4]) {
        for (i, val) in vals.iter().enumerate() {
            self.set_node(NodeKind::from_usize(i+3), *val);
        }
    }

    #[allow(unused)]
    pub fn set_node(&mut self, node: NodeKind, val: Option<u8>) {
        match node {
            NodeKind::Root       => self.root = val.expect("\nRootNode cannot be null\nThis is a bug"),
            NodeKind::Manner     => self.manner = val.expect("\nMannerNode cannot be null\nThis is a bug"),
            NodeKind::Laryngeal  => self.laryngeal = val.expect("\nLaryngealNode cannot be null\nThis is a bug"),
            NodeKind::Labial     => self.labial = val,
            NodeKind::Coronal    => self.coronal = val,
            NodeKind::Dorsal     => self.dorsal = val,
            NodeKind::Pharyngeal => self.pharyngeal = val,
            NodeKind::Place      => unreachable!(),
        }
    }

    #[allow(unused)]
    pub fn get_feat(&self, node: NodeKind, feat: u8) -> Option<u8> {
        Some(self.get_node(node)? & feat)
    }

    #[allow(unused)]
    pub fn set_feat(&mut self, node: NodeKind, feat: u8, to_positive: bool) {

        let n = self.get_node(node).unwrap_or(0u8);

        if to_positive {
            self.set_node(node, Some(n | feat)) 
        } else {
            self.set_node(node, Some(n & !(feat)))
        }
    }

    pub fn feat_match(&self, node: NodeKind, mask: u8, positive: bool) -> bool {
        let Some(n) = self.get_node(node) else {
            return false
        };

        if positive {
            n & mask == mask
        } else {
            n & mask == 0
        }
    }

    pub fn is_node_some(&self, node: NodeKind) -> bool {
        self.get_node(node).is_some()
    }

    pub fn is_node_none(&self, node: NodeKind) -> bool {
        self.get_node(node).is_none()
    }

    #[allow(unused)]
    pub fn node_match(&self, node: NodeKind, match_value: Option<u8>) -> bool {
        let Some(n) = self.get_node(node) else {
            return match_value.is_none()
        };

        let Some(m) = match_value else {return false};

        n == m
    }

    fn apply_diacritic_payload(&mut self, dm:&DiaMods) {
        for (i, m) in dm.nodes.iter().enumerate() {
            if let Some(kind) = m {
                let node = NodeKind::from_usize(i);
                match kind {
                    SegMKind::Binary(b) => match b {
                        BinMod::Negative => self.set_node(node, None),
                        BinMod::Positive => self.set_node(node, Some(0))
                    },
                    SegMKind::Alpha(_) => unreachable!(),
                }
            }
        }

        for (i, m) in dm.feats.iter().enumerate() {
            if let Some(kind) = m {
                let (n,f) = feature_to_node_mask(FType::from_usize(i));
                match kind {
                    SegMKind::Binary(b) => match b {
                        BinMod::Negative => self.set_feat(n, f, false),
                        BinMod::Positive => self.set_feat(n, f, true),
                    },
                    SegMKind::Alpha(_) => unreachable!(),
                }
            }
        }
    }

    fn check_and_apply_diacritic(&mut self, d: &Diacritic) -> Option<()> {
        // check if we meet prereqs 
        if self.match_modifiers(&d.prereqs) {
            self.apply_diacritic_payload(&d.payload);
            return Some(())
        }
        
        None
    }

    #[allow(unused)]
    pub fn apply_feat_mods(&mut self, mods: &Modifiers) {
        // NOTE: This ignores any Supra Mods as the Segment does not control them
        for (i, m) in mods.feats.iter().enumerate() {
            if let Some(kind) = m { 
                let (n, f) = feature_to_node_mask(FType::from_usize(i));
                match kind {
                    SegMKind::Binary(b) => match b {
                        BinMod::Negative => self.set_feat(n, f, false),
                        BinMod::Positive => self.set_feat(n, f, true),
                    },
                    SegMKind::Alpha(_) => unreachable!(),
                }
            }
        }

        for (i, m) in mods.nodes.iter().enumerate() { 
            todo!()
        }

    } 

    // pub fn inv_feat(&mut self, node: NodeKind, feat: u8) {
    //     let n = match self.get_node(&node) {
    //         Some(x) => x,
    //         None => 0u8, // todo: maybe we should just return (or error) in this case?
    //     };
    //     self.set_node(node, Some(n ^ feat))
    // }

    // pub fn inv_node(&mut self, node: NodeKind) {
    //     let n = match self.get_node(&node) {
    //         Some(x) => x,
    //         None => 0u8, // todo: again maybe we should just return/error
    //     };
    //     self.set_node(node, Some(!n))   
    // }
}

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RUT: {:b} ", self.root)?;
        write!(f, "MAN: {:b} ", self.manner)?;
        write!(f, "LAR: {:b} ", self.laryngeal)?;


        match self.labial {
            Some(v) => write!(f, "LAB: {v:b} ")?,
            None    => write!(f, "LAB: - ")?
        }
        match self.coronal {
            Some(v) => write!(f, "COR: {v:b} ")?,
            None    => write!(f, "COR: - ")?
        }
        match self.dorsal {
            Some(v) => write!(f, "DOR: {v:b} ")?,
            None    => write!(f, "DOR: - ")?
        }
        match self.pharyngeal {
            Some(v) => write!(f, "PHR: {v:b} ")?,
            None    => write!(f, "PHR: - ")?
        }

        Ok(())
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

#[derive(Clone)]
pub struct Word {
    pub segments: Vec<Segment>,
    pub syllables: Vec<Syllable>,
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (pos, seg) in self.segments.iter().enumerate() {
            writeln!(f, "{pos} | {seg:?}")?;
        }
        writeln!(f)?;
        for syll in &self.syllables {
            writeln!(f, "{syll}")?;
        }

        Ok(())
    }
}

impl Word {
    pub fn new(text: String) -> Result<Self, WordSyntaxError>  {
        let mut w = Self { segments: Vec::new(), syllables: Vec::new() };
        // let split_txt = w.format_text(txt);
        let t = text.replace('\'', "ˈ")
                            .replace(',',  "ˌ")
                            .replace('g',  "ɡ")
                            .replace(':',  "ː")
                            .replace(';',  "ː.")
                            .replace('ǝ',  "ə");
        w.setup(t)?;

        Ok(w)
    }
    // TODO: `render` probably isn't the right verb here
    #[allow(unused)]
    pub fn render_only_segments(&self) -> Option<String> {
        let mut buffer = String::new();
        for seg in self.segments.iter() {
            buffer.push_str(&seg.get_as_grapheme()?);
        }
        Some(buffer)
    }

    // TODO: This works, but could be improved
    pub fn render(&self) -> Result<String, (String, usize)> {

        let mut buffer = String::new();
        
        'outer: for (i, seg) in self.segments.iter().enumerate() {
            
            for (y, syll) in self.syllables.iter().enumerate() {

                if i == syll.end + 1 {
                    buffer.push_str(&syll.tone);
                    continue;
                }

                if i == syll.start {
                    match syll.stress {
                        StressKind::Primary => buffer.push('ˈ'), 
                        StressKind::Secondary => buffer.push('ˌ'),
                        StressKind::Unstressed =>  if y > 0 { buffer.push('.') },
                    }
                    break;
                }

                if i == self.segments.len()-1 && syll.end == i {
                    if i != 0 && *seg == self.segments[i-1] && !buffer.ends_with('.') && !buffer.ends_with('ˈ') && !buffer.ends_with('ˌ') {
                        buffer.push('ː');
                    } else {
                        let Some(x) = &seg.get_as_grapheme() else { return Err((buffer, i)) };
                        buffer.push_str(x);
                    }
                    buffer.push_str(&syll.tone);
                    break 'outer;
                }
            }

            if i != 0 && *seg == self.segments[i-1] && !buffer.ends_with('.') && !buffer.ends_with('ˈ') && !buffer.ends_with('ˌ') {
                buffer.push('ː');
                continue;
            }
            
            let Some(x) = &seg.get_as_grapheme() else { return Err((buffer, i)) };
            buffer.push_str(x);
            
        }

        Ok(buffer)
    }

    #[allow(unused)]
    pub fn get_segs_in_syll(&self, syll_index: usize) -> Vec<Segment> {

        assert!(syll_index < self.syllables.len());

        let start = self.syllables[syll_index].start;
        let end = self.syllables[syll_index].end;

        assert!(end >= start);

        self.segments[start..=end].to_owned()

    } 

    pub fn seg_length_at(&self, seg_index: usize) -> usize {
        // NOTE: does not take into account if index is in the middle of the repetition
        // E.G. `aaa` len == 2 where as 'aaa' len == 3
        //        ^                      ^
        if self.is_syll_final(seg_index) {
            return 1
        }
        let syll_index = self.get_syll_index_from_seg_index(seg_index);

        let mut i = seg_index+1;
        let mut len = 1;

        while self.get_syll_index_from_seg_index(seg_index) == syll_index 
        && self.get_seg_at(seg_index).unwrap() == self.get_seg_at(i).unwrap() {
            len +=1;
            i += 1;
        }

        len
    }

    pub fn seg_count(&self) -> usize {
        self.segments.len() - 1
    } 

    #[allow(unused)]
    pub fn syll_count(&self) -> usize {
        self.syllables.len() - 1
    } 

    pub fn get_seg_at(&self, seg_index: usize) -> Option<Segment> {
        if seg_index < self.segments.len() {
            Some(self.segments[seg_index])
        } else {
            None
        }
    }

    pub fn get_syll_at(&self, syll_index: usize) -> Option<Syllable> {
        if syll_index < self.syllables.len() {
            Some(self.syllables[syll_index].clone())
        } else {
            None
        }
    }

    pub fn get_syll_index_from_seg_index(&self, seg_index: usize) -> usize {
        
        assert!(seg_index < self.segments.len());

        for (i, syll) in self.syllables.iter().enumerate() {
            if seg_index > syll.end {
                continue;
            }

            return i
        }

        unreachable!();
    }

    #[allow(unused)]
    pub fn is_syll_final(&self, seg_index: usize) -> bool {
        let syll_index = self.get_syll_index_from_seg_index(seg_index);
        seg_index == self.syllables[syll_index].end
    }

    pub fn is_syll_initial(&self, seg_index: usize) -> bool {
        let syll_index = self.get_syll_index_from_seg_index(seg_index);
        seg_index == self.syllables[syll_index].start
    }

    #[allow(unused)]
    pub fn is_word_final(&self, seg_index: usize) -> bool {
        seg_index == self.segments.len() - 1
    }

    #[allow(unused)]
    pub fn is_word_initial(&self, seg_index: usize) -> bool {
        seg_index == 0
    }

    #[allow(unused)]
    pub fn match_mod_at(&self, md: &SegMKind, seg_index: usize) -> bool {
        todo!()
    }

    pub fn match_supra_at(&self, supr: &Supr, seg_index: usize) -> bool {
        todo!()
    }

    #[allow(unused)]
    pub fn apply_mod_at(&mut self, m: &Modifiers, seg_index: usize) {
        todo!()
    } 

    fn setup(&mut self, input_txt: String) -> Result<(), WordSyntaxError> {
        let mut i = 0;
        let txt: Vec<char> = input_txt.chars().collect();

        let mut sy = Syllable::new();

        'outer: while i < txt.len() {

            if txt[i] == 'ˌ' || txt[i] == 'ˈ'  {
                if !self.segments.is_empty() {
                    sy.end = self.segments.len()-1;
                    self.syllables.push(sy.clone());
                }

                sy.start = self.segments.len();
                
                match txt[i] {
                    'ˌ' => sy.stress = StressKind::Secondary,
                    'ˈ' => sy.stress = StressKind::Primary,
                    _ => unreachable!()
                }
                
                i += 1;
                continue;
            }

            if txt[i] == '.' || txt[i].is_ascii_digit() {

                if self.segments.is_empty() || txt[i-1] == '.' || txt[i-1].is_ascii_digit() {
                    i+=1;           // NOTE: We could (or should) error here, but we can just skip
                    continue;
                }

                sy.end = self.segments.len()-1;

                if txt[i].is_ascii_digit() {

                    let mut tone_buffer = "".to_string();

                    while i < txt.len() && txt[i].is_ascii_digit() {
                        tone_buffer.push(txt[i]);
                        i+=1;
                    }

                    sy.tone = tone_buffer;
                }

                self.syllables.push(sy.clone());
                
                // Reset syllable for next pass
                sy.start = self.segments.len();
                sy.stress = StressKind::default();
                sy.tone = String::new();

                i+=1;
                continue;

            }

            if txt[i] == 'ː' {
                if self.segments.is_empty() {
                    return Err(WordSyntaxError::NoSegmentBeforeColon(input_txt, i))
                } 
                self.segments.push(*self.segments.last().unwrap());
                i += 1;
                continue;
            }

            let mut buffer = txt[i].to_string();

            if CARDINALS_TRIE.contains_prefix(buffer.as_str()) {
                i += 1;
                while i < txt.len() {
                    let mut tmp = buffer.clone(); tmp.push(txt[i]);
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push(txt[i]);
                        i += 1;
                        continue;
                    }

                    if txt[i] == '^' {
                        tmp.pop();
                        tmp.push('\u{0361}');
    
                        if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                            buffer.push('\u{0361}');
                            i += 1;
                            continue;
                        }

                    
                        tmp.pop();
                        tmp.push('\u{035C}');
    
                        if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                            buffer.push('\u{035C}');
                            i += 1;
                            continue;
                        }
                    }

                    break;
                }
                let maybe_seg = CARDINALS_MAP.get(&buffer);

                if let Some(seg_stuff) = maybe_seg {
                    self.segments.push(*seg_stuff);
                } else {
                    i-=1;

                    let last_char = buffer.pop();
                    let last_buffer = buffer;

                    let maybe_seg = if last_buffer.is_empty() {
                        CARDINALS_MAP.get(&last_char.expect("buffer is empty").to_string())
                    } else {
                        CARDINALS_MAP.get(&last_buffer)
                    };

                    if let Some(seg) = maybe_seg {
                        self.segments.push(*seg) 
                    } else {
                        return Err(WordSyntaxError::UnknownChar(input_txt, i));
                    }
                }

            } else {
                let c = buffer.chars().next().unwrap();
                for d in DIACRITS.iter() {
                    if c == d.diacrit {
                        match self.segments.last_mut() {
                            Some(s) => if s.check_and_apply_diacritic(d).is_some() {
                                i+=1;
                                continue 'outer;
                            } else {
                                return Err(WordSyntaxError::DiacriticDoesNotMeetPreReqs(input_txt, i))
                            },
                            None => return Err(WordSyntaxError::DiacriticBeforeSegment(input_txt, i))
                        }
                    }
                }
                return Err(WordSyntaxError::UnknownChar(input_txt, i));
            }

        }

        if self.segments.is_empty() {
            return Err(WordSyntaxError::CouldNotParse(input_txt));
        }
        
        sy.end = self.segments.len()-1;

        if sy.end >= sy.start {
            self.syllables.push(sy);
        }


        Ok(())
    }
}



#[cfg(test)]
mod word_tests {

    use super::*;

    #[test]
    fn test_get_grapheme() {
        let s = Word::new("n".to_owned()).unwrap().segments[0];
        assert_eq!(s.get_as_grapheme().unwrap(), "n")
    }

    #[test]
    fn test_render_word() {
        let w = Word::new("ˌna.kiˈsa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˌna.kiˈsa");

        let w = Word::new(",na.ki'sa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˌna.kiˈsa");

        let w = Word::new("ˈna.ki.sa123".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈna.ki.sa123");

        let w = Word::new("aɫ.ɫa:h".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "aɫ.ɫaːh");

        let w = Word::new("aɫ.ɫa;hu".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "aɫ.ɫaː.hu");

        let w = Word::new("ˈɫɫaa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈɫːaː");

        let w = Word::new("ˈt͡saa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈt͡saː");

        let w = Word::new("ˈt^saa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈt͡saː");
    }

    #[test]
    fn test_render_diacritics() {
        // TODO: Need to test other diacritic combinations
        let w = Word::new("ˈmu.ðr̩".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈmu.ðr̩");

        let w = Word::new("ˈpʰiːkʲ".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈpʰiːkʲ");

        let w = Word::new("ˈpʰiːkʲ".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈpʰiːkʲ");
    }

}