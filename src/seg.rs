use std  ::fmt;
use serde::Deserialize;

use crate ::{
    lexer ::{FType, NodeType}, 
    parser::{ModKind, BinMod, Modifiers}, 
    CARDINALS_VEC, CARDINALS_MAP, DIACRITS
};

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

// fn modifier_index_to_node_mask(i: usize) -> (NodeKind, u8) {
//     assert!(i < FType::count());
//     feature_to_node_mask(FType::from_usize(i))
// }

#[derive(Debug, Clone)]
pub struct Diacritic {
    pub name: String,
    pub diacrit: char,
    pub prereqs: DiaMods,
    pub payload: DiaMods,
}


#[derive(Clone, PartialEq, Eq)]
pub struct DiaMods {
    pub nodes: [Option<ModKind>; NodeType::Pharyngeal as usize + 1],
    pub feats: [Option<ModKind>; FType::RetractedTongueRoot as usize + 1],
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
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => nodes.push('-'),
                        BinMod::Positive => nodes.push('+'),
                    },
                    ModKind::Alpha(am) => match am {
                        crate::parser::AlphaMod::Alpha(a) => nodes.push(*a),
                        crate::parser::AlphaMod::InvAlpha(ia) => nodes.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => nodes.push('0'),
            }
        }

        let mut feats = String::new();
        for val in self.feats.iter() {
            match val {
                Some(v) => match v {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => feats.push('-'),
                        BinMod::Positive => feats.push('+'),
                    },
                    ModKind::Alpha(am) => match am {
                        crate::parser::AlphaMod::Alpha(a) => feats.push(*a),
                        crate::parser::AlphaMod::InvAlpha(ia) => feats.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => feats.push('0'),
            }
        }

        f.debug_struct("DiaMods").field("nodes", &nodes).field("feats", &feats).finish()
    }
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

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        if let Some(grapheme) = self.get_as_grapheme(){
            return write!(f, "{}", grapheme)
        }

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

    pub fn match_node_mod(&self, md: &Option<ModKind>, node_index: usize) -> bool {
        if let Some(kind) = md {
            let node = NodeKind::from_usize(node_index);
            return self.match_node_mod_kind(kind, node)
        }
        true
    }

    pub fn match_node_mod_kind(&self, kind: &ModKind, node: NodeKind) -> bool {
        match kind {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => self.is_node_none(node),
                BinMod::Positive => self.is_node_some(node),
            },
            // NOTE: Alpha's don't make sense here
            // this is a consequence of using SegMKind for everything
            ModKind::Alpha(_) => unreachable!(),
        }
    }

    pub fn match_feat_mod(&self, md: &Option<ModKind>, feat_index: usize) -> bool {
        if let Some(kind) = md {
            let (node, mask) = feature_to_node_mask(FType::from_usize(feat_index));
            return self.match_feat_mod_kind(kind, node, mask)
        }

        true
    }

    pub fn match_feat_mod_kind(&self, kind: &ModKind, node: NodeKind, mask: u8) -> bool {
        match kind {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => self.feat_match(node, mask, false),
                BinMod::Positive => self.feat_match(node, mask, true),
            },
            // NOTE: Alpha's don't make sense here
            // this is a consequence of using SegMKind for everything
            ModKind::Alpha(_) => unreachable!(),
        }
    }

    fn diff(&self, other: &Segment) -> Segment {
        fn dif_option(f: Option<u8>, s: Option<u8>) -> Option<u8> {
            let Some(a) = f else { return s };
            let Some(b) = s else { return f };
            Some(a ^ b)
        }

        Segment { 
            root: self.root ^ other.root,
            manner: self.manner ^ other.manner, 
            laryngeal: self.laryngeal ^ other.laryngeal, 
            labial:  dif_option(self.labial, other.labial),
            coronal: dif_option(self.coronal, other.coronal), 
            dorsal:  dif_option(self.dorsal, other.dorsal), 
            pharyngeal: dif_option(self.pharyngeal, other.pharyngeal), 
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
            NodeKind::Root       => self.root = val.expect("RootNode cannot be null"),
            NodeKind::Manner     => self.manner = val.expect("MannerNode cannot be null"),
            NodeKind::Laryngeal  => self.laryngeal = val.expect("LaryngealNode cannot be null"),
            NodeKind::Labial     => self.labial = val,
            NodeKind::Coronal    => self.coronal = val,
            NodeKind::Dorsal     => self.dorsal = val,
            NodeKind::Pharyngeal => self.pharyngeal = val,
            NodeKind::Place      => unreachable!(),
        }
    }

    pub fn get_feat(&self, node: NodeKind, feat: u8) -> Option<u8> {
        Some(self.get_node(node)? & feat)
    }

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
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => self.set_node(node, None),
                        BinMod::Positive => self.set_node(node, Some(0))
                    },
                    ModKind::Alpha(_) => unreachable!(),
                }
            }
        }

        for (i, m) in dm.feats.iter().enumerate() {
            if let Some(kind) = m {
                let (n,f) = feature_to_node_mask(FType::from_usize(i));
                match kind {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => self.set_feat(n, f, false),
                        BinMod::Positive => self.set_feat(n, f, true),
                    },
                    ModKind::Alpha(_) => unreachable!(),
                }
            }
        }
    }

    pub fn check_and_apply_diacritic(&mut self, d: &Diacritic) -> Option<()> {
        // check if we meet prereqs 
        if self.match_modifiers(&d.prereqs) {
            self.apply_diacritic_payload(&d.payload);
            return Some(())
        }
        None
    }

    #[allow(unused)]
    pub fn apply_seg_mods(&mut self, mods: &Modifiers) {
        // NOTE: This ignores any Supra Mods as the Segment does not control them
        for (i, m) in mods.feats.iter().enumerate() {
            if let Some(kind) = m { 
                let (n, f) = feature_to_node_mask(FType::from_usize(i));
                match kind {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => self.set_feat(n, f, false),
                        BinMod::Positive => self.set_feat(n, f, true),
                    },
                    ModKind::Alpha(_) => unreachable!(),
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
