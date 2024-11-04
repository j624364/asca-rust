use std::{cell::RefCell, collections::HashMap, fmt};
use serde::Deserialize;

use crate ::{
    error ::RuleRuntimeError, 
    lexer ::{FType, NodeType}, 
    parser::{BinMod, ModKind}, 
    CARDINALS_MAP, CARDINALS_VEC, DIACRITS,
    Alpha, AlphaMod, 
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



#[derive(Debug, Copy, Clone, PartialEq)]
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
            _ => unreachable!("\nOut of Range converting `{value}` to `NodeType`(max: 7) \nThis is a bug!\n")
        }
    }
}

// pub fn test_node_variants() {

//     fn asdf(cache: HashMap<u8, u8>) -> Vec<String> {
//         let mut v: Vec<_> = cache.into_iter().collect();
//         v.sort_by(|x, y| x.0.cmp(&y.0));
//         let width = 8 - v.last().unwrap().0.leading_zeros() as usize;
//         v.iter().map(|(x, v)| format!("{:0width$b} : {}", x, v)).collect()
//     }
//     fn qwer(cache: HashMap<Option<u8>, u8>) -> Vec<String> {
//         let mut v: Vec<_> = cache.into_iter().collect();
//         v.sort_by(|x, y| x.0.cmp(&y.0));
//         let width = 8 - v.last().unwrap().0.unwrap().leading_zeros() as usize;
//         v.iter().map(|(x, v)| {
//             if let Some(q) = x {
//                 format!("{:0width$b} : {}", q, v)
//             } else {
//                 format!("- : {v}")
//             }
//         }).collect()
//     }
    
//     let mut rut_cache = std::collections::HashMap::<u8, u8>::new();
//     let mut man_cache = std::collections::HashMap::<u8, u8>::new();
//     let mut lar_cache = std::collections::HashMap::<u8, u8>::new();
//     let mut lab_cache = std::collections::HashMap::<Option<u8>, u8>::new();
//     let mut cor_cache = std::collections::HashMap::<Option<u8>, u8>::new();
//     let mut dor_cache = std::collections::HashMap::<Option<u8>, u8>::new();
//     let mut phr_cache = std::collections::HashMap::<Option<u8>, u8>::new();

//     let mut count = 0;
    
//     CARDINALS_VEC.iter().for_each(|s| {
//         let seg = CARDINALS_MAP.get(s).unwrap();
        
//         *rut_cache.entry(seg.root).or_default() += 1;
//         *man_cache.entry(seg.manner).or_default() += 1;
//         *lar_cache.entry(seg.laryngeal).or_default() += 1;
//         *lab_cache.entry(seg.labial).or_default() += 1;
//         *cor_cache.entry(seg.coronal).or_default() += 1;
//         *dor_cache.entry(seg.dorsal).or_default() += 1;
//         *phr_cache.entry(seg.pharyngeal).or_default() += 1;
//         count += 1;
//     });

//     println!("rut: {} of {count} - {:#?}", rut_cache.len(), asdf(rut_cache));
//     println!("man: {} of {count} - {:#?}", man_cache.len(), asdf(man_cache));
//     println!("lar: {} of {count} - {:#?}", lar_cache.len(), asdf(lar_cache));
//     println!("lab: {} of {count} - {:#?}", lab_cache.len(), qwer(lab_cache));
//     println!("cor: {} of {count} - {:#?}", cor_cache.len(), qwer(cor_cache));
//     println!("dor: {} of {count} - {:#?}", dor_cache.len(), qwer(dor_cache));
//     println!("phr: {} of {count} - {:#?}", phr_cache.len(), qwer(phr_cache));
// }


// More than 246172 legal variants
// pub fn test_variants() {
//     let mut errs = 0;
//     let mut count = 0;
//
//     for root in 1..=0b111 {
//         for manner in 0..=0b11111111 {
//             for laryngeal in 0..=0b111 {
//                 for l in 0..=0b11 {
//                     for c in 0..=0b11 {
//                         for d in 0..=0b111111 {
//                             for p in 0..=0b11 {
//                                 let seg = Segment { root, manner, laryngeal, labial: Some(l), coronal: Some(c), dorsal: Some(d), pharyngeal: Some(p) };
//                                 count +=1;
//                                 if let Some(g) = seg.get_as_grapheme() {
//                                     if g == "�".to_string() {
//                                         errs += 1;
//                                     }
//                                 } else {
//                                     errs += 1;
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//         println!("{root}/7: {errs} out of {count} ");
//     } 
//     println!("Total {errs} out of {count}");
// }

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

        // test against all cardinal values for a match
        for c_grapheme in CARDINALS_VEC.iter() {
            let x = CARDINALS_MAP.get(c_grapheme).unwrap();
            if *x == *self { return Some(c_grapheme.to_string()) }
        }

        // if no match is found, 
        // loop through again, but this time test cardinal + diacritics

        // Sort by difference (maybe also filter diff > 8 to cut on size)
        // iterate and match starting from smallest difference

        // Because CARDINALS_MAP's order is random, this can lead to the `random discovery` of edge cases
        // TODO(girv): test, test, test

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
                        let before = buf_seg;
                        buf_seg.apply_diacritic_payload(&d.payload);
                        if buf_seg == before {
                            buf_seg = before;
                            continue;
                        }
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

        Some("�".to_string())
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
            // this is a consequence of using ModKind for everything
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
        debug_assert_ne!(node, NodeKind::Place);
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
        debug_assert_ne!(node, NodeKind::Place);
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
                debug_assert_ne!(node, NodeKind::Place);
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

    pub fn apply_seg_mods(&mut self, alphas: &RefCell<HashMap<char, Alpha>> , nodes: [Option<ModKind>; NodeType::count()], feats: [Option<ModKind>; FType::count()]) -> Result<(), RuleRuntimeError>{
        for (i, m) in nodes.iter().enumerate() { 
            let node = NodeKind::from_usize(i);
            if let Some(kind) = m {
                match kind {
                    ModKind::Binary(bm) => match bm {
                        BinMod::Negative => match node {
                            NodeKind::Root | NodeKind::Manner | NodeKind::Laryngeal => todo!("Err: Cannot be set to null"),
                            NodeKind::Place => {},
                            _ => self.set_node(node, None),
                            
                        },
                        BinMod::Positive => match node {
                            NodeKind::Root | NodeKind::Manner | NodeKind::Laryngeal => todo!("Err: Cannot be set to null"),
                            NodeKind::Place => {},
                            _ => {
                                // preserve node if already positive
                                if self.get_node(node).is_none() {
                                    self.set_node(node, Some(0))
                                }
                            },
                        },
                    },
                    ModKind::Alpha(am) => match am {
                        AlphaMod::Alpha(a) => {
                            if let Some(alpha) = alphas.borrow().get(a) {
                                if let Some((n, m)) = alpha.as_node() {
                                    if *n == node {
                                        self.set_node(*n, *m);
                                    } else {
                                        todo!("Err: alpha must be assigned to same node")
                                    }

                                } else if let Some((n, place)) = alpha.as_place() {
                                    if *n == node { 
                                        self.set_node(NodeKind::Labial    , place.lab);
                                        self.set_node(NodeKind::Coronal   , place.cor);
                                        self.set_node(NodeKind::Dorsal    , place.dor);
                                        self.set_node(NodeKind::Pharyngeal, place.phr);
                                    } else {
                                        todo!("Err: alpha must be assigned to same node")
                                    }
                                } else {
                                    todo!("Err: alpha is not a node")
                                }
                            } else {
                                todo!("Err: no alpha set")
                            }
                        },
                        AlphaMod::InvAlpha(_) => todo!("Err: Nodes cannot be inverse alpha"), // I don't think this makes sense for nodes 
                    },
                }
            }
        }
        for (i, m) in feats.iter().enumerate() {
            if let Some(kind) = m { 
                let (n, f) = feature_to_node_mask(FType::from_usize(i));
                match kind {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => self.set_feat(n, f, false),
                        BinMod::Positive => self.set_feat(n, f, true),
                    },
                    ModKind::Alpha(am) => match am {
                        AlphaMod::Alpha(a) => {
                            if let Some(alpha) = alphas.borrow().get(a) {
                                if let Some((&_alp_nk, &_alp_ft, &tp)) = alpha.as_feature() {
                                    // self.set_feat(alp_nk, alp_ft, tp);
                                    self.set_feat(n, f, tp);
                                } else { unreachable!() }
                            } else {
                                todo!("Err: no alpha set")
                            }
                        },
                        AlphaMod::InvAlpha(ia) => {
                            if let Some(alpha) = alphas.borrow().get(ia) {
                                if let Some((&_alp_nk, &_alp_ft, &tp)) = alpha.as_feature() {
                                    self.set_feat(n, f, !tp);
                                }
                            } else {
                                todo!("Err: no alpha set")
                            }
                        },
                    },
                }
            }
        }
        Ok(())
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