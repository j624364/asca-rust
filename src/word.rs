use   std::fmt;
use serde::Deserialize;

use crate :: {
    lexer::{  FType, NodeType}, 
    error :: WordSyntaxError, 
    parser:: {SegMKind, BinMod, Modifiers}, 
    CARDINALS_MAP, CARDINALS_TRIE, 
    CARDINALS_VEC, DIACRITS
};

// match feature {
//     RootNode     | MannerNode  | LaryngealNode | PlaceNode 
//     | LabialNode | CoronalNode | DorsalNode    | PharyngealNode 
//     | Long | Overlong | Stress | Length | Tone => { do x },
//     _ => segment_to_byte(feature)
// }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LengthKind {
    Short,
    Long,
    Overlong
}

impl Default for LengthKind {
    fn default() -> Self {
        Self::Short
    }
}

impl fmt::Display for LengthKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LengthKind::Short    => write!(f, "-"),
            LengthKind::Long     => write!(f, ":"),
            LengthKind::Overlong => write!(f, "::"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct Diacritic {
    pub name: String,
    pub diacrit: char,
    pub prereqs: DiaMods,
    pub payload: DiaMods,
}

pub enum NodeKind {
    Root,
    Manner,
    Laryngeal,
    Labial,
    Coronal,
    Dorsal,
    Pharyngeal
}

#[allow(unused)]
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

        // if Node or SupraSeg
        _ => unreachable!()
    }
}

fn modifier_index_to_node_mask(i: usize) -> (NodeKind, u8) {

    assert!(i < FType::count());

    // if i <= FeatType::PharyngealNode as usize || i > FeatType::RetractedTongueRoot as usize {
    //     assert!(false);
    // }

    let ft = FType::from_usize(i);

    feature_to_node_mask(ft)
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
    fn get_as_grapheme(&self) -> Option<String> {
        fn match_from_modifiers(seg: &Segment, mods:&DiaMods) -> bool {

            // TODO: deal with mods.nodes

            for (i, md) in mods.feats.iter().enumerate() {

                let positive = match md {
                    Some(SegMKind::Binary(b)) => match b {
                        BinMod::Negative => false,
                        BinMod::Positive => true,
                    }
                    Some(SegMKind::Alpha(_)) => todo!(),
                    None => continue,
                };

                let (node, mask) = modifier_index_to_node_mask(i);
                
                if seg.feat_match(node, mask, positive) {
                    continue;
                }

                return false
            }

            true
        }


        // test against all cardinals for a match
        for c_grapheme in CARDINALS_VEC.iter() {
            let x = CARDINALS_MAP.get(c_grapheme).unwrap();
            if *x == *self { return Some(c_grapheme.to_string()) }

        }

        // if no match is found, 
        // loop through again, but this time test cardinal + diacritics

        for c_grapheme in CARDINALS_VEC.iter() {
            let x = CARDINALS_MAP.get(c_grapheme).unwrap();
            let buffer = (c_grapheme, x);

            for d in DIACRITS.iter() {
                if match_from_modifiers(buffer.1, &d.prereqs) && match_from_modifiers(buffer.1, &d.payload) {
                    todo!("add diacritic to buffer")
                }

                if *buffer.1 == *self { return Some(buffer.0.to_string()) }
            }
            
        }

        

        // Err(RuntimeError::UnknownSegment(*self))
        None
    }

    pub fn match_modifiers(&self, mods: &DiaMods) -> bool {
        todo!()
    }

    pub fn get_node(&self, node: &NodeKind) -> Option<u8> {
        match node {
            NodeKind::Root       => Some(self.root),
            NodeKind::Manner     => Some(self.manner),
            NodeKind::Laryngeal  => Some(self.laryngeal),
            NodeKind::Labial     => self.labial,
            NodeKind::Coronal    => self.coronal,
            NodeKind::Dorsal     => self.dorsal,
            NodeKind::Pharyngeal => self.pharyngeal
        }
    }

    pub fn set_node(&mut self, node: NodeKind, val: Option<u8>) {
        match node {
            NodeKind::Root       => self.root = val.expect("\nRootNode cannot be null\nThis is a bug"),
            NodeKind::Manner     => self.manner = val.expect("\nMannerNode cannot be null\nThis is a bug"),
            NodeKind::Laryngeal  => self.laryngeal = val.expect("\nLaryngealNode cannot be null\nThis is a bug"),
            NodeKind::Labial     => self.labial = val,
            NodeKind::Coronal    => self.coronal = val,
            NodeKind::Dorsal     => self.dorsal = val,
            NodeKind::Pharyngeal => self.pharyngeal = val
        }
    }

    pub fn get_feat(&self, node: NodeKind, feat: u8) -> Option<u8> {
        Some(self.get_node(&node)? & feat)
    }

    pub fn set_feat(&mut self, node: NodeKind, feat: u8, to_positive: bool) {

        let n = self.get_node(&node).unwrap_or(0u8);

        if to_positive {
            self.set_node(node, Some(n | feat)) 
        } else {
            self.set_node(node, Some(n & !(feat)))
        }
    }

    pub fn feat_match(&self, node: NodeKind, mask: u8, positive: bool) -> bool {
        let Some(n) = self.get_node(&node) else {
            return false
        };

        if positive {
            n & mask == mask
        } else {
            n & mask == 0
        }
    }

    pub fn node_match(&self, node: NodeKind, match_value: Option<u8>) -> bool {
        let Some(n) = self.get_node(&node) else {
            return match_value.is_none()
        };

        let Some(m) = match_value else {return false};

        n == m
    }

    fn apply_diacritic(&mut self, d: &Diacritic) {
        // check if we meet prereqs
        if self.match_modifiers(&d.prereqs) {
            // then apply payload
        }
        //else TODO: error
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
        let mut w = Self {segments: Vec::new(), syllables: Vec::new() };
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
    pub fn render_only_segments(&self) -> Option<String> {

        let mut buffer = String::new();
        
        for seg in self.segments.iter() {
            buffer.push_str(seg.get_as_grapheme()?.as_str());
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

    pub fn get_segs_in_syll(&self, syll_index: usize) -> Vec<Segment> {

        assert!(syll_index < self.syllables.len());

        let start = self.syllables[syll_index].start;
        let end = self.syllables[syll_index].end;

        assert!(end >= start);

        self.segments[start..=end].to_owned()

    } 

    pub fn get_seg_at(&self, seg_index: usize) -> Option<Segment> {
        if seg_index < self.segments.len() {
            Some(self.segments[seg_index])
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

            if seg_index < syll.start {
                panic!();
            }

            return i
        }

        unreachable!();
    }

    pub fn is_syll_final(&self, seg_index: usize) -> bool {
        let syll_index = self.get_syll_index_from_seg_index(seg_index);
        syll_index == self.syllables[syll_index].end
    }

    pub fn is_syll_initial(&self, seg_index: usize) -> bool {
        let syll_index = self.get_syll_index_from_seg_index(seg_index);
        syll_index == self.syllables[syll_index].start
    }

    pub fn is_word_final(&self, seg_index: usize) -> bool {
        seg_index == self.segments.len() - 1
    }

    pub fn is_word_initial(&self, seg_index: usize) -> bool {
        seg_index == 0
    }

    pub fn match_modifiers_at(&self, mods: &Modifiers, seg_index: usize) -> bool {
        todo!()
    }

    fn setup(&mut self, input_txt: String) -> Result<(), WordSyntaxError> {
        let mut i = 0;
        let txt: Vec<char> = input_txt.chars().collect();

        let mut sy = Syllable::new();

        while i < txt.len() {

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

            if CARDINALS_TRIE.contains_partial(buffer.as_str()) {
                i += 1;
                while i < txt.len() {
                    let mut tmp = buffer.clone(); tmp.push(txt[i]);
                    if CARDINALS_TRIE.contains_partial(tmp.as_str()) {
                        buffer.push(txt[i]);
                        i += 1;
                        continue;
                    }
                    break;
                }
                let maybe_seg = CARDINALS_MAP.get(&buffer);

                let seg_stuff = *match maybe_seg {
                    Some(s) => Ok(s),
                    None => Err(WordSyntaxError::UnknownChar(input_txt.clone(), i)) 
                }?;

                self.segments.push(seg_stuff);
            } else {
                let c = buffer.chars().next().unwrap();
                for d in DIACRITS.iter() {
                    if  c == d.diacrit {
                        match self.segments.last_mut() {
                            Some(s) => s.apply_diacritic(d),
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

        // let w = Word::new("ˈmu.ðr̩".to_owned()).unwrap(); // TODO: Requires diacritic support
        // assert_eq!(w.render().unwrap(), "ˈmu.ðr̩");

        let w = Word::new("ˈna.ki.sa123".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈna.ki.sa123");

        let w = Word::new("aɫ.ɫa:h".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "aɫ.ɫaːh");

        let w = Word::new("aɫ.ɫa;hu".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "aɫ.ɫaː.hu");

        let w = Word::new("ˈɫɫaa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈɫːaː");
    }

}