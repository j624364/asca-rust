use   std::fmt;

use crate :: {
    error ::WordSyntaxError, 
    parser::{SegMKind, Modifiers, Supr}, 
    syll  ::{Syllable, StressKind},
    seg   ::Segment,
    CARDINALS_MAP, CARDINALS_TRIE, DIACRITS
};

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum LengthKind {
//     Short,
//     Long,
//     Overlong
// }
//
// impl Default for LengthKind {
//     fn default() -> Self {
//         Self::Short
//     }
// }
//
// impl fmt::Display for LengthKind {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             LengthKind::Short    => write!(f, "-"),
//             LengthKind::Long     => write!(f, ":"),
//             LengthKind::Overlong => write!(f, "::"),
//         }
//     }
// }

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

    pub fn get_segs_in_syll(&self, syll_index: usize) -> &[Segment] {

        assert!(syll_index < self.syllables.len());

        let start = self.syllables[syll_index].start;
        let end = self.syllables[syll_index].end;

        assert!(end >= start);

        &self.segments[start..=end]
    } 

    pub fn get_segs_in_syll_mut(&mut self, syll_index: usize) -> &mut [Segment] {

        assert!(syll_index < self.syllables.len());

        let start = self.syllables[syll_index].start;
        let end = self.syllables[syll_index].end;

        assert!(end >= start);

        &mut self.segments[start..=end]
    } 

    pub fn swap_syll(&mut self, a_index: usize, b_index: usize) {
        // this works, but I hate it
        let a_start = self.syllables[a_index].start;
        let a_end = self.syllables[a_index].end;
        let a_tone = &self.syllables[a_index].tone;
        let a_stress = self.syllables[a_index].stress;
        let b_start = self.syllables[b_index].start;
        let b_end = self.syllables[b_index].end;
        let b_tone = &self.syllables[b_index].tone;
        let b_stress = self.syllables[b_index].stress;


        let mut new_segs = vec![];
        let mut new_syls = vec![];
        for (syll_index, _) in self.syllables.iter().enumerate() {
            let (sg, sy) = 
            if syll_index == a_index { 
                let mut sy = self.syllables[a_index].clone();
                sy.end = a_start + b_end - b_start;
                sy.tone = b_tone.clone();
                sy.stress = b_stress;
                (self.get_segs_in_syll(b_index), sy) 
            } else if syll_index == b_index {
                let mut sy = self.syllables[b_index].clone();
                sy.start = b_end - a_end - a_start;
                sy.tone = a_tone.clone();
                sy.stress = a_stress;
                (self.get_segs_in_syll(a_index), sy)
            } else {
                let (start, end) = if syll_index > 0 {
                    (self.syllables[syll_index - 1].start, self.syllables[syll_index - 1].end)
                } else {
                    (self.syllables[syll_index].start, self.syllables[syll_index].end)
                };
                
                let mut sy = self.syllables[syll_index].clone();
                sy.start = start;
                sy.end = end;
                (self.get_segs_in_syll(syll_index), sy)
            };
            new_segs.extend_from_slice(sg);
            new_syls.push(sy);
        }

        self.segments = new_segs;
        self.syllables = new_syls;

        println!("{:?}", self.syllables)
    }

    /// Finds number of consecutive identical segments ***within a syllable*** starting from the given index.
    /// 
    /// Does not take into account if said index is in the middle of the repetition
    /// # Examples
    /// ``` 
    /// let word = Word::new("aa.a").unwrap();
    /// assert_eq!(word.seg_length_at(0), 2);
    /// assert_eq!(word.seg_length_at(1), 1);
    /// ```
    pub fn seg_length_in_syll(&self, seg_index: usize) -> usize {
        if self.is_syll_final(seg_index) {
            return 1
        }

        let syll_index = self.get_syll_index_from_seg_index(seg_index);
        let mut seg_index = seg_index + 1;
        let mut len = 1;

        // FIXME: Out of Bounds is possible
        while seg_index < self.segments.len()
        && self.get_syll_index_from_seg_index(seg_index) == syll_index 
        && self.get_seg_at(seg_index).unwrap() == self.get_seg_at(seg_index).unwrap() {
            len +=1;
            seg_index += 1;
        }

        len
    }

    
    /// Finds number of consecutive identical segments starting from the given index.
    /// Does not take into account if said index is in the middle of the repetition
    /// # Examples
    /// ``` 
    /// let word = Word::new("aaa").unwrap();
    /// assert_eq!(word.seg_length_at(0), 3);
    /// assert_eq!(word.seg_length_at(1), 2);
    /// ```
    pub fn seg_length_at(&self, seg_index: usize) -> usize {
        if self.is_word_final(seg_index) {
            return 1
        }

        let mut seg_index = seg_index+1;
        let mut len = 1;

        while seg_index < self.segments.len()
        && self.get_seg_at(seg_index).unwrap() == self.get_seg_at(seg_index).unwrap() {
            len +=1;
            seg_index += 1;
        }

        len
    }

    pub fn seg_count(&self) -> usize {
        self.segments.len() - 1
    } 

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

    #[allow(unused)]
    pub fn seg_is_syll_initial(&self, seg_index: usize) -> bool {
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

    #[allow(unused)]
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
                    i+=1;           // NOTE: We could (or maybe should) error here, but we can just skip
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
                    // Is this needed?
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