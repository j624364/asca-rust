use std::{
    collections::{HashMap, VecDeque}, 
    cell::RefCell, 
    fmt, 
};

use crate :: {
    error :: { RuleRuntimeError, WordSyntaxError },
    lexer :: { FType, NodeType, Position },
    parser:: { BinMod, Modifiers, ModKind },
    rule  :: Alpha,
    seg   :: Segment,
    syll  :: { StressKind, Syllable },
    CARDINALS_MAP, CARDINALS_TRIE, DIACRITS,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct SegPos {
    pub(crate) syll_index: usize,
    pub(crate) seg_index: usize
}

impl fmt::Debug for SegPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.syll_index, self.seg_index)
    }
}

impl SegPos {
    pub(crate) fn new(syll_index: usize, seg_index: usize) -> Self {
        Self { syll_index, seg_index }
    }

    pub(crate) fn reversed(&self, word: &Word) -> Self {
        debug_assert!(word.in_bounds(*self));
        SegPos::new(
            word.syllables.len() - 1 - self.syll_index, 
            word.syllables[self.syll_index].segments.len() - 1 - self.seg_index
        )
    }

    pub(crate) fn increment(&mut self, word: &Word) {
        // NOTE: Does not guarantee that the resulting position is within the bounds of the word
        // debug_assert!(self.syll_index < word.syllables.len(), "error incrementing");

        if self.syll_index >= word.syllables.len() {
            return
        }

        self.seg_index += 1;
        if self.seg_index >= word.syllables[self.syll_index].segments.len() {
            self.seg_index = 0;
            self.syll_index += 1;
        }
    }

    pub(crate) fn decrement(&mut self, word: &Word) {
        // debug_assert!(self.syll_index < word.syllables.len(), "error decrementing");

        if self.syll_index > word.syllables.len() {
            self.syll_index = word.syllables.len() - 1;
            self.seg_index = word.syllables[self.syll_index].segments.len() - 1;
        } else if self.seg_index > 0 {
            self.seg_index -= 1;
        } else if self.syll_index > 0 {
            self.syll_index -= 1;
            self.seg_index = word.syllables[self.syll_index].segments.len() - 1;
        }
        // if 0:0, do nothing
    }

    pub(crate) fn at_word_start(&self) -> bool {
        self.syll_index == 0 && self.seg_index == 0
    }

    pub(crate) fn at_word_end(&self, word: &Word) -> bool {
        self.syll_index == word.syllables.len() - 1 && self.seg_index >= word.syllables[self.syll_index].segments.len() - 1
    }

    pub(crate) fn at_syll_start(&self) -> bool {
        // NOTE: does not account for out_of_bounds
        self.seg_index == 0
    }
    #[allow(unused)]
    pub(crate) fn at_syll_end(&self, word: &Word) -> bool {
        // NOTE: returns false if out_of_bounds
        self.syll_index < word.syllables.len() && self.seg_index >= word.syllables[self.syll_index].segments.len() - 1
    }
}

#[derive(Clone)]
pub(crate) struct Word {
    pub(crate) syllables: Vec<Syllable>,
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, syll) in self.syllables.iter().enumerate() {
            for (j, seg) in syll.segments.iter().enumerate() {
                writeln!(f, "{i}:{j} | {seg:?}")?;
            }
        }
        Ok(())
    }
}

impl Word {
    pub(crate) fn new(text: String) -> Result<Self, WordSyntaxError>  {
        let mut w = Self { syllables: Vec::new() };
        let t = text.replace('\'', "ˈ")
                    .replace(',',  "ˌ")
                    .replace(':',  "ː")
                    .replace(';',  "ː.")
                    .replace('g',  "ɡ")
                    .replace('?',  "ʔ")
                    .replace('!',  "ǃ")
                    .replace('S',  "ʃ")
                    .replace('Z',  "ʒ")
                    .replace('C',  "ɕ")
                    .replace('G',  "ɢ")
                    .replace('N',  "ɴ")
                    .replace('B',  "ʙ")
                    .replace('R',  "ʀ")
                    .replace('X',  "χ")
                    .replace('H',  "ʜ")
                    .replace('A',  "ɐ")
                    .replace('E',  "ɛ")
                    .replace('I',  "ɪ")
                    .replace('O',  "ɔ")
                    .replace('U',  "ʊ")
                    .replace('Y',  "ʏ")
                    .replace('ǝ',  "ə");
        w.setup(t)?;

        Ok(w)
    }

    pub(crate) fn render(&self) -> Result<String, (String, usize)> {
        let mut buffer = String::new();
        for (i, syll) in self.syllables.iter().enumerate() {
            match syll.stress {
                StressKind::Primary => buffer.push('ˈ'), 
                StressKind::Secondary => buffer.push('ˌ'),
                StressKind::Unstressed =>  if i > 0 { buffer.push('.') },
            }
            for (j, seg) in syll.segments.iter().enumerate() {
                if j != 0 && *seg == syll.segments[j-1] {
                    buffer.push('ː');
                } else {
                    let Some(x) = &seg.get_as_grapheme() else { return Err((buffer, i)) };
                    buffer.push_str(x);
                }
            }
            buffer.push_str(&syll.tone);
        }
        Ok(buffer)
    }
    #[allow(unused)]
    pub(crate) fn get_syll_segments(&self, syll_index: usize) -> &VecDeque<Segment> {
        debug_assert!(syll_index < self.syllables.len());
        &self.syllables[syll_index].segments
    } 
    #[allow(unused)]
    pub(crate) fn get_syll_segments_mut(&mut self, syll_index: usize) -> &mut VecDeque<Segment> {
        debug_assert!(syll_index < self.syllables.len());
        &mut self.syllables[syll_index].segments
    } 

    pub(crate) fn remove_syll(&mut self, syll_index: usize) {
        debug_assert!(self.syllables.len() > 1);
        debug_assert!(syll_index < self.syllables.len());
        self.syllables.remove(syll_index);
    }

    pub(crate) fn swap_syll(&mut self, a_index: usize, b_index: usize) {
        debug_assert!(a_index < self.syllables.len());
        debug_assert!(b_index < self.syllables.len());
        self.syllables.swap(a_index, b_index)
    }

    /// Finds number of consecutive identical segments ***within a syllable*** starting from the given index.
    /// <br>
    /// Does not take into account if said index is in the middle of the repetition
    /// # Panics
    /// If SegPos is out of bounds
    pub(crate) fn seg_length_at(&self, seg_index: SegPos) -> usize {
        self.syllables[seg_index.syll_index].get_seg_length_at(seg_index.seg_index)
    }

    // pub(crate) fn find_start_of_long_seg(&self, seg_pos: SegPos) -> SegPos {
    //     if seg_pos.seg_index == 0 {
    //         return seg_pos
    //     }
    //     let syll = &self.syllables[seg_pos.syll_index];
    //     let mut s_i = seg_pos.seg_index - 1;
    //     while s_i > 0 && syll.segments[seg_pos.seg_index] == syll.segments[s_i] {
    //         s_i -= 1;
    //     }
    //     SegPos { syll_index: seg_pos.syll_index, seg_index: s_i }
    // }

    pub(crate) fn in_bounds(&self, seg_pos: SegPos) -> bool {
        seg_pos.syll_index < self.syllables.len() && seg_pos.seg_index < self.syllables[seg_pos.syll_index].segments.len()
    }

    pub(crate) fn out_of_bounds(&self, seg_pos: SegPos) -> bool {
        seg_pos.syll_index >= self.syllables.len() || seg_pos.seg_index >= self.syllables[seg_pos.syll_index].segments.len()
    }

    pub(crate) fn get_seg_at(&self, seg_pos: SegPos) -> Option<Segment> {
        if self.in_bounds(seg_pos) {
            Some(self.syllables[seg_pos.syll_index].segments[seg_pos.seg_index])
        } else {
            None
        }
    }

    // pub(crate) fn first_diff(before: &Word, after: &Word) -> Option<(usize, isize)> {
    //     let mut first_diff = None;
    //     for (i,( bfr, aft)) in before.syllables.iter().zip(after.syllables.iter()).enumerate() {
    //         print!("{}, ", bfr.segments.len());
    //         println!("{}", aft.segments.len());
    //         let diff = aft.segments.len() as isize - bfr.segments.len() as isize;
    //         if diff != 0 {
    //             first_diff = Some((i, diff));
    //             break;
    //         } 
    //     }
    //     match first_diff {
    //         Some(_) => first_diff,
    //         None => {
    //             if before.syllables.len() > after.syllables.len() {
    //                 let i = after.syllables.len() - 1;
    //                 let diff = -(before.syllables[i].segments.len() as isize);
    //                 Some((i, diff))
    //             } else if before.syllables.len() < after.syllables.len() {
    //                 let i = before.syllables.len() - 1;
    //                 let diff = after.syllables[i].segments.len() as isize;
    //                 Some((i, diff))
    //             } else {
    //                 None
    //             } 
    //         },
    //     }
    // }

    fn setup(&mut self, input_txt: String) -> Result<(), WordSyntaxError> {
        let mut i = 0;
        let txt: Vec<char> = input_txt.chars().collect();

        let mut sy = Syllable::new();

        'outer: while i < txt.len() {
            if txt[i] == 'ˌ' || txt[i] == 'ˈ' {
                if !sy.segments.is_empty() {
                    self.syllables.push(sy);
                }

                // Reset for next syllable
                sy = Syllable::new();
                
                // set stress for next syllable
                match txt[i] {
                    'ˌ' => sy.stress = StressKind::Secondary,
                    'ˈ' => sy.stress = StressKind::Primary,
                    _ => unreachable!()
                }

                i+=1;
                continue;
            }

            if txt[i] == '.' || txt[i].is_ascii_digit() {
                if sy.segments.is_empty() {
                    i+=1;           // NOTE: We could (or maybe should) error here, but we can just skip
                    continue;
                }

                if txt[i].is_ascii_digit() {
                    let mut tone_buffer = String::new();
                    while i < txt.len() && txt[i].is_ascii_digit() {
                        tone_buffer.push(txt[i]);
                        i+=1;
                    }
                    i-=1;
                    sy.tone = tone_buffer;                    
                }
                self.syllables.push(sy.clone());
                // Reset syllable for next pass
                sy = Syllable::new();
                i+=1;
                continue;
            }
            if txt[i] == 'ː' {
                if sy.segments.is_empty() {
                    return Err(WordSyntaxError::NoSegmentBeforeColon(input_txt, i))
                }
                sy.segments.push_back(*sy.segments.back().unwrap());
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
                        // if a click consonant
                        if let Some('ʘ' | 'ǀ' | 'ǁ' | 'ǃ'  | '‼' | 'ǂ') = txt.get(i+1) {
                            tmp.pop(); tmp.push(txt[i]);
                            i += 1;
                            continue;
                        }
                        // if a contour click
                        if let Some('q'| 'ɢ' | 'ɴ' | 'χ' | 'ʁ') = txt.get(i+1) {
                            if let Some('ʘ' | 'ǀ' | 'ǁ' | 'ǃ' | '‼' | 'ǂ') = tmp.chars().next() {
                                tmp.pop(); tmp.push(txt[i]);
                                i += 1;
                                continue;
                            }
                        }
                    }

                    break;
                }
                let maybe_seg = CARDINALS_MAP.get(&buffer);
                if let Some(seg_stuff) = maybe_seg {
                    sy.segments.push_back(*seg_stuff);
                } else {
                    // Is this needed?
                    i -= 1;

                    let last_char = buffer.pop();
                    let last_buffer = buffer;

                    let maybe_seg = if last_buffer.is_empty() {
                        CARDINALS_MAP.get(&last_char.expect("buffer should not be empty").to_string())
                    } else {
                        CARDINALS_MAP.get(&last_buffer)
                    };

                    if let Some(seg) = maybe_seg {
                        sy.segments.push_back(*seg) 
                    } else {
                        return Err(WordSyntaxError::UnknownChar(input_txt, i));
                    }
                }
            } else {
                let c = buffer.chars().next().unwrap();
                for d in DIACRITS.iter() {
                    if c == d.diacrit {
                        match sy.segments.back_mut() {
                            Some(s) => match s.check_and_apply_diacritic(d) {
                                Ok(_) => {
                                    i += 1;
                                    continue 'outer;
                                },
                                Err((mod_index, is_node)) => {
                                    if !is_node {
                                        let ft = FType::from_usize(mod_index);
                                        let pos = match d.prereqs.feats[mod_index].unwrap() {
                                            ModKind::Binary(bin_mod) => bin_mod == BinMod::Positive,
                                            _ => unreachable!(),
                                        };
                                        return Err(WordSyntaxError::DiacriticDoesNotMeetPreReqsFeat(input_txt, i, ft.to_string(), pos))
                                    } else {
                                        let nt = NodeType::from_usize(mod_index);
                                        let pos = match d.prereqs.nodes[mod_index].unwrap() {
                                            ModKind::Binary(bin_mod) => bin_mod == BinMod::Positive,
                                            _ => unreachable!(),
                                        };
                                        return Err(WordSyntaxError::DiacriticDoesNotMeetPreReqsNode(input_txt, i, nt.to_string(), pos))
                                    };
                                },
                            },
                            None => return Err(WordSyntaxError::DiacriticBeforeSegment(input_txt, i))
                        }
                    }
                }
                return Err(WordSyntaxError::UnknownChar(input_txt, i));
            }
        }
        if sy.segments.is_empty() {
            if !sy.tone.is_empty() || sy.stress != StressKind::Unstressed {
                if sy.stress == StressKind::Primary {
                    if let Some(syll) = self.syllables.last() {
                        if !syll.segments.is_empty() {
                            return Err(WordSyntaxError::CouldNotParseEjective(input_txt))
                        }
                    }
                }
                return Err(WordSyntaxError::CouldNotParse(input_txt));
            } 
        } else {
            self.syllables.push(sy);
        }
        Ok(())

    }

    pub(crate) fn apply_seg_mods(&mut self, alphas: &RefCell<HashMap<char, Alpha>>, mods: &Modifiers, start_pos: SegPos, err_pos: Position) -> Result<i8, RuleRuntimeError> {
        self.syllables[start_pos.syll_index].apply_seg_mods(alphas, mods, start_pos.seg_index, err_pos)
    }
    
    // This is not efficient in the slightest
    // but it allows us to properly bounds check when matching the before context
    pub(crate) fn reverse(&self) -> Self {
        let mut word = self.clone();
        for syll in &mut word.syllables {
            syll.segments.make_contiguous().reverse();
        }
        word.syllables.reverse();

        word
    }
}

#[cfg(test)]
mod word_tests {

    use crate::ASCAError;

    use super::*;

    #[test]
    fn test_get_grapheme() {
        let s = Word::new("n".to_owned()).unwrap().syllables[0].segments[0];
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

        let w = Word::new("ɴǃa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ɴǃa");

        let w = Word::new("ǃɴa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ǃɴa");
    }

    #[test]
    fn test_render_diacritics() {
        // TODO: Test other diacritic combinations
        let w = Word::new("ˈmu.ðr̩".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈmu.ðr̩");

        let w = Word::new("ˈpʰiːkʲ".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈpʰiːkʲ");

        let w = Word::new("ˈpʰiikʲ".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈpʰiːkʲ");
    }

    #[test]
    fn test_render_aliases() {
        match Word::new("'GAN;CEUN!eB.gRǝ:S.XOI?,HYZ".to_owned()) {
            Ok(w) => assert_eq!(w.render().unwrap(), "ˈɢɐɴː.ɕɛʊɴǃeʙ.ɡʀəːʃ.χɔɪʔˌʜʏʒ"),
            Err(e) => {
                println!("{}", e.format_error(&[]));
                assert!(false);
            }
        } 
    }
}