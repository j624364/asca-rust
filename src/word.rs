use std::collections::HashMap;
use std::{fmt, fmt::Display};

use crate::error::WordSyntaxError;
use crate::lexer::FeatType;
use crate::JSON;
use crate::CARDINALS;

// use regex::Regex;
// use lazy_static::lazy_static;

// match feature {
//     RootNode     | MannerNode  | LaryngealNode | PlaceNode 
//     | LabialNode | CoronalNode | DorsalNode    | PharyngealNode 
//     | Long | Overlong | Stress | Length | Tone => { do x },
//     _ => segment_to_byte(feature)
// }

pub fn feature_to_byte(feat: FeatType) -> (&'static str, u8) {
    use FeatType::*;
    match feat {
        Consonantal    => ("RUT", 0b100),
        Sonorant       => ("RUT", 0b010),
        Syllabic       => ("RUT", 0b001),
        
        Continuant     => ("MAN", 0b10000000),
        Approximant    => ("MAN", 0b01000000),
        Lateral        => ("MAN", 0b00100000),
        Nasal          => ("MAN", 0b00010000),
        DelayedRelease => ("MAN", 0b00001000),
        Strident       => ("MAN", 0b00000100),
        Rhotic         => ("MAN", 0b00000010),
        Click          => ("MAN", 0b00000001),
        
        Voice          => ("LAR", 0b100),
        SpreadGlottis  => ("LAR", 0b010),
        ConstrGlottis  => ("LAR", 0b001),
        
        Bilabial       => ("LAB", 0b10),
        Round          => ("LAB", 0b01),

        Anterior       => ("COR", 0b10),
        Distributed    => ("COR", 0b01),

        Front          => ("DOR", 0b100000),
        Back           => ("DOR", 0b010000),
        High           => ("DOR", 0b001000),
        Low            => ("DOR", 0b000100),
        Tense          => ("DOR", 0b000010),
        Reduced        => ("DOR", 0b000001),

        AdvancedTongueRoot  => ("PHR", 0b10),
        RetractedTongueRoot => ("PHR", 0b01),

        // if Node or SupraSeg
        _ => unreachable!()
    }
}



#[derive(Debug, Clone)]
pub struct Segment {
    grapheme: String,
    matrix: HashMap<String, Option<usize>>
}

impl Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: ", self.grapheme)?;

        match self.matrix.get("root").unwrap() {
            Some(v) => write!(f, "RUT: {} ", v)?,
            None => write!(f, "RUT: - ")?
        }
        match self.matrix.get("manner").unwrap() {
            Some(v) => write!(f, "MAN: {} ", v)?,
            None => write!(f, "MAN: - ")?
        }
        match self.matrix.get("laryngeal").unwrap() {
            Some(v) => write!(f, "LAR: {} ", v)?,
            None => write!(f, "LAR: - ")?
        }
        match self.matrix.get("labial").unwrap() {
            Some(v) => write!(f, "LAB: {} ", v)?,
            None => write!(f, "LAB: - ")?
        }
        match self.matrix.get("coronal").unwrap() {
            Some(v) => write!(f, "COR: {} ", v)?,
            None => write!(f, "COR: - ")?
        }
        match self.matrix.get("dorsal").unwrap() {
            Some(v) => write!(f, "DOR: {} ", v)?,
            None => write!(f, "DOR: - ")?
        }
        match self.matrix.get("pharyngeal").unwrap() {
            Some(v) => write!(f, "PHR: {} ", v)?,
            None => write!(f, "PHR: - ")?
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Syllable {
    start: usize,
    end: usize,
    stress: u8,
    tone: String
}

impl Syllable {
    pub fn new() -> Self {
        Self {start: 0, end: 0, stress: 0, tone: String::new()}
    }
}

impl Display for Syllable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let binding = format!("E:{}", self.stress).clone();
        let stress = match self.stress {
            0 => "-",
            1 => "S",
            2 => "P",
            _ => binding.as_str()
        };
        write!(f, "({}:{},{},'{}')", self.start, self.end, &stress, self.tone)
    }
}


#[derive(Debug, Clone)]
pub struct Word {
    segments: Vec<Segment>,
    syllables: Vec<Syllable>

}

impl Display for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for seg in &self.segments {
            writeln!(f, "{}", seg)?;
        }
        writeln!(f, "")?;
        for syll in &self.syllables {
            writeln!(f, "{}", syll)?;
        }

        Ok(())
    }
}

impl Word {
    pub fn new(text: String) -> Result<Self, WordSyntaxError>  {
        let mut w = Self {segments: Vec::new(), syllables: Vec::new() };
        // let split_txt = w.format_text(txt);
        let t = text.replace("'", "ˈ")
                            .replace("g", "ɡ")
                            .replace(":", "ː")
                            .replace("ǝ", "ə");


        w.setup(t)?;

        Ok(w)
    }

    fn setup(&mut self, input_txt: String) -> Result<(),WordSyntaxError> {
        println!("{}", input_txt);
        let mut i = 0;
        let txt: Vec<char> = input_txt.chars().collect();

        let mut start: usize = 0;
        let mut end: usize = 0;
        let mut sy = Syllable::new();

        while i < txt.len() {
            // println!("{:?}", self.segments);
            // println!("{:?}", self.syllables);
            // println!("");
            let c = txt[i];
            if c == 'ˌ' || c == 'ˈ'  {
                if !self.segments.is_empty() {
                    sy.end = self.segments.len()-1;
                    self.syllables.push(sy.clone());
                }

                sy.start = self.segments.len();
                
                match c {
                    'ˌ' => sy.stress = 1,
                    'ˈ' => sy.stress = 2,
                    _ => unreachable!()
                }
                
                i +=1;
                continue;
            }

            if c == '.' || c.is_ascii_digit() {
                if self.segments.is_empty() {
                    panic!();
                }

                sy.end = self.segments.len()-1;

                if c.is_ascii_digit() {

                    let mut tone_buffer = "".to_string();

                    while i < txt.len() && c.is_ascii_digit() {
                        tone_buffer.push(txt[i]);
                        i+=1;
                    }

                    sy.tone = tone_buffer;
                }

                self.syllables.push(sy.clone());
                
                sy.start = self.segments.len();
                sy.stress = 0;
                sy.tone = "".to_string();

                i+=1;
                continue;

            }

            if c == 'ː' {
                if self.segments.len() > 1 {
                    let lst = self.segments.last().unwrap();
                    self.segments.push(lst.clone())
                }
                i+=1;
                continue;
            }

            let mut buffer = c.to_string();

            if CARDINALS.contains_partial(&buffer.as_str()) {
                i +=1;
                while i < txt.len() {
                    let mut tmp = buffer.clone(); tmp.push(txt[i]);
                    if CARDINALS.contains_partial(&tmp.as_str()) {
                        buffer.push(txt[i]);
                        i += 1;
                        continue;
                    }
                    break;
                }
                let maybe_seg = JSON.get(&buffer);

                let seg_stuff = match maybe_seg {
                    Some(s) => Ok(s),
                    None => Err(WordSyntaxError::UnknownChar(buffer.clone()))
                }?.clone();

                self.segments.push(Segment { grapheme: buffer, matrix: seg_stuff });
            } else {
                return Err(WordSyntaxError::UnknownChar(buffer.clone()));
            }

        }

        if self.segments.is_empty() {
            panic!();
        }

        sy.end = self.segments.len()-1;
        self.syllables.push(sy);

        Ok(())
    }

    pub fn _format_text(&self, mut txt: String ) -> Vec<String> {
        txt = txt.replace("'", "ˈ")
                 .replace("g", "ɡ")
                 .replace(":", "ː")
                 .replace("ǝ", "ə");
        
        
        // what we want is to split the string with the delimiters at the start each slice
        // however split_inclusive splits with the delimiters at the end — and there is seamingly no alternative (for some reason) 
        // so this mess is needed for now unless something better comes to me

        let mut split_str : Vec<String> = txt.split_inclusive(&['ˈ', 'ˌ', '.']).map(|f| f.to_string()).collect();
        let mut split_text = Vec::new();
        
        let mut i = 0;
        // if the first character is a delimiter, it will be on it's own as the first element
        // we want to append it to the front of the second element
        if split_str[0] == "ˈ" || split_str[0] == "ˌ" {
            split_str[1] = split_str[0].clone() + split_str[1].clone().as_str();
            i = 1;
        } else if split_str[0] == "." { i = 1 } // this would is malformed, but i think we shouldn't error, and just move on in this case

        // we then go through each element, removing and re-appending the delimiters
        while i < split_str.len() {
            if split_str[i].ends_with("ˈ") || split_str[i].ends_with("ˌ") {
                let chr = split_str[i].pop().unwrap().to_string();
                split_text.push(split_str[i].clone());
                i += 1;
                split_str[i] = chr + split_str[i].as_str();
            }
            
            if split_str[i].ends_with(".") { split_str[i].pop(); }
            split_text.push(split_str[i].clone());
            i += 1;
        }

        // would be malformed, same as above. But as with that, i think we should just ignore it
        let lst = split_text.last().unwrap();
        if lst.ends_with("ˈ") || lst.ends_with("ˌ") {
            let l = split_text.len()-1;
            split_text[l].pop();
        }
        
        split_text

    }
}



#[cfg(test)]
mod word_tests {

    use super::*;


    #[test]
    fn test_text_split() {
        let w = Word::new("na".to_owned()).unwrap();


        assert_eq!(w._format_text("ˌna.kiˈsa".to_owned()), vec!["ˌna", "ki", "ˈsa"]);
        assert_eq!(w._format_text(".na.kiˈsa".to_owned()), vec!["na", "ki", "ˈsa"]);
        assert_eq!(w._format_text("na.kiˈsa.".to_owned()), vec!["na", "ki", "ˈsa"]);
        assert_eq!(w._format_text("na.kiˈsaˌ".to_owned()), vec!["na", "ki", "ˈsa"]);
    }

    // #[test]
    // fn test_asdf() {
    //     let w = Word::new("al.lah".to_owned());
    // }

}