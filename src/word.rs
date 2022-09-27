use std::collections::HashMap;

use crate::lexer::FeatType;

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
        Consonantal    => ("ROOT", 0b100),
        Sonorant       => ("ROOT", 0b010),
        Syllabic       => ("ROOT", 0b001),
        
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

        AdvancedTongueRoot  => ("PHAR", 0b10),
        RetractedTongueRoot => ("PHAR", 0b01),

        // if Node or SupraSeg
        _ => unreachable!()
    }
}



#[derive(Debug, Clone)]
pub struct Segment {
    grapheme: String,
    matrix: HashMap<String, Option<usize>>
}


#[derive(Debug, Clone)]
pub struct Word {
    text: String,
    split_text: Vec<String>,
    segment_list: Vec<String>
}

impl Word {
    pub fn new(txt: String) -> Self {
        let mut w = Self { text: txt, split_text: Vec::new(),segment_list: Vec::new() };
        w.setup_text();

        w
    }

    pub fn setup_text(&mut self ) {
        self.text = self.text.replace("'", "ˈ")
                             .replace("g", "ɡ")
                             .replace(":", "ː")
                             .replace("ǝ", "ə");
        
        
        // what we want is to split the string with the delimiters at the start each slice
        // however split_inclusive splits with the delimiters at the end — and there is seamingly no alternative (for some reason) 
        // so this mess is needed for now unless something better comes to me

        let mut split_str : Vec<String> = self.text.split_inclusive(&['ˈ', 'ˌ', '.']).map(|f| f.to_string()).collect();
        
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
                self.split_text.push(split_str[i].clone());
                i += 1;
                split_str[i] = chr + split_str[i].as_str();
            }
            
            if split_str[i].ends_with(".") { split_str[i].pop(); }
            self.split_text.push(split_str[i].clone());
            i += 1;
        }

        // would be malformed, same as above. But as with that, i think we should just ignore it
        let lst = self.split_text.last().unwrap();
        if lst.ends_with("ˈ") || lst.ends_with("ˌ") {
            let l = self.split_text.len()-1;
            self.split_text[l].pop();
        }
        
        println!("{:?}", self.split_text);
    }
}



#[cfg(test)]
mod word_tests {

    use super::*;


    #[test]
    fn test_text_split() {
        let w1 = Word::new("ˌna.kiˈsa".to_owned());
        let w2 = Word::new(".na.kiˈsa".to_owned());
        let w3 = Word::new("na.kiˈsa.".to_owned());
        let w4 = Word::new("na.kiˈsaˌ".to_owned());

        assert_eq!(w1.split_text, vec!["ˌna", "ki", "ˈsa"]);
        assert_eq!(w2.split_text, vec!["na", "ki", "ˈsa"]);
        assert_eq!(w3.split_text, vec!["na", "ki", "ˈsa"]);
        assert_eq!(w4.split_text, vec!["na", "ki", "ˈsa"]);
    }

}