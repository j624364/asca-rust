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
        
        // LabialPlace => ("LAB", 0b10),
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
        
        let mut split_str : Vec<String> = self.text.split_inclusive(&['ˈ', 'ˌ', '.']).map(|f| f.to_string()).collect();

        println!("{:?}", split_str);

        // let mut x: Vec<String> = Vec::new();
        // for mut i in s {
        //     if i.chars().count() > 1 && ( i.ends_with("ˈ") || i.ends_with("ˌ")) {
        //         let q = i.pop().unwrap().to_string();
        //         x.push(i);
        //         x.push(q);
        //         continue;
        //     }
        //     if i.ends_with(".") { i.pop(); }
        //     x.push(i);
        // }

        let mut i = 0;

        if split_str[i] == "ˈ" || split_str[i] == "ˌ" {
            self.split_text.push(split_str[i].clone() + split_str[i+1].as_str());

            if self.split_text[0].ends_with("ˈ") || self.split_text[0].ends_with("ˌ")  {
                let chr = self.split_text[0].pop().unwrap().to_string();
                split_str[2] = chr + split_str[2].clone().as_str();
            } else if self.split_text[0].ends_with(".") {self.split_text[0].pop();}

            i += 2;
        }

        while i < split_str.len() {
            if split_str[i].ends_with("ˈ") || split_str[i].ends_with("ˌ") {
                let c = split_str[i].pop().unwrap().to_string();
                self.split_text.push(split_str[i].clone());
                self.split_text.push(c + split_str[i+1].as_str());
                i += 2;
                continue;
            }
            if split_str[i].ends_with(".") { split_str[i].pop(); }

            self.split_text.push(split_str[i].clone());
            i += 1;
        }
        
        println!("{:?}", self.split_text);
    }
}