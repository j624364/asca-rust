use std::collections::HashMap;

// use regex::Regex;
// use lazy_static::lazy_static;


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
        Self { text: txt, split_text: Vec::new(),segment_list: Vec::new() }

    }

    pub fn setup_text(&mut self ) {
        self.text = self.text.replace("'", "ˈ")
                             .replace("g", "ɡ")
                             .replace(":", "ː")
                             .replace("ǝ", "ə");
        // lazy_static! {
        //     static ref SEP: Regex = Regex::new(r"(?=[ˈˌ.])").unwrap();
        // }
        
        // self.pro_text = SEP.split(&self.text).map(|f| f.to_string()).collect();

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