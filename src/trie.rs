use std::{fmt::Display, collections::VecDeque};

pub struct Node {
    pub key: Option<char>,
    pub val: Option<String>,
    pub is_terminal: bool,
    pub children: Vec<Node>

}

impl Node {
    pub fn new() -> Self {
        Self {key: None, val: None, is_terminal: false, children: Vec::new() }
    }

    pub fn with_key(c: char) -> Self {
        Node { 
            key: Some(c), 
            val: None,
            is_terminal: false, 
            children: Vec::new()
        }
    }
}


pub struct Trie {
    root: Node,
    elements: usize
}

impl Trie {
    pub fn new() -> Self {
        Self { root: Node::new(), elements: 0 }
    }
    
    pub fn length(&self) -> usize { self.elements + 1 }
    
    pub fn insert(&mut self, s: &str) {
        
        let mut cur = &mut self.root;
        for c in s.chars() {
            match cur.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
                Ok(i) => {
                    cur = &mut cur.children[i];
                }
                Err(i) => {
                    cur.children.insert(i, Node::with_key(c));
                    cur = &mut cur.children[i];
                }
            }
        }
        if !cur.is_terminal {
            cur.is_terminal = true;
            self.elements += 1;
        }
        cur.val.replace(s.to_string());
    }

    pub fn contains(&self, s: &str) -> bool { 
        let mut cur = &self.root;

        for c in s.chars() {
            match cur.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
                Ok(i) => {
                    cur = &cur.children[i];
                }
                Err(_) => {
                    return false;
                }
            }
        }

        cur.is_terminal
    }

    pub fn find(&mut self, s: &str) -> Vec<String> { 
        let mut cur = &self.root;

        for c in s.chars() {
            match cur.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
                Ok(i) => {
                    cur = &cur.children[i];
                }
                Err(_) => {
                    return Vec::new();
                }
            }
        }

        let mut results = Vec::new();
        let mut q = Vec::new();
        q.push(cur);
        while let Some(c) = q.pop() {
            for child in c.children.iter() {
                q.push(&child);
            }

            if c.is_terminal {
                let value = c.val.as_ref().unwrap();
                results.push(value.clone());
            }
        }

        return results;

     }

}

impl Display for Trie {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut q: VecDeque<&Node> = VecDeque::new();
        let root  = &self.root;
        q.push_back(root);

        while !q.is_empty() {
            for _ in 0..q.len() {
                if let Some(node) = q.pop_front() {
                    for c in node.children.iter() {
                        let r = write!(f, "{} ", &c.key.unwrap());
                        if r.is_err() {
                            return r;
                        }
                        if c.children.len() > 0 {
                            q.push_back(&c);
                        }
                    }
                }
            }

            if q.len() > 0 {
                let r = writeln!(f);
                if r.is_err() {
                    return r;
                }
            }
        }

        Ok(())
    }
}
