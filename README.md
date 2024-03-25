# Logic for ASCA

ASCA is a Sound Change Applier written in Rust.
This repo contains the logic for the applier which will be connected to a web front-end.

## Notable Features
- Out of the box Distinctive Features and Alpha Notation
- Manipulation of Syllables, Stress, and Tone
- Metathesis and Hyperthesis
- Syntax which adheres to conventional standard notation
- Digraph and Diacritic Support


## Progress
- [x] Parsing Words
- [x] Parsing Rules
- [ ] Matching Input (~90% complete)
- [ ] Applying Output
    - [x] Simple Metathesis
    - [x] Simple Deletion
    - [ ] Simple Insertion (in progress)
    - [x] Simple Substitution 
- [ ] Matching Environment (in progress)
- [ ] Applying Exceptions  (in progress)
- [ ] WASM Bindgen