# Logic for ASCA

ASCA is a Sound Change Applier written in Rust.
This repo contains the logic for the applier which will be connected to a web front-end.

## Notable Features
- Out of the box Distinctive Features and Alpha Notation
- Manipulation of Syllables, Stress, and Tone
- Metathesis and Hyperthesis
- Optional/Repeating Segments
- Syntax which adheres to conventional standard notation
- Digraph and Diacritic Support


## Progress
- [x] Parsing Words
- [x] Parsing Rules
- [x] Matching Input
    - [x] Simple segments
    - [X] Ellipsis
- [ ] Applying Output
    - [x] Metathesis
    - [x] Deletion
    - [ ] Insertion (borked)
    - [x] Substitution
        - [x] Same length Substitution 
        - [x] Longer Input Substitution
        - [x] Longer Output Substitution
- [x] Matching Environment and Exceptions
    - [x] Simple segments
    - [x] Ellipsis and Optional Segments
- [ ] WASM Bindgen