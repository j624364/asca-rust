# ASCA

ASCA is a Sound Change Applier written in Rust with WASM.

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
- [x] Applying Output
    - [x] Metathesis
    - [x] Deletion
    - [ ] Insertion
        - [x] Segment Insertion
        - [ ] Syllable and boundary Insertion (NOT YET PROPERLY TESTED)
    - [x] Substitution
        - [x] Same length Substitution 
        - [x] Longer Input Substitution
        - [x] Longer Output Substitution
- [x] Matching Environment and Exceptions
    - [x] Simple segments
    - [x] Ellipsis and Optional Segments
- [x] WASM Bindgen



## Dev

Install `wasm-pack` and `wasm-bindgen-cli`.

```
wasm-pack build --release --target web --out-dir libasca
```