# ASCA (fork)

The original project can be found [here](https://github.com/Girv98/asca-rust). This was forked for a personal use case.

[ASCA](https://asca.girv.dev) is a Sound Change Applier written in Rust with WASM.

Repo for the web UI is [here](https://github.com/Girv98/asca)

## Notable Features
- Out of the box Distinctive Features and Alpha Notation
- Manipulation of Syllables, Stress, and Tone
- Metathesis and Hyperthesis (Long Range Metathesis)
- Optional/Repeating Segments
- Variables
- Syntax which adheres to conventional standard notation
- Digraph and Diacritic Support
- Romanisation/Deromanisation

User guide can be found [here](./doc/doc.md).

[Changelog](./CHANGELOG.md)

## Changes from original.

The original project can be found [here](https://github.com/Girv98/asca-rust).

- Formatted with `cargo fmt`
- Some slight performance improvements.
- (API) ParsedRules struct added so that repeated calls to run() are less expensive.

