TBD
==================
Unreleased changes.

0.3.0
==================

Features: 
* Cli: Ability to convert a config tag into a web-asca json file 
* Cli: Ability to generate tab completions for a given shell
* Cli: Print available config tags when no match found

Tweaks:
* Cli: More consistent errors
* Lib: Change wasm function to `run_wasm`
* Lib: Re-export Error module

0.2.1
==================

Bug fixes:
* Fix bug whereby IPA literals with a feature alpha modifier were not correctly matching.
    * For example, `l:[Asyll] > r:[Asyll]` would not match the positive case.