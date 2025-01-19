Unreleased changes
==================

Performance:
* Reduction in unnecessary allocations

Tweaks:
* Changes to tone
    * Capped at 4 digits
    * Tone of `0` is now equivalent to no tone
    * Improvements to how tone is dealt with when merging syllables

0.4.2
==================

Features:
* Lib: Allow for groups and matrices to be used in alias rules
* Lib: Introduce character addition in alias rules

Fixes:
* Cli: Bug introduced in 0.4.0 whereby tags were not being recognised

Tweaks:
* Lib: Word-initial stress characters are removed when aliasing syllable boundaries

0.4.1
==================

Features:
* Lib: Allow unicode escapes within alias rules
* Lib: Adds several named escapes for common diacritics

0.4.0
==================

Features: 
* Lib: Ability to add custom aliases/romanisation
* Cli: Introduce alias file type
* Cli: Apply alias files to sequences

0.3.1
==================

Features: 
* Lib: Add `ᶻ` diacritic as a voiced alternative to the stridentised diacritic `ˢ`.

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