# ASCA Documentation and User Guide
[Defining Words](#defining-words) | [Defining Sound Changes](#defining-sound-changes)
| [Distinctive Features](#distinctive-features) | [Suprasegmental Features](#suprasegmental-features)
| [Groupings](#groupings) | [Sets](#sets) | [Gemination](#gemination) | [Optional Segments](#optional-segments) | [Alpha Notation](#alpha-notation) | [Variables](#variables) | [Propagation](#propagation) | [Considerations](#considerations) 
| [User Interface](#user-interface)

## Defining Words

### IPA Characters

ASCA recognises over 360 base IPA phones which can be modified with any of 30 diacritics. Meaning that most commonly used IPA codepoints are representable, including:
- Clicks (Velar, uvular, and uvular contour)
- Ejectives & Implosives
- Voiceless, Creaky, & Breathy Phonation
- Syllabic Consonants
- Affricates and Pre-nasalised stops
- Advanced & Retracted Tongue Root
- Labialised, Glottalised, Velarised, Palatalised, Pharyngealised (etc.) Segments

ASCA supports digraphs; where two characters are joined by a tie (`◌͡◌`or `◌͜◌`) or, if not available, a caret `^` (i.e. `d͡ʒ` can be represented as `d^ʒ`). The tie/caret is not optional, `dʒ` is considered a sequence of two segments `d` and `ʒ`.

Clicks are preceded by a velar or uvular plosive/nasal, denoting place of rear articulation, voicing, and nasality. These do not need to be joined by a tie as it is implicit (i.e. `ŋʘ` not `ŋ^ʘ` nor `ʘ`).

Doubly articulated stops, such as `ɡ͡b`, are not supported.

In the event that ASCA is unable to render a segment in IPA, `�` will be used in its place.

Unless the diacritic is inherent to the base phone (e.g. `𝼆̬`) then diacritic order does not matter. ASCA also does not currently normalise all UNICODE code points (though many are aliased), so precomposed code points such as `ẽ (U+1EBD)` are invalid. When generating the output word list, ASCA tries to adhere to [PHOIBLE conventions](https://phoible.org/conventions#ordering-of-diacritics-and-modifier-letters) where possible. Meaning that the segments in the output may be in a different order than was input. Additionally, if a base phoneme with combination of diacritics is equal in value to another base phoneme (or can be composed with less diacritics), then it shall be generated as such (i.e. `ɢ̃` will become `ᶰɢ`). 

A full list of supported base phones and diacritics (with their values) can be found [here](https://bit.ly/3sHjqvA).

### Suprasegmentals

#### Syllable Boundary
Syllables are separated by `.`.
A word with no marked boundaries is considered one syllable. There are no rules regarding internal syllable structure.

#### Length
Segment length can be represented by either `ː` or `:`. A segment can be followed by multiple length markers, representing overlong segments. Alternatively, length can be represented by repetition of the segment (i.e. `si:m` can be `siim`). Identical segments that are separated by a syllable boundary are not considered one long segment. If a long segment falls at the end of a syllable, `;` can be used as shorthand to also close the syllable (i.e. `si:.tiŋ` can be `si;tiŋ`).

#### Stress
Primary stress can be represented by either `ˈ` or `'` and secondary stress by either `ˌ` or `,`. These are placed at the start of the syllable. The boundary marker can be omitted if followed by a stressed syllable (i.e. `ə'gəʊ` instead of `ə.'gəʊ`). Note that ejective consonants cannot be marked with a `'` as this will be interpreted as stress. `ʼ` must be used instead `i.e. /pʼ/`.

#### Tone
ASCA does not currently support tone diacritics or tone letters. Tone instead is represented by numbers following the syllable. As of yet, there are no rules regarding the meaning or syntax of these numbers; However, for demonstration we will follow the [Chinese convention](https://en.wikipedia.org/wiki/Tone_letter#Numerical_values), using numbers from 1 (lowest pitch) to 5 (highest pitch). As with stress, either a syllable or a segment can be matched or modified with tone.

The tones of Mandarin would be represented in this system as:
```
mā => `ma55`
má => `ma35`
mǎ => `ma214`
mà => `ma51`
ma => `ma0` or just `ma`
```
Tone is placed at the end of a syllable and therefore automatically closes it. However, you may still mark the boundary for clarity.
```
pu35.jɑʊ̯51.tan55.ɕin55 == pu35jɑʊ̯51tan55ɕin55
```

### Aliases
Some common IPA characters that may be annoying to type without an IPA keyboard have aliases:
```
g => ɡ
? => ʔ
! => ǃ
ǝ => ə
φ => ɸ

(The following cannot be used inside a rule)

S => ʃ
Z => ʒ
C => ɕ
G => ɢ
N => ɴ
B => ʙ
R => ʀ
X => χ (voiceless uvular fricative)
H => ʜ
A => ɐ
E => ɛ
I => ɪ
O => ɔ
U => ʊ
Y => ʏ
```
Aliases are rendered as their target IPA characters in the output.


A few common americanist characters can also be used:
```
ł => ɬ
ñ => ɲ
¢ => t͡s
ƛ => t͡ɬ
λ => d͡ɮ
```
Unlike with regular aliases, if a input word contains americanist characters, the output will be be rendered with these characters.

## Defining Sound Changes

### The Basics
ASCA tries to stick to commonly used [notation](https://en.wikipedia.org/wiki/Phonological_rule) wherever possible. Though, it may differ from other appliers.
In general, a rule is made of 4 parts:
```
input     -> the content to be transformed
output    -> the result of the transformation
context   -> the specificity of the surrounding environment
exception -> the exclusivity of the surrounding environment
```
These blocks are divided by specific separators so that a given rule looks like this:
```
input (=)> output / context (| or //) exception

e.g. ei > ie | c_ (/ei/ changes to /ie/, except when directly after /c/)
```

An environment can only contain one underline `_`. An empty environment can be omitted:
```
a > e           (/saj/ > /sej/)
a > e / _       (this is equivalent to the above)
a > e / __      (this is invalid)
```

### Special Characters

`%` represents a syllable.

`$` represents a syllable boundary.

`#` represents a word boundary.

Word boundaries may only be used in environments, and must only be used once on either periphery.

```
 a > e / #_#    (valid)
 a > e / _s#    (valid, /a.has/ > /a.hes/)

 a > e / _##    (invalid)
 a > e / _#s    (invalid)
```

### Insertion and Deletion Rules

Unlike [SCA²](https://www.zompist.com/sca2.html), the input and output cannot be omitted. Insertion and deletion are marked by the `*` operator.
The input or output must contain *only* this operator to be valid.

```
e > * / #_      (Apheresis: a vowel elides at the beginning of a word)
e > * / _#      (Apocope: a vowel elides at the end of a word)
* > e / #_      (Prothesis: /e/ is inserted at the beginning of a word)
* > e / _#      (Paragoge: /e/ is inserted at the end of a word)
```
You may use the empty set character `∅` instead.

### Metathesis Rules
The ampersand operator ```&``` states that the order of the matched input is reversed. So that, for example, a sequence of matched segments `ABC` becomes `CBA`. The operator can be used to flip an arbitrary number of segments.
Like deletion, the output of a metathesis rule must contain *only* `&` and nothing else. 

```
Old English R Metathesis (hros => hors)
[+rhotic]V > & / _s
```

An ellipsis `…` or double `..` or triple dot `...` can be used to implement long-range metathesis:

```
Spanish Hyperthesis (Old Spanish parabla => Spanish palabra)
r...l > &
```

Note that the ellipsis must match at least one segment, so a word such as `ar.la` would not change under the above rule.

### Condensed Rules
Multiple rules can be condensed into one line. This can be useful when you have two or more sequential rules that share identical inputs, outputs, or environments.

For Example:
```
e > * / #_
e > * / _#
```
can be condensed into:
```
e > * / #_, _#
```
It is important to remember that the rules are still applied sequentially and not at the same time.

### Special Environment

You may often have condensed rules like the one above where the same environs are being matched both before and after the input. As this can be common, there is a shorthand form of `_,` followed by the environment elements in question.

```
e > * / #_, _#
(becomes)
e > * / _,#
```
The before case is always comes first.
Any elements past the comma are mirrored such that:
```
_,ABC => ABC_ , _CBA
```

## Distinctive Features
ASCA allows for 26 segmental features.  
A full table of segments and there values can be found [here](https://bit.ly/3sHjqvA).

```
┌────────┬─────────┬─────────┬─────────────────────────────┬────────────────────────────┐
│  Node  │ SubNode │ Feature │              +              │             -              │
├────────┼─────────┴─────────┼─────────────────────────────┼────────────────────────────┤
│        │    consonantal    │ obstruents, nasals, liquids │ vowels, glides, laryngeals │
│  ROOT  │      sonorant     │      vowels, sonorants      │         obstruents         │
│        │      syllabic     │ vowels, syllabic consonants │     glides, consonants     │
├────────┼───────────────────┼─────────────────────────────┼────────────────────────────┤
│        │    continuant     │  fricatives, approximants,  │    Plosives, affricates,   │
│        │                   │       vowels, trills        │        nasals, flaps       │
│        │    approximant    │   vowels, glides, liquids   │     nasals, obstruents     │
│        │      lateral      │ l-like and lateralised segs │             -              │
│ MANNER │       nasal       │  nasals, nasalised vowels,  │ oral consonants and vowels │
│        │                   │      prenasalised stops     │                            │
│        │  delayed release  │     affricate consonants    │       Plosives, etc.       │
│        │     strident      │    f, v, s, z, ʃ, ʒ etc.    │   ɸ, β, θ, ð, ç, ʝ, etc.   │
│        │      rhotic       │    r-like trills & flaps    │             -              │
│        │                   │ rhoticised vowels and cons. │             -              │
│        │       click       │       click consonants      │             _              │
├────────┼───────────────────┼─────────────────────────────┼────────────────────────────┤
│        │       voice       │       voiced segments       │     voiceless segments     │
│ LARYNG │   spread glottis  │   aspirates, breathy voice  │             -              │
│        │   const glottis   │    ejectives, implosives    │             -              │
│        │                   │         creaky voice        │             -              │
├────────┼─────────┬─────────┼─────────────────────────────┼────────────────────────────┤
│        │ LABIAL  │ labdent │       ɱ, ʋ, f, v, etc.      │      ɸ, β, p, b, etc.      │
│        │         │  round  │       rounded segments      │      p, b, f, v, etc.      │
│        ├─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
│        │ CORONAL │ anterior│      dentals, alveolars     │ post-palatals, retroflexes │
│        │         │ distrib │    palatals, post-palatals  │   alveolars, retroflexes   │
│        ├─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
│        │         │  front  │    palatals, front vowels   │             -              │
│ PLACE  │         │  back   │ velars/uluvars, back vowels │             -              │
│        │ DORSAL  │  high   │     velars, high vowels     │             -              │
│        │         │   low   │   pharyngeals, low vowels   │             -              │
│        │         │  tense  │     tense vowels & cons.    │         lax vowels         │
│        │         │ reduced │    schwa, reduced vowels    │             -              │
│        ├─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
│        │ PHARYNG │   atr   │      advanced root segs     │                            │
│        │         │   rtr   │         pharyngeals         │        epiglottals         │
└────────┴─────────┴─────────┴─────────────────────────────┴────────────────────────────┘
```

```
┌────────┬─────────────┬─────────────┬─────────────┬─────────────┐
│        │ -s.g. -c.g. │ -s.g. +c.g. │ +s.g. -c.g. │ +s.g. +c.g. │
├────────┼─────────────┼─────────────┼─────────────┼─────────────┤
│        │             │ ejectives,  │             │             │
│ -voice │  voiceless  │ glottalised │  aspirated  │     n/a     │
│        │             │  sonorants  │             │             │
├────────┼─────────────┼─────────────┼─────────────┼─────────────┤
│        │             │ implosives, │             │             │
│ +voice │   voiced    │   creaky    │   breathy   │     n/a     │
│        │             │  sonorants  │             │             │
└────────┴─────────────┴─────────────┴─────────────┴─────────────┘
```

### Using Distinctive Features

Distinctive features are defined between square brackets `e.g. [+cons]`. These are called matrices. A matrix can have multiple features, each separated by a comma `e.g. [+cons, -syll]`. 
Whitespace is not important, meaning `[+del.rel.]` is identical to `[ + d e l . r e l . ]`. Many features also have shorthands `e.g. [bk, hi, lo, dr] = [back, high, low, del.rel.]`.

A matrix can be used standalone to represent a segment, or can be used to modify a segment by joining them with a colon `:`.
```
[-cons, +son, +syll] > [+rtr] / q_  (vowels pharyngealise following /q/)
a:[-stress, -long] > ə              (unstressed short /a/ becomes schwa) 

note a[-stress, -long] would match two segments: /a/ followed by a short, unstressed segment 
```

```
Rule Example: Grimm's Law

Simple IPA:
p, t, k, kʷ > ɸ, θ, x, xʷ 
b, d, g, gʷ > p, t, k, kʷ
bʱ, dʱ, gʱ, gʷʱ > b, d, g, gʷ

Using Distinctive Features:
[+cons, -son, -cont, -voice] > [+cont]
[+cons, -son, -cont, +voice, -sg] > [-voice]
[+cons, +voice, +sg] > [-sg]
```
An empty matrix `[]` can be used to match any one segment (similar to a Regex wildcard).

### Node and Subnode features
#### Matching a subnode
SubNodes can be used to match segments by place of articulation.
```
[+labial]  -> rounded, labial, and labiodental segments
[+coronal] -> dental, alveolar, retroflex, palatal (etc.) segments
[+dorsal]  -> vowels & velar, uvular, palatal segments
[+phargyn] -> epiglottal/pharyngeal segments, and atr/rtr
[+place]   -> matches all non glottal segments
[-place]   -> glottal segments; h, ɦ, ʔ, etc. 
```
The major nodes Root, Manner, and Largyngeal cannot be positive or negative. See [alpha notation](#alpha-notation) for their use cases.

#### Applying a subnode
In the output block, these features can be used to add or remove a place of articulation:
```
Rule Example: Plosive Debuccalisation

[+cons, -son, -voi] > [-cons, +c.g., -place] ( {p,t,k} > ʔ)
```
When adding a node, all features within the node are set to `-`.

Again; Root, Manner, and Largyngeal cannot be used in this way. Place also cannot be `+place` in this case.

## Suprasegmental Features

### Stress
ASCA allows for a 3-way distinction between primary, secondary, and unstressed syllables.

```
┌──────────────────┬────────────────────────────────┐
│    Stress Type   │            Modifier            │
├──────────────────┼──────────────┬─────────────────┤
│    Unstressed    │  [- stress]  │                 │
├──────────────────┼──────────────┤ [- sec. stress] │
│  Primary Stress  │              │                 │
├──────────────────┤  [+ stress]  ├─────────────────┤
│ Secondary Stress │              │ [+ sec. stress] │
└──────────────────┴──────────────┴─────────────────┘
```

For example, if one wanted to match for syllables with primary stress but exclude secondary stress, `[+stress, -sec. stress]`.

Stress can be used on a whole syllable or on a segment. This allows you to change the stress of a syllable based on segments within it and vice-versa.

```
Rule Example: Latin Stress

% > [+str] / #_#          (If there is only one syllable, it is stressed)
V:[+long] > [+str] / _%#  (A penult syll ending with a long vowel becomes stressed)
V > [+str] / _C%#         (A penult syll ending with a consonant or glide becomes stressed)
% > [+str] / _%:[-str]%#  (If the penult is unstressed, the antepenult becomes stressed)

(Rules 2 and 3 could be condensed into one by matching to the consonant instead of the vowel in rule 3)
V > [+str] / _C%#
(becomes)
C > [+str] / _%#
(therefore)
V:[+long], C > [+str] / _%# (A penult syll ending with either a long vowel or a consonant/glide becomes stressed)
```

```
Rule Example: Germanic Inital Stress Shift

%:[+stress] > [-stress] (All stressed syllables become unstressed)
% > [+stress] / #_      (The syllable at the beginning of the word becomes stressed)
```

### Length
Length also has a 3-way distinction; allowing for the overlong vowels of languages like Estonian and Proto-Germanic.

```
┌──────────────┬────────────────────────────────┐
│    Length    │            Modifier            │
├──────────────┼──────────────┬─────────────────┤
│     Short    │   [- long]   │                 │
├──────────────┼──────────────┤   [-overlong]   │
│     Long     │   [+ long]   │                 │
├──────────────┼──────────────┴─────────────────┤
│   Overlong   │          [+ overlong]          │
└──────────────┴────────────────────────────────┘
```

```
Rule Example: Compensatory Lengthening

V > [+long] / _C#   (A vowel becomes long before a consonant at the end of a word)
C > * / V:[+long]_# (A consonant at the end of a word before the long vowel elides)

(or by using variable substitution)

V=1 C > 1:[+long] / _#
```

### Tone
Tone has a unique syntax within matrices. That is, `[tone: X]`, where `X` are the tone numbers.  
As of yet, tone cannot be used with alpha notation; nor can it be negated.

```
Rule Example: Mandarin 3rd Tone Sandhi

%:[tone: 214] > [tone:35] / _%[tone: 214] 
(3rd tone becomes 2nd tone before another 3rd tone)
```

```
Rule Example: Middle Chinese Tonogenesis

% > [tone: 33]                       (平 and 入)
V > [tone: 35], [tone: 51] / _ʔ, _s  (上 then 去)
ʔ , s > * / _$                       (Phonemicisation)
```

## Groupings

Groupings can be used as shorthand to match often used parts of speech.
```
C -> Consonants (obstruents and sonorants)          (equiv. to [-syll])
O -> Obstruents (plosives, fricatives, affricates)  (equiv. to [+cons, -son, -syll])
S -> Sonorants  (nasals and liquids)                (equiv. to [+cons, +son, -syll])
P -> Plosives                                       (equiv, to [+cons, -son, -syll, -delrel, -cont])
F -> Fricatives                                     (equiv, to [+cons, -son, -syll, -approx, +cont])
L -> Liquids                                        (equiv. to [+cons, +son, -syll, +approx])
N -> Nasals                                         (equiv. to [+cons, +son, -syll, -approx, +nasal])
G -> Glides                                         (equiv. to [-cons, +son, -syll])
V -> Vowels                                         (equiv. to [-cons, +son, +syll])
```
Note that purely glottalic consonants such as `/h/ and /ʔ/` are considered `[-cons, -son, -syll]` and are therefore not captured by any grouping other than `C`. 

## Sets
Sets are defined between curly brackets `{}` and can contain IPA, Groups, Matrices, Syllables, or Boundaries.  
Currently, sets cannot contain sequences (i.e. cannot have `{nd, NC, %%}`).

```
p, t, k > b, d, g       (3 Rules)   
{p, t, k} > {b, d, g}   (1 Rule)
```
A set in the output, if matched to a set in the input, must contain the same number of segments. 
```
{p, t} > {b, d, g}      (ERROR)
```
A set in the input or output cannot contain word boundaries.

## Gemination
Geminating a consonant is as simple as making a vowel long.

```
C > [+long] / V:[-long]_#
(A consonant is geminated at the end of a word, before a short vowel)
```

## Optional Segments
Optional Segments are declared as `(S, M:N)` where: 
```
S = the segment(s) to be repeated
M = the minimum number of iterations (optional, default = 0)
N = the maximum number of iterations (inclusive). N must be greater than or equal to M.
```
For example, ```(C,5)_```  matches up to 5 consonants preceding the target. This will lazily target environments of `_`, `C_`, `CC_`, `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,3:5)` matches `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,0)_` matches any number of consonants preceding the target. This is equal to regex’s Lazy-Zero-Or-More operator (*?)

`(C)_` matches zero or one consonant preceding the target. This is the same as `(C,1)_` or `(C,0:1)_`

`([])_` matches zero or one of *any* segment preceding the target.

## Alpha Notation

Take these two rules:
```
[+son] > [-nasal] / [-nasal]_
[+son] > [+nasal] / [+nasal]_
``` 
Both are identical, except both `nasal` features are positive in one and negative in the other. These rules will also be applied sequentially.
We can replace these with a single rule, which is only applied once, by replacing the +/- with a greek character `α..ω`. If greek characters are not available, latin capitals `A..Z` can be used instead.

```
Rule Example: Malay Nasalisation

[+son] > [α nasal] / [α nasal]_
```
```
Rule Example: Turkish Suffix Vowel Harmony

V:[+hi] > [αbk, βfr, γrnd] / V:[αbk, βfr, γrnd] (C,0) _ (C) #
```

Alphas are processed first in the input, then the context, and lastly the output. Alphas are done left to right within each block. 
Any alpha in the output must be prior set in either the input or context.

### Node and Subnodes

An node alpha carries all the features within it. This can be very useful for assimilation rules.

```
Rule Example: Nasal Assimilation

[+cons, +nasal] > [α PLACE] / _[+cons, αPLACE] 
(A nasal consonant takes the place of a following consonant i.e. [nk] > [ŋk])
```
An alpha assigned to a subnode can be +/- when used on a binary feature. The place node is positive when any subnode is.



### Inversion
Imagine we have two debuccalisation rules, one for plosives and one for fricatives
```
O:[-voi, -cont] > [-cons, -c.g., -place] / _#           (pat > paʔ)
O:[-voi, +cont] > [-cons, +s.g., -place, -strid] / _#   (pas > pah)
```
It would be nice if we were able to join them into one rule. To accomplish this, we can use inversion:
```
O:[-voi, Acont] > [-cons, As.g., -Ac.g., -place, -strid] / _#
(pat, pas > paʔ, pah)
```
When matching an obstruent that is `[-cont]`, the output becomes `[-s.g., +c.g.]`. While when the obstruent is `[+cont]`, the output is `[+s.g., -c.g.]`

This can be used with nodes for conditional clustering:
```
ə$ > * / P:[-nas, αPLACE]_N:[-αPLACE]
(pə.no > pno, pə.mo > pə.mo)
```
In the rule above, plosives and nasals cluster only if they are of a different place of articulation.

## Variables
Variables allow us to invoke a previously matched element. Variables are declared by using the `=` operator, followed by a number. This number can then be used later in the rule to invoke the variable.
Currently; matrices, groups, and syllables can be assigned to a variable.

Using variables, we can implement metathesis without need of the `&` operator.
```
Old English R metathesis (hros > hors)
[+rho]=1 V=2 > 2 1 / _s
```

It can also be used to define a simple haplology rule.
```
%=1 > * / 1_ (A syllable is deleted if preceded by an identical syllable)
```
Despite the name, variables cannot be reassigned. However, they can be modified with a feature matrix.

## Propagation 
As ASCA changes all matching environments in a word sequentially, left-to-right harmonies naturally propagate.


```
V > [α front, β back] > V:[α front, β back]C_	
/sinotehu/ becomes /sinøtehy/, not /sinøtɤhy/
```

To achieve right-to-left propagation, … must be used and the harmonic “trigger” must be fixed (i.e. a vowel at the word boundary). 

```
V > [α front, β back] / _CV:[α front, β back]
/sinotehu/ becomes /sɯnøtɤhu/, note no propagation

V > [α front, β back] / _...V:[α front, β back]#
/sinotehu/ becomes /sɯnotɤhu/, as expected
```

For left-to-right propagation, it may be stylistically justified to do the same, but it will not affect the result.


## Considerations

### Syllable Structure
ASCA does not enforce 'legal' syllables and it is up to you to maintain syllable boundaries.
This can be done by metathesising, inserting, and deleting $.

For example, imagine a input word of `'si.te`. If we apply the rule `V > * / C_#`, we end up with a floating consonant `'si.t`.

This can be fixed in a few ways, including: 
```
$C > & / _# (the consonant is moved into the first syllable, with the now empty second syllable being deleted)
or
$ > * / _C# (the two syllables are merged by deleting the boundary between them)
```

### Syllable Stress
Currently, when a syllable is inserted to the beginning of a word, the added syllable steals the stress/tone of the previously initial syllable.
This is because the current implementation cannot differentiate between it and the scenario of adding a syllable to the end, or middle, of a word. 

Take this copy vowel insertion rule: 
```
* > 1$ / #_CV=1
('de.no > 'e.de.no NOT e'de.no)
```
To fix this, we can use a syllable instead of a boundary and alpha notation to 'save' the stress.
```
* > 1:[-str]%:[Astr] / #_CV:[Astr]=1
```


## User Interface

ASCA organises rules such that you can optionally document your sound changes in situ. In the future, this could lead to the ability to automatically generate a sound change document from your work in ASCA. This also allows you to group a multi-stage sound change (such as a chain shift) together, and clearly demarcate them from the potentially long list of sound changes you may have. These rule blocks can then be collapsed so that you can forget about them once completed.

### Drag and Drop Reordering
This structure allows for the ability to easily reorder rules to test certain orderings without accidentally reordering dependant rules. In ASCA, this is achieved through dragging and dropping the rules into the desired order. This feature currently does not work on mobile, but is as priority for the future.

### Saving and Loading Files

Input words and rules can be saved to desktop and loaded into asca using JSON format:

``` JSON
{
    "words": ["words", "go", "here"],
    "rules": [
        {
            "name": "Rule 1",
            "rule": ["First subrule", "Second subrule"],
            "description": "Does something"
        },
        {
            "name": "Rule 2",
            "rule": ["First subrule", "Second subrule"],
            "description": "Does something"
        }
    ]
}
```
On each run, the current words and rules are saved to local storage. This affords you the ability to quit out and not lose progess.
