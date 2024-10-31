# ASCA Documentation and User Guide
[Defining Sound Changes](#defining-sound-changes) | [The Basics](#the-basics) | [Condensed Rules](#condensed-rules) 
| [Distinctive Features](#distinctive-features) | [Suprasegmental Features](#suprasegmental-features)
| [Groupings](#groupings) | [Metathesis](#metathesis) | [Gemination](#gemination) | [Optional Segments](#optional-segments) | [Alpha Notation](#alpha-notation) | [Variables](#variables) | [Propagation](#propagation) 
 | [Saving and Loading Files](#saving-and-loading-files) | [Limitations](#limitations)
## Defining Sound Changes

### The Basics
ASCA tries to stick to commonly used [formal notation](https://en.wikipedia.org/wiki/Phonological_rule) wherever possible.
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

e.g. ei > ie | c_ (/i/ will flip to be before /e/, except when directly before /c/)
```

A environment can only contain one underline `_`. An empty environment can be omitted:
```
a > e           (/saj/ > /sej/)
a > e / _       (this is equivalent to the above)
a > e / __      (this is invalid)
```

### IPA Characters

ASCA recognises over 250 base IPA phones which can be modified with any of 28 diacritics, creating thousands of variants.

Types of phones include:
- Clicks (velar and uvular)
- Ejectives & Implosives
- Voiceless, Creaky, Breathy Phonation
- Syllabic Consonants
- Affricates
- Advanced & Retracted Tongue Root
- Labialised, Glottalised, Velarised, Palatalised, Pharyngealised (etc.) Segments

ASCA supports digraphs; where two characters are joined by `◌͡◌`or `◌͜◌` or alternatively `^`. I.e. `d͡ʒ` can be represented as `d^ʒ`.

Clicks are not separated by a joining character. I.e. `kʘ` not `k^ʘ`.

A full list of supported base phones and diacritics can be found [here](https://bit.ly/3sHjqvA).

### Special Characters

`%` represents a syllable

`$` represents a syllable boundary

`#` represents a word boundary

### Insertion and Deletion Rules

Unlike SCA², the input and output cannot be omitted. Insertion and deletion are marked by the `*` operator.
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

Any elements past the comma are mirrored such that:
```
_,ABC => ABC_CBA
```


## Distinctive Features
ASCA allows for 24 segmental features.  
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
│        │      lateral      │  lat. and lateralised cons. │             -              │
│ MANNER │       nasal       │  nasals, nasalised vowels,  │ oral consonants and vowels │
│        │                   │      prenasalised stops     │                            │
│        │  delayed release  │     affricate consonants    │       Plosives, etc.       │
│        │     strident      │    f, v, s, z, ʃ, ʒ etc.    │   ɸ, β, θ, ð, ç, ʝ, etc.   │
│        │      rhotic       │        trills, flaps,       │             -              │
│        │                   │ rhoticised vowels and cons. │             -              │
│        │       click       │       click consonants      │             _              │
├────────┼───────────────────┼─────────────────────────────┼────────────────────────────┤
│        │       voice       │       voiced segments       │     voiceless segments     │
│ LARYNG │   spread glottis  │      aspirates, breathy     │             -              │
│        │   const glottis   │    ejectives, implosives    │             -              │
├────────┼─────────┬─────────┼─────────────────────────────┼────────────────────────────┤
│        │ LABIAL  │  bilab  │       p, b, f, v, etc.      │             -              │
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
│        │         │ reduced │            schwa            │             -              │
│        ├─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
│        │ PHARYNG │   atr   │                             │                            │
│        │         │   rtr   │         pharyngeals         │        Epiglottals         │
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
bʰ, dʰ, gʰ, gʷʰ > b, d, g, gʷ

Using Distinctive Features:
[+cons, -son, -cont, -voice] > [+cont]
[+cons, -son, -cont, +voice, -sg] > [-voice]
[+cons, +voice, +sg] > [-sg]
```

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
ASCA does not currently support tone diacritics or tone letters. Tone instead is represented by numbers following the syllable. As of yet, there are no rules regarding the meaning or syntax of these numbers; However, for demonstration we will follow the [Chinese convention](https://en.wikipedia.org/wiki/Tone_letter#Numerical_values), using numbers from 1 (lowest pitch) to 5 (highest pitch). As with stress, either a syllable or a segment can be matched or modified with tone.

The tones of Mandarin would be represented in this system as:
```
mā => `ma55`
má => `ma35`
mǎ => `ma214`
mà => `ma51`
ma => `ma0` or just `ma`
```

Tone also uses a different syntax within matrices. That is, `[tone: X]`, where `X` are the tone numbers  

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
As of yet, tone cannot be used with alpha notation.

## Groupings

Groupings can be used as shorthand to match often used parts of speech.
```
C -> Consonants (obstruents and sonorants)          (equiv. to [-syll])
O -> Obstruents (plosives, fricatives, affricates)  (equiv. to [+cons, -son, -syll])
S -> Sonorants  (nasals and liquids)                (equiv. to [+cons, +son, -syll])
L -> Liquids                                        (equiv. to [+cons, +son, -syll, +approx])
N -> Nasals                                         (equiv. to [+cons, +son, -syll, -approx])
G -> Glides                                         (equiv. to [-cons, +son, -syll ])
V -> Vowels                                         (equiv. to [-cons, +son, +syll])
```
Note that glottalic consontants such as `/h/ and /ʔ/` are considered `[-cons, -son, -syll]` and are therefore not captured by any grouping other than `C`. 

## Sets
Sets are defined between curly brackets `{}` and can contain IPA, Groups, Matrices or Boundaries.

```
p, t, k > b, d, g       (3 Rules)   
{p, t, k} > {b, d, g}   (1 Rule)
```
A set in output if matched to a set in the input must contain the same number of segments.

```
{p, t} > {b, d, g}      (ERROR)
```

## Gemination
Geminating a consonant is as simple as making a vowel long.

```
C > [+long] / V:[-long]_#
(A consonant is geminated at the end of a word, before a short vowel)
```

## Optional Segments
Optional Segments are declared as ```(S, M:N)``` where: 
```
S = the segment(s) to be repeated
M = the minimum number of iterations (optional, default = 0)
N = the maximum number of iterations (inclusive). N must be greater than or equal to M.
```
For example, ```(C,5)_```  matches up to 5 consonants preceding the target. This will lazily target environments of `_`, `C_`, `CC_`, `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,3:5)` matches `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,0)_` matches any number of consonants preceding the target. This is equal to regex’s Lazy Zero-Or-More operator (*?)

`(C)_` matches zero or one consonant preceding the target. This is the same as `(C,1)_` or `(C,0:1)`

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
Rule Example: Turkish Vowel Harmony

V:[+hi] > [αbk, βfr, γrnd] / V:[αbk, βfr, γrnd] (C,0) _ (C) #"
```

Alpha notation is very useful for rules requiring feature assimilation.

```
Rule Example: Nasal Assimilation

[+cons, +nasal] > [α PLACE] / _[+cons, αPLACE] 
(A nasal consonant takes the place of a following consonant i.e. [nk] > [ŋk])
```

### Inversion
Imagine the original two rules were instead as such.
```
[+son] > [-nasal] / [+nasal]_
[+son] > [+nasal] / [-nasal]_
```
To join these with alpha notation, we can invert the output alpha by putting a minus `-` in front.
```
[+son] > [-α nasal] / [α nasal]_
```
This is useful for dissimilation rules.

## Variables
Variables are declared by using the `=` operator, followed by a number. This number can then be used later in the rule to invoke the variable.
Currently; matrices, groups, and syllables can be assigned to a variable.

Using variables, we can implement basic metathesis without need of the `&` operator.
```
Old English R metathesis (hros > hors)
[+rho]=1 V=2 > 2 1  / _s
```

It can also be used to define a simple haplology rule.
```
%=1 > * / 1_ (A syllable is deleted if preceded by an identical syllable)
```
Despite the name, variables cannot be reassigned.

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




## Saving and Loading Files



## Limitations

### Syllable Structure
It is up to you to maintain syllable boundaries.
This can be done by using metathesising, inserting, and deleting $.


For example, imagine a input word of `'si.te`. If we apply the rule `V > * / C_#`, we end up with a floating consonant `'si.t`.

This can be fixed in two ways: 
```
$C > & / _# (the consonant is moved into the first syllable, with the now empty second syllable being deleted)
or
$ > * / _C# (the two syllables are merged by deleting the boundary between them)
```


`Can only check the presence of a syllable boundary, not the absence of one`

`Labiodental vs labial consonants`

`Tone cannot be alpha'd`

`Sequence of identical segments (e.g. VV_) are parsed differently to long segments (e.g. V:[+long]) in before_context`

`Special Environment` 

```
V:[-long] > [+long] > _,ə
ə > * 

Expected:
/sa.ə.o/ > /sa:.o/
Actual:
/sa.ə.o/ > /sa:.o:/
```