# ASCA Documentation and User Guide
[Defining Sound Changes](#Defining-Sound-Changes) | 
[Basics](#Basics) | [Examples](#Examples) | [Groupings](#Groupings) | [Metathesis](#Metathesis) | [Gemination](#Gemination) | [Optional Segments](#Optional-Segments) | [Alpha Notation](#Alpha-Notation) | [Variables](#Variables) | [Propagation](#Propagation) |
[Distinctive Features](#Distinctive-Features) | 
[Suprasegmental Features](#Suprasegmental-Features) | [Saving and Loading Files](#saving-and-loading-files)
## Defining Sound Changes

### The Basics

```
input (=)> output / context (| or //) exception
i.e. ei > & | c
```

```
a > e             (sai > sei)
a > e / _         (this is equivalent to the above) 

```

```
V > * / #_ "Apheresis: a vowel elides at the beginning of a word"
V > * / _# "Apocope: a vowel elides at the end of a word"
```

### Examples

Grimm's Law
```
Simple
p, t, k, kʷ > ɸ, θ, x, xʷ 
b, d, g, gʷ > p, t, k, kʷ
bʰ, dʰ, gʰ, gʷʰ > b, d, g, gʷ

Using Distinctive Features
[+cons, -son, -voice, -cont] > [+cont]
[+cons, -son, +voice, -cont] > [-voice]
[+cons, +voice, +sg] > [-sg]
```

Latin Stress
```
Standard Version
V:[+long] > [+stress] / _%# "A penult syll ending with a long vowel becomes stressed"
V > [+stress] / _C%#        "A penult syll ending with a consonant becomes stressed"
% > [+stress] / %:[-str]%#  "If the penult is unstressed, the antepenult becomes stressed"

Condensed Version
V:[+lng], V, % => [+stress] / _%#, _C%#, %:[-str]%#
```

Nasal Assimilation
```
[+cons, +nasal] > [α PLACE] / _[+cons, αPLACE] 
"A nasal consonant takes the place of following consonant i.e. [nk] > [ŋk]"
```

### Groupings

Groupings can be used as shorthand to match often used parts of speech
```
C -> Consonants (equiv. to [-syll])
O -> Obstruents (equiv. to [+cons, -son, -syll])
S -> Sonorants  (equiv. to [+cons, +son, -syll])
L -> Liquids    (equiv. to [+cons, +sin, -syll, +approx])
N -> Nasals     (equiv. to [+cons, +sin, -syll, -approx])
G -> Glides     (equiv. to [-cons, +son, -syll ])
V -> Vowels     (equiv. to [-cons, +son, +syll ])
```

### Metathesis
The ampersand operator ```&``` states that the order of the matched input is reversed. So that, for example, a sequence of matched segments `ABC` becomes `CBA`. The operator can be used to flip an arbitrary number of segments.
The output of a metathesis rule must contain *only* `&` and nothing else. 

```
Old English R Metathesis (hros => hors)
[+rhotic]V > & / _s
```

An ellipsis `…` or double `..` or triple dot `...` can be used to implement long-range metathesis:

```
Spanish Hyperthesis (Old Spanish parabla => Spanish palabra)
r...l > &       
```

### Gemination
Geminating a consonant is as simple as making a vowel long.

```
C > [+long] / V:[-long]_#
(A consonant is geminated at the end of a word, before a short vowel)
```

### Optional Segments
Optional Segments are declared as ```(S, M:N)``` where: 
```
S = the segment(s) to be repeated
M = the minimum number of iterations (optional, default = 0)
N = the maximum number of iterations (inclusive). N must be greater than or equal to M.
```
For example, ```(C,5)_```  matches up to 5 consonants preceding the target. This will lazily target environments of `_`, `C_`, `CC_`, `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,3:5)` matches `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,0)_` matches any number of consonants preceding the target. This is equal to regex’s Lazy Zero-Or-More operator (*?)

`(C)_` matches zero or one consonant preceding the target, this is the same as (C,1)_ or (C,0:1)

### Alpha Notation

```
[+cons, +nasal] > [α PLACE] / _[+cons, αPLACE]
```

### Variables
Variables are

Variables are declared by using the `=` operator, followed by a number. This number can then be used later in the rule to invoke the variable.
Currently, only matrices and groups can be assigned to a variable.

Using variables, we can implement metathesis without need of the `&` operator.
```
Old English R metathesis (hros > hors)
[+rho]=1 V=2 > 2 1  / _s
```

It can also be used to define a simple haplology rule.
```
%=1 > * / 1_ "A syllable is deleted if preceded by an identical syllable"
```

### Propagation 
As ASCA changes all matching environments in a word sequentially,  left-to-right harmonies naturally propagate.


```
V > [α front, β back] > V:[α front, β back]C_	
/sinotehu/ becomes /sinøtehy/, not /sinøtɤhy/
```

To achieve right-to-left propagation, … must be used and the harmonic “trigger” must be fixed (i.e. a vowel at the word boundary). 

```
V > [α front, β back] > _CV:[α front, β back]
/sinotehu/ becomes /sɯnøtɤhu/, note no propagation

V > [α front, β back] > _...V:[α front, β back]#
/sinotehu/ becomes /sɯnotɤhu/, as expected
```

For left-to-right propagation, it may be stylistically justified to do the same, but it will not affect the result.


## Distinctive Features
ASCA allows for 24 segmental features.  
A full table of segments and there values can be found here: `TODO`.

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
│        │       click       │       click consonants      │             -              │
├────────┼───────────────────┼─────────────────────────────┼────────────────────────────┤
│        │       voice       │       voiced segments       │     voiceless segments     │
│ LARYNG │   spread glottis  │      aspirates, breathy     │             -              │
│        │   const glottis   │    ejectives, implosives    │             -              │
├────────┼─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
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

## Suprasegmental Features

### Stress
ASCA allows for a 3-way distinction between primary, secondary, and unstressed syllables

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

For example, if one wants to match for syllables with primary stress but exclude secondary stress, `[+stress, -sec. stress]`.

```

```

Example: Inital Syllable Stress Shift
```
%:[+stress] > [-stress] "All stressed syllables become unstressed"
% > [=stress] / #_      "The syllable at the beginning of the word becomes stressed"
```

### Length

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

Example: Compensatory Lengthening
```
V > [+long] / _C#   "A vowel becomes long before a consonant at the end of a word"
C > * / V:[+long]_# "A consonant at the end of a word before the long vowel elides"
```

### Tone
ASCA does not currently support tone diacritics or tone letters.

```
mā => `ma55`
má => `ma35`
mǎ => `ma214`
mà => `ma51`
ma => `ma0` or just `ma`
```

Example: Mandarin 3rd Tone Sandhi
```
%:[tone: 214] > [tone:35] / _%[tone: 214]
"3rd tone becomes 2nd tone before another 3rd tone"
```

Example: Middle Chinese Tonogenesis
```
% > [tone: 33]                       (平 and 入)
V > [tone: 35], [tone: 51] / _ʔ, _s, (上 then 去)
ʔ , s / _$                           (Phonemicisation)
```

## Saving and Loading Files