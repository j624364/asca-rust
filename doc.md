# ASCA Documentation and User Guide
[Defining Sound Changes](#Defining-Sound-Changes) | 
[Basics](#Basics) | [Groupings](#Groupings) | [Metathesis](#Metathesis) | [Gemination](#Gemination) | [Optional Segments](#Optional-Segments) | [Alpha Notation](#Alpha-Notation) | [Variables](#Variables) | [Propagation](#Propagation) |
[Distinctive Features](#Distinctive-Features) | 
[Suprasegmental Features](#Suprasegmental-Features) |
## Defining Sound Changes

### Basica

### Groupings
```
C -> Consonants
O -> Obstruents
S -> Sonorants
L -> Liquids
N -> Nasals
G -> Glides
V -> Vowels
```

### Metathesis
The ampersand operator ```&``` states that the order of the matched input is reversed. So that, for example, a sequence of matched segments ```ABC``` becomes ```CBA```. The operator can be used to flip an arbitrary number of segments.
The output of a metathesis rule must contain *only* ```&``` and nothing else. 

``` asca
Old English R Metathesis (hros => hors)
[+rhotic]V > & / _s
```

An ellipsis ```…``` or double ```..``` or triple dot ```...``` can be used to implement long-range metathesis:

``` asca
Spanish Hyperthesis (Old Spanish parabla => Spanish palabra)
r...l > &       
```

### Gemination

### Optional Segments
Optional Segments are declared as ```(S, M:N)``` where: 
```
S = the segment(s) to be repeated
M = the minimum number of iterations (optional, default = 0)
N = the maximum number of iterations (inclusive). N must be greater than or equal to M.
```
For example, ```(C,5)_```  matches up to 5 consonants preceding the target. This will target environments of CCCCC_, CCCC_,  CCC_, CC_, C_, and  _.

```(C,3:5)``` matches CCC_, CCCC_, CCCCC_.

```(C,0)_``` matches any number of consonants preceding the target. This is equal to regex’s Zero-Or-More operator (*)

```(C)_``` matches zero or one consonant preceding the target, this is the same as (C,1)_ or (C,0:1)

### Alpha Notation

### Variables
Variables are

Variables are declared by using the ```=``` operator, followed by a number. This number can then be used later in the rule to invoke the variable.

Using variables, we can implement metathesis without need of the ```&``` operator.
```
Old English R metathesis (hros > hors)
[+rho]=1 V=2 > 2 1  / _s

Latin to Spanish Metathesis (parabola > palabra)
r=1…l=2 > 2 1
```

It can also be used to define a simple haplology rule.
```
%=1 > * / 1_
"A syllable is deleted if preceded by an identical syllable"

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

## Suprasegmental Features

### Stress

### Length

### Tone

## Saving and Loading Files