```peg

ALIAS   ←   INTO / FROM

INTO    ←   // TODO:  REPLACE ARR TERMS EOL 

FROM    ←   INPUT ARR REPLACE EOL

REPLACE ←   RPL_TRM ( ',' RPL_TRM )* ','?
RPL_TRM ←   EMP / [Alphabetic-Unicode-Char]+

INPUT   ←   INP_TRM ( ',' INP_TRM )* ','?
INP_TRM ←   SBOUND / SEG+

SEG     ←   IPA (':' PARAMS)?

PARAMS  ←   '[' (ARG (',' ARG)*)? ']' 
ARG     ←   ARG_MOD [a-zA-Z]+ / TONE
ARG_MOD ←   '+' / '-'
TONE    ←   [a-zA-Z]+ ':' [0-9]+ 

EMP     ←   '*' / '∅'   
SBOUND  ←   '$'
ARR     ←   ('='/'-')? '>'  
EOL     ←   <End of Line>
```