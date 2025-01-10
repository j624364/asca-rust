```peg

ALIAS   ←   INTO / FROM

INTO    ←   RPL_STR ARR SEG+ EOL

FROM    ←   INPUT ARR REPLACE EOL

REPLACE ←   RPL_TRM ( ',' RPL_TRM )* ','?
RPL_TRM ←   EMP / RPL_STR
RPL_STR ←   [Alphabetic-Unicode-Char]+

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