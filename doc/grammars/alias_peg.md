```peg

ALIAS   ←   INTO / FROM

INTO    ←   REPLACE ARR SEG+ EOL
FROM    ←   INPUT ARR REPLACE EOL

REPLACE ←   RPL_TRM ( ',' RPL_TRM )* ','?
RPL_TRM ←   EMP / RPL_ELS+
RPL_ELS ←   ESCAPE / VAL_CHR

ESCAPE  ←   ESC_NME / ESC_UNI / ESC_LIT
ESC_NME ←   '@' '{' [A-Za-z]+ '}'
ESC_UNI ←   '\' 'u' '{' [0-9A-F]+ '}'
ESC_LIT ←   '\' SPC_CHR

INPUT   ←   INP_TRM ( ',' INP_TRM )* ','?
INP_TRM ←   SBOUND / SEG+

SEG     ←   IPA (':' PARAMS)?

PARAMS  ←   '[' (ARG (',' ARG)*)? ']' 
ARG     ←   ARG_MOD [a-zA-Z]+ / TONE
ARG_MOD ←   '+' / '-'
TONE    ←   [a-zA-Z]+ ':' [0-9]+ 

VAL_CHR ←   !SPC_CHR !WSPACE .
SPC_CHR ←   ( '\' / '@' / '$' / '∅' / '*' / '>' / '=' / '+' / '-' / ',' )
PLUS    ←   '+'
EMP     ←   '*' / '∅'   
SBOUND  ←   '$'
ARR     ←   ('='/'-')? '>'  

IPA     ←   PRE_NAS? IPA_CHR (TIE IPA_CHR)? IPA_DIA*
PRE_NAS ←   'ᵐ' / 'ⁿ' / 'ᶯ' / 'ᶮ' / 'ᵑ' / 'ᶰ'
IPA_CHR ←   [<Unicode IPA Character>]
TIE     ←   '^' / [U+0361] / [U+035C]
IPA_DIA ←   !PRE_NAS [<Unicode Diacritic character>]

WSPACE  ←   [<Whitespace>]
EOL     ←   [<End of Line>]
```