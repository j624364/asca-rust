``` peg
RULE    ←   SUB_RUL / RED_RUL / MET_RUL / DEL_RUL / INS_RUL

SUB_RUL ←   INP '>' OUT ('/' ENV)? ('|' ENV)? EOL
RED_RUL ←   INP '>' '+' ('/' ENV)? ('|' ENV)? EOL
MET_RUL ←   INP '>' '&' ('/' ENV)? ('|' ENV)? EOL
DEL_RUL ←   INP '>' EMP ('/' ENV)? ('|' ENV)? EOL
INS_RUL ←   EMP '>' OUT ('/' ENV)? ('|' ENV)? EOL

INP     ←d   EMP / INP_TRM  ( ',' INP_TRM )* 
INP_TRM ←d   ( '...' / TERM )+

OUT     ←p   EMP / '&' / '+' / OUT_TRM  ( ',' OUT_TRM )* 
OUT_TRM ←d   OUT_EL+
OUT_EL  ←d   SYLL / SEG

ENV     ←d   '_' ',' ENV_EL / ENV_TRM  (',' ENV_TRM)*   // _,# ==> #_ , _#
ENV_TRM ←d   ENV_EL?  '_' ENV_EL?
ENV_EL  ←d   ( BOUND / '...' / TERM )+

TERM    ←d   SYLL / SET / SEG / OPT
SYLL    ←d   '%' (':' PARAM)?
SET     ←d   '{' SEG (',' SEG)* '}'
OPT     ←d   '(' SEG+ (',' [0-9]+ (':' [0-9]+)?)? ')'    // (S,M:N) => (C, 0:1) etc.
SEG     ←d   MATRIX / IPA

MATRIX  ←p   (IPA / CHAR) (':' PARAM)? / PARAM 
CHAR	←d   'C' / 'V'
PARAM   ←d   '[' ARG (',' ARG)* ']'
ARG	    ←d   ( '+' / '-' / [α-ω] ) [a-zA-Z]+ / [a-zA-Z]+ ':' [0-9]+ 

EMP     ←d   '*' / '∅'
BOUND	←d   '$' / '#'
IPA     ←d   Any phone represented by IPA characters
```


←d = implemented
←p = partially implemented
←  = todo
