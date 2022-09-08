``` peg
RULE    ←   SUB_RUL / RED_RUL / MET_RUL / DEL_RUL / INS_RUL

SUB_RUL ←d   INP '>' OUT ('/' ENV)? ('|' ENV)? EOL
RED_RUL ←d   INP '>' '+' ('/' ENV)? ('|' ENV)? EOL
MET_RUL ←d   INP '>' '&' ('/' ENV)? ('|' ENV)? EOL
DEL_RUL ←d   INP '>' EMP ('/' ENV)? ('|' ENV)? EOL
INS_RUL ←d   EMP '>' OUT ('/' ENV)? ('|' ENV)? EOL

INP     ←d   EMP / INP_TRM  ( ',' INP_TRM )* 
INP_TRM ←d   ( '...' / TERM )+

OUT     ←p   EMP / '&' / '+' / OUT_TRM  ( ',' OUT_TRM )* 
OUT_TRM ←d   OUT_EL+
OUT_EL  ←d   SYLL / SEG / VAR

ENV     ←d   '_' ',' ENV_EL / ENV_TRM  (',' ENV_TRM)*   // _,# ==> #_ , _#
ENV_TRM ←d   ENV_EL?  '_' ENV_EL?
ENV_EL  ←d   ( BOUND / '...' / TERM )+

TERM    ←d   SYLL / SET / SEG / OPT / VAR
SYLL    ←d   '%' (':' PARAM)?
SET     ←d   '{' SEG (',' SEG)* '}'
OPT     ←d   '(' SEG+ (',' [0-9]+ (':' [0-9]+)?)? ')'    // (S,M:N) => (C, 0:1) etc.
SEG     ←d   MATRIX
VAR     ←d   [0-9]+ (':' PARAM)?

MATRIX  ←d   (IPA / CHAR) (':' PARAM)? / PARAM 
CHAR	←d   [A-Z] VAR_ASS?
PARAM   ←p   '[' ARG (',' ARG)* ']' VAR_ASS?
ARG     ←d   ( '+' / '-' / [α-ω] ) [a-zA-Z]+ / [a-zA-Z]+ ':' [0-9]+ 
VAR_ASS ←d   '=' [0-9]+

EMP     ←d   '*' / '∅'
BOUND	←d   '$' / '#'
IPA     ←d   [Unicode-IPA-character]+
```


←d = implemented
←p = partially implemented
←  = todo
