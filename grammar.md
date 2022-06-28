``` peg
RULE    ←   SUB_RUL / RED_RUL / MET_RUL / DEL_RUL / INS_RUL

SUB_RUL ←   INP '>' OUT ('/' ENV)? ('|' ENV)? EOL
RED_RUL ←   INP '>' '+' ('/' ENV)? ('|' ENV)? EOL
MET_RUL ←   INP '>' '&' ('/' ENV)? ('|' ENV)? EOL
DEL_RUL ←   INP '>' EMP ('/' ENV)? ('|' ENV)? EOL
INS_RUL ←   EMP '>' OUT ('/' ENV)? ('|' ENV)? EOL

INP     ←   EMP / INP_TRM  ( ',' INP_TRM )* 
INP_TRM ←   ( '...' / TERM )+

OUT     ←   EMP / OUT_TRM  ( ',' OUT_TRM )* 
OUT_TRM ←   '&' / '+' / (SYLL / SET / SEG)+

ENV     ←   ENVEXPR  (',' ENVEXPR)*    // _#, ==> #_ , _#
ENVEXPR ←   ENV_EL*  '_' ENV_EL*
ENV_EL  ←   ( BOUND / '...' / TERM )+

TERM    ←   SYLL / SET / SEG / OPT
SYLL    ←   '%' (':' PARAM)?
SET     ←   '{' SEG (',' SEG)* '}'
OPT     ←   '(' SEG+ ('=' [0-9]+ (':' [0-9]+)?)? ')'
SEG     ←   IPA / MATRIX

MATRIX  ←   CHAR (':' PARAM)? / PARAM 
CHAR	←   'C' / 'V'
PARAM   ←   '[' ARG (',' ARG)* ']'
ARG	    ←   ( '+' / '-' / [α-ω] ) [a-zA-Z]+ / [a-zA-Z]+ ':' [0-9]+ 

EMP     ←   '*' / '∅'
BOUND	←   '$' / '#'
IPA     ←   Any phone represented by IPA characters
```