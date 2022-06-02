``` peg
RULE    ←   SUBRUL / REDRUL / METRUL / DELRUL / INSRUL

SUBRUL  ←   INP '>' OUT ('/' ENV)? ('|' ENV)? EOL
REDRUL  ←   INP '>' '+' ('/' ENV)? ('|' ENV)? EOL
METRUL  ←   INP '>' '&' ('/' ENV)? ('|' ENV)? EOL
DELRUL  ←   INP '>' EMP ('/' ENV)? ('|' ENV)? EOL
INSRUL  ←   EMP '>' OUT ('/' ENV)? ('|' ENV)? EOL

INP     ←   INP_EL ( ',' INP_EL )*
INP_EL  ←   ( TERM / '...' )+

OUT     ←   OUT_EL ( ',' OUT_EL )*
OUT_EL  ←   TERM+ / '&' / '+'

ENV     ←   ENVEXPR (',' ENVEXPR)*
ENVEXPR ←   ENV_EL*  '_' ENV_EL*
ENV_EL  ←   ( TERM / BOUND / '...' )+

TERM    ←   SYLL / ELEM
SYLL    ←   '%' ( L ( ELEM/'...' )+ R )? (':' PARAM)?
ELEM    ←   SEG / SET / OPT
L       ←   '⟨' / '<'
R       ←   '⟩' / '>'

SEG     ←   IPA / MATRIX
SET     ←   '{' SEG (',' SEG)* '}'
OPT     ←   '(' SEG+ ('=' [0-9]+ (':' [0-9]+)?)? ')'

MATRIX  ←   CHAR ':' PARAM / CHAR / PARAM 
PARAM   ←   '[' ARG (',' ARG)* ']'
ARG	    ←   ( '+' / '-' / [α-ω] ) [a-zA-Z]+ / [a-zA-Z]+ ':' [0-9]+ 

EMP     ←   '*' / '∅'
CHAR	←   'C' / 'V'
BOUND	←   '$' / '#'
IPA     ←   Any phone represented by IPA characters
```