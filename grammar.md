``` peg
RULE    ←   SUB_RUL / RED_RUL / MET_RUL / DEL_RUL / INS_RUL

SUB_RUL ←   INP ARR OUT ('/' ENV)? ('|' ENV)? EOL           // [x]
RED_RUL ←   INP ARR '+' ('/' ENV)? ('|' ENV)? EOL           // [ ] :: NOTE: no plans to supported this
MET_RUL ←   INP ARR '&' ('/' ENV)? ('|' ENV)? EOL           // [x] :: FIXME: 'INP' here is currently actually INP_TRM
DEL_RUL ←   INP ARR EMP ('/' ENV)? ('|' ENV)? EOL           // [x] :: FIXME: 'INP' here is currently actually INP_TRM
INS_RUL ←   EMP ARR OUT ('/' ENV)? ('|' ENV)? EOL           // [x] :: FIXME: 'OUT' here is currently actually OUT_TRM

INP     ←   INP_TRM  ( ',' INP_TRM )*                       // [x]
INP_TRM ←   INP_EL+                                         // [x]
INP_EL  ←   ELLIPSS / SBOUND / TERM                         // [p]

OUT     ←   OUT_TRM  ( ',' OUT_TRM )*                       // [x]
OUT_TRM ←   OUT_EL+                                         // [x]
OUT_EL  ←   SYL / SET / SEG / VAR                           // [x] :: NOTE: 'SET' here only makes sense if it corresponds to a SET in INP

ENV     ←   '_' ',' ENV_EL / ENV_TRM  (',' ENV_TRM)*        // [x] :: _,# ==> #_ , _#
ENV_TRM ←   ENV_EL?  '_' ENV_EL?                            // [x]
ENV_EL  ←   ( BOUND / ELLIPSS / TERM )+                     // [x]

TERM    ←   SYL / SET / SEG / OPT / VAR                     // [x]
SYL     ←   '%' (':' PARAMS)?                               // [x]
SET     ←   '{' SEG (',' SEG)* '}'                          // [x] :: NOTE: At the moment, we can't have multi-segment sets i.e. "{nd}" is not allowed 
OPT     ←   '(' OPT_TRM+ (',' [0-9]+ (':' [1-9]+)?)? ')'    // [x] :: NOTE: (C) === (C,1) === (C, 0:1)
OPT_TRM ←   BOUND / SYL / SET / SEG / VAR                   // [x] :: NOTE: WBOUND in Input shouldn't be allowed
SEG     ←   IPA (':' PARAMS)? / MATRIX VAR_ASN?             // [x]
MATRIX  ←   (GROUP (':' PARAMS)? / PARAMS                   // [x]
VAR     ←   [0-9]+ (':' PARAMS)?                            // [x]
VAR_ASN ←   '=' [0-9]+                                      // [x]

GROUP	←   [A-Z]                                           // [x]
PARAMS  ←   '[' ARG (',' ARG)* ']'                          // [x]
ARG     ←   ARG_VAL [a-zA-Z]+ / TONE                        // [x]
TONE    ←   [a-zA-Z]+ ':' [0-9]+                            // [x]
ARG_VAL ←   '+' / '-' / [α-ω] / '-'[α-ω]                    // [x]

EMP     ←   '*' / '∅'                                       // [x]
BOUND	←   WBOUND / SBOUND                                 // [x]
WBOUND  ←   '#'                                             // [x]
SBOUND  ←   '$'                                             // [x]
ELLIPSS ←   '...' / '..' / …                                // [x]
IPA     ←   IPA_CAR (^ IPA_CAR)? IPA_DIA*                   // [x]
IPA_CAR ←   [Unicode-IPA-character]                         // [x] :: NOTE: As defined in `cardinals.json`
IPA_DIA ←   [Unicode-DIACRITIC-character]                   // [p] :: NOTE: As defined in `diacritics.json`
ARR     ←   ('='/'-')? '>'                                  // [x]
```


\[x] = implemented
\[p] = partially implemented
[ ]  = todo


