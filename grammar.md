``` peg
RULE    ←   SUB_RUL / RED_RUL / MET_RUL / DEL_RUL / INS_RUL

SUB_RUL ←   INP '>' OUT ('/' ENV)? ('|' ENV)? EOL           // [x]
RED_RUL ←   INP '>' '+' ('/' ENV)? ('|' ENV)? EOL           // [ ] :: NOTE: currently unsupported
MET_RUL ←   INP '>' '&' ('/' ENV)? ('|' ENV)? EOL           // [x]
DEL_RUL ←   INP '>' EMP ('/' ENV)? ('|' ENV)? EOL           // [x]
INS_RUL ←   EMP '>' OUT ('/' ENV)? ('|' ENV)? EOL           // [x]

INP     ←   INP_TRM  ( ',' INP_TRM )*                       // [x]
INP_TRM ←   ( ELLIPSS / TERM )+                             // [x]

OUT     ←   OUT_TRM  ( ',' OUT_TRM )*                       // [x]
OUT_TRM ←   OUT_EL+                                         // [x]
OUT_EL  ←   SYLL / SET / SEG / VAR                          // [x]

ENV     ←   '_' ',' ENV_EL / ENV_TRM  (',' ENV_TRM)*        // [x] :: _,# ==> #_ , _#
ENV_TRM ←   ENV_EL?  '_' ENV_EL?                            // [x]
ENV_EL  ←   ( BOUND / '...' / TERM )+                       // [x]

TERM    ←   SYLL / SET / SEG / OPT / VAR                    // [x]
SYLL    ←   '%' (':' PARAM)?                                // [x]
SET     ←   '{' SEG (',' SEG)* '}'                          // [x]
OPT     ←   '(' OPT_TRM+ (',' [0-9]+ (':' [1-9]+)?)? ')'    // [x] :: NOTE: (C) === (C,1) === (C, 0:1)
OPT_TRM ←   BOUND / SYLL / SET / SEG / VAR                  // [x]
SEG     ←   MATRIX                                          // [x]
VAR     ←   [0-9]+ (':' PARAM)?                             // [p]

MATRIX  ←   (IPA / CHAR) (':' PARAM)? / PARAM               // [x]
CHAR	←   [A-Z] VAR_ASN?                                  // [x]
PARAM   ←   '[' ARG (',' ARG)* ']' VAR_ASN?                 // [p]
ARG     ←   PAR_VAL [a-zA-Z]+ / [a-zA-Z]+ ':' [0-9]+        // [p]
VAR_ASN ←   '=' [0-9]+                                      // [p]
PAR_VAL ←   '+' / '-' / [α-ω] / '-'[α-ω]                    // [x]

EMP     ←   '*' / '∅'                                       // [x]
BOUND	←   '$' / '#'                                       // [x]
ELLIPSS ←   '...' / '..' / …
IPA     ←   IPA_CAR IPA_DIA+                                // [p] :: NOTE: tie-bar support not fully implemented: IPA_CAR '^' IPA_CAR 
IPA_CAR ←   [Unicode-IPA-character]                         // [x] :: NOTE: As defined in `cardinals.json`
IPA_DIA ←   [Unicode-DIACRITIC-character]                   // [p] :: NOTE: As defined in `diacritics.json`
```


\[x] = implemented
\[p] = partially implemented
[ ]  = todo


