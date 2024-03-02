``` peg
RULE    ←   SUB_RUL / MET_RUL / DEL_RUL / INS_RUL

SUB_RUL ←   INP ARR OUT ('/' ENV)? (PIPE ENV)? EOL          //
MET_RUL ←   INP ARR '&' ('/' ENV)? (PIPE ENV)? EOL          // FIXME: 'INP' here is currently actually INP_TRM
DEL_RUL ←   INP ARR EMP ('/' ENV)? (PIPE ENV)? EOL          // FIXME: 'INP' here is currently actually INP_TRM
INS_RUL ←   EMP ARR OUT ('/' ENV)? (PIPE ENV)? EOL          // FIXME: 'OUT' here is currently actually OUT_TRM

INP     ←   INP_TRM  ( ',' INP_TRM )*                       // 
INP_TRM ←   INP_EL+                                         // 
INP_EL  ←   ELLIPSS / SBOUND / TERM                         // 

OUT     ←   OUT_TRM  ( ',' OUT_TRM )*                       //
OUT_TRM ←   OUT_EL+                                         //
OUT_EL  ←   SYL / SET / SEG / VAR                           // NOTE: 'SET' here only makes sense if it corresponds to a SET in INP

ENV     ←   '_' ',' ENV_EL / ENV_TRM  (',' ENV_TRM)*        // e.g. _,# ==> #_ , _#
ENV_TRM ←   ('WBOUND')? ENV_EL?  '_' ENV_EL? ('WBOUND')?    //
ENV_EL  ←   ( SBOUND / ELLIPSS / TERM )+                    //

TERM    ←   SYL / SET / SEG / OPT / VAR                     //
SYL     ←   '%' (':' PARAMS)? VAR_ASN?                      //
SET     ←   '{' SEG (',' SEG)* '}'                          // NOTE: At the moment, we can't have multi-segment sets i.e. "{nd}" is not allowed 
OPT     ←   '(' OPT_TRM+ (',' [0-9]+ (':' [1-9]+)?)? ')'    // NOTE: (C) === (C,1) === (C, 0:1)
OPT_TRM ←   BOUND / SYL / SET / SEG / VAR                   // FIXME: WBOUND in Input/Output shouldn't be allowed
SEG     ←   IPA (':' PARAMS)? / MATRIX VAR_ASN?             //
MATRIX  ←   GROUP (':' PARAMS)? / PARAMS                    //
VAR     ←   [0-9]+ (':' PARAMS)?                            //
VAR_ASN ←   '=' [0-9]+                                      //

GROUP	←   [A-Z]                                           //
PARAMS  ←   '[' ARG (',' ARG)* ']'                          //
ARG     ←   ARG_VAL [a-zA-Z]+ / TONE                        //
TONE    ←   [a-zA-Z]+ ':' [0-9]+                            //
ARG_VAL ←   '+' / '-' / [α-ω] / '-'[α-ω]                    //

EMP     ←   '*' / '∅'                                       //
BOUND	←   WBOUND / SBOUND                                 //
WBOUND  ←   '#'                                             //
SBOUND  ←   '$'                                             //
ELLIPSS ←   '...' / '..' / …                                //
IPA     ←   IPA_CAR (^ IPA_CAR)? IPA_DIA*                   //
IPA_CAR ←   [Unicode-IPA-character]                         // NOTE: As defined in `cardinals.json`
IPA_DIA ←   [Unicode-DIACRITIC-character]                   // NOTE: As defined in `diacritics.json`
ARR     ←   ('='/'-')? '>'                                  //
PIPE    ←   '|' / '//'                                      //
```