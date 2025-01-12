```peg

CONF    ← SEQ+

SEQ     ← TAG FROM? ALIAS? W_PATHS? ':' R_PATHS

TAG     ← '@' [ALPHANUMERIC]+

FROM    ← '%' [ALPHANUMERIC]+

ALIAS   ← '$' [ALPHANUMERIC]+

W_PATHS ← '[' STRING (',' STRING)* ','? ']'

R_PATHS ← ENTRY (',' ENTRY) ','?
ENTRY   ← STRING FILTER? 

FILTER  ← ('!' | '~') F_LIST
F_LIST  ← '{' STRING (',' STRING)* ','? '}'

COMMENT ← '#' CHAR* '\n'

STRING  ← '"' CHAR+ '"'
CHAR    ← <Valid Unicode Character>
```