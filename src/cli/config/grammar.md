```peg

SEQS    ← SEQ+

SEQ     ← TAG W_PATHS? ':' R_PATHS

TAG     ← '@' [ALPHANUMERIC]+

W_PATHS ← '[' STRING (',' STRING)* ','? ']'

R_PATHS ← ENTRY (',' ENTRY) ','?
ENTRY   ← STRING FILTER? 

FILTER  ← ('!' | '~') F_LIST
F_LIST  ← '{' STRING (',' STRING)* ','? '}'

COMMENT ← '#' CHAR* '\n'

STRING  ← '"' CHAR+ '"'
CHAR    ← <Valid Unicode Character>
```