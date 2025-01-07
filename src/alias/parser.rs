use super::{AliasKind, AliasPosition, AliasToken, AliasTokenKind};

pub(crate) struct AliasParser {
    kind: AliasKind,
    token_list: Vec<AliasToken>,
    line: usize,
    pos: usize,
    curr_tkn: AliasToken,
}


impl AliasParser {
    pub(crate) fn new(token_list: Vec<AliasToken>, kind: AliasKind, line: usize) -> Self {
        let mut s = Self {
            kind,
            token_list, 
            line,
            pos: 0, 
            curr_tkn: AliasToken { kind: AliasTokenKind::Eol, value: String::new(), position: AliasPosition::new(line, 0, 1 ) },
        };
        s.curr_tkn = s.token_list[s.pos].clone();

        s
    }

}