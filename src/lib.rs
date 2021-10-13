#[derive(PartialEq)]
enum TokenKind {
    NUM,
    OPERA,
}
pub struct Token<'a> {
    kind: TokenKind,
    value: u64,
    chara: &'a str,
}

impl<'a> Token<'a> {

    pub fn new_opera(chara: &'a str) -> Self {
        Self {
            kind: TokenKind::OPERA,
            value: 0,
            chara: chara,
        }
    }

    pub fn new_num(value: u64) -> Self {
        Self {
            kind: TokenKind::NUM,
            value: value,
            chara: "",
        }
    }
}


pub struct TokenGroup<'a> {
    tokens: Vec<Token<'a> >,
    point: usize,
}

impl<'a> TokenGroup<'a> {

    pub fn new() -> Self {
        Self {
            tokens: vec![],
            point: 0,
        }
    }

    pub fn push(&mut self, tok: Token<'a>) {
        self.tokens.push(tok);
    }

    pub fn is_end(&self) -> bool {
        self.tokens.len() == self.point
    }

    pub fn is_equal(&mut self, chara: &str) -> bool {
        if self.is_end() {
            std::process::exit(1);
        }
        if self.tokens[self.point].kind == TokenKind::OPERA && self.tokens[self.point].chara == chara {
            self.point += 1;
            return true;
        }
        return false;
    }

    pub fn expected(&mut self, chara: &str) {
        if self.is_end() || self.tokens[self.point].kind != TokenKind::OPERA || self.tokens[self.point].chara != chara {
            std::process::exit(1);
        }
        self.point += 1;
    }

    pub fn get_num(&mut self) -> u64 {
        if self.is_end() || self.tokens[self.point].kind != TokenKind::NUM {
            std::process::exit(1);
        }
        self.point += 1;
        return self.tokens[self.point - 1].value;
    }
}