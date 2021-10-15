fn is_number(chara: &str) -> Option<char> {
    let chara: char = chara.chars().next().unwrap();
    if '0' <= chara && chara <= '9' {
        return Some(chara);
    }
    return None;
}

fn is_alpha(chara: &str) -> bool {
    let chara: char = chara.chars().next().unwrap();
    return ('a' <= chara && chara <= 'z') || ('A' <= chara && chara <= 'Z');
}

enum TokenKind<'a> {
    Num(u64),
    Opera(&'a str),
}
struct Token<'a> {
    kind: TokenKind<'a>,
}

impl<'a> Token<'a> {

    fn new_num(value: u64) -> Self {
        Self {
            kind: TokenKind::Num(value),
        }
    }

    fn new_opera(chara: &'a str) -> Self {
        Self {
            kind: TokenKind::Opera(chara),
        }
    }
}


pub struct TokenGroup<'a> {
    tokens: Vec<Token<'a> >,
    point: usize,
}

impl<'a> TokenGroup<'a> {

    fn push(&mut self, tok: Token<'a>) {
        self.tokens.push(tok);
    }
    
    pub fn new(code: &'a str) -> Self {
        let mut tokens = Self {
            tokens: vec![],
            point: 0,
        };
        let mut p = 0;
    
        'tokenizer: while p < code.len() {
            let c = &code[p .. p + 1];

            // Space
            if c == " " {
                p += 1;
                continue;
            }
    
            // Operator
            if c == "+" || c == "-" || c == "*" || c == "/" || c == "(" || c == ")" || c == ";" {
                tokens.push(Token::new_opera(c));
                p += 1;
                continue;
            } 
    
            // Number
            if let Some(num) = is_number(c) {
                let mut value = num as u64 - 48;
                p += 1;
                while p < code.len() {
                    let c = &code[p .. p + 1];
                    if let Some(num) = is_number(c) {
                        value = value * 10 + (num as u64 - 48);
                        p += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token::new_num(value));
                continue;
            }

            // Keyword
            if is_alpha(c) {
                let start = p;
                p += 1;
                while p < code.len() {
                    let c = &code[p .. p + 1];
                    if is_alpha(c) {
                        p += 1;
                    } else if let Some(_) = is_number(c) {
                        p += 1;
                    } else {
                        break;
                    }
                }

                let keywords = ["return"];
                for keyword in keywords {
                    if &code[start .. p] == keyword {
                        tokens.push(Token::new_opera(keyword));
                        continue 'tokenizer;
                    }
                }
            }
    
            eprintln!("invalid symbol");
            std::process::exit(1);
        }
    
        return tokens;
    }

    pub fn is_end(&self) -> bool {
        self.tokens.len() == self.point
    }

    pub fn is_equal(&mut self, chara: &str) -> bool {
        if !self.is_end() {
            if let TokenKind::Opera(opera) = self.tokens[self.point].kind {
                if opera == chara {
                    self.point += 1;
                    return true;
                }
            }
        }
        return false;
    }

    pub fn expected(&mut self, chara: &str) {
        if !self.is_end() {
            if let TokenKind::Opera(opera) = self.tokens[self.point].kind {
                if opera == chara {
                    self.point += 1;
                    return;
                }
            } 
        }
        eprintln!("{} is expected", chara);
        std::process::exit(1);
    }

    pub fn is_number(&mut self) -> Option<u64> {
        if !self.is_end() {
            if let TokenKind::Num(value) = self.tokens[self.point].kind {
                self.point += 1;
                return Some(value);
            }
        }
        return None;
    }
}