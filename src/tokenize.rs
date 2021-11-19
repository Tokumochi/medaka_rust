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

#[derive(Clone, Copy)]
enum TokenKind<'a> {
    Num(u64),
    Opera(&'a str),
    Ident(&'a str),
}
#[derive(Clone, Copy)]
pub struct Token<'a> {
    kind: TokenKind<'a>,
    line: &'a str,
    pos: usize,
    len: usize,
}

impl<'a> Token<'a> {

    fn new_num(value: u64, line: &'a str, pos: usize, len: usize) -> Self {
        Self {
            kind: TokenKind::Num(value),
            line: line,
            pos: pos,
            len: len,
        }
    }

    fn new_opera(chara: &'a str, line: &'a str, pos: usize, len: usize) -> Self {
        Self {
            kind: TokenKind::Opera(chara),
            line: line,
            pos: pos,
            len: len,
        }
    }

    fn new_ident(chara: &'a str, line: &'a str, pos: usize, len: usize) -> Self {
        Self {
            kind: TokenKind::Ident(chara),
            line: line,
            pos: pos,
            len: len,
        }
    }

    pub fn token_error(&self, message: String) {
        eprintln!("{}", self.line);
        eprintln!("{0:>1$}{0:~>2$}", "", self.pos, self.len);
        eprintln!("{0:>1$}", "^", self.pos + 1);
        eprintln!("{0:>1$}{2}", "", self.pos, message);
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
            if p + 1 < code.len() {
                let cc = &code[p .. p + 2];
                if cc == "==" || cc == "!=" || cc == "<=" || cc == ">=" {
                    tokens.push(Token::new_opera(cc, code, p, 2));
                    p += 2;
                    continue;
                }
            }
            if c == "=" || c == "<" || c == ">" || c == "+" || c == "-" || c == "*" || c == "/" ||
               c == "(" || c == ")" || c == ";" || c == ":" || c == "{" || c == "}" || c == "," || c == "." {
                tokens.push(Token::new_opera(c, code,  p, 1));
                p += 1;
                continue;
            }
    
            // Number
            if let Some(num) = is_number(c) {
                let mut value = num as u64 - 48;
                let start = p;
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
                tokens.push(Token::new_num(value, code, start, p - start));
                continue;
            }

            // Keyword or Identifier
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
                let keywords = ["struct", "define", "skill", "dec", "i8", "i32", "if", "else", "return"];
                for keyword in keywords {
                    if &code[start .. p] == keyword {
                        tokens.push(Token::new_opera(keyword, code, start, p - start));
                        continue 'tokenizer;
                    }
                }
                tokens.push(Token::new_ident(&code[start .. p], code, start, p - start));
                continue;
            }
    
            eprintln!("{}", code);
            eprintln!("{0:>1$}", "^", p + 1);
            eprintln!("{0:>1$}{2}", "", p, "invalid symbol");
            std::process::exit(1);
        }
    
        return tokens;
    }

    pub fn get_current_token(&self) -> Token {
        return self.tokens[self.point];
    }

    pub fn get_previous_token(&self) -> Token {
        if self.point == 0 {
            eprintln!("What is happening in mochi code!?");
            std::process::exit(1);
        }
        return self.tokens[self.point - 1];
    }

    pub fn current_token_error(&self, message: String) {
        self.tokens[self.point].token_error(message);
    }

    pub fn previous_token_error(&self, message: String) {
        if self.point == 0 {
            eprintln!("What is happening in mochi code!?");
            std::process::exit(1);
        }
        self.tokens[self.point - 1].token_error(message);
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
        self.current_token_error(format!("\"{}\" is expected", chara));
        std::process::exit(1);
    }

    pub fn is_ident(&mut self) -> Option<&str> {
        if !self.is_end() {
            if let TokenKind::Ident(name) = self.tokens[self.point].kind {
                self.point += 1;
                return Some(name);
            }
        }
        return None;
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