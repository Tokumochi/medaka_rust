#[derive(PartialEq)]
enum TokenKind<'a> {
    NUM(u64),
    OPERA(&'a str),
}
struct Token<'a> {
    kind: TokenKind<'a>,
}

impl<'a> Token<'a> {

    fn new_opera(chara: &'a str) -> Self {
        Self {
            kind: TokenKind::OPERA(chara),
        }
    }

    fn new_num(value: u64) -> Self {
        Self {
            kind: TokenKind::NUM(value),
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
    
        while p < code.len() {
            let c = &code[p .. p + 1];

            if c == " " {
                p += 1;
                continue;
            }
    
            if c == "+" || c == "-" || c == "*" || c == "/" || c == "(" || c == ")" {
                tokens.push(Token::new_opera(c));
                p += 1;
                continue;
            } 
    
            if c == "0" || c == "1" || c == "2" || c == "3" || c == "4" ||
               c == "5" || c == "6" || c == "7" || c == "8" || c == "9" {
                let mut value: u64 = 0;
                let chara: Vec<char> = code[p..].chars().collect();
                for c in chara {
                    if '0' <= c && c <= '9' {
                        value = value * 10 + c as u64 - 48;
                        p += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token::new_num(value));
                continue;
            }
    
            eprintln!("invalid expression");
            std::process::exit(1);
        }
    
        return tokens;
    }

    pub fn is_end(&self) -> bool {
        self.tokens.len() == self.point
    }

    pub fn is_equal(&mut self, chara: &str) -> bool {
        if !self.is_end() {
            if let TokenKind::OPERA(opera) = self.tokens[self.point].kind {
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
            if let TokenKind::OPERA(opera) = self.tokens[self.point].kind {
                if opera == chara {
                    self.point += 1;
                    return;
                }
            } 
        }
        eprintln!("{} is expected", chara);
        std::process::exit(1);
    }

    pub fn get_num(&mut self) -> u64 {
        if !self.is_end() {
            if let TokenKind::NUM(value) = self.tokens[self.point].kind {
                self.point += 1;
                return value;
            }
        }
        eprintln!("number is expected");    
        std::process::exit(1);
    }
}