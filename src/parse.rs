use super::tokenize;

pub enum UnaryKind {
    Neg,
}

pub enum BinaryKind {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum ExprKind {
    Num(u64),
    Unary(UnaryKind, Box<Expr>),
    Binary(BinaryKind, Box<Expr>, Box<Expr>),
}

pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {
    
    fn new_unary_node(kind: UnaryKind, ohs: Self) -> Self {
        Self {
            kind: ExprKind::Unary(kind, Box::new(ohs)),
        }
    }

    fn new_binary_node(kind: BinaryKind, lhs: Self, rhs: Self) -> Self {
        Self {
            kind: ExprKind::Binary(kind, Box::new(lhs), Box::new(rhs)),
        }
    }

    fn expr(tokens: &mut tokenize::TokenGroup) -> Self {
        Expr::add(tokens)
    }
    
    // add -> mul ("+" mul | "-" mul)*
    fn add(tokens: &mut tokenize::TokenGroup) -> Self {
        let mut node = Expr::mul(tokens);
        loop {
            if tokens.is_equal("+") {
                node = Expr::new_binary_node(BinaryKind::Add, node, Expr::mul(tokens));
                continue;
            }
            if tokens.is_equal("-") {
                node = Expr::new_binary_node(BinaryKind::Sub, node, Expr::mul(tokens));
                continue;
            }
            return node;
        }
    }
    
    // mul -> unary ("*" unary | "/" unary)*
    fn mul(tokens: &mut tokenize::TokenGroup) -> Self {
        let mut node = Expr::unary(tokens);
        loop {
            if tokens.is_equal("*") {
                node = Expr::new_binary_node(BinaryKind::Mul, node, Expr::unary(tokens));
                continue;
            }
            if tokens.is_equal("/") {
                node = Expr::new_binary_node(BinaryKind::Div, node, Expr::unary(tokens));
                continue;
            }
            return node;
        }
    }

    // unary -> ("+" | "-") unary | primary
    fn unary(tokens: &mut tokenize::TokenGroup) -> Self {
        if tokens.is_equal("+") {
            return Expr::unary(tokens);
        }
        if tokens.is_equal("-") {
            return Expr::new_unary_node(UnaryKind::Neg, Expr::unary(tokens));
        }
        return Expr::primary(tokens);
    }
    
    // primary -> "(" expr ")" | num
    fn primary(tokens: &mut tokenize::TokenGroup) -> Self {
        if tokens.is_equal("(") {
            let node = Expr::expr(tokens);
            tokens.expected(")");
            return node;
        }

        if let Some(num) = tokens.is_number() {
            return Self {
                kind: ExprKind::Num(num),
            };
        }

        eprintln!("expected an expression");
        std::process::exit(1);
    }
}

pub enum StmtKind {
    Ret(Expr),
    ExprStmt(Expr),
}

pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    // stmt -> "return" expr ";" | expr_stmt
    fn stmt(tokens: &mut tokenize::TokenGroup) -> Self {
        if tokens.is_equal("return") {
            let node = Self { kind: StmtKind::Ret(Expr::expr(tokens)) };
            tokens.expected(";");
            return node;
        }
        return Stmt::expr_stmt(tokens);
    }

    // expr_stmt -> expr ";"
    fn expr_stmt(tokens: &mut tokenize::TokenGroup) -> Self {
        let node = Self { kind: StmtKind::ExprStmt(Expr::expr(tokens)) };
        tokens.expected(";");
        return node;
    }
}

pub struct Func {
    pub body: Vec<Stmt>,
}

impl<'a> Func {

    pub fn new(tokens: &mut tokenize::TokenGroup) -> Self {
        let mut body = vec![];
        while !tokens.is_end() {
            body.push(Stmt::stmt(tokens));
        }
        return Self {
            body: body,
        }
    }
}