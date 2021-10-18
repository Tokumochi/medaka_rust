use super::tokenize::TokenGroup;

pub struct Var {
    pub name: String,
}

impl Var {

    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

struct VarGroup {
    locals: Vec<Var>,
}

impl VarGroup {

    fn find_var(&self, name: &str) -> Option<usize> {
        for (index, var) in self.locals.iter().enumerate() {
            if var.name == name {
                return Some(index);
            }
        }
        return None;
    }

    fn new_local_var(&mut self, name: &str) -> usize {
        let var = Var::new(name);
        self.locals.push(var);
        return self.locals.len() - 1;
    }
}

pub enum UnaryKind {
    Neg,
}

pub enum BinaryKind {
    Equ,
    Neq,
    Les,
    Leq,
    Add,
    Sub,
    Mul,
    Div,
}

pub enum ExprKind {
    Num(u64),
    Var(usize),
    Assign(usize, Box<Expr>),
    Unary(UnaryKind, Box<Expr>),
    Binary(BinaryKind, Box<Expr>, Box<Expr>),
}

pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {

    fn new_var_node(index: usize) -> Self {
        Self {
            kind: ExprKind::Var(index),
        }
    }

    fn new_assign_node(index: usize, rhs: Self) -> Self {
        Self {
            kind: ExprKind::Assign(index, Box::new(rhs)),
        }
    }
    
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

    // expr -> assign
    fn expr(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        Expr::assign(tokens, vars)
    }

    // assign -> equality ("=" equality)?
    fn assign(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        let node = Expr::equality(tokens, vars);
        if tokens.is_equal("=") {
            if let ExprKind::Var(index) = node.kind {
                return Expr::new_assign_node(index, Expr::equality(tokens, vars));
            } else {
                tokens.previous_token_error("The left side must be variable.".to_string());
                std::process::exit(1);
            }
        }
        return node;
    }

    // equality -> relational ("==" relational | "!=" relational)*
    fn equality(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        let mut node = Expr::relational(tokens, vars);
        loop {
            if tokens.is_equal("==") {
                node = Expr::new_binary_node(BinaryKind::Equ, node, Expr::relational(tokens, vars));
                continue;
            }
            if tokens.is_equal("!=") {
                node = Expr::new_binary_node(BinaryKind::Neq, node, Expr::relational(tokens, vars));
                continue;
            }
            return node;
        }
    }

    // relational -> add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        let mut node = Expr::add(tokens, vars);
        loop {
            if tokens.is_equal("<") {
                node = Expr::new_binary_node(BinaryKind::Les, node, Expr::add(tokens, vars));
                continue;
            }
            if tokens.is_equal("<=") {
                node = Expr::new_binary_node(BinaryKind::Leq, node, Expr::add(tokens, vars));
                continue;
            }
            if tokens.is_equal(">") {
                node = Expr::new_binary_node(BinaryKind::Les, Expr::add(tokens, vars), node);
                continue;
            }
            if tokens.is_equal(">=") {
                node = Expr::new_binary_node(BinaryKind::Leq, Expr::add(tokens, vars), node);
                continue;
            }
            return node;
        }
    }
    
    // add -> mul ("+" mul | "-" mul)*
    fn add(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        let mut node = Expr::mul(tokens, vars);
        loop {
            if tokens.is_equal("+") {
                node = Expr::new_binary_node(BinaryKind::Add, node, Expr::mul(tokens, vars));
                continue;
            }
            if tokens.is_equal("-") {
                node = Expr::new_binary_node(BinaryKind::Sub, node, Expr::mul(tokens, vars));
                continue;
            }
            return node;
        }
    }
    
    // mul -> unary ("*" unary | "/" unary)*
    fn mul(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        let mut node = Expr::unary(tokens, vars);
        loop {
            if tokens.is_equal("*") {
                node = Expr::new_binary_node(BinaryKind::Mul, node, Expr::unary(tokens, vars));
                continue;
            }
            if tokens.is_equal("/") {
                node = Expr::new_binary_node(BinaryKind::Div, node, Expr::unary(tokens, vars));
                continue;
            }
            return node;
        }
    }

    // unary -> ("+" | "-") unary | primary
    fn unary(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        if tokens.is_equal("+") {
            return Expr::unary(tokens, vars);
        }
        if tokens.is_equal("-") {
            return Expr::new_unary_node(UnaryKind::Neg, Expr::unary(tokens, vars));
        }
        return Expr::primary(tokens, vars);
    }
    
    // primary -> "(" expr ")" | ident | num
    fn primary(tokens: &mut TokenGroup, vars: &VarGroup) -> Self {
        if tokens.is_equal("(") {
            let node = Expr::expr(tokens, vars);
            tokens.expected(")");
            return node;
        }

        if let Some(name) = tokens.is_ident() {
            if let Some(index) = vars.find_var(name) {
                return Expr::new_var_node(index);
            } else {
                let message = format!("The variable name \"{}\" doesn't exists.", name);
                tokens.previous_token_error(message);
                std::process::exit(1);
            }
        }

        if let Some(num) = tokens.is_number() {
            return Self {
                kind: ExprKind::Num(num),
            };
        }

        tokens.current_token_error("expected an expression".to_string());
        std::process::exit(1);
    }
}

pub enum StmtKind {
    Ret(Expr),
    Block(Vec<Stmt>),
    ExprStmt(Expr),
}

pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    // stmt -> "return" expr ";" | "{" block_stmt | "dec" declaration | expr_stmt
    fn stmt(tokens: &mut TokenGroup, vars: &mut VarGroup) -> Self {
        if tokens.is_equal("return") {
            let node = Self { kind: StmtKind::Ret(Expr::expr(tokens, vars)) };
            tokens.expected(";");
            return node;
        }

        if tokens.is_equal("{") {
            return Stmt::block_stmt(tokens, vars);
        }

        if tokens.is_equal("dec") {
            return Stmt::declaration(tokens, vars);
        }

        return Stmt::expr_stmt(tokens, vars);
    }

    // block
    fn block_stmt(tokens: &mut TokenGroup, vars: &mut VarGroup) -> Self {
        let mut body = vec![];
        while !tokens.is_equal("}") {
            body.push(Stmt::stmt(tokens, vars));
        }
        return Self { kind: StmtKind::Block(body) };
    }

    // declaration -> ident ":" "i32" "=" expr ";"
    fn declaration(tokens: &mut TokenGroup, vars: &mut VarGroup) -> Self {
        if let Some(name) = tokens.is_ident() {
            if let Some(_) = vars.find_var(name) {
                let message = format!("The variable name \"{}\" already exists.", name);
                tokens.previous_token_error(message);
                std::process::exit(1);
            } else {
                let index = vars.new_local_var(name);
                tokens.expected(":");
                tokens.expected("i32");
                tokens.expected("=");

                let node = Self {
                    kind: StmtKind::ExprStmt(Expr::new_assign_node(index, Expr::expr(tokens, vars))),
                };
                tokens.expected(";");
                return node;
            }
        }
        tokens.current_token_error("identifier is expected".to_string());
        std::process::exit(1);
    }

    // expr_stmt ->　expr ";"
    fn expr_stmt(tokens: &mut TokenGroup, vars: &mut VarGroup) -> Self {
        let node = Self { kind: StmtKind::ExprStmt(Expr::expr(tokens, vars)) };
        tokens.expected(";");
        return node;
    }
}

pub struct Func {
    pub locals: Vec<Var>,
    pub body: Stmt,
}

impl Func {

    pub fn new(tokens: &mut TokenGroup) -> Self {
        tokens.expected("{");
        let mut vars = VarGroup { locals: vec![] };
        let body = Stmt::block_stmt(tokens, &mut vars);

        return Self {
            locals: vars.locals,
            body: body,
        }
    }
}