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

pub struct Func {
    pub name: String,
    pub locals: Vec<Var>,
    pub num_of_params: usize,
    pub body: Stmt,
}

// Manager of Function Creation
struct CreatManager<'a> {
    name: String,
    locals: Vec<Var>,
    num_of_params: usize,
    funcs: &'a Vec<Func>,
}

impl<'a> CreatManager<'a> {

    fn find_local_var(&self, name: &str) -> Option<usize> {
        for (index, var) in self.locals.iter().enumerate() {
            if var.name == name {
                return Some(index);
            }
        }
        return None;
    }

    fn find_func(&self, name: &str) -> Option<(usize, usize)> {
        for (index, func) in self.funcs.iter().enumerate() {
            if func.name.as_str() == name {
                return Some((index, func.num_of_params));
            }
        }
        if name == self.name {
            return Some((self.funcs.len(), self.num_of_params));
        }
        return None;
    }

    fn new_local_var(&mut self, name: &str) -> usize {
        let var = Var::new(name);
        self.locals.push(var);
        return self.locals.len() - 1;
    }

    // declarator -> ident ":" "i32"
    fn declarator(&mut self, tokens: &mut TokenGroup) -> usize {
        if let Some(name) = tokens.is_ident() {
            if self.find_local_var(name) == None && self.find_func(name) == None {
                let index = self.new_local_var(name);
                tokens.expected(":");
                tokens.expected("i32");
                return index;
            }
            let message = format!("The variable name \"{}\" already exists.", name);
            tokens.previous_token_error(message);
            std::process::exit(1);
        }
        tokens.current_token_error("identifier is expected".to_string());
        std::process::exit(1);
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
    Call(usize, Vec<Expr>),
    Unary(UnaryKind, Box<Expr>),
    Binary(BinaryKind, Box<Expr>, Box<Expr>),
    Assign(usize, Box<Expr>),
}

pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {

    fn new_num_node(value: u64) -> Self {
        Self {
            kind: ExprKind::Num(value),
        }
    }

    fn new_var_node(index: usize) -> Self {
        Self {
            kind: ExprKind::Var(index),
        }
    }

    fn new_call_node(index: usize, args: Vec<Expr>) -> Self {
        Self {
            kind: ExprKind::Call(index, args),
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
    fn expr(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        Expr::assign(tokens, manager)
    }

    // assign -> equality ("=" assign)?
    fn assign(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        let node = Expr::equality(tokens, manager);
        if tokens.is_equal("=") {
            if let ExprKind::Var(index) = node.kind {
                return Expr::new_assign_node(index, Expr::assign(tokens, manager));
            } else {
                tokens.previous_token_error("The left side must be variable.".to_string());
                std::process::exit(1);
            }
        }
        return node;
    }

    // equality -> relational ("==" relational | "!=" relational)*
    fn equality(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        let mut node = Expr::relational(tokens, manager);
        loop {
            if tokens.is_equal("==") {
                node = Expr::new_binary_node(BinaryKind::Equ, node, Expr::relational(tokens, manager));
                continue;
            }
            if tokens.is_equal("!=") {
                node = Expr::new_binary_node(BinaryKind::Neq, node, Expr::relational(tokens, manager));
                continue;
            }
            return node;
        }
    }

    // relational -> add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        let mut node = Expr::add(tokens, manager);
        loop {
            if tokens.is_equal("<") {
                node = Expr::new_binary_node(BinaryKind::Les, node, Expr::add(tokens, manager));
                continue;
            }
            if tokens.is_equal("<=") {
                node = Expr::new_binary_node(BinaryKind::Leq, node, Expr::add(tokens, manager));
                continue;
            }
            if tokens.is_equal(">") {
                node = Expr::new_binary_node(BinaryKind::Les, Expr::add(tokens, manager), node);
                continue;
            }
            if tokens.is_equal(">=") {
                node = Expr::new_binary_node(BinaryKind::Leq, Expr::add(tokens, manager), node);
                continue;
            }
            return node;
        }
    }
    
    // add -> mul ("+" mul | "-" mul)*
    fn add(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        let mut node = Expr::mul(tokens, manager);
        loop {
            if tokens.is_equal("+") {
                node = Expr::new_binary_node(BinaryKind::Add, node, Expr::mul(tokens, manager));
                continue;
            }
            if tokens.is_equal("-") {
                node = Expr::new_binary_node(BinaryKind::Sub, node, Expr::mul(tokens, manager));
                continue;
            }
            return node;
        }
    }
    
    // mul -> unary ("*" unary | "/" unary)*
    fn mul(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        let mut node = Expr::unary(tokens, manager);
        loop {
            if tokens.is_equal("*") {
                node = Expr::new_binary_node(BinaryKind::Mul, node, Expr::unary(tokens, manager));
                continue;
            }
            if tokens.is_equal("/") {
                node = Expr::new_binary_node(BinaryKind::Div, node, Expr::unary(tokens, manager));
                continue;
            }
            return node;
        }
    }

    // unary -> ("+" | "-") unary | primary
    fn unary(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        if tokens.is_equal("+") {
            return Expr::unary(tokens, manager);
        }
        if tokens.is_equal("-") {
            return Expr::new_unary_node(UnaryKind::Neg, Expr::unary(tokens, manager));
        }
        return Expr::primary(tokens, manager);
    }
    
    // primary -> "(" expr ")" | ident ("(" (expr ("," expr)*)? ")")? | num
    fn primary(tokens: &mut TokenGroup, manager: &CreatManager) -> Self {
        if tokens.is_equal("(") {
            let node = Expr::expr(tokens, manager);
            tokens.expected(")");
            return node;
        }

        if let Some(name) = tokens.is_ident() {
            // call
            if let Some((index, num_of_params)) = manager.find_func(name) {
                let name = name.to_string();
                tokens.expected("(");
                let mut args = vec![];
                let mut is_first = true;
                while !tokens.is_equal(")") {
                    if is_first {
                        is_first = false;
                    } else {
                        tokens.expected(",");
                    }
                    args.push(Expr::expr(tokens, manager));
                }
                if args.len() != num_of_params {
                    tokens.previous_token_error(format!("The function \"{}\" needs {} args, but found {} args.", name, num_of_params, args.len()));
                    std::process::exit(1);
                }
                return Expr::new_call_node(index, args);
            }
            // variable
            if let Some(index) = manager.find_local_var(name) {
                return Expr::new_var_node(index);
            }

            let message = format!("The variable name \"{}\" doesn't exists.", name);
            tokens.previous_token_error(message);
            std::process::exit(1);
        }

        if let Some(value) = tokens.is_number() {
            return Expr::new_num_node(value);
        }

        tokens.current_token_error("expected an expression".to_string());
        std::process::exit(1);
    }
}

pub enum StmtKind {
    Ret(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Block(Vec<Stmt>),
    ExprStmt(Expr),
}

pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    // stmt -> "return" expr ";"
    //       | "if" expr ":" stmt ("else" stmt)?
    //       | "{" block_stmt
    //       | expr_stmt
    fn stmt(tokens: &mut TokenGroup, manager: &mut CreatManager) -> Self {
        if tokens.is_equal("return") {
            let node = Self { kind: StmtKind::Ret(Expr::expr(tokens, manager)) };
            tokens.expected(";");
            return node;
        }

        if tokens.is_equal("if") {
            let cond = Expr::expr(tokens, manager);
            tokens.expected(":");
            let then = Stmt::stmt(tokens, manager);
            if tokens.is_equal("else") {
                let els = Stmt::stmt(tokens, manager);
                return Self { kind: StmtKind::If(cond, Box::new(then), Some(Box::new(els))) };
            } else {
                return Self { kind: StmtKind::If(cond, Box::new(then), None) };
            }
        }

        if tokens.is_equal("{") {
            return Stmt::block_stmt(tokens, manager);
        }

        return Stmt::expr_stmt(tokens, manager);
    }

    // block -> stmt* "}"
    fn block_stmt(tokens: &mut TokenGroup, manager: &mut CreatManager) -> Self {
        let mut body = vec![];
        while !tokens.is_equal("}") {
            body.push(Stmt::stmt(tokens, manager));
        }
        return Self { kind: StmtKind::Block(body) };
    }

    // declaration -> (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(tokens: &mut TokenGroup, manager: &mut CreatManager) -> Self {
        let mut body = vec![];
        let mut is_first = true;
        while !tokens.is_equal(";") {
            if is_first {
                is_first = false;
            } else {
                tokens.expected(",");
            }
            let index = manager.declarator(tokens);
            if tokens.is_equal("=") {
                body.push(Self { kind: StmtKind::ExprStmt(Expr::new_assign_node(index, Expr::expr(tokens, manager))) });
            }
        }
        return Self {
            kind: StmtKind::Block(body),
        };
    }

    // expr_stmt -> "dec" declaration |　expr ";"
    fn expr_stmt(tokens: &mut TokenGroup, manager: &mut CreatManager) -> Self {
        if tokens.is_equal("dec") {
            return Stmt::declaration(tokens, manager);
        }
        let node = Self { kind: StmtKind::ExprStmt(Expr::expr(tokens, manager)) };
        tokens.expected(";");
        return node;
    }
}

pub struct DefGroup {
    pub funcs: Vec<Func>,
}

impl DefGroup {
    // func -> ident "(" ")" ":" "i32" "{" block
    fn func(tokens: &mut TokenGroup, funcs: &mut Vec<Func>) -> Func {
        if let Some(name) = tokens.is_ident() {
            let name = name.to_string();
            tokens.expected("(");

            let mut manager = CreatManager { name: name.clone(), locals: vec![ Var::new("return") ],  num_of_params: 0, funcs: funcs };
            let mut is_first = true;
            let mut num_of_params = 0;
            while !tokens.is_equal(")") {
                if is_first {
                    is_first = false;
                } else {
                    tokens.expected(",");
                }
                manager.declarator(tokens);
                num_of_params += 1;
            }

            tokens.expected(":");
            tokens.expected("i32");
            tokens.expected("{");

            manager.num_of_params = num_of_params;
            let body = Stmt::block_stmt(tokens, &mut manager);
    
            return Func {
                name: name,
                locals: manager.locals,
                num_of_params: num_of_params,
                body: body,
            }
        }
        tokens.current_token_error("identifier is expected".to_string());
        std::process::exit(1);
    }

    pub fn new(tokens: &mut TokenGroup) -> Self {
        let mut funcs = vec![];
        while !tokens.is_end() {
            tokens.expected("define");
            let func = DefGroup::func(tokens, &mut funcs);
            funcs.push(func);
        }

        return Self {
            funcs: funcs,
        }
    }
}