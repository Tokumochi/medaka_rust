use super::tokenize::Token;
use super::tokenize::TokenGroup;

#[derive(PartialEq, PartialOrd, Clone, Copy)]
pub enum IntType {
    Int8,
    Int32,
}

#[derive(PartialEq, PartialOrd, Clone, Copy)]
pub enum Type {
    Int(IntType),
    Struct(usize),
}

impl Type {
    // decl_spec -> "i8" | "32" | struct_name
    fn decl_spec(tokens: &mut TokenGroup, strucs: &Vec<Struct>) -> Self {
        if tokens.is_equal("i8") {
            return Self::Int(IntType::Int8);
        }
        if tokens.is_equal("i32") {
            return Self::Int(IntType::Int32);
        }
        if let Some(name) = tokens.is_ident() {
            for (index, struc) in strucs.iter().enumerate() {
                if struc.name == name {
                    return Self::Struct(index);
                }
            }
        }
        tokens.current_token_error("type name is expected".to_string());
        std::process::exit(1);
    }
}

pub struct Struct {
    name: String,
    pub members: Vec<(String, Type)>,
}

impl Struct {
    // decl_member -> ident ":" decl_spec
    fn decl_member(&mut self, tokens: &mut TokenGroup, strucs: &Vec<Struct>) {
        if let Some(name) = tokens.is_ident() {
            let name = name.to_string();
            for (member_name, _) in &self.members {
                if *member_name == name {
                    tokens.previous_token_error(format!("The member name \"{}\" already exists.", name));
                    std::process::exit(1);
                }
            }
            tokens.expected(":");
            self.members.push((name.to_string(), Type::decl_spec(tokens, strucs)));
        } else {
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
    }
}

#[derive(PartialEq)]
pub struct Var {
    pub name: String,
    pub typed: Type,
}

impl Var {

    fn new(name: String, typed: Type) -> Self {
        Self {
            name: name,
            typed: typed,
        }
    }
}

pub struct Func {
    pub name: String,
    pub typed: Type,
    pub num_of_params: usize,
    pub locals: Vec<Var>,
    pub body: Stmt,
}

// Manager of Function Creation
struct CreatManager<'a> {
    name: String,
    typed: Type,
    num_of_params: usize,
    locals: Vec<Var>,
    strucs: &'a Vec<Struct>,
    funcs: &'a Vec<Func>,
}

impl<'a> CreatManager<'a> {

    // return (index of var, type of var)
    fn find_local_var(&self, name: &str) -> Option<(usize, Type)> {
        for (index, var) in self.locals.iter().enumerate() {
            if var.name == name {
                return Some((index, var.typed));
            }
        }
        return None;
    }

    // return (index of func, type of func, param vars)
    fn find_func(&self, name: &str) -> Option<(usize, Type, &[Var])> {
        for (index, func) in self.funcs.iter().enumerate() {
            if func.name.as_str() == name {
                return Some((index, func.typed, &func.locals[..func.num_of_params]));
            }
        }
        if name == self.name {
            return Some((self.funcs.len(), self.typed, &self.locals[..self.num_of_params]));
        }
        return None;
    }

    fn new_local_var(&mut self, name: String, typed: Type) -> usize {
        let var = Var::new(name, typed);
        self.locals.push(var);
        return self.locals.len() - 1;
    }

    // declarator -> ident ":" decl_spec
    fn declarator(&mut self, tokens: &mut TokenGroup) -> (usize, Type) {
        if let Some(name) = tokens.is_ident() {
            let name = name.to_string();
            if self.find_local_var(name.as_str()) == None && self.find_func(name.as_str()) == None && self.name != name {
                tokens.expected(":");
                let typed = Type::decl_spec(tokens, self.strucs);
                let index = self.new_local_var(name, typed);
                return (index, typed);
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
    Sext,
    Trunc,
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
    pub typed: Type,
}

impl Expr {

    fn new_num_node(value: u64) -> Self {
        Self {
            kind: ExprKind::Num(value),
            typed: Type::Int(IntType::Int32),
        }
    }

    fn new_var_node(index: usize, typed: Type) -> Self {
        Self {
            kind: ExprKind::Var(index),
            typed: typed,
        }
    }

    fn new_call_node(index: usize, typed: Type, args: Vec<Expr>) -> Self {
        Self {
            kind: ExprKind::Call(index, args),
            typed: typed,
        }
    }

    fn new_assign_node(token: &Token, index: usize, typed: Type, rhs: Self) -> Self {
        Self {
            kind: ExprKind::Assign(index, Box::new(Expr::new_type_conv_node(token, typed, rhs))),
            typed: typed,
        }
    }

    fn new_type_conv_node(token: &Token, typed: Type, node: Self) -> Self {
        match typed {
            Type::Int(int_typed) => {
                if let Type::Int(node_int_typed) = node.typed {
                    if int_typed > node_int_typed {
                        return Self { kind: ExprKind::Unary(UnaryKind::Sext, Box::new(node)), typed: typed };
                    } else if int_typed < node_int_typed {
                        return Self { kind: ExprKind::Unary(UnaryKind::Trunc, Box::new(node)), typed: typed };
                    } else {
                        return node;
                    }
                }
            },
            Type::Struct(struct_index) => {
                if let Type::Struct(node_struct_index) = node.typed {
                    if struct_index == node_struct_index {
                        return node;
                    }
                }
            }
        }
        token.token_error("type mismatch".to_string());
        std::process::exit(1);
    }
    
    fn new_unary_node(kind: UnaryKind, ohs: Self) -> Self {
        let typed = ohs.typed.clone();
        Self {
            kind: ExprKind::Unary(kind, Box::new(ohs)),
            typed: typed,
        }
    }

    fn new_binary_node(kind: BinaryKind, lhs: Self, rhs: Self) -> Self {
        let lhs = if lhs.typed > rhs.typed {
            Self { kind: ExprKind::Unary(UnaryKind::Trunc, Box::new(lhs)), typed: rhs.typed, }
        } else {
            lhs
        };
        let rhs = if lhs.typed < rhs.typed {
            Self { kind: ExprKind::Unary(UnaryKind::Trunc, Box::new(rhs)), typed: lhs.typed, }
        } else {
            rhs
        };

        let typed = lhs.typed;
        return Self {
            kind: ExprKind::Binary(kind, Box::new(lhs), Box::new(rhs)),
            typed: typed,
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
                let rhs = Expr::assign(tokens, manager);
                return Expr::new_assign_node(&tokens.get_previous_token(), index, node.typed, rhs);
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
            if let Some((index, typed, params)) = manager.find_func(name) {
                let name = name.to_string();
                tokens.expected("(");
                let mut args = vec![];
                for (index, param) in params.iter().enumerate() {
                    if tokens.is_equal(")") {
                        tokens.previous_token_error(format!("The function \"{}\" needs {} args, but found only {} args.", name, params.len(), index));
                        std::process::exit(1);
                    }
                    if index > 0 {
                        tokens.expected(",");
                    }
                    let arg = Expr::expr(tokens, manager);
                    args.push(Expr::new_type_conv_node(&tokens.get_current_token(), param.typed, arg));
                }
                tokens.expected(")");
                return Expr::new_call_node(index, typed, args);
            }
            // variable
            if let Some((index, typed)) = manager.find_local_var(name) {
                return Expr::new_var_node(index, typed);
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
            let ret = Expr::expr(tokens, manager);
            let node = Self { kind: StmtKind::Ret(Expr::new_type_conv_node(&tokens.get_previous_token(), manager.typed, ret)) };
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
            let (index, typed) = manager.declarator(tokens);
            if tokens.is_equal("=") {
                let rhs = Expr::expr(tokens, manager);
                body.push(Self { kind: StmtKind::ExprStmt(Expr::new_assign_node(&tokens.get_previous_token(), index, typed, rhs)) });
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
    pub strucs: Vec<Struct>,
    pub funcs: Vec<Func>,
}

impl DefGroup {

    fn is_exist_struct(strucs: &Vec<Struct>, name: &str) -> bool {
        if let Some(_) = strucs.iter().find(|&struc| struc.name == name) {
            return true;
        } else {
            return false;
        }
    }

    fn is_exist_func(funcs: &Vec<Func>, name: &str) -> bool {
        if let Some(_) = funcs.iter().find(|&func| func.name == name) {
            return true;
        } else {
            return false;
        }
    }

    // struc -> ident "{" (decl_member ("," decl_member)*)? "}"
    fn struc(tokens: &mut TokenGroup, funcs: &Vec<Func>, strucs: &Vec<Struct>) -> Struct {
        if let Some(name) = tokens.is_ident() {
            if DefGroup::is_exist_struct(strucs, name) || DefGroup::is_exist_func(funcs, name) {
                let message = format!("The struct name \"{}\" already exists.", name);
                tokens.previous_token_error(message);
                std::process::exit(1);
            }

            let name = name.to_string();
            tokens.expected("{");

            let mut struc = Struct { name: name, members: vec![] };
            let mut is_first = true;
            while !tokens.is_equal("}") {
                if is_first {
                    is_first = false;
                } else {
                    tokens.expected(",");
                }
                struc.decl_member(tokens, &strucs);
            }

            return struc;
        }
        tokens.current_token_error("identifier is expected".to_string());
        std::process::exit(1);
    }
    // func -> ident "(" (declarator ("," declarator)*)? ")" ":" decl_spec "{" block
    fn func(tokens: &mut TokenGroup, funcs: &Vec<Func>, strucs: &Vec<Struct>) -> Func {
        if let Some(name) = tokens.is_ident() {
            if DefGroup::is_exist_struct(strucs, name) || DefGroup::is_exist_func(funcs, name) {
                let message = format!("The function name \"{}\" already exists.", name);
                tokens.previous_token_error(message);
                std::process::exit(1);
            }

            let name = name.to_string();
            tokens.expected("(");

            let mut manager = CreatManager { name: name.clone(), typed: Type::Int(IntType::Int32), num_of_params: 0, locals: vec![], funcs: funcs, strucs: strucs };
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
            let typed = Type::decl_spec(tokens, strucs);

            tokens.expected("{");
            manager.typed = typed;
            manager.num_of_params = num_of_params;
            let body = Stmt::block_stmt(tokens, &mut manager);
    
            return Func {
                name: name,
                num_of_params: num_of_params,
                typed: typed,
                locals: manager.locals,
                body: body,
            }
        }
        tokens.current_token_error("identifier is expected".to_string());
        std::process::exit(1);
    }

    pub fn new(tokens: &mut TokenGroup) -> Self {
        let mut strucs = vec![];
        let mut funcs = vec![];
        while !tokens.is_end() {
            if tokens.is_equal("struct") {
                let struc = DefGroup::struc(tokens, &funcs, &mut strucs);
                strucs.push(struc);
                continue;
            }
            tokens.expected("define");
            let func = DefGroup::func(tokens, &funcs, &strucs);
            funcs.push(func);
        }

        return Self {
            strucs: strucs,
            funcs: funcs,
        }
    }
}