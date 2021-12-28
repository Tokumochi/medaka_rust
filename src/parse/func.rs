use crate::tokenize::{Token, TokenGroup};
use super::typed::{IntType, Type};
use super::struc::Struct;
use super::define::Skill;

#[derive(PartialEq)]
pub struct Var {
    pub name: String,
    pub default: (usize, Type),      // (index of default-var, type of default-var)
    pub extend: Option<(usize, Type)>, // (index of extend-var, type of extend-var) if exist
}

// local variable scope in function definition
pub struct VarScope {
    vars: Vec<Var>,
    child: Option<Box<VarScope>>,
}

impl VarScope {

    // return (index of var, type of var)
    fn find_scope_var(&self, name: &str) -> Option<(usize, Type)> {
        for var in &self.vars {
            if var.name == name {
                let (number, typed) = var.default;
                return Some((number, typed));
            }
        }

        if let Some(child_scope) = &self.child {
            return child_scope.find_scope_var(name)
        }
        return None;
    }

    fn new_scope_var(&mut self, name: String, default_number: usize, default_typed: Type) {
        if let Some(child_scope) = &mut self.child {
            child_scope.new_scope_var(name, default_number, default_typed);
            return;
        }

        self.vars.push(Var {
            name: name,
            default: (default_number, default_typed),
            extend: None,
        });
    }

    fn extend_scope_var(&mut self, name: String, extend_number: usize, extend_typed: Type) {
        for var in &mut self.vars {
            if var.name == name {
                var.extend = Some((extend_number, extend_typed));
                return;
            }
        }

        if let Some(child_scope) = &mut self.child {
            child_scope.extend_scope_var(name, extend_number, extend_typed)
        }
    }

    fn enter_scope(&mut self) {
        if let Some(child_scope) = &mut self.child {
            child_scope.enter_scope();
            return;
        }

        let scope = VarScope {
            vars: vec![],
            child: None,
        };
        self.child = Some(Box::new(scope));
    }

    fn leave_scope(&mut self, locals: &mut Vec<Var>) {
        if let Some(child_scope) = &mut self.child {
            if let Some(_) = child_scope.child {
                child_scope.leave_scope(locals);
            } else {
                // add super variables to locals
                locals.append(&mut child_scope.vars);
                self.child = None;
            }
            return;
        }
        eprintln!("What's happening in leaving scope of mochi code!?");
        std::process::exit(1);
    }
}

struct SkillScope<'a> {
    skill: Option<&'a Skill>,
}

impl<'a> SkillScope<'a> {

    fn enter_scope(&mut self, skill: &'a Skill) {
        self.skill = Some(skill);
    }

    fn leave_scope(&mut self) {
        self.skill = None;
    }

    fn find_typed(&self, typed: Type) -> Option<Type> {
        if let Some(skill) = self.skill {
            for (target, extend_index) in &skill.extends {
                if typed == *target {
                    return Some(Type::Struct(*extend_index));
                }
            }
        }
        return None;
    }
}

// Parsing Manager
struct ParsingManager<'a> {
    name: String,
    typed: Type,
    top_var_scope: &'a mut VarScope,
    skill_scope: &'a mut SkillScope<'a>,
    num_of_locals: usize,
    locals: Vec<Var>,
    strucs: &'a Vec<Struct>,
    funcs: &'a Vec<Func>,
    skills: &'a Vec<Skill>,
}

impl<'a> ParsingManager<'a> {

    // return type of struct
    fn find_struc(&self, typed: Type) -> Option<&Struct> {
        if let Type::Struct(struc_index) = typed {
            return Some(&self.strucs[struc_index]);
        }
        return None;
    }

    // return (number of func, type of func, param vars)
    fn find_func(&self, name: &str) -> Option<(usize, Type, &[Var])> {
        // general
        for func in self.funcs {
            if func.name.as_str() == name {
                return Some((func.number, func.typed, &func.params));
            }
        }
        // skill
        if let Some(skill) = self.skill_scope.skill {
            for func in &skill.funcs {
                if func.name.as_str() == name {
                    return Some((func.number, func.typed, &func.params));
                }
            }
        }
        // self
        if name == self.name {
            return Some((self.funcs.len(), self.typed, &self.top_var_scope.vars));
        }
        return None;
    }

    fn find_and_enter_skill(&mut self, tokens: &mut TokenGroup, name: &str) {
        for skill in self.skills {
            if skill.name.as_str() == name {
                self.skill_scope.enter_scope(skill);
                return;
            }
        }
        tokens.previous_token_error(format!("The skill name \"{}\" doesn't exists.", name));
        std::process::exit(1);
    }

    // declarator -> ident ":" type_spec
    fn declarator(&mut self, tokens: &mut TokenGroup) -> (usize, Type) {
        if let Some(name) = tokens.is_ident() {
            let name = name.to_string();
            if self.top_var_scope.find_scope_var(name.as_str()) == None && self.find_func(name.as_str()) == None && self.name != name {
                tokens.expected(":");

                let default_var_index = self.num_of_locals;
                let typed = Type::type_spec(tokens, self.strucs);
                let extend_typed = self.skill_scope.find_typed(typed);

                // add default variable to var scope
                self.top_var_scope.new_scope_var(name.clone(), self.num_of_locals, typed);
                self.num_of_locals += 1;
                // add extend variable to var scope if Super is expected
                if let Some(extend_typed) = extend_typed {
                    self.top_var_scope.extend_scope_var(name, self.num_of_locals, extend_typed);
                    self.num_of_locals += 1;
                }

                return (default_var_index, typed);
            }
            tokens.previous_token_error(format!("The variable name \"{}\" already exists.", name));
            std::process::exit(1);
        }
        tokens.current_token_error("identifier is expected".to_string());
        std::process::exit(1);
    }
}

pub enum StorageKind {
    Var(usize),                      // (index of variable)
    Member(usize, Box<StorageKind>), // (index of the struct member, kind of the struct)
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
    Num(u64),                                 // (value)
    Storage(StorageKind),                     // (storage kind, index of variable)
    Call(usize, Vec<Expr>),                   // (index of function, Expr arguments)
    Unary(UnaryKind, Box<Expr>),              // (unary kind, one-hand-side Expr)
    Binary(BinaryKind, Box<Expr>, Box<Expr>), // (binary kind, left-hand-side Expr, right-hand-side Expr)
    Assign(StorageKind, Box<Expr>),           // (storage kind, rhs-hand-side Expr)
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

    fn new_storage_node(storage_kind: StorageKind, typed: Type) -> Self {
        Self {
            kind: ExprKind::Storage(storage_kind),
            typed: typed,
        }
    }

    fn new_call_node(index: usize, typed: Type, args: Vec<Expr>) -> Self {
        Self {
            kind: ExprKind::Call(index, args),
            typed: typed,
        }
    }

    fn new_assign_node(token: &Token, lhs: Self, rhs: Self) -> Self {
        if let ExprKind::Storage(storage) = lhs.kind {
            return Self {
                kind: ExprKind::Assign(storage, Box::new(Expr::new_type_conv_node(token, lhs.typed, rhs))),
                typed: lhs.typed,
            }
        }
        token.token_error(String::from("The left side must be variable or struct member."));
        std::process::exit(1);
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
        Self {
            typed: ohs.typed,
            kind: ExprKind::Unary(kind, Box::new(ohs)),
        }
    }

    fn new_binary_node(token: &Token, kind: BinaryKind, lhs: Self, rhs: Self) -> Self {
        let mut lhs = lhs;
        let mut rhs = rhs;
        if let Type::Int(lhs_int_typed) = lhs.typed {
            if let Type::Int(rhs_int_typed) = rhs.typed {
                if lhs_int_typed > rhs_int_typed {
                    lhs = Self { kind: ExprKind::Unary(UnaryKind::Trunc, Box::new(lhs)), typed: rhs.typed, };
                } else  if lhs_int_typed < rhs_int_typed {
                    rhs = Self { kind: ExprKind::Unary(UnaryKind::Trunc, Box::new(rhs)), typed: lhs.typed, }
                }
                return Self {
                    typed: lhs.typed,
                    kind: ExprKind::Binary(kind, Box::new(lhs), Box::new(rhs)),
                };
            }
        }
        token.token_error("type mismatch".to_string());
        std::process::exit(1);
    }

    // expr -> assign
    fn expr(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
        Expr::assign(tokens, manager)
    }

    // assign -> equality ("=" assign)?
    fn assign(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
        let lhs = Expr::equality(tokens, manager);
        if tokens.is_equal("=") {
            let rhs = Expr::assign(tokens, manager);
            return Expr::new_assign_node(&tokens.get_previous_token(), lhs, rhs);
        }
        return lhs;
    }

    // equality -> relational ("==" relational | "!=" relational)*
    fn equality(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
        let mut node = Expr::relational(tokens, manager);
        loop {
            if tokens.is_equal("==") {
                let rhs = Expr::relational(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Equ, node, rhs);
                continue;
            }
            if tokens.is_equal("!=") {
                let rhs = Expr::relational(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Neq, node, rhs);
                continue;
            }
            return node;
        }
    }

    // relational -> add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
        let mut node = Expr::add(tokens, manager);
        loop {
            if tokens.is_equal("<") {
                let rhs = Expr::add(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Les, node, rhs);
                continue;
            }
            if tokens.is_equal("<=") {
                let rhs = Expr::add(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Leq, node, rhs);
                continue;
            }
            if tokens.is_equal(">") {
                let lhs = Expr::add(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Les, lhs, node);
                continue;
            }
            if tokens.is_equal(">=") {
                let lhs = Expr::add(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Leq, lhs, node);
                continue;
            }
            return node;
        }
    }
    
    // add -> mul ("+" mul | "-" mul)*
    fn add(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
        let mut node = Expr::mul(tokens, manager);
        loop {
            if tokens.is_equal("+") {
                let rhs = Expr::mul(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Add, node, rhs);
                continue;
            }
            if tokens.is_equal("-") {
                let rhs = Expr::mul(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Sub, node, rhs);
                continue;
            }
            return node;
        }
    }
    
    // mul -> unary ("*" unary | "/" unary)*
    fn mul(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
        let mut node = Expr::unary(tokens, manager);
        loop {
            if tokens.is_equal("*") {
                let rhs = Expr::unary(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Mul, node, rhs);
                continue;
            }
            if tokens.is_equal("/") {
                let rhs = Expr::unary(tokens, manager);
                node = Expr::new_binary_node(&tokens.get_previous_token(), BinaryKind::Div, node, rhs);
                continue;
            }
            return node;
        }
    }

    // unary -> ("+" | "-") unary | primary
    fn unary(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
        if tokens.is_equal("+") {
            return Expr::unary(tokens, manager);
        }
        if tokens.is_equal("-") {
            let ohs = Expr::unary(tokens, manager);
            if let Type::Int(_) = ohs.typed {
                return Expr::new_unary_node(UnaryKind::Neg, ohs);
            }
            tokens.previous_token_error("negative operation is for int type".to_string());
            std::process::exit(1);
        }
        return Expr::primary(tokens, manager);
    }
    
    // primary -> "(" expr ")" | ident ("(" (expr ("," expr)*)? ")")? | num
    fn primary(tokens: &mut TokenGroup, manager: &ParsingManager) -> Self {
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
                    let (_, default_param_typed) = param.default;
                    args.push(Expr::new_type_conv_node(&tokens.get_previous_token(), default_param_typed, arg));
                }
                tokens.expected(")");
                return Expr::new_call_node(index, typed, args);
            }
            // storage
            if let Some((var_index, typed)) = manager.top_var_scope.find_scope_var(name) {
                // variable
                let mut storage_kind = StorageKind::Var(var_index);
                let mut storage_typed = typed;
                // member
                'period: while tokens.is_equal(".") {
                    if let Some(struc) = manager.find_struc(storage_typed) {
                        if let Some(name) = tokens.is_ident() {
                            for (member_index, (member_name, member_typed)) in struc.members.iter().enumerate() {
                                if name == member_name {
                                    storage_kind = StorageKind::Member(member_index, Box::new(storage_kind));
                                    storage_typed = *member_typed;
                                    continue 'period;
                                }
                            }
                            tokens.previous_token_error(String::from("This member name doesn't exist."));
                            std::process::exit(1);
                        }
                        tokens.current_token_error(String::from("Member name is needed."));
                        std::process::exit(1);
                    }
                    tokens.previous_token_error(String::from("This is not a struct variable."));
                    std::process::exit(1);
                }
                return Expr::new_storage_node(storage_kind, storage_typed);
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
    //       | "|" skill-ident "|" "{" block_stmt
    //       | "{" block_stmt
    //       | expr_stmt
    fn stmt(tokens: &mut TokenGroup, manager: &mut ParsingManager) -> Self {
        if tokens.is_equal("return") {
            let ret = Expr::expr(tokens, manager);
            let node = Self { kind: StmtKind::Ret(Expr::new_type_conv_node(&tokens.get_previous_token(), manager.typed, ret)) };
            tokens.expected(";");
            return node;
        }

        if tokens.is_equal("if") {
            let cond = Expr::expr(tokens, manager);
            if let Type::Int(_) = cond.typed {
                tokens.expected(":");
                let then = Stmt::stmt(tokens, manager);
                if tokens.is_equal("else") {
                    let els = Stmt::stmt(tokens, manager);
                    return Self { kind: StmtKind::If(cond, Box::new(then), Some(Box::new(els))) };
                } else {
                    return Self { kind: StmtKind::If(cond, Box::new(then), None) };
                }
            }
            tokens.previous_token_error("if-condition is for int type".to_string());
            std::process::exit(1);
        }

        if tokens.is_equal("|") {
            if let Some(name) = tokens.is_ident() {
                let name = String::from(name);
                manager.find_and_enter_skill(tokens, name.as_str());

                tokens.expected("|");
                tokens.expected("{");
                let stmts_in_skill = Stmt::block_stmt(tokens, manager);

                manager.skill_scope.leave_scope();
                return stmts_in_skill;
            }
            tokens.current_token_error(String::from("expected skill name"));
            std::process::exit(1);
        }

        if tokens.is_equal("{") {
            return Stmt::block_stmt(tokens, manager);
        }

        return Stmt::expr_stmt(tokens, manager);
    }

    // block -> stmt* "}"
    fn block_stmt(tokens: &mut TokenGroup, manager: &mut ParsingManager) -> Self {
        let mut body = vec![];

        manager.top_var_scope.enter_scope();
        while !tokens.is_equal("}") {
            body.push(Stmt::stmt(tokens, manager));
        }
        manager.top_var_scope.leave_scope(&mut manager.locals);
    
        return Self { kind: StmtKind::Block(body) };
    }

    // declaration -> (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(tokens: &mut TokenGroup, manager: &mut ParsingManager) -> Self {
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
                body.push(Self { kind: StmtKind::ExprStmt(Expr::new_assign_node(&tokens.get_previous_token(), Expr::new_storage_node(StorageKind::Var(index), typed), rhs)) });
            }
        }
        return Self {
            kind: StmtKind::Block(body),
        };
    }

    // expr_stmt -> "dec" declaration |　expr ";"
    fn expr_stmt(tokens: &mut TokenGroup, manager: &mut ParsingManager) -> Self {
        if tokens.is_equal("dec") {
            return Stmt::declaration(tokens, manager);
        }
        let node = Self { kind: StmtKind::ExprStmt(Expr::expr(tokens, manager)) };
        tokens.expected(";");
        return node;
    }
}

pub struct Func {
    pub number: usize,
    pub name: String,
    pub typed: Type,
    pub num_of_locals: usize,
    pub params: Vec<Var>,
    pub locals: Vec<Var>,
    pub body: Stmt,
}

impl Func {
    // func -> "(" (declarator ("," declarator)*)? ")" ":" type_spec "{" block
    pub fn func(tokens: &mut TokenGroup, number: usize, name: String, strucs: &Vec<Struct>, funcs: &Vec<Func>, skills: &Vec<Skill>) -> Func {
        tokens.expected("(");

        let mut top_var_scope = VarScope { vars: vec![], child: None };
        let mut skill_scope = SkillScope { skill: None };
        let mut manager = ParsingManager { name: name.clone(), typed: Type::Int(IntType::Int32), skill_scope: &mut skill_scope, top_var_scope: &mut top_var_scope, num_of_locals: 0, locals: vec![], funcs: funcs, strucs: strucs, skills: skills };

        let mut is_first = true;

        while !tokens.is_equal(")") {
            if is_first {
                is_first = false;
            } else {
                tokens.expected(",");
            }
            manager.declarator(tokens);
        }

        tokens.expected(":");
        let typed = Type::type_spec(tokens, strucs);

        tokens.expected("{");

        manager.typed = typed;

        let body = Stmt::block_stmt(tokens, &mut manager);
        let locals = manager.locals;
    
        return Func {
            number: number,
            name: name,
            typed: typed,
            num_of_locals: manager.num_of_locals,
            params: top_var_scope.vars,
            locals: locals,
            body: body,
        }
    }
    
}