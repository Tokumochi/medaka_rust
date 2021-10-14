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

pub enum NodeKind {
    Num(u64),
    Unary(UnaryKind, Box<Node>),
    Binary(BinaryKind, Box<Node>, Box<Node>),
}

pub struct Node {
    pub kind: NodeKind,
}

impl Node {
    
    fn new_unary_node(kind: UnaryKind, ohs: Self) -> Self {
        Self {
            kind: NodeKind::Unary(kind, Box::new(ohs)),
        }
    }

    fn new_binary_node(kind: BinaryKind, lhs: Self, rhs: Self) -> Self {
        Self {
            kind: NodeKind::Binary(kind, Box::new(lhs), Box::new(rhs)),
        }
    }

    pub fn expr(tokens: &mut tokenize::TokenGroup) -> Self {
        Node::add(tokens)
    }
    
    // add -> mul ("+" mul | "-" mul)*
    fn add(tokens: &mut tokenize::TokenGroup) -> Self {
        let mut node = Node::mul(tokens);
        loop {
            if tokens.is_equal("+") {
                node = Node::new_binary_node(BinaryKind::Add, node, Node::mul(tokens));
                continue;
            }
            if tokens.is_equal("-") {
                node = Node::new_binary_node(BinaryKind::Sub, node, Node::mul(tokens));
                continue;
            }
            break;
        }

        return node;
    }
    
    // mul -> unary ("*" unary | "/" unary)*
    fn mul(tokens: &mut tokenize::TokenGroup) -> Self {
        let mut node = Node::unary(tokens);
        loop {
            if tokens.is_equal("*") {
                node = Node::new_binary_node(BinaryKind::Mul, node, Node::unary(tokens));
                continue;
            }
            if tokens.is_equal("/") {
                node = Node::new_binary_node(BinaryKind::Div, node, Node::unary(tokens));
                continue;
            }
            break;
        }

        return node;
    }

    // unary -> ("+" | "-") unary | primary
    fn unary(tokens: &mut tokenize::TokenGroup) -> Self {
        if tokens.is_equal("+") {
            return Node::unary(tokens);
        }
        if tokens.is_equal("-") {
            return Node::new_unary_node(UnaryKind::Neg, Node::unary(tokens));
        }
        return Node::primary(tokens);
    }
    
    // primary -> "(" expr ")" | num
    fn primary(tokens: &mut tokenize::TokenGroup) -> Self {
        if tokens.is_equal("(") {
            let node = Node::expr(tokens);
            tokens.expected(")");
            return node;
        }

        Self {
            kind: NodeKind::Num(tokens.get_num()),
        }
    }
}
