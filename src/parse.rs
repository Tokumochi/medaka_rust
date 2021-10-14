use super::tokenize;

#[derive(PartialEq)]
pub enum NodeKind {
    ADD,
    SUB,
    MUL,
    DIV,
    NEG,
    NUM(u64),
}

pub struct Node {
    pub kind: NodeKind,
    pub lhs: Option<Box<Node> >,
    pub rhs: Option<Box<Node> >,
}

impl Node {

    fn new_both_hands(kind: NodeKind, lhs: Self, rhs: Self) -> Self {
        Self {
            kind: kind,
            lhs: Some(Box::new(lhs)),
            rhs: Some(Box::new(rhs)),
        }
    }

    fn new_one_hand(kind: NodeKind, lhs: Self) -> Self {
        Self {
            kind: kind,
            lhs: Some(Box::new(lhs)),
            rhs: None,
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
                node = Node::new_both_hands(NodeKind::ADD, node, Node::mul(tokens));
                continue;
            }
            if tokens.is_equal("-") {
                node = Node::new_both_hands(NodeKind::SUB, node, Node::mul(tokens));
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
                node = Node::new_both_hands(NodeKind::MUL, node, Node::unary(tokens));
                continue;
            }
            if tokens.is_equal("/") {
                node = Node::new_both_hands(NodeKind::DIV, node, Node::unary(tokens));
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
            return Node::new_one_hand(NodeKind::NEG, Node::unary(tokens));
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
            kind: NodeKind::NUM(tokens.get_num()),
            lhs: None,
            rhs: None,
        }
    }
}
