use super::tokenize;

pub enum NodeKind {
    ADD,
    SUB,
    MUL,
    DIV,
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
    
    // mul -> primary ("*" primary | "/" primary)*
    fn mul(tokens: &mut tokenize::TokenGroup) -> Self {
        let mut node = Node::primary(tokens);
        loop {
            if tokens.is_equal("*") {
                node = Node::new_both_hands(NodeKind::MUL, node, Node::mul(tokens));
                continue;
            }
            if tokens.is_equal("/") {
                node = Node::new_both_hands(NodeKind::DIV, node, Node::mul(tokens));
                continue;
            }
            break;
        }

        return node;
    }
    
    // primary -> num
    fn primary(tokens: &mut tokenize::TokenGroup) -> Self {
        Self {
            kind: NodeKind::NUM(tokens.get_num()),
            lhs: None,
            rhs: None,
        }
    }
}
