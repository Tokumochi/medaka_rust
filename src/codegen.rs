use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::values::IntValue;
use super::parse::{NodeKind, Node};

pub fn gen_expr<'a>(node: &Node, context: &'a Context, builder: &'a Builder) -> IntValue<'a> {
    if let NodeKind::NUM(value) = node.kind {
        return context.i32_type().const_int(value, false);
    }

    let lhs = match &node.lhs {
        Some(node) => gen_expr(&node, context, builder),
        None => std::process::exit(1),
    };
    let rhs = match &node.rhs {
        Some(node) => gen_expr(&node, context, builder),
        None => std::process::exit(1),
    };

    match &node.kind {
        NodeKind::ADD => builder.build_int_nsw_add(lhs, rhs, ""),
        NodeKind::SUB => builder.build_int_nsw_sub(lhs, rhs, ""),
        NodeKind::MUL => builder.build_int_nsw_mul(lhs, rhs, ""),
        NodeKind::DIV => builder.build_int_unsigned_div(lhs, rhs, ""),
        _ => std::process::exit(1),
    }
}