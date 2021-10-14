use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::values::IntValue;
use crate::parse::{BinaryKind, UnaryKind};

use super::parse::{NodeKind, Node};

pub fn gen_expr<'a>(node: &Node, context: &'a Context, builder: &'a Builder) -> IntValue<'a> {

    match &node.kind {
        NodeKind::Num(value) => {
            return context.i32_type().const_int(*value, false);
        },
        NodeKind::Unary(kind, ohs) => {
            match kind {
                UnaryKind::Neg => return builder.build_int_neg(gen_expr(&ohs, context, builder), ""),
            }
        },
        NodeKind::Binary(kind, lhs, rhs) => {
            let lhs = gen_expr(&lhs, context, builder);
            let rhs = gen_expr(&rhs, context, builder);
            match kind {
                BinaryKind::Add => return builder.build_int_nsw_add(lhs, rhs, ""),
                BinaryKind::Sub => return builder.build_int_nsw_sub(lhs, rhs, ""),
                BinaryKind::Mul => return builder.build_int_nsw_mul(lhs, rhs, ""),
                BinaryKind::Div => return builder.build_int_unsigned_div(lhs, rhs, ""),
            }
        },
    }
}