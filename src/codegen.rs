use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::values::IntValue;
use crate::parse::{BinaryKind, UnaryKind};

use super::parse::{ExprKind, Expr, StmtKind, Stmt, Func};

fn gen_expr<'a>(node: &Expr, context: &'a Context, builder: &'a Builder) -> IntValue<'a> {

    match &node.kind {
        ExprKind::Num(value) => {
            return context.i32_type().const_int(*value, false);
        },
        ExprKind::Unary(kind, ohs) => {
            let ohs = gen_expr(ohs, context, builder);
            match kind {
                UnaryKind::Neg => return builder.build_int_neg(ohs, ""),
            }
        },
        ExprKind::Binary(kind, lhs, rhs) => {
            let lhs = gen_expr(lhs, context, builder);
            let rhs = gen_expr(rhs, context, builder);
            match kind {
                BinaryKind::Add => return builder.build_int_nsw_add(lhs, rhs, ""),
                BinaryKind::Sub => return builder.build_int_nsw_sub(lhs, rhs, ""),
                BinaryKind::Mul => return builder.build_int_nsw_mul(lhs, rhs, ""),
                BinaryKind::Div => return builder.build_int_unsigned_div(lhs, rhs, ""),
            }
        },
    }
}

fn gen_stmt<'a>(node: Stmt, context: &'a Context, builder: &'a Builder) {

    match node.kind {
        StmtKind::Ret(expr) => {
            builder.build_return(Some(&gen_expr(&expr, context, builder)));
        },
        StmtKind::ExprStmt(expr) => {
            gen_expr(&expr, context, builder);
        },
    }
}

pub fn codegen<'a>(func: Func, context: &'a Context, builder: &'a Builder) {

    for stmt in func.body {
        gen_stmt(stmt, context, builder);
    }
}