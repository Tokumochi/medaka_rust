use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::values::{IntValue, PointerValue, BasicValueEnum};
use crate::parse::{BinaryKind, UnaryKind};
use super::parse::{ExprKind, Expr, StmtKind, Stmt, Func};

fn gen_expr<'a>(node: &Expr, locals: &Vec<PointerValue<'a> >, context: &'a Context, builder: &'a Builder) -> IntValue<'a> {

    match &node.kind {
        ExprKind::Num(value) => {
            return context.i32_type().const_int(*value, false);
        },
        ExprKind::Var(index) => {
            if let BasicValueEnum::IntValue(var) = builder.build_load(locals[*index], "") {
                return var;
            }
            eprintln!("What's happening!?");
            std::process::exit(1);
        },
        ExprKind::Assign(lhs, rhs) => {
            if let ExprKind::Var(index) = lhs.kind {
                let rhs = gen_expr(&rhs, locals, context, builder);
                builder.build_store(locals[index], rhs);
                return rhs;
            }
            eprintln!("The left side must be variable.");
            std::process::exit(1);
        }
        ExprKind::Unary(kind, ohs) => {
            let ohs = gen_expr(&ohs, locals, context, builder);
            match kind {
                UnaryKind::Neg => return builder.build_int_neg(ohs, ""),
            }
        },
        ExprKind::Binary(kind, lhs, rhs) => {
            let lhs = gen_expr(&lhs, locals, context, builder);
            let rhs = gen_expr(&rhs, locals, context, builder);
            match kind {
                BinaryKind::Add => return builder.build_int_nsw_add(lhs, rhs, ""),
                BinaryKind::Sub => return builder.build_int_nsw_sub(lhs, rhs, ""),
                BinaryKind::Mul => return builder.build_int_nsw_mul(lhs, rhs, ""),
                BinaryKind::Div => return builder.build_int_unsigned_div(lhs, rhs, ""),
            }
        },
    }
}

fn gen_stmt<'a>(node: Stmt, locals: &Vec<PointerValue<'a> >, context: &'a Context, builder: &'a Builder) {

    match node.kind {
        StmtKind::Ret(expr) => {
            builder.build_return(Some(&gen_expr(&expr, locals, context, builder)));
        },
        StmtKind::ExprStmt(expr) => {
            gen_expr(&expr, &locals, context, builder);
        },
    }
}

pub fn codegen<'a>(func: Func, context: &'a Context, builder: &'a Builder) {

    let mut locals = vec![];

    for local in func.locals {
        locals.push(builder.build_alloca(context.i32_type(), local.name.as_str()));
    }

    for stmt in func.body {
        gen_stmt(stmt, &locals, context, builder);
    }
}