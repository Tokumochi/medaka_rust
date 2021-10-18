use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::IntPredicate::{EQ, NE, SLT, SLE};
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
        ExprKind::Assign(index, rhs) => {
            let rhs = gen_expr(&rhs, locals, context, builder);
            builder.build_store(locals[*index], rhs);
            return rhs;
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
                BinaryKind::Equ => return builder.build_int_z_extend(builder.build_int_compare(EQ, lhs, rhs, ""), context.i32_type(), ""),
                BinaryKind::Neq => return builder.build_int_z_extend(builder.build_int_compare(NE, lhs, rhs, ""), context.i32_type(), ""),
                BinaryKind::Les => return builder.build_int_z_extend(builder.build_int_compare(SLT, lhs, rhs, ""), context.i32_type(), ""),
                BinaryKind::Leq => return builder.build_int_z_extend(builder.build_int_compare(SLE, lhs, rhs, ""), context.i32_type(), ""),
                BinaryKind::Add => return builder.build_int_nsw_add(lhs, rhs, ""),
                BinaryKind::Sub => return builder.build_int_nsw_sub(lhs, rhs, ""),
                BinaryKind::Mul => return builder.build_int_nsw_mul(lhs, rhs, ""),
                BinaryKind::Div => return builder.build_int_unsigned_div(lhs, rhs, ""),
            }
        },
    }
}

// getting "return" and return true
fn gen_stmt<'a>(node: Stmt, locals: &Vec<PointerValue<'a> >, func: FunctionValue, ret_block: BasicBlock, context: &'a Context, builder: &'a Builder) -> bool {

    match node.kind {
        StmtKind::Ret(expr) => {
            builder.build_store(locals[0], gen_expr(&expr, locals, context, builder));
            builder.build_unconditional_branch(ret_block);
            return true;
        },
        StmtKind::If(cond, then, els) => {
            let then_block = context.append_basic_block(func, "");
            let end_block = context.append_basic_block(func, "");
            let cond = gen_expr(&cond, locals, context, builder);
            let comp = builder.build_int_compare(EQ, cond, context.i32_type().const_int(0, false), "");

            if let Some(els) = els {
                let else_block = context.append_basic_block(func, "");
                builder.build_conditional_branch(comp, else_block, then_block);
                builder.position_at_end(else_block);
                if !gen_stmt(*els, locals, func, ret_block, context, builder) {
                    builder.build_unconditional_branch(end_block);
                }
            } else {
                builder.build_conditional_branch(comp, end_block, then_block);
            }

            builder.position_at_end(then_block);
            if !gen_stmt(*then, locals, func, ret_block, context, builder) {
                builder.build_unconditional_branch(end_block);
            }

            builder.position_at_end(end_block);
            return false;
        },
        StmtKind::Block(body) => {
            for stmt in body {
                if gen_stmt(stmt, locals, func, ret_block, context, builder) {
                    return true;
                }
            }
            return false;
        },
        StmtKind::ExprStmt(expr) => {
            gen_expr(&expr, locals, context, builder);
            return false;
        },
    }
}

pub fn codegen<'a>(func: Func) {

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();

    let main_func = module.add_function("main", context.i32_type().fn_type(&[], false), None);
    let basic_block = context.append_basic_block(main_func, "");
    builder.position_at_end(basic_block);

    let mut locals = vec![];

    for local in func.locals {
        locals.push(builder.build_alloca(context.i32_type(), local.name.as_str()));
    }

    let ret_block = context.append_basic_block(main_func, "");
    if !gen_stmt(func.body, &locals, main_func, ret_block, &context, &builder) {
        builder.build_unconditional_branch(ret_block);
    }

    builder.position_at_end(ret_block);    
    builder.build_return(Some(&builder.build_load(locals[0], "")));

    println!("{}", module.print_to_string().to_string());
}