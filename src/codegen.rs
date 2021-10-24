use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::IntPredicate::{EQ, NE, SLT, SLE};
use inkwell::types::IntType;
use super::parse::{Type, UnaryKind, BinaryKind, ExprKind, Expr, StmtKind, Stmt, DefGroup};

fn gen_type<'a>(typed: Type, context: &'a Context) -> IntType {
    match typed {
        Type::Int32 => return context.i32_type(),
        Type::Int8 => return context.i8_type(),
    }
}

fn gen_expr<'a>(node: &Expr, locals: &Vec<PointerValue<'a> >, funcs: &Vec<FunctionValue<'a>>, context: &'a Context, builder: &'a Builder) -> IntValue<'a> {

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
        ExprKind::Call(index, args) => {
            if funcs[*index].count_params() as usize == args.len() {
                let mut arg_values = vec![];
                for arg in args {
                    arg_values.push(gen_expr(arg, locals, funcs, context, builder).as_basic_value_enum());
                }
                if let Some(BasicValueEnum::IntValue(value)) = builder.build_call(funcs[*index], &arg_values, "").try_as_basic_value().left() {
                    return value;
                }
            }
            eprintln!("What's happening!?");
            std::process::exit(1);
        },
        ExprKind::Assign(index, rhs) => {
            let rhs = gen_expr(&rhs, locals, funcs, context, builder);
            builder.build_store(locals[*index], rhs);
            return rhs;
        }
        ExprKind::Unary(kind, ohs) => {
            let ohs = gen_expr(&ohs, locals, funcs, context, builder);
            match kind {
                UnaryKind::Sext => return builder.build_int_s_extend(ohs, gen_type(node.typed, context), ""),
                UnaryKind::Trunc => return builder.build_int_truncate(ohs, gen_type(node.typed, context), ""),
                UnaryKind::Neg => return builder.build_int_neg(ohs, ""),
            }
        },
        ExprKind::Binary(kind, lhs, rhs) => {
            let lhs = gen_expr(&lhs, locals, funcs,  context, builder);
            let rhs = gen_expr(&rhs, locals, funcs, context, builder);
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
fn gen_stmt<'a>(node: Stmt, locals: &Vec<PointerValue<'a> >, funcs: &Vec<FunctionValue>, func_value: FunctionValue, ret_block: BasicBlock, context: &'a Context, builder: &'a Builder) -> bool {

    match node.kind {
        StmtKind::Ret(expr) => {
            builder.build_store(locals[0], gen_expr(&expr, locals, funcs, context, builder));
            builder.build_unconditional_branch(ret_block);
            return true;
        },
        StmtKind::If(cond, then, els) => {
            let then_block = context.append_basic_block(func_value, "");
            let end_block = context.append_basic_block(func_value, "");
            let cond = gen_expr(&cond, locals, funcs, context, builder);
            let comp = builder.build_int_compare(EQ, cond, context.i32_type().const_int(0, false), "");

            if let Some(els) = els {
                let else_block = context.append_basic_block(func_value, "");
                builder.build_conditional_branch(comp, else_block, then_block);
                builder.position_at_end(else_block);
                if !gen_stmt(*els, locals, funcs, func_value, ret_block, context, builder) {
                    builder.build_unconditional_branch(end_block);
                }
            } else {
                builder.build_conditional_branch(comp, end_block, then_block);
            }

            builder.position_at_end(then_block);
            if !gen_stmt(*then, locals, funcs, func_value, ret_block, context, builder) {
                builder.build_unconditional_branch(end_block);
            }

            builder.position_at_end(end_block);
            return false;
        },
        StmtKind::Block(body) => {
            for stmt in body {
                if gen_stmt(stmt, locals, funcs, func_value, ret_block, context, builder) {
                    return true;
                }
            }
            return false;
        },
        StmtKind::ExprStmt(expr) => {
            gen_expr(&expr, locals, funcs, context, builder);
            return false;
        },
    }
}

pub fn codegen<'a>(defs: DefGroup) {

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();

    let mut funcs = vec![];

    for func in defs.funcs {
        let mut param_types = vec![];
        for index in 1 .. func.num_of_params + 1 {
            param_types.push(gen_type(func.locals[index].typed, &context).into());
        }

        let func_value = module.add_function(&func.name, gen_type(func.typed, &context).fn_type(&param_types, false), None);
        funcs.push(func_value);

        let basic_block = context.append_basic_block(func_value, "");
        builder.position_at_end(basic_block);

        let mut locals = vec![];
    
        for local in func.locals {
            locals.push(builder.build_alloca(gen_type(local.typed, &context), local.name.as_str()));
        }

        for (index, param) in func_value.get_param_iter().enumerate() {
            if let BasicValueEnum::IntValue(param) = param {
                builder.build_store(locals[index + 1], param);
            } else {
                eprintln!("What's happening!?");
            }
        }
    
        let ret_block = context.append_basic_block(func_value, "");
        if !gen_stmt(func.body, &locals, &funcs, func_value, ret_block, &context, &builder) {
            builder.build_unconditional_branch(ret_block);
        }
    
        builder.position_at_end(ret_block);    
        builder.build_return(Some(&builder.build_load(locals[0], "")));
    }

    println!("{}", module.print_to_string().to_string());
}