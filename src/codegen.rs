use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::IntPredicate::{EQ, NE, SLT, SLE};
use inkwell::types::{self, StructType, BasicType, BasicTypeEnum};

use super::parse::{IntType, Type, StorageKind, UnaryKind, BinaryKind, ExprKind, Expr, StmtKind, Stmt, DefGroup};

fn gen_type<'a>(typed: &Type, strucs: &Vec<StructType<'a>>, context: &'a Context) -> BasicTypeEnum<'a> {
    match typed {
        Type::Int(typed) => {
            match typed {
                IntType::Int8 => return context.i8_type().into(),
                IntType::Int32 => return context.i32_type().into(),
            }
        },
        Type::Struct(index) => return strucs[*index].into(),
    }
}

fn gen_int_type<'a>(typed: &Type, strucs: &Vec<StructType<'a>>, context: &'a Context) -> types::IntType<'a> {
    let typed = gen_type(typed, strucs, context);
    if let BasicTypeEnum::IntType(int_typed) = typed {
        return int_typed;
    }
    eprintln!("What's happening?");
    std::process::exit(1);
}

fn gen_storage<'a>(storage_kind: &StorageKind, locals: &Vec<PointerValue<'a> >, context: &'a Context, builder: &'a Builder) -> PointerValue<'a> {
    match storage_kind {
        StorageKind::Var(var_index) => return locals[*var_index],
        StorageKind::Member(member_index, storage_kind) => {
            let ordered_indexes = [context.i32_type().const_int(0, false), context.i32_type().const_int(*member_index as u64, false)];
            unsafe {
                return builder.build_in_bounds_gep(gen_storage(storage_kind, locals, context, builder), &ordered_indexes, "");
            }
        }
    }
}

fn gen_expr<'a>(node: &Expr, locals: &Vec<PointerValue<'a> >, strucs: &Vec<types::StructType<'a>>, funcs: &Vec<FunctionValue<'a>>, context: &'a Context, builder: &'a Builder) -> BasicValueEnum<'a> {

    match &node.kind {
        ExprKind::Num(value) => {
            return context.i32_type().const_int(*value, false).into();
        },
        ExprKind::Storage(storage) => {
            return builder.build_load(gen_storage(storage, locals, context, builder), "");
        }
        ExprKind::Call(index, args) => {
            if funcs[*index].count_params() as usize == args.len() {
                let mut arg_values = vec![];
                for arg in args {
                    arg_values.push(gen_expr(arg, locals, strucs, funcs, context, builder).as_basic_value_enum());
                }
                if let Some(value) = builder.build_call(funcs[*index], &arg_values, "").try_as_basic_value().left() {
                    return value;
                }
            }
            eprintln!("What's happening!?");
            std::process::exit(1);
        },
        ExprKind::Assign(storage, rhs) => {
            let rhs = gen_expr(&rhs, locals, strucs, funcs, context, builder);
            builder.build_store(gen_storage(storage, locals, context, builder), rhs);
            return rhs;
        }
        ExprKind::Unary(kind, ohs) => {
            if let BasicValueEnum::IntValue(ohs) = gen_expr(&ohs, locals, strucs, funcs, context, builder) {
                match kind {
                    UnaryKind::Sext => return builder.build_int_s_extend(ohs, gen_int_type(&node.typed, strucs, context), "").into(),
                    UnaryKind::Trunc => return builder.build_int_truncate(ohs, gen_int_type(&node.typed, strucs, context), "").into(),
                    UnaryKind::Neg => return builder.build_int_neg(ohs, "").into(),
                }
            }
            eprintln!("What's happening!?");
            std::process::exit(1);
        },
        ExprKind::Binary(kind, lhs, rhs) => {
            if let BasicValueEnum::IntValue(lhs) = gen_expr(&lhs, locals, strucs, funcs,  context, builder) {
                if let BasicValueEnum::IntValue(rhs) = gen_expr(&rhs, locals, strucs, funcs,  context, builder) {
                    match kind {
                        BinaryKind::Equ => return builder.build_int_z_extend(builder.build_int_compare(EQ, lhs, rhs, ""), context.i32_type(), "").into(),
                        BinaryKind::Neq => return builder.build_int_z_extend(builder.build_int_compare(NE, lhs, rhs, ""), context.i32_type(), "").into(),
                        BinaryKind::Les => return builder.build_int_z_extend(builder.build_int_compare(SLT, lhs, rhs, ""), context.i32_type(), "").into(),
                        BinaryKind::Leq => return builder.build_int_z_extend(builder.build_int_compare(SLE, lhs, rhs, ""), context.i32_type(), "").into(),
                        BinaryKind::Add => return builder.build_int_nsw_add(lhs, rhs, "").into(),
                        BinaryKind::Sub => return builder.build_int_nsw_sub(lhs, rhs, "").into(),
                        BinaryKind::Mul => return builder.build_int_nsw_mul(lhs, rhs, "").into(),
                        BinaryKind::Div => return builder.build_int_unsigned_div(lhs, rhs, "").into(),
                    }
                }
            }
            eprintln!("What's happening!?");
            std::process::exit(1);
        },
    }
}

// getting "return" and return true
fn gen_stmt<'a>(node: Stmt, ret_value: PointerValue<'a>, locals: &Vec<PointerValue<'a> >, strucs: &Vec<types::StructType<'a>>, funcs: &Vec<FunctionValue>, func_value: FunctionValue, ret_block: BasicBlock, context: &'a Context, builder: &'a Builder) -> bool {

    match node.kind {
        StmtKind::Ret(expr) => {
            builder.build_store(ret_value, gen_expr(&expr, locals, strucs, funcs, context, builder));
            builder.build_unconditional_branch(ret_block);
            return true;
        },
        StmtKind::If(cond, then, els) => {
            let then_block = context.append_basic_block(func_value, "");
            let end_block = context.append_basic_block(func_value, "");
            if let BasicValueEnum::IntValue(cond) = gen_expr(&cond, locals, strucs, funcs, context, builder) {
                let comp = builder.build_int_compare(EQ, cond, context.i32_type().const_int(0, false), "");

                if let Some(els) = els {
                    let else_block = context.append_basic_block(func_value, "");
                    builder.build_conditional_branch(comp, else_block, then_block);
                    builder.position_at_end(else_block);
                    if !gen_stmt(*els, ret_value, locals, strucs, funcs, func_value, ret_block, context, builder) {
                        builder.build_unconditional_branch(end_block);
                    }
                } else {
                    builder.build_conditional_branch(comp, end_block, then_block);
                }
    
                builder.position_at_end(then_block);
                if !gen_stmt(*then, ret_value, locals, strucs, funcs, func_value, ret_block, context, builder) {
                    builder.build_unconditional_branch(end_block);
                }
    
                builder.position_at_end(end_block);
                return false;
            }
            eprintln!("What's happening!?");
            std::process::exit(1);
        },
        StmtKind::Block(body) => {
            for stmt in body {
                if gen_stmt(stmt, ret_value, locals, strucs, funcs, func_value, ret_block, context, builder) {
                    return true;
                }
            }
            return false;
        },
        StmtKind::ExprStmt(expr) => {
            gen_expr(&expr, locals, strucs, funcs, context, builder);
            return false;
        },
    }
}

pub fn codegen<'a>(defs: DefGroup) {

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();

    let mut strucs: Vec<StructType> = vec![];

    for struc in defs.strucs {
        let mut fields = vec![];
        for (_, typed) in struc.members {
            match typed {
                Type::Int(typed) => {
                    match typed {
                        IntType::Int8 => fields.push(context.i8_type().into()),
                        IntType::Int32 => fields.push(context.i32_type().into()),
                    }
                },
                Type::Struct(index) => {
                    fields.push(strucs[index].into());
                },
            }
        }
        strucs.push(context.struct_type(&fields, false))
    }

    let mut funcs = vec![];

    for func in defs.funcs {
        let mut param_types = vec![];
        for index in 0..func.num_of_params {
            param_types.push(gen_type(&func.locals[index].typed, &strucs, &context).into());
        }

        let func_value = module.add_function(&func.name, gen_type(&func.typed, &strucs, &context).fn_type(&param_types, false), None);
        funcs.push(func_value);

        let basic_block = context.append_basic_block(func_value, "");
        builder.position_at_end(basic_block);

        let ret_value = builder.build_alloca(gen_type(&func.typed, &strucs, &context), "return");
        let mut locals = vec![];
        for local in func.locals {
            locals.push(builder.build_alloca(gen_type(&local.typed, &strucs, &context), local.name.as_str()));
        }

        for (index, param) in func_value.get_param_iter().enumerate() {
            if let BasicValueEnum::IntValue(param) = param {
                builder.build_store(locals[index], param);
            } else {
                eprintln!("What's happening!?");
            }
        }
    
        let ret_block = context.append_basic_block(func_value, "");
        if !gen_stmt(func.body, ret_value, &locals, &strucs, &funcs, func_value, ret_block, &context, &builder) {
            builder.build_unconditional_branch(ret_block);
        }
    
        builder.position_at_end(ret_block);    
        builder.build_return(Some(&builder.build_load(ret_value, "")));
    }

    println!("{}", module.print_to_string().to_string());
}