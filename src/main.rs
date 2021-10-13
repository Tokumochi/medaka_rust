mod tokenize;
mod parse;
mod codegen;

use std::env;
use inkwell::context::Context;
use tokenize::TokenGroup;
use parse::Node;

fn main() {
    let args: Vec<String> = env::args().collect();

    let code: &str = args[1].as_str();

    let mut tokens = TokenGroup::new(code);
    let node = Node::expr(&mut tokens);

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();

    let main_func = module.add_function("main", context.i32_type().fn_type(&[], false), None);

    let basic_block = context.append_basic_block(main_func, "");
    builder.position_at_end(basic_block);

    let expr = codegen::gen_expr(&node, &context, &builder);

    builder.build_return(Some(&expr));

    println!("{}", module.print_to_string().to_string());
}
