use std::env;
use inkwell::context::Context;

fn main() {
    let args: Vec<String> = env::args().collect();

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();

    let main_func = module.add_function("main", context.i32_type().fn_type(&[], false), None);

    let basic_block = context.append_basic_block(main_func, "");
    builder.position_at_end(basic_block);

    builder.build_return(Some(&context.i32_type().const_int(args[1].parse().unwrap(), false)));

    println!("{}", module.print_to_string().to_string());
}
