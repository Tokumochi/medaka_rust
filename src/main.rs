use std::env;
use inkwell::context::Context;

mod lib;

fn main() {
    let args: Vec<String> = env::args().collect();

    let code: &str = args[1].as_str();

    let mut tokens = lib::TokenGroup::new();
    
    let mut p = 0;

    while p < code.len() {
        let c = &code[p .. p + 1];

        if c == "+" || c == "-" {
            tokens.push(lib::Token::new_opera(c));
            p += 1;
            continue;
        } 

        if c == "0" || c == "1" || c == "2" || c == "3" || c == "4" ||
           c == "5" || c == "6" || c == "7" || c == "8" || c == "9" {
            let mut value: u64 = 0;
            let chara: Vec<char> = code[p..].chars().collect();
            for c in chara {
                if '0' <= c && c <= '9' {
                    value = value * 10 + c as u64 - 48;
                    p += 1;
                } else {
                    break;
                }
            }
            tokens.push(lib::Token::new_num(value));
            continue;
        }

        std::process::exit(1);
    }

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();

    let main_func = module.add_function("main", context.i32_type().fn_type(&[], false), None);

    let basic_block = context.append_basic_block(main_func, "");
    builder.position_at_end(basic_block);

    let mut value = context.i32_type().const_int(tokens.get_num(),false);

    while !tokens.is_end() {
        if tokens.is_equal("+") {
            value = builder.build_int_add(value, context.i32_type().const_int(tokens.get_num(),false), "");
            continue;
        }

        tokens.expected("-");
        value = builder.build_int_sub(value, context.i32_type().const_int(tokens.get_num(),false), "");
    }

    builder.build_return(Some(&value));

    println!("{}", module.print_to_string().to_string());
}
