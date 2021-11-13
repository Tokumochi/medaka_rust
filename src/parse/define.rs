use crate::tokenize::TokenGroup;
use super::struc::Struct;
use super::func::Func;

fn is_exist_struct(strucs: &Vec<Struct>, name: &str) -> bool {
    if let Some(_) = strucs.iter().find(|&struc| struc.name == name) {
        return true;
    } else {
        return false;
    }
}

fn is_exist_func(funcs: &Vec<Func>, name: &str) -> bool {
    if let Some(_) = funcs.iter().find(|&func| func.name == name) {
        return true;
    } else {
        return false;
    }
}

pub struct DefGroup {
    pub strucs: Vec<Struct>,
    pub funcs: Vec<Func>,
}

impl DefGroup {

    pub fn new(tokens: &mut TokenGroup) -> Self {
        let mut strucs = vec![];
        let mut funcs = vec![];
        while !tokens.is_end() {
            // struct
            if tokens.is_equal("struct") {
                if let Some(name) = tokens.is_ident() {
                    if is_exist_struct(&strucs, name) || is_exist_func(&funcs, name) {
                        let message = format!("The struct name \"{}\" already exists.", name);
                        tokens.previous_token_error(message);
                        std::process::exit(1);
                    }
                    let name = String::from(name);
                    strucs.push(Struct::struc(tokens, String::from(name), &strucs));
                    continue;
                }
                tokens.current_token_error("identifier is expected".to_string());
                std::process::exit(1);
            }
            // define
            if tokens.is_equal("define"){
                if let Some(name) = tokens.is_ident() {
                    if is_exist_struct(&strucs, name) || is_exist_func(&funcs, name) {
                        let message = format!("The function name \"{}\" already exists.", name);
                        tokens.previous_token_error(message);
                        std::process::exit(1);
                    }
                    let name = String::from(name);
                    funcs.push(Func::func(tokens, String::from(name), &funcs, &strucs));
                    continue;
                }
                tokens.current_token_error("identifier is expected".to_string());
                std::process::exit(1);
            }

            tokens.current_token_error(String::from("unvalid define"));
            std::process::exit(1);
        }

        return Self {
            strucs: strucs,
            funcs: funcs,
        }
    }
}