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

struct Skill {
    name: String,
    funcs: Vec<Func>,
}

pub struct DefGroup {
    pub strucs: Vec<Struct>,
    pub funcs: Vec<Func>,
}

impl DefGroup {

    fn struc_def(tokens: &mut TokenGroup, strucs: &Vec<Struct>, funcs: &Vec<Func>) -> Option<Struct> {
        if tokens.is_equal("struct") {
            if let Some(name) = tokens.is_ident() {
                if is_exist_struct(&strucs, name) || is_exist_func(&funcs, name) {
                    let message = format!("The struct name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
                let name = String::from(name);
                return Some(Struct::struc(tokens, String::from(name), &strucs));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn func_def(tokens: &mut TokenGroup, strucs: &Vec<Struct>, funcs: &Vec<Func>) -> Option<Func> {
        if tokens.is_equal("define"){
            if let Some(name) = tokens.is_ident() {
                if is_exist_struct(&strucs, name) || is_exist_func(&funcs, name) {
                    let message = format!("The function name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
                let name = String::from(name);
                return Some(Func::func(tokens, String::from(name), &funcs, &strucs));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn skill_def(tokens: &mut TokenGroup, strucs: &Vec<Struct>, funcs: &Vec<Func>) -> Option<Skill> {
        if tokens.is_equal("skill"){
            if let Some(name) = tokens.is_ident() {
                if is_exist_struct(&strucs, name) || is_exist_func(&funcs, name) {
                    let message = format!("The skill name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
                let name = String::from(name);
                return Some(Self::define_in_skill(tokens, name, strucs, funcs));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn define_in_skill(tokens: &mut TokenGroup, name: String, strucs: &Vec<Struct>, funcs: &Vec<Func>) -> Skill {
        let mut skill_funcs = vec![];

        tokens.expected("{");
        while !tokens.is_equal("}") {
            // skill function
            if let Some(skill_func) = Self::func_def(tokens, &strucs, &funcs) {
                skill_funcs.push(skill_func);
                continue;
            }
            tokens.current_token_error(String::from("unvalid define in skill"));
            std::process::exit(1);
        }

        return Skill {
            name: name,
            funcs: skill_funcs,
        }
    }

    pub fn define_in_general(tokens: &mut TokenGroup) -> Self {
        let mut strucs = vec![];
        let mut funcs = vec![];
        let mut skills = vec![];
        while !tokens.is_end() {
            // struct
            if let Some(struc) = Self::struc_def(tokens, &strucs, &funcs) {
                strucs.push(struc);
                continue;
            }
            // function
            if let Some(func) = Self::func_def(tokens, &strucs, &funcs) {
                funcs.push(func);
                continue;
            }
            // skill
            if let Some(skill) = Self::skill_def(tokens, &strucs, &funcs) {
                skills.push(skill);
                continue;
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