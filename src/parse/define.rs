use crate::tokenize::TokenGroup;
use super::struc::Struct;
use super::func::{Func};
use super::typed::{Type};

fn is_exist_struct_name(strucs: &Vec<Struct>, name: &str) -> bool {
    if let Some(_) = strucs.iter().find(|&struc| struc.name == name) {
        return true;
    } else {
        return false;
    }
}

fn is_exist_func_name(funcs: &Vec<Func>, name: &str) -> bool {
    if let Some(_) = funcs.iter().find(|&func| func.name == name) {
        return true;
    } else {
        return false;
    }
}

fn is_exist_skill_name(skills: &Vec<Skill>, name: &str) -> bool {
    if let Some(_) = skills.iter().find(|&skill| skill.name == name) {
        return true;
    } else {
        return false;
    }
}

fn is_exist_name(strucs: &Vec<Struct>, funcs: &Vec<Func>, skills: &Vec<Skill>, name: &str) -> bool {
    if is_exist_struct_name(strucs, name) || is_exist_func_name(funcs, name) || is_exist_skill_name(skills, name) {
        return true;
    }
    for skill in skills {
        if is_exist_func_name(&skill.funcs, name) {
            return true;
        }
    }
    return false;
}

pub struct Skill {
    pub name: String,
    strucs: Vec<Struct>,
    pub funcs: Vec<Func>,
    pub extends: Vec<(Type, usize)>, // (extended-type, extend-struct-index)
}

pub struct DefGroup {
    num_of_strucs: usize,
    num_of_funcs: usize,
    pub strucs: Vec<Struct>,
    pub funcs: Vec<Func>,
}

impl<'a> DefGroup {

    fn struc_def(&self, tokens: &mut TokenGroup, skills: &Vec<Skill>) -> Option<Struct> {
        if tokens.is_equal("struct") {
            if let Some(name) = tokens.is_ident() {
                let name = String::from(name);
                if is_exist_name(&self.strucs, &self.funcs, &skills, name.as_str()) {
                    let message = format!("The struct name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
                return Some(Struct::struc(tokens, self.num_of_strucs, name, &self.strucs));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn func_def(&self, tokens: &mut TokenGroup, skills: &Vec<Skill>, belong: Option<&Skill>) -> Option<Func> {
        if tokens.is_equal("define") {
            if let Some(name) = tokens.is_ident() {
                let name = String::from(name);
                if is_exist_name(&self.strucs, &self.funcs, &skills, name.as_str()) {
                    let message = format!("The function name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
/*
                let mut func_parse = FuncParse {
                    id: self.num_of_funcs,
                    name: name,
                    typed: Type::Int(IntType::Int32),
                    top_var_scope: VarScope { vars: vec![], child: None },
                    top_skill_scope: SkillScope { skill: None },
                    num_of_locals: 0,
                    locals: vec![],

                    strucs: &self.strucs,
                    funcs: &self.funcs,
                    skills: &skills,
                    belong: belong,
                };
*/
                return Some(Func::new(tokens, self.num_of_funcs, name, &self.strucs, &self.funcs, &skills, belong));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn skill_def(&'a mut self, tokens: &mut TokenGroup, skills: &Vec<Skill>) -> Option<Skill> {
        if tokens.is_equal("skill") {
            if let Some(name) = tokens.is_ident() {
                if is_exist_name(&self.strucs, &self.funcs, &skills, name) {
                    let message = format!("The skill name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
                let name = String::from(name);
                return Some(self.define_in_skill(tokens, name, skills));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn extend_def(&mut self, tokens: &mut TokenGroup) -> Option<(Type, Struct)> {
        if tokens.is_equal("extend") {
            let typed = Type::type_spec(tokens, &self.strucs);
            let extend = Struct::struc(tokens, self.num_of_strucs, String::from(""), &self.strucs);
            return Some((typed, extend));
        }
        return None;
    }

    fn define_in_skill(&mut self, tokens: &mut TokenGroup, name: String, skills: &Vec<Skill>) -> Skill {
        let mut skill = Skill {
            name: name,
            strucs: vec![],
            funcs: vec![],
            extends: vec![],
        };

        tokens.expected("{");
        while !tokens.is_equal("}") {
            // skill function
            if let Some(skill_func) = self.func_def(tokens, skills, Some(&skill)) {
                skill.funcs.push(skill_func);
                self.num_of_funcs += 1;
                continue;
            }
            // skill extend
            if let Some((typed, extend_struct)) = self.extend_def(tokens) {
                skill.strucs.push(extend_struct);
                skill.extends.push((typed, self.num_of_strucs));
                self.num_of_strucs += 1;
                continue;
            }
            tokens.current_token_error(String::from("unvalid define in skill"));
            std::process::exit(1);
        }

        return skill;
    }

    pub fn define_in_general(tokens: &mut TokenGroup) -> Self {
        let mut defs = Self { num_of_strucs: 0, num_of_funcs: 0, strucs: vec![], funcs: vec![] };
        let mut skills = vec![];

        while !tokens.is_end() {
            // struct
            if let Some(struc) = defs.struc_def(tokens, &skills) {
                defs.strucs.push(struc);
                defs.num_of_strucs += 1;
                continue;
            }
            // function
            if let Some(func) = defs.func_def(tokens, &skills, None) {
                defs.funcs.push(func);
                defs.num_of_funcs += 1;
                continue;
            }
            // skill
            if let Some(skill) = defs.skill_def(tokens, &skills) {
                skills.push(skill);
                continue;
            }
            tokens.current_token_error(String::from("unvalid define"));
            std::process::exit(1);
        }

        for skill in &mut skills {
            defs.strucs.append(&mut skill.strucs);
            defs.funcs.append(&mut skill.funcs);
        }

        defs.strucs.sort_by(|a, b| a.id.cmp(&b.id));
        defs.funcs.sort_by(|a, b| a.id.cmp(&b.id));

        return defs;
    }
}