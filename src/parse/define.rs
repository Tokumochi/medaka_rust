use crate::tokenize::TokenGroup;
use super::struc::Struct;
use super::func::Func;
use super::typed::Type;

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
    pub funcs: Vec<Func>,
    extends: Vec<(Type, Struct)>,
}

pub struct DefGroup {
    num_of_funcs: usize,
    pub strucs: Vec<Struct>,
    pub funcs: Vec<Func>,
}

impl DefGroup {

    fn struc_def(&self, tokens: &mut TokenGroup, skills: &Vec<Skill>) -> Option<Struct> {
        if tokens.is_equal("struct") {
            if let Some(name) = tokens.is_ident() {
                let name = String::from(name);
                if is_exist_name(&self.strucs, &self.funcs, &skills, name.as_str()) {
                    let message = format!("The struct name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
                return Some(Struct::struc(tokens, name, &self.strucs));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn func_def(&self, tokens: &mut TokenGroup, skills: &Vec<Skill>) -> Option<Func> {
        if tokens.is_equal("define") {
            if let Some(name) = tokens.is_ident() {
                let name = String::from(name);
                if is_exist_name(&self.strucs, &self.funcs, &skills, name.as_str()) {
                    let message = format!("The function name \"{}\" already exists.", name);
                    tokens.previous_token_error(message);
                    std::process::exit(1);
                }
                return Some(Func::func(tokens, self.num_of_funcs, name, &self.strucs, &self.funcs, &skills));
            }
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
        return None;
    }

    fn skill_def(&mut self, tokens: &mut TokenGroup, skills: &Vec<Skill>) -> Option<Skill> {
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
            let extend = Struct::struc(tokens, String::from(""), &self.strucs);
            return Some((typed, extend));
        }
        return None;
    }

    fn define_in_skill(&mut self, tokens: &mut TokenGroup, name: String, skills: &Vec<Skill>) -> Skill {
        let mut skill_funcs = vec![];
        let mut skill_extends: Vec<(Type, Struct)> = vec![];

        tokens.expected("{");
        while !tokens.is_equal("}") {
            // skill function
            if let Some(skill_func) = self.func_def(tokens, skills) {
                skill_funcs.push(skill_func);
                self.num_of_funcs += 1;
                continue;
            }
            // skill extend
            if let Some(skill_extend) = self.extend_def(tokens) {
                skill_extends.push(skill_extend);
                continue;
            }
            tokens.current_token_error(String::from("unvalid define in skill"));
            std::process::exit(1);
        }

        return Skill {
            name: name,
            funcs: skill_funcs,
            extends: skill_extends,
        }
    }

    pub fn define_in_general(tokens: &mut TokenGroup) -> Self {
        let mut defs = Self { num_of_funcs: 0, strucs: vec![], funcs: vec![] };
        let mut skills = vec![];
        while !tokens.is_end() {
            // struct
            if let Some(struc) = defs.struc_def(tokens, &skills) {
                defs.strucs.push(struc);
                continue;
            }
            // function
            if let Some(func) = defs.func_def(tokens, &skills) {
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
            defs.funcs.append(&mut skill.funcs);
        }

        defs.funcs.sort_by(|a, b| a.number.cmp(&b.number));

        return defs;
    }
}