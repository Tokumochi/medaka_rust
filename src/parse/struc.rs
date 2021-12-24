use crate::tokenize::TokenGroup;
use super::typed::Type;

pub struct Struct {
    pub number: usize,
    pub name: String,
    pub members: Vec<(String, Type)>,
}

impl Struct {
    // decl_member -> ident ":" type_spec
    pub fn decl_member(&mut self, tokens: &mut TokenGroup, strucs: &Vec<Struct>) {
        if let Some(name) = tokens.is_ident() {
            let name = name.to_string();
            for (member_name, _) in &self.members {
                if *member_name == name {
                    tokens.previous_token_error(format!("The member name \"{}\" already exists.", name));
                    std::process::exit(1);
                }
            }
            tokens.expected(":");
            self.members.push((name.to_string(), Type::type_spec(tokens, strucs)));
        } else {
            tokens.current_token_error("identifier is expected".to_string());
            std::process::exit(1);
        }
    }

    // struc -> "{" (decl_member ("," decl_member)*)? "}"
    pub fn struc(tokens: &mut TokenGroup, number: usize, name: String, strucs: &Vec<Struct>) -> Self {
        tokens.expected("{");
        let mut struc = Struct { number: number, name: name, members: vec![] };
        let mut is_first = true;
        while !tokens.is_equal("}") {
            if is_first {
                is_first = false;
            } else {
                tokens.expected(",");
            }
            struc.decl_member(tokens, &strucs);
        }
        return struc;
    }
}