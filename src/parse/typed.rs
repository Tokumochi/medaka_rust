use crate::tokenize::TokenGroup;
use super::struc::Struct;

#[derive(PartialEq, PartialOrd, Clone, Copy)]
pub enum IntType {
    Int8,
    Int32,
}

#[derive(PartialEq, Clone, Copy)]
pub enum Type {
    Int(IntType),
    Struct(usize),
}

impl Type {
    // type_spec -> "i8" | "32" | struct_name
    pub fn type_spec(tokens: &mut TokenGroup, strucs: &Vec<Struct>) -> Self {
        if tokens.is_equal("i8") {
            return Self::Int(IntType::Int8);
        }
        if tokens.is_equal("i32") {
            return Self::Int(IntType::Int32);
        }
        if let Some(name) = tokens.is_ident() {
            for (index, struc) in strucs.iter().enumerate() {
                if struc.name == name {
                    return Self::Struct(index);
                }
            }
        }
        tokens.current_token_error("type name is expected".to_string());
        std::process::exit(1);
    }
}