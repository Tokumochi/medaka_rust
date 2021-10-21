mod tokenize;
mod parse;
mod codegen;

use std::env;
use tokenize::TokenGroup;
use parse::DefGroup;

fn main() {
    let args: Vec<String> = env::args().collect();

    let code: &str = args[1].as_str();

    let mut tokens = TokenGroup::new(code);
    let defs = DefGroup::new(&mut tokens);

    codegen::codegen(defs);
}
