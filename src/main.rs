mod parser;
mod type_system;

use parser::parse;
use std::io::{self, Read};
use type_system::*;

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    buffer = buffer.trim().to_string();

    println!("{:?}", buffer);

    let term = parse(&buffer);
    println!("term: {:?}", &term);

    if let Ok(term) = term {
        let ty = typecheck_with_rectype(&term);
        println!("type: {:?}", &ty);
    }
}
