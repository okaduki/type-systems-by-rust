#![allow(unused_imports)]
mod parser;
mod type_system_generics;
mod type_system_rectype;
mod type_system_simple;
mod type_system_subtype;

use parser::parse;
use std::io::{self, Read};
use type_system_generics::*;
use type_system_rectype::*;
use type_system_simple::*;
use type_system_subtype::*;

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    buffer = buffer.trim().to_string();

    println!("{:?}", buffer);

    let term = parse(&buffer);
    println!("term: {:?}", &term);

    if let Ok(term) = term {
        let ty = typecheck_with_generics(&term);
        println!("type: {:?}", &ty);
    }
}
