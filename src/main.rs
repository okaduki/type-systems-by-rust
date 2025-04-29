mod parser;

use parser::parse;
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    buffer = buffer.trim().to_string();

    println!("{:?}", buffer);
    println!("{:?}", parse(&buffer));
}
