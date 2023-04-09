#![allow(non_camel_case_types)]

use either::*;
use std::slice::Iter;
use std::vec::Vec;

// struct GrammarRule<Terminal, NonTerminal>{
//     productions : Vec< Vec<Either<Terminal, NonTerminal>> >
// }

struct Parser<A>(dyn Fn(&str) -> Vec<(&str, A)>);

const parseEmpty : Parser<()>  = {
    fn runParser(inp : &str) -> Vec<(&str, ())> {
        vec![(inp, ())]
    };
    Parser(runParser)
};

fn parseChar(c : char) -> Parser<()> {
    let h = c.clone()
    let runParser = |str : &str| {
        if (str.chars().next() == Some(h)) {
            vec![(str, ())]
        } else {
            Vec::new()
        }
    };
    Parser(runParser)
};

// struct C{a : fn(i32) -> i32}

// fn parseChar

fn main() {
    let a = [1, 2, 3];
    let b:Iter<i32> = a.iter();

}
