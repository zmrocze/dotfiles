use either::*;
use std::slice::Iter;
use std::vec::Vec;

// struct GrammarRule<Terminal, NonTerminal>{
//     productions : Vec< Vec<Either<Terminal, NonTerminal>> >
// }

struct Parser<A>(fn(&str) -> Vec<(&str, A)>);

const empty : Parser<()>  = {
    fn runParser(inp : &str) -> Vec<(&str, ())> {
        vec![(inp, ())]
    };
    Parser(runParser)
};
// struct C{a : fn(i32) -> i32}

// fn parseChar

fn main() {
    let a = [1, 2, 3];
    let b:Iter<i32> = a.iter();

}
