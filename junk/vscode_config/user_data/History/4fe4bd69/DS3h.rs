use either::*;
use std::slice::Iter;

// struct GrammarRule<Terminal, NonTerminal>{
//     productions : Vec< Vec<Either<Terminal, NonTerminal>> >
// }

struct Parser<A>(fn(&str) -> Iter<(&str, A)>);

const empty : Parser<()>  = {
    fn runParser(inp : &str) -> Vector<(&str, ())> {
        [(inp, ())]
    };
    Parser(runParser)
};
// struct C{a : fn(i32) -> i32}

// fn parseChar

fn main() {
    let a = [1, 2, 3];
    let b:Iter<i32> = a.iter();

}
