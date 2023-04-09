import scala.util.control.NonLocalReturns.*

case class Lex[X, Y](x : X, y: Y) extends Ordering[Lex[X,Y]] {}

//     def ordering(ordX: Ordering[X], ordY: Ordering[Y]) = Ordering{
        
//     }

//     def compare(a: Lex[X, Y], b: Lex[X, Y])(implicit ordX : Ordering[X], ordY : Ordering[Y]): Int = 
//         val cmp = ordX.compare(a.x, b.x)
//         if cmp == 0 then 
//             ordY.compare(b.y, b.y)
//         else 
//             cmp

// }

// class OrdLex[X, Y](ordX: Ordering[X], ordX: Ordering[Y]) extends Ordering[Lex[X,Y]] {

// }

def normalize[X](A : Term[X]): Term[Int] = {
    var n : Map[X, Int] = Map.empty
    var notadded = A.freeVars()
    // def relationalSymbols : LazyList[RelationalSymbol] = 
//   val alpha = LazyList.from("abcdefghijklmnopqrstuvwxyz")
    def split(args : List[Term[X]]): (Vector[FuncTerm[X]], Vector[VarTerm[X]]) = 
        args.foldLeft((Vector.empty, Vector.empty))( (tpl, t1) => 
            tpl match
                case (fs, xs) => t1 match
                    case v@VarTerm(_) => (fs, xs.appended(v))
                    case t@FuncTerm(_, _) => (fs.appended(t), xs)
            )
    var i = 0 
    def update(x : X) = 
        if ! n.contains(x) then 
            n = n.updated(x, i)
            i += 1
            notadded = notadded.excl(x)
    def go(t : Term[X]): Unit = t match
        case VarTerm(variable) => update(variable)
        case FuncTerm(function, arglist) =>
            val (fs, xs) = split(arglist)
            returning { 
                for (f <- fs) {
                    if notadded.isEmpty then
                        throwReturn(())
                    else 
                        go(f)
                }
                for (x <- xs) {
                    if notadded.isEmpty then
                        throwReturn(())
                    update(x.variable)
                }
            }
    A.fmaped(n.apply)
}
