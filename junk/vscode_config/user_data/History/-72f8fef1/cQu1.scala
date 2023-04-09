import scala.util.control.NonLocalReturns.*

def normalize[X](A : AtomicFormula[X]): Map[X,Int] = {
    var n : Map[X, Int] = Map.empty
    var notadded = A.freeVars()
    // def relationalSymbols : LazyList[RelationalSymbol] = 
//   val alpha = LazyList.from("abcdefghijklmnopqrstuvwxyz")
    def split(args : List[Term[X]]): (Vector[Term[X]], Vector[Term[X]]) = 
        args.foldLeft[(Vector[Term[X]], Vector[Term[X]])]
            ((Vector.empty, Vector.empty))( (tpl, t1) => {
                tpl match
                    case (fs, xs) => t1 match
                        case v@VarTerm(_) => (fs, xs.appended(v))
                        case t@FuncTerm(_, _) => (fs.appended(t), xs)
        })
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
                    x match
                        case VarTerm(x1) => update(x1)
                        case _ => ()
                }
            }
    
    n
}
