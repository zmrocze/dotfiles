import scala.collection.mutable.HashMap
import math.Ordered.orderingToOrdered

def mgu[X](A : AtomicFormula[X], B : AtomicFormula[X])(implicit ord: Ordering[X]): Option[Map[X, Term[X]]] = 
    if (A.relation == B.relation) {
        termMGU(A.arglist, B.arglist)
    } else {
        None
    }

def termMGU[X](A : List[Term[X]], B : List[Term[X]])(implicit ord: Ordering[X]): Option[Map[X, Term[X]]] = {
    // Naive implementation for general syntactic unification
    // Returns substitution, identity on unincluded variables
    var S : Map[X, Term[X]] = Map.empty
    
    def nocycle() : Boolean = {
        S.exists( (x , t ) => t match
            case VarTerm(variable) => false
            case FuncTerm(function, arglist) => { 
                println()
                t.freeVars().contains(x)}
         )
    }
    // true on success
    def substitute(v : X , t : Term[X]): Boolean = {
        S.get(v) match
            case None => {
                S = S.updated(v, t)
                S = S.transform((_, tx) => tx.substituted(v, t) )
                println("S: " ++ S.toString())
                val b = nocycle()
                println(b)
                b
            }
            case Some(value) => {
                if unify(value, t) then {
                    S = S.transform((_, tx) => tx.substituted(v, t) )
                    nocycle()
                } else 
                    false
            }
    }

    def unify(a : Term[X], b : Term[X]): Boolean = {
        println( a.pretty() ++ "=?=" ++ b.pretty() )
        if a.substitutedMany(S) == b.substitutedMany(S) then {
            println( a.pretty() ++ "unified" ++ b.pretty() ++ S.toString())
            true
        }
        else (a,b) match
            case (aa@VarTerm(ax), bb@VarTerm(bx)) =>
                if (ax <= bx) then
                    substitute(bx, aa)
                else 
                    substitute(ax, bb)
            case (aa@VarTerm(ax), bb@FuncTerm(_, _)) =>
                if (bb.freeVars() contains ax) then
                    false
                else {
                    println(ax.toString() ++ "<-" ++ bb.toString())
                    substitute(ax, bb)
                }
            case (aa@FuncTerm(_, _), bb@VarTerm(_)) => unify(bb, aa)
                // substitute(bx, aa)
            case (aa@FuncTerm(asym, aargs), bb@FuncTerm(bsym, bargs)) =>
                asym == bsym && aargs.zip(bargs).forall(unify)
        }
    
    if (A.length == B.length) && A.zip(B).forall(unify) then
        Some(S)
    else 
        None
}
