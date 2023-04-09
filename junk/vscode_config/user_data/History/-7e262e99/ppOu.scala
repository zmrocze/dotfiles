import scala.collection.mutable.HashMap
import math.Ordered.orderingToOrdered
import cats.conversions.variance
import scala.util.control.NonLocalReturns.*

def compareLex[X](ordX : Ordering[X])(a: Either[X, X], b: Either[X, X]): Int = {
    // val cmp = ordX.compare(a., b.x)
    (a, b) match 
        case (Left(_), Right(_)) => 1
        case (Right(_), Left(_)) => -1
        case (Right(x), Right(y)) => ordX.compare(x, y)
        case (Left(x), Left(y)) => ordX.compare(x, y)

def mguResolvedClause[X](AA : SkolemLiteral[X], BB : SkolemLiteral[X], c1 : UqClause[X], c2 : UqClause[X])(implicit ord: Ordering[X]): Option[UqClause[Int]] = {
    given lexOrd: Ordering[Either[X, X]] with {
        def compare(a: Either[X, X], b: Either[X, X]): Int = 
            compareLex(ord)(a, b)
    }
    val A1: AtomicFormula[Either[X,X]] = A.fmaped[Either[X, X]](Left(_))
    val B1: AtomicFormula[Either[X,X]] = A.fmaped[Either[X, X]](Right(_))
    val s = mgu(A1, B1)(lexOrd)
    s match
        case None => None
        case Some(x) => {
            val s1 = normalize[Either[X,X]](A1.substitutedMany(x))
            UqClause((c1.psis excl A) union (c2.psis excl B))
        }
            // x. c1. fmaped
    // finds mgu of renamed A and B
}
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
        ! S.exists( (x , t ) => t match
            case VarTerm(variable) => false
            case ft@FuncTerm(function, arglist) => ft.freeVars().contains(x)
         )
    }
    // true on success
    def substitute(v : X , t : Term[X]): Boolean = {
        S.get(v) match
            case None => {
                S = S.updated(v, t)
                S = S.transform((_, tx) => tx.substituted(v, t) )
                nocycle()
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
        // println( a.pretty() ++ "=?=" ++ b.pretty() )
        if a.substitutedMany(S) == b.substitutedMany(S) then 
            true
        else (a,b) match
            case (aa@VarTerm(ax), bb@VarTerm(bx)) =>
                if (ax <= bx) then
                    substitute(bx, aa)
                else 
                    substitute(ax, bb)
            case (aa@VarTerm(ax), bb@FuncTerm(_, _)) =>
                if (bb.freeVars() contains ax) then
                    false
                else 
                    substitute(ax, bb)
            case (aa@FuncTerm(_, _), bb@VarTerm(bx)) => unify(bb, aa)
                // substitute(bx, aa)
            case (aa@FuncTerm(asym, aargs), bb@FuncTerm(bsym, bargs)) =>
                asym == bsym && aargs.zip(bargs).forall(unify)
        }
    
    if A.length == B.length && A.zip(B).forall(unify) then
        Some(S)
    else 
        None
}
