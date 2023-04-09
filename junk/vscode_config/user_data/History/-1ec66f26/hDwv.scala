import cats.syntax.contravariant
import cats.instances.unit
import scala.util.control.NonLocalReturns.*

case class UqClause[X](psis : Set[SkolemLiteral[X]])  extends Pretty {
    def concat(y : Clause[X]) = Clause(this.psis.concat(y.psis).toList)
    override def prettyPrio(prio: Int): String = textInterleaved(" v ", this.psis.map(_.prettyPrio(prio)))
    def substitutedMany(mgu : Map[X, Term[X]]) : UqClause[X] = 
        UqClause( this.psis.map(_.substitutedMany(mgu)) )
    def fmaped[Y](f : X => Y): UqClause[Y] = UqClause(this.psis.map(_.fmaped(f)))
}

case class UqClauseSet[X](clauses : Set[UqClause[X]]) extends Pretty {
    def concat(y : UqClauseSet[X]) = UqClauseSet(this.clauses.concat(y.clauses).toSet)
    override def prettyPrio(prio: Int): String = "{" ++ textInterleaved("; ", clauses.map(_.prettyPrio(prio))) ++ "}"
    // def substitutedMany(mgu : Map[X, Term[X]]) : UqClauseSet[X] = 
    //     UqClauseSet( this.clauses.map(_.substitutedMany(mgu)) )
}

def toUq[X](c : ClauseSet[X]) = {
    UqClauseSet(c.clauses.map( x => UqClause(x.psis.toSet) ).toSet)
}
def greater[X](a : SkolemLiteral[X] , b : SkolemLiteral[X]) = {
    (a.vardepth() < b.vardepth()) 
    || ((a.freeVars() subsetOf b.freeVars()) && (a.freeVars() != b.freeVars()))}

def maximals[X](c : UqClause[X]) = {    
    def ismaximal(A : SkolemLiteral[X]) = 
        c.psis.forall( B => ! ( greater(A, B)) )
    c.psis.filter(ismaximal)
}

def resolve(c1 : UqClause[Int], c2 : UqClause[Int])(implicit ord: Ordering[Int]) : UqClauseSet[Int] = UqClauseSet {
    var res : Set[UqClause[Int]] = Set.empty
    for (A <- maximals(c1)) {
        for (B <- maximals(c2)) {
            if (A.sign == ! B.sign && ) {
                mguResolvedClause(A, B, c1, c2) match
                    case None => unit
                    case Some(s) => {
                        println("in resolve maximals: A=" ++ A.pretty() ++ " B= " ++ B.pretty())
                        println("in resolve clauses: c1=" ++ c1.pretty() ++ " c2= " ++ c2.pretty())
                        val cnew = UqClause((c1.psis excl A) union (c2.psis excl B))
                        res = res + cnew
                    }
            }
        }
    }
    res
}

def factor[X](c1 : UqClause[X])(implicit ord: Ordering[X]) : Set[UqClause[X]] = {
    var res : Set[UqClause[X]] = Set.empty
    for (A <- maximals(c1)) {
        for (B <- c1.psis excl A) {
            mgu[X](A.atom, B.atom) match
                case None => unit
                case Some(s) => {
                    val cnew = UqClause((c1.psis excl B))
                    res += cnew.substitutedMany(s)
                }
        }
    }
    res
}


def resolutionAux(c0 : ClauseSet[Int])(implicit ord: Ordering[Int]) : Set[UqClause[Int]] = {
    var C = toUq(c0).clauses
    var continue = true
    var i = 0
    returning { 
        while (continue) {
            continue = false
            for (c1 <- C) {
                for (c2 <- C excl c1) {
                    val r = resolve(c1, c2).clauses
                    if ! (r subsetOf C) then 
                        continue = true
                    C ++= r
                    if (r contains UqClause(Set.empty)) then throwReturn(C)
                }
                val f = factor(c1)
                if ! (f subsetOf C) then 
                    continue = true
                C ++= f
                if (f contains UqClause(Set.empty)) then throwReturn(C)
            }
        }
        C
    }
    // C
}

def resolution(c0 : ClauseSet[Int])(implicit ord: Ordering[Int]) : Boolean = {
    val C = resolutionAux(c0)
    // println(UqClauseSet(C).pretty())

    ! (C contains UqClause(Set.empty))
}

def verboseResolution(c0 : ClauseSet[Int]) : Boolean = {
    val C = resolutionAux(c0)
    println("Final clause set: " ++ UqClauseSet(C).pretty())
    println()

    ! (C contains UqClause(Set.empty))
}