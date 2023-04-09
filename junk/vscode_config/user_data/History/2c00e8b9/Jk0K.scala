import cats.syntax.contravariant
import cats.instances.unit

case class UqClause[X](psis : Set[SkolemLiteral[X]])  extends Pretty {
    def concat(y : Clause[X]) = Clause(this.psis.concat(y.psis).toList)
    override def prettyPrio(prio: Int): String = textInterleaved(" v ", this.psis.map(_.prettyPrio(prio)))
    def substitutedMany(mgu : Map[X, Term[X]]) : UqClause[X] = 
        UqClause( this.psis.map(_.substitutedMany(mgu)) )
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
def greater[X](a : SkolemLiteral[X] , b : SkolemLiteral[X]) = 
        (a.vardepth() < b.vardepth()) || (a.freeVars() subsetOf b.freeVars())

def maximals[X](c : UqClause[X]) = {    
    def ismaximal(A : SkolemLiteral[X]) = 
        c.psis.forall( B => ! ( greater(A, B)) )
    c.psis.filter(ismaximal)
}

def resolve[X](c1 : UqClause[X], c2 : UqClause[X])(implicit ord: Ordering[X]) : UqClauseSet[X] = UqClauseSet {
    var res : Set[UqClause[X]] = Set.empty
    for (A <- maximals(c1)) {
        for (B <- maximals(c2)) {
            if (A.sign == ! B.sign) {
                mgu[X](A.atom, B.atom) match
                    case None => unit
                    case Some(s) => {
                        val cnew = UqClause((c1.psis excl A) union (c2.psis excl B))
                        res = res + cnew.substitutedMany(s)
                    }
            }
        }
    }
    res
}

def factor[X](c1 : UqClause[X])(implicit ord: Ordering[X]) : Set[UqClause[X]] = {
    var res : Set[UqClause[X]] = Set.empty
    for (A <- maximals(c1)) {
        for (B <- c1.psis) {
            if (A != B) {
            mgu[X](A.atom, B.atom) match
                case None => unit
                case Some(s) => {
                    val cnew = UqClause((c1.psis excl B))
                    res += cnew.substitutedMany(s)
                }
            }
        }
    }
    res
}


def resolutionAux[X](c0 : ClauseSet[X])(implicit ord: Ordering[X]) : Set[UqClause[X]] = {
    var C = toUq(c0).clauses
    var continue = true
    var i = 0
    while (continue) {
        continue = false
        for (c1 <- C) {
            for (c2 <- C excl c1) {
                val r = resolve(c1, c2).clauses
                C ++= r
                if ! (r subsetOf C) then 
                    continue = true
            }
            val f = factor(c1)
            C ++= f
            if ! (f subsetOf C) then 
                continue = true
        }
        if (i >= 1) { 
            println("turn")
        }
        i = i + 1
    }
    C
}

def resolution[X](c0 : ClauseSet[X])(implicit ord: Ordering[X]) : Boolean = {
    val C = resolutionAux(c0)
    // println(UqClauseSet(C).pretty())

    ! (C contains UqClause(Set.empty))
}

def verboseResolution[X](c0 : ClauseSet[X])(implicit ord: Ordering[X]) : Boolean = {
    val C = resolutionAux(c0)
    println("Final clause set: " ++ UqClauseSet(C).pretty())
    println()

    ! (C contains UqClause(Set.empty))
}