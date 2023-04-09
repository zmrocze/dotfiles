import scala.util.chaining._

case class Clause[X](psis : List[SkolemLiteral[X]]) {
    def concat(y : Clause[X]) = Clause(this.psis.concat(y.psis).toList)
}

case class ClauseSet[X](clauses : List[Clause[X]]) {
    def concat(y : ClauseSet[X]) = ClauseSet(this.clauses.concat(y.clauses).toList)
}

def clausifySkolemed[X](psi : SkolemedFormula[X]): ClauseSet[X] = 
    def clauseProduct(x : ClauseSet[X], y : ClauseSet[X]): ClauseSet[X] = 
        ClauseSet(x.clauses.zip(y.clauses)
            .map((a,b) => a concat b))
    def rec(phi : SkolemedSubFormula[X]): ClauseSet[X] = phi match
        case lit @ SkolemLiteral(_, _) => ClauseSet(List(Clause(List(lit))))
        case SkolemAnd(left, right) => rec(left) concat rec(right)
        case SkolemOr(left, right) => clauseProduct(rec(left), rec(right))
    def falseLit(g : AtomicFormula[X]): SkolemLiteral[X] = SkolemLiteral(false, g)

    psi match
        case SkolemNoForall(sub) => rec(sub)
        case SkolemTopForall(variables1, guard1, quantifier2, sub) => 
            
            rec(SkolemOr(
                falseLit(guard1)
                , quantifier2 match
                    case None => sub
                    case Some (_, guard2) => SkolemOr(falseLit(guard2) ,sub)
            ))

def clausify[X](psi : GFFormula[X]) : ClauseSet[X] = 
    psi
    .pipe(nnf[X])
    .pipe(struct[Unit, X])
    .map(skolem[X])
    .map(clausifySkolemed[X]).foldLeft(ClauseSet[X](List()))( _  concat _)
    