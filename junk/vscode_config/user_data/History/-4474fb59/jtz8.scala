import scala.util.chaining._

case class Clause[X](psis : List[SkolemLiteral[X]])  extends Pretty {
    def concat(y : Clause[X]) = Clause(this.psis.concat(y.psis).toList)
    override def prettyPrio(prio: Int): String = textInterleaved(" v ", this.psis.map(_.prettyPrio(prio)))
}

case class ClauseSet[X](clauses : List[Clause[X]]) extends Pretty {
    def concat(y : ClauseSet[X]) = ClauseSet(this.clauses.concat(y.clauses).toList)
    override def prettyPrio(prio: Int): String = "{" ++ textInterleaved("; ", clauses.map(_.prettyPrio(prio))) ++ "}"
}

def clausifySkolemed[X](psi : SkolemedFormula[X]): ClauseSet[X] = 
    def clauseProduct(x : ClauseSet[X], y : ClauseSet[X]): ClauseSet[X] = 
        ClauseSet(
            for a <- x.clauses
                b <- y.clauses
            yield a concat b)
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
    .map(clausifySkolemed[X]).foldLeft(ClauseSet[X](List()))(_ concat _)

def verboseClausify[X](psi : GFFormula[X]) : ClauseSet[X] = 
    def trace[A <: Pretty](msg : String)(x: A) = {
        println(msg ++ " " ++ x.pretty())
        println()
        x
    }
    def traceWith[A](msg : String)(f: A => String)(x: A) = {
        println(msg ++ " " ++ f(x))
        println()
        x
    }
    psi
    .pipe(trace[GFFormula[X]]())
    .pipe(nnf[X])
    .pipe(trace)
    .pipe(struct[Unit,X])
    .pipe(traceWith(prettySet))
    .map(skolem[X])
    .pipe(traceWith(prettySet))
    .map(clausifySkolemed[X]).foldLeft(ClauseSet[X](List()))(_ concat _)
    .pipe(trace)