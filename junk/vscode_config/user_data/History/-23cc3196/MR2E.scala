
sealed trait GFFormula[X] extends Pretty {
    // TODO: avoid repetition
    override def prettyPrio(prio : Int): String = 
        def parens(b : Boolean, str : String) = if b then "(" + str + ")" else str
        this match {
        case Atom(atom) => atom.pretty()
        case And(left, right) => parens ( 6 < prio, 
            left.prettyPrio(6) + " ∧ " + right.prettyPrio(6))
        case Or(left, right) => parens ( 4 < prio, 
            left.prettyPrio(4) + " v " + right.prettyPrio(4))
        case Forall(variables, guard, sub) => parens ( 0 < prio,
            "∀ (" + commaInterleaved(variables.map(_.toString)) + ") " + guard.pretty() + " → " + sub.prettyPrio(2))
        case Exist(variables, guard, sub) => parens ( 0 < prio, 
            "∃ (" + commaInterleaved(variables.map(_.toString)) + ") " + guard.pretty() + " ∧ " + sub.prettyPrio(6))
        case Not(sub) => 
        }
}

case class Atom[X](atom : AtomicFormula[X]) extends GFFormula[X]
case class Not[X](sub : GFFormula[X]) extends GFFormula[X]
case class And[X](left : GFFormula[X], right : GFFormula[X]) extends GFFormula[X]
case class Or[X](left : GFFormula[X], right : GFFormula[X]) extends GFFormula[X]
case class Forall[X](variables : List[X]
    , guard : AtomicFormula[X] // where guard contains all the free variables of sub, that is `variables` + all the variables quantified over higher up
    , sub : GFFormula[X]) extends GFFormula[X]
case class Exist[X](variables : List[X]
    , guard : AtomicFormula[X] // where guard contains all the free variables of sub, that is `variables` + all the variables quantified over higher up
    , sub : GFFormula[X]) extends GFFormula[X]
