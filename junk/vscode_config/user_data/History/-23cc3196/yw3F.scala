
sealed trait GFFormula[T, X]

case class Atom[T, X](tag : T, atom : AtomicFormula[X]) extends GFFormula[T, X]
case class Not[T, X](tag : T, sub : GFFormula[T, X]) extends GFFormula[T, X], NNFedGFFormula[T, X]
case class And[T, X](tag : T, left : GFFormula[T, X], right : GFFormula[T, X]) extends GFFormula[T, X]
case class Or[T, X](tag : T, left : GFFormula[T, X], right : GFFormula[T, X]) extends GFFormula[T, X]
case class Forall[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : GFFormula[T, X]) extends GFFormula[T, X]
case class Exist[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : GFFormula[T, X]) extends GFFormula[T, X]

case class AndF[T, X](tag : T, left : GFFormula[T, X], right : GFFormula[T, X]) extends GFFormula[T, X]
case class OrF[T, X](tag : T, left : GFFormula[T, X], right : GFFormula[T, X]) extends GFFormula[T, X]
case class ForallF[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : GFFormula[T, X]) extends GFFormula[T, X]
case class ExistF[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : GFFormula[T, X]) extends GFFormula[T, X]


case class NNFLiteral[T, X](tag : T, sign : Boolean, atom : AtomicFormula[X]) extends NNFedGFFormula[T, X]

// case class NNFAnd[T, X](tag : T, left : NNFedGFFormula[T,  X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
// case class NNFOr[T, X](tag : T, left : NNFedGFFormula[T, X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
// case class NNFForall[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
// case class NNFExist[T, X](tag : T, variable : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]


sealed trait NNFedGFFormula[T, X] extends Pretty {
  val tag : T
  override def prettyPrio(prio : Int): String = 
    def parens(b : Boolean, str : String) = if b then "(" + str + ")" else str
    this match {
      case NNFLiteral(tag, sign, atom) => (if sign then "" else "¬") + atom.pretty()
      case NNFAnd(tag, left, right) => parens ( 6 < prio, 
        left.prettyPrio(6) + " ∧ " + right.prettyPrio(6))
      case NNFOr(tag, left, right) => parens ( 4 < prio, 
        left.prettyPrio(4) + " ∨ " + right.prettyPrio(4))
      case NNFForall(tag, variables, guard, sub) => parens ( 0 < prio,
        "∀ (" + commaInterleaved(variables.map(_.toString)) + ") " + guard.pretty() + " → " + sub.prettyPrio(2))
      case NNFExist(tag, variables, guard, sub) => parens ( 0 < prio, 
        "∃ (" + commaInterleaved(variables.map(_.toString)) + ") " + guard.pretty() + " ∧ " + sub.prettyPrio(6))
  }

  def substituted(variable : X , term : Term[X]) 
  : NNFedGFFormula[T, X] = this match
    case NNFLiteral(tag, sign, AtomicFormula(relation, arglist)) => 
        NNFLiteral(tag, sign, AtomicFormula(relation, arglist.map(_.substituted(variable, term))))
    case NNFAnd(tag, left, right) => NNFAnd(tag, left.substituted(variable, term), right.substituted(variable, term))
    case NNFOr(tag, left, right) => NNFOr(tag, left.substituted(variable, term), right.substituted(variable, term))
    case NNFForall(tag, variables, guard, sub) => 
        val (guard1 , sub1) = if variables.contains(variable) 
            then (guard, sub) 
            else (guard.substituted(variable, term) , sub.substituted(variable, term))
        NNFForall(tag, variables, guard1, sub1)
    case NNFExist(tag, variables, guard, sub) => 
        val (guard1 , sub1) = if variables.contains(variable) 
            then (guard, sub) 
            else (guard.substituted(variable, term) , sub.substituted(variable, term))
        NNFExist(tag, variables, guard1, sub1)

}

