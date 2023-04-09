
sealed trait GFFormula[T, X]

case class Atom[T, X](tag : T, atom : AtomicFormula[X]) extends GFFormula[T, X]
case class Not[T, X](tag : T, sub : GFFormula[T, X]) extends GFFormula[T, X]
case class And[T, X](tag : T, left : GFFormula[T, X], right : GFFormula[T, X]) extends GFFormula[T, X]
case class Or[T, X](tag : T, left : GFFormula[T, X], right : GFFormula[T, X]) extends GFFormula[T, X]
case class Forall[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : GFFormula[T, X]) extends GFFormula[T, X]
case class Exist[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : GFFormula[T, X]) extends GFFormula[T, X]

