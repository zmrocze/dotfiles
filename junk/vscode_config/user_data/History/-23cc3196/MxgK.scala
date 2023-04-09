
sealed trait GFFormula[X]

case class Atom[X](atom : AtomicFormula[X]) extends GFFormula[X]
case class Not[X](sub : GFFormula[X]) extends GFFormula[X]
case class And[X](left : GFFormula[X], right : GFFormula[X]) extends GFFormula[X]
case class Or[X](left : GFFormula[X], right : GFFormula[X]) extends GFFormula[X]
case class Forall[X](variables : List[X], guard : AtomicFormula[X], sub : GFFormula[X]) extends GFFormula[X]
case class Exist[X](variables : List[X], guard : AtomicFormula[X], sub : GFFormula[X]) extends GFFormula[X]

