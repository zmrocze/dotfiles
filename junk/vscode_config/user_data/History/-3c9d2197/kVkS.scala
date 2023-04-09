
import cats.syntax.flatMap
import cats.conversions.variance

sealed abstract class NNFedGFFormula[T, X] extends Pretty {
  val tag : T

  def toGFormula(): GFFormula[X] = this match
    case NNFLiteral(tag, sign, atom) => if sign then Atom(atom) else Not(Atom(atom))
    case NNFAnd(tag, left, right) => And(left.toGFormula(), right.toGFormula())
    case NNFOr(tag, left, right) => Or(left.toGFormula(), right.toGFormula())
    case NNFForall(tag, variables, guard, sub) => Forall(variables, guard, sub.toGFormula())
    case NNFExist(tag, variables, guard, sub) => Exist(variables, guard, sub.toGFormula())

  override def prettyPrio(prio : Int): String = 
    prettyFormula[NNFedGFFormula[T,X], X](_.toGFormula())(prio)(this)

  // def substituted(variable : X , term : Term[X]) 
  // : NNFedGFFormula[T, X] = this match
  //   case NNFLiteral(tag, sign, AtomicFormula(relation, arglist)) => 
  //       NNFLiteral(tag, sign, AtomicFormula(relation, arglist.map(_.substituted(variable, term))))
  //   case NNFAnd(tag, left, right) => NNFAnd(tag, left.substituted(variable, term), right.substituted(variable, term))
  //   case NNFOr(tag, left, right) => NNFOr(tag, left.substituted(variable, term), right.substituted(variable, term))
  //   case NNFForall(tag, variables, guard, sub) => 
  //       val (guard1 , sub1) = if variables.contains(variable) 
  //           then (guard, sub) 
  //           else (guard.substituted(variable, term) , sub.substituted(variable, term))
  //       NNFForall(tag, variables, guard1, sub1)
  //   case NNFExist(tag, variables, guard, sub) => 
  //       val (guard1 , sub1) = if variables.contains(variable) 
  //           then (guard, sub) 
  //           else (guard.substituted(variable, term) , sub.substituted(variable, term))
  //       NNFExist(tag, variables, guard1, sub1)

}

case class NNFLiteral[T, X](tag : T, sign : Boolean, atom : AtomicFormula[X]) extends NNFedGFFormula[T, X]
// case class NNFNot[T, X](tag : T, sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFAnd[T, X](tag : T, left : NNFedGFFormula[T,  X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFOr[T, X](tag : T, left : NNFedGFFormula[T, X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFForall[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFExist[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]

def nnf[X](phi : GFFormula[X]): NNFedGFFormula[Unit, X] = phi match {
  case Not (Atom(a)) => NNFLiteral((), false, a)
  case Not (Not(g)) => nnf(g)
  case Not (And(g, h)) => NNFOr((), nnf (Not(g)), nnf (Not(h)))
  case Not (Or(g, h)) => NNFAnd((), nnf (Not(g)), nnf (Not(h)))
  case Not (Forall(variable, p, g)) => NNFExist ((), variable, p, (nnf (Not(g))))
  case Not (Exist(variable, p, g)) => NNFForall((), variable, p, (nnf (Not(g))))
  case And(g, h) => NNFAnd((), (nnf(g)), (nnf(h)))
  case Or(g, h) => NNFOr((), nnf(g), nnf(h))
  case Atom(a) => NNFLiteral((), true, a)
  case Exist(variable, p, g) => NNFExist((), variable, p, nnf(g))
  case Forall(variable, p, g) => NNFForall ((), variable, p, nnf(g))
}

type RelationalSymbol = String
type FunctionalSymbol = String

sealed abstract class Term[X] extends Pretty {
  def freeVars() : Set[X] = this match
    case VarTerm(variable) => Set(variable)
    case FuncTerm(function, arglist) => Set.from( arglist.flatMap(x => x.freeVars()) )
  
  def vardepth() : Int = this match
    case VarTerm(variable) => 0
    case FuncTerm(function, arglist) =>
      if arglist.isEmpty then
        -1
      else 
        val d = arglist.map(_.vardepth()).max
        if d <= -1 then -1 else 1 + d

  def substituted(variable : X, subst : Term[X]) : Term[X] = this match
    case VarTerm(var1) => if (var1 == variable) then subst else this
    case FuncTerm(function, arglist) => FuncTerm(function, arglist . map((x => x.substituted(variable, subst))))

  def substitutedMany(subs : Map[X, Term[X]]) = 
    subs.map(identity).foldLeft(this)( (atom, pair) => { (atom, pair) match 
      case (atom, (x, term)) => atom.substituted(x,term) 
    })

  lazy val usedFunctionSymbols : Set[FunctionalSymbol] = this match
    case VarTerm(variable) => Set.empty
    case FuncTerm(function, arglist) => 
      arglist.map(_.usedFunctionSymbols)
      .fold(Set.from(List(function)))(((x : Set[FunctionalSymbol], y: Set[FunctionalSymbol]) => x concat y))
  
  def prettyPrio(prio: Int): String = this match
    case VarTerm(variable) => variable.toString()
    case FuncTerm(function, arglist) => 
      function.toString() ++ "(" ++ commaInterleaved(arglist.map(_.prettyPrio(prio))) ++ ")"
  
}

case class VarTerm[X](variable : X) extends Term[X]
case class FuncTerm[X](function : FunctionalSymbol, arglist : List[Term[X]]) extends Term[X]

// we need to generate new relational symbols so lets fix R right away 
// case class AtomicFormula[R, X](relation : R, varlist : List[X])
case class AtomicFormula[X](relation : RelationalSymbol, arglist : List[Term[X]]) extends Pretty:
  override def prettyPrio(prio : Int) : String =
    this.relation + "(" + commaInterleaved(this.arglist.map(_.prettyPrio(prio))) + ")"
  
  def freeVars() : Set[X] = Set.from( arglist.flatMap(x => x.freeVars()) )
  def vardepth() : Int = 
      if arglist.isEmpty then 
        -1 
      else
        val d = arglist.map(_.vardepth()).max
        if d <= -1 then -1 else 1 + d
        

  def substituted(variable : X, term : Term[X]) = this match
    case AtomicFormula(relation, arglist) => AtomicFormula(relation, arglist.map((x => x.substituted(variable, term))))

  def substitutedMany(subs : Map[X, Term[X]]) = this match
    case AtomicFormula(relation, arglist) => AtomicFormula(relation, arglist.map((x => x.substitutedMany(subs))))
    // subs.map(identity).foldLeft(this)( (atom, pair) => { (atom, pair) match 
    //   case (atom, (x, term)) => atom.substituted(x,term) 
    // })

  lazy val usedFunctionSymbols = this match
    case AtomicFormula(relation, arglist) => arglist.map(_.usedFunctionSymbols).fold(Set.empty)(_ concat _)
  

def relationalSymbols : LazyList[RelationalSymbol] = 
  val alpha = LazyList.from("abcdefghijklmnopqrstuvwxyz")
  def go(xs : LazyList[RelationalSymbol]) : LazyList[RelationalSymbol] =
    val r = for (a <- alpha; x <- xs)
        yield (a.toString() + x)  
    r #::: go(r)
  
  go(LazyList(""))

// type AtomicFormula1[R] = ({ type T[X] = AtomicFormula[R, X] })

def tagWithFreeVariables
  [T, X]
  (phi : NNFedGFFormula[T, X]) 
  : NNFedGFFormula[Set[X], X] = phi match
    case NNFLiteral(tag, sign, atom) => NNFLiteral(atom.freeVars(), sign, atom)
    case NNFAnd(tag, left, right) => {
      val l = tagWithFreeVariables(left)
      val r = tagWithFreeVariables(right)
      NNFAnd(l.tag concat r.tag, l, r)
    }
    case NNFOr(tag, left, right) => {
      val l = tagWithFreeVariables(left)
      val r = tagWithFreeVariables(right)
      NNFOr(l.tag concat r.tag, l, r)
    }
    case NNFForall(_, variables, guard, sub) =>
      val r = tagWithFreeVariables(sub)
      NNFForall((r.tag concat guard.freeVars()) diff Set.from(variables), variables, guard, r)
    case NNFExist(tag, variables, guard, sub) =>
      val r = tagWithFreeVariables(sub)
      NNFExist((r.tag concat guard.freeVars()) diff Set.from(variables), variables, guard, r)
  
  
  