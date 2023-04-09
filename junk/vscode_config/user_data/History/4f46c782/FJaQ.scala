
sealed trait StructedSubFormula[X]

case class StructLiteral[X](sign : Boolean, atom : AtomicFormula[X]) extends StructedSubFormula[X]
case class StructAnd[X](left : StructedSubFormula[X], right : StructedSubFormula[X]) extends StructedSubFormula[X]
case class StructOr[X](left : StructedSubFormula[X], right : StructedSubFormula[X]) extends StructedSubFormula[X]
case class StructExist[X](variables : List[X], guard : AtomicFormula[X], sub : StructedSubFormula[X]) extends StructedSubFormula[X]

sealed trait StructedFormula[X]

// Represents formula of form:
// Forall(variables1) guard1 -> Forall(variables2) guard2 -> subf
// why? no
// Forall(variables1) guard1 -> subf
// ^ because thats the form of definining formulas, not of the first top level
case class StructTopForall[X](
    variables1 : List[X], guard1 : AtomicFormula[X], 
    Option[variables2 : List[X], guard2 : AtomicFormula[X], 
    sub : StructedSubFormula[X]) extends StructedFormula[X]:
    

case class StructNoForall[X](sub : StructedSubFormula[X]) extends StructedFormula[X]


def struct[T, A[_], X](phi : NNFedGFFormula[T, X]) : Set[NNFedGFFormula[Unit, X]] =
    val allUsedRelations : Set[RelationalSymbol] = {
        def rec(psi : NNFedGFFormula[T, X]) : Set[RelationalSymbol] = psi match
            case NNFLiteral(tag, sign, atom) => Set(atom.relation)
            case NNFAnd(tag, left, right) => rec(left) ++ rec(right)
            case NNFOr(tag, left, right) => rec(left) ++ rec(right)
            case NNFForall(tag, variables, guard, sub) => rec(sub) + guard.relation
            case NNFExist(tag, variable, guard, sub) => rec(sub) + guard.relation
        rec(phi)
    }
    var remainingSymbols = relationalSymbols . filter (x => ! (allUsedRelations contains x))
    def newRel() : RelationalSymbol = remainingSymbols match
        case x #:: xs => {
            remainingSymbols = xs
            x
        }
        case _ => throw new Error("List is infinite, impossible.")

    var res : Set[StructedFormula[X]] = Set()
    
    def struct(phi : NNFedGFFormula[Set[X], X]) : StructedFormula[X] = phi match
        case NNFForall(tag, variables, guard, sub) => 
            // We rewrite only subformulas
            StructTopForall(variables, guard, y, x, structRec(sub))
        case sub => StructNoForall(structRec(sub))
    def structRec(phi : NNFedGFFormula[Set[X], X]) : StructedSubFormula[X] = phi match
        case NNFLiteral(tag, sign, atom) => StructLiteral(sign, atom)
        case NNFAnd(tag, left, right) => StructAnd(structRec(left), structRec(right)) 
        case NNFOr(tag, left, right) => StructOr(structRec(left), structRec(right))
        case NNFForall(tag, variables, guard, sub) => 
            // We rewrite only subformulas
            val freeVars = tag.toList
            val newAtom = AtomicFormula(newRel(), freeVars.map(VarTerm(_)))
            val sub1 = structRec(sub)
            val defining = StructTopForall(freeVars, newAtom, variables, guard, sub1)
            res += defining
            return StructLiteral(true,  newAtom)
        case NNFExist(tag, variable, guard, sub) => StructExist(variable, guard, structRec(sub))
    
    val t = structRecAux( {
        val r = tagWithFreeVariables(phi)
        println(r)
        println()
        r
    }, true)
    res += t
    return res
