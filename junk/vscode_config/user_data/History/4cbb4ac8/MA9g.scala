// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

def atom[X](rel : RelationalSymbol, args: List[X]) = AtomicFormula(rel, args.map(VarTerm(_)))
def aatom[X](rel : RelationalSymbol, args: List[X]) = Atom(atom(rel, args))
class MySuite extends munit.FunSuite {
  val phi : GFFormula[Int] =
    // Exist(List(0), AtomicFormula("n", List(VarTerm(0))),
    //   Forall(List(1), AtomicFormula("a", List(VarTerm(0), VarTerm(1))),
    //     Not(Exist(List(2), AtomicFormula("p", List(VarTerm(0), VarTerm(2))), 
        Forall(List(2, 3), AtomicFormula("a", List(VarTerm(3), VarTerm(2))), 
            // And(
              Atom(AtomicFormula("b", List(VarTerm(2), VarTerm(2))))
        )
                // ,
                // Atom(AtomicFormula("c", List(VarTerm(3), VarTerm(3))))))
                // )))))

  test("Propositional anti-tautology instance simple") {
    println("Propositional anti-tautology instance simple")
    val phi3 : GFFormula[Int] =
      // - ((phi -> phi2) -> (-phi2 -> -phi))
      Not(Or(Not(phi), phi))
    assertEquals(verboseSAT(phi3), false)
  }
  // test("Debug formulas"){
  //   val phi = Not(Or(Not(aatom("R", List(1))), aatom("R", List(1))))
  //   assertEquals(verboseSAT(phi), false) 
  // }
}
