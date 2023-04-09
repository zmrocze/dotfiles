// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

def atom[X](rel : RelationalSymbol, args: List[X]) = AtomicFormula(rel, args.map(VarTerm(_)))
def aatom[X](rel : RelationalSymbol, args: List[X]) = Atom(atom(rel, args))
class MySuite extends munit.FunSuite {
  val phi : GFFormula[Int] =
    Exist(List(0), atom("n", List(VarTerm(0))),
      Forall(List(1), atom("a", List(VarTerm(0), VarTerm(1))),
        Not(Exist(List(2), atom("p", List(VarTerm(0), VarTerm(2))), 
          Forall(List(3), atom("a", List(VarTerm(3), VarTerm(2))), 
            And(Atom(atom("b", List(VarTerm(2), VarTerm(2)))),
                Atom(atom("c", List(VarTerm(3), VarTerm(3))))))))))
  val phi2 : GFFormula[Int] =
    Exist(List(0), atom("n", List(VarTerm(0))),
      Exist(List(1), atom("a", List(VarTerm(0), VarTerm(1))),
        Or(Not(Atom(atom("n", List(VarTerm(0))))), Not(Atom(atom("a", List(VarTerm(0), VarTerm(1))))))))
  val parity : GFFormula[Int] = 
    And(And(And(
      And(Forall(List(1), atom("P", List(VarTerm(1))), Not(Atom(atom("N", List(VarTerm(1)))))), 
          Forall(List(1), atom("N", List(VarTerm(1))), Not(Atom(atom("P", List(VarTerm(1))))))),
      Forall(List(1,2), atom("S", List(VarTerm(1), VarTerm(2))), 
        And(Or( aatom("P", List(1)), 
              aatom("P", List(2)) ),
            Or( aatom("N", List(1)), 
              aatom("N", List(2)) )))),
      Exist(List(1), atom("P", List(1)), aatom("P", List(1)))),
      Forall(List(1), atom("P", List(1)), 
        Exist(List(2), atom("S", List(1,2)), aatom("S", List(1,2))))
    )
  test("Phi from Nivelle") {
    assertEquals(verboseSAT(phi), true)
  }
  test("Phi2") {
    assertEquals(verboseSAT(phi2), false)
  }
  test("Propositional anti-tautology simple instance") {
    println("Propositional anti-tautology instance simple")
    val phi3 : GFFormula[Int] =
      // - ((phi -> phi2) -> (-phi2 -> -phi))
      Not(Or(Not(phi), phi))
    assertEquals(verboseSAT(phi3), false)
  }
  test("Propositional tautology instance simple") {
    val phi3 : GFFormula[Int] =
      // - ((phi -> phi2) -> (-phi2 -> -phi))
      Or(Not(phi), phi)
    assertEquals(verboseSAT(phi3), true)
  }
  test("Exist simple") {
    val phi3 : GFFormula[Int] = Exist(List(1,2), atom("R", List(1,2)), Or(aatom("P", List(1)), aatom("P", List(2))))
    assertEquals(verboseSAT(phi3), true)
  }
  test("Forward GF formula"){
    val phi3 = And(
      Forall(List(1, 2), atom("a", List(VarTerm(1), VarTerm(2))),
        Exist(List(3), atom("b", List(VarTerm(1),VarTerm(2),VarTerm(3))),
          Atom(atom("a" , List(VarTerm(2), VarTerm(3)))))),
      Exist(List(1,2,3), atom("b", List(VarTerm(1), VarTerm(2), VarTerm(3))), 
        Not(Atom(atom("a", List(VarTerm(1), VarTerm(2)))))))
    
    assertEquals(verboseSAT(phi3), true)
  }
  test("Pairs formula"){ 
    assertEquals(verboseSAT(parity), true) 
  }
}
