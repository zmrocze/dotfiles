// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

def atom[X](rel : RelationalSymbol, args: List[X]) = AtomicFormula(rel, args.map(VarTerm(_)))
def aatom[X](rel : RelationalSymbol, args: List[X]) = Atom(atom(rel, args))
def ands[X](args: List[GFFormula[X]]) = args.tail.foldLeft(args.head)((x, y) =>  And(x, y))
class MySuite extends munit.FunSuite {
  val phi : GFFormula[Int] =
    Exist(List(0), atom("n", List(0)),
      Forall(List(1), atom("a", List(0, 1)),
        Not(Exist(List(2), atom("p", List(0, 2)), 
          Forall(List(3), atom("a", List(3, 2)), 
            And(Atom(atom("b", List(2, 2))),
                Atom(atom("c", List(3, 3)))))))))
  val phi2 : GFFormula[Int] =
    Exist(List(0), atom("n", List(0)),
      Exist(List(1), atom("a", List(0, 1)),
        Or(Not(Atom(atom("n", List(0)))), Not(Atom(atom("a", List(0, 1)))))))
  val parity : GFFormula[Int] = 
    And(And(And(
      And(Forall(List(1), atom("P", List(1)), Not(Atom(atom("N", List(1))))), 
          Forall(List(2), atom("N", List(2)), Not(Atom(atom("P", List(2)))))),
      Forall(List(1,2), atom("S", List(1, 2)), 
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
  test("Simple false") {
    val phi3 : GFFormula[Int] =
      Exist(List(1), atom("R", List(1)), Not(aatom("R", List(1))))
    assertEquals(verboseSAT(phi3), false)
  }
  test("Exist simple") {
    val phi3 : GFFormula[Int] = Exist(List(1,2), atom("R", List(1,2)), Or(aatom("P", List(1)), aatom("P", List(2))))
    assertEquals(verboseSAT(phi3), true)
  }
  test("Forward GF formula"){
    val phi3 = And(
      Forall(List(1, 2), atom("a", List(1, 2)),
        Exist(List(3), atom("b", List(1,2,3)),
          Atom(atom("a" , List(2, 3))))),
      Exist(List(1,2,3), atom("b", List(1, 2, 3)), 
        Not(Atom(atom("a", List(1, 2))))))
    
    assertEquals(verboseSAT(phi3), true)
  }
  test("Pairs formula"){ 
    assertEquals(verboseSAT(parity), true) 
  }
  // \forall{x_1,x_2}(\Rrel(x_1, x_2) \to \neg \forall{x_3,x_4} (\Qrel(x_1,x_2,x_3,x_4) \to \Rrel(x_2,x_3) \lor \Rrel(x_3,x_4)))
  val phipresentation = And(Forall(List(1,2), atom("R", List(1,2)), 
    Not(Forall(List(3,4), atom("Q", List(1,2,3,4)), 
      And(aatom("R", List(2,3)), aatom("R", List(3,4)))))), 
    Exist(List(1,2), atom("R", List(1,2)), aatom("P", List(2))))
  test("Presentation example"){ 
    assertEquals(verboseSAT(phipresentation), true) 
  }        
  val phipresentation2 = And(Forall(List(1,2), atom("S", List(1,2)), 
    And(), 
      And(aatom("R", List(1,2)), aatom("R", List(3,4)))))), 
    Exist(List(1,2), atom("R", List(1,2)), aatom("P", List(2))))
  test("Presentation example"){ 
    assertEquals(verboseSAT(phipresentation2), true) 
  }        
  // val phidebug : GFFormula[Int] = 
  //   ands(List(
  //     And(Forall(List(1), atom("P", List(1)), Not(Atom(atom("N", List(1))))), 
  //         Forall(List(2), atom("N", List(2)), Not(Atom(atom("P", List(2))))))
  //         ,
  //     Forall(List(3,4), atom("S", List(3, 4)), 
  //       And(
  //         Or( aatom("P", List(3)), 
  //             aatom("P", List(4)) )
  //             ,
  //           Or( aatom("N", List(3)), 
  //             aatom("N", List(4)) ))
  //           ),
  //             // ,
  //     Exist(List(5), atom("P", List(5)), aatom("P", List(5))),
  //     Forall(List(6), atom("P", List(6)), 
  //       Exist(List(7), atom("S", List(6,7)), aatom("S", List(6,7))))
  //     ))
  // val parityUnsat = ands(List(
  //   parity,
  //   Forall()
  // ))
  
}
