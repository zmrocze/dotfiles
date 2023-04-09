import cats.Functor

@main def hello: Unit = 
  val phi =
    Exist(List('x'), AtomicFormula("n", List(VarTerm('x'))),
      Forall(List('y'), AtomicFormula("a", List(VarTerm('x'), VarTerm('y'))),
        Not(Exist(List('z'), AtomicFormula("p", List(VarTerm('x'), VarTerm('z'))), 
          Forall(List('x'), AtomicFormula("a", List(VarTerm('x'), VarTerm('z'))), 
            And(Atom(AtomicFormula("b", List(VarTerm('z'), VarTerm('z')))),
                Atom(AtomicFormula("c", List(VarTerm('x'), VarTerm('x'))))))))))
  // relationalSymbols.take(64).foreach(println)
  // println(phi)
  val psi = struct({
      val x = nnf(phi)
      println(x.pretty())
      x}
    )
  psi.foreach(x =>
    // println(x.pretty())
    println())
  val skolemed = psi.map(skolem)
  skolemed.foreach(println)
  
