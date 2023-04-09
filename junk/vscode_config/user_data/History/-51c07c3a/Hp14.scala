import cats.Functor
import scala.util.chaining._

@main def hello: Unit = 
  val phi =
    Exist(List('x'), AtomicFormula("n", List(VarTerm('x'))),
      Forall(List('y'), AtomicFormula("a", List(VarTerm('x'), VarTerm('y'))),
        Not(Exist(List('z'), AtomicFormula("p", List(VarTerm('x'), VarTerm('z'))), 
          Forall(List('x'), AtomicFormula("a", List(VarTerm('x'), VarTerm('z'))), 
            And(Atom(AtomicFormula("b", List(VarTerm('z'), VarTerm('z')))),
                Atom(AtomicFormula("c", List(VarTerm('x'), VarTerm('x'))))))))))
  
  val phi2 : GFFormula[Int] =
    Exist(List(0), AtomicFormula("n", List(VarTerm(0))),
      Forall(List(1), AtomicFormula("a", List(VarTerm(0), VarTerm(1))),
        Not(Exist(List(2), AtomicFormula("p", List(VarTerm(0), VarTerm(2))), 
          Forall(List(0), AtomicFormula("a", List(VarTerm(0), VarTerm(2))), 
            And(Atom(AtomicFormula("b", List(VarTerm(2), VarTerm(2)))),
                Atom(AtomicFormula("c", List(VarTerm(0), VarTerm(0))))))))))
  // relationalSymbols.take(64).foreach(println)
  // println(phi)
  // val psi = struct({
  //     val x = nnf(phi)
  //     println(x.pretty())
  //     x}
  //   )
  // psi.foreach(x =>
  //   // println(x.pretty())
  //   println())
  // val skolemed = psi.map(skolem)
  // skolemed.foreach(println)
  def trace[A <: Pretty](x: A) = {
    println(x.pretty())
    println()
    x
  }
  def traceWith[A](f: A => String)(x: A) = {
    println(f(x))
    println()
    x
  }
  phi
    .pipe(trace[GFFormula[Char]])
    .pipe(nnf[Char])
    .pipe(trace)
    .pipe(struct[Unit,Char])
    .pipe(traceWith(prettySet))
    .map(skolem[Char])
    .pipe(traceWith(prettySet))
    .map(clausifySkolemed[Char]).foldLeft(ClauseSet[Char](List()))(_ concat _)
    .pipe(trace)
  val cnf = clausify(phi)
  println(cnf.pretty())

  println(SAT(phi2))

  // TODO: use uppercase symbols for relations to visually distinguish from skolems