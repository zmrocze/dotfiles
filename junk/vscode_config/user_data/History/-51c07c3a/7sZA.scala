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

  println(SAT(phi))

  // TODO: use uppercase symbols for relations to visually distinguish from skolems