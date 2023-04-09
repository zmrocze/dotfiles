// import cats.kernel.Order

def SAT[X <: Ordering[X]](phi : GFFormula[X]) : Boolean = 
    resolution(clausify(phi))