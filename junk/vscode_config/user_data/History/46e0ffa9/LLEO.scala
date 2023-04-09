import cats.kernel.Order

def SAT[X <: Ordered[X]](phi : GFFormula[X]) : Boolean = 
    resolution(clausify(phi))