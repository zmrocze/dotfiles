// import cats.kernel.Order

def SAT[X](phi : GFFormula[X])(implicit ord: Ordering[X]) : Boolean = 
    resolution(clausify(phi))

def verboseSAT[X](phi : GFFormula[X])(implicit ord: Ordering[X]) : Boolean = 
    verboseResolution(verboseClausify(phi))