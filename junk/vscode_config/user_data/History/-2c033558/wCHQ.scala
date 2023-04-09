// import cats.kernel.Order

// 

def SAT[X](phi : GFFormula[Int]) : Boolean = 
    resolution(clausify(phi))

def verboseSAT(phi : GFFormula[Int]) : Boolean = 
    verboseResolution(verboseClausify(phi))