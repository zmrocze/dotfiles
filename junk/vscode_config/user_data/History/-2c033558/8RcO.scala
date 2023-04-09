// import cats.kernel.Order

// Type monomorphized to Int after Normalization was added. 
// To improve one can normalize the resulting cnf and retain polymorphic variable type.  

def SAT[X](phi : GFFormula[Int]) : Boolean = 
    resolution(clausify(phi))

def verboseSAT(phi : GFFormula[Int]) : Boolean = 
    verboseResolution(verboseClausify(phi))