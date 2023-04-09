
case class C[X](x : X) {
    def f() : Int = 1
}

def g() =
    val c = C(0)
    c.f()