
case class C(x : Int) {
    def f() : Int = 1
}

def g() =
    val c = C(0)
    c.f()