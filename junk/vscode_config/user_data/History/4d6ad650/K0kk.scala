
class C(x : Int) {
    def f() = 1
}

val x = {
    val c = C(1)
    f(c)
}
