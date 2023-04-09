
class C(x : Int) {
    def f() = 1
}

val x = {
    val c = C(1)
    def g(x : Int, y : Int) = x + y
    val x = 1
    x.g(4)
}
