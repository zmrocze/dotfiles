
class C(x : Int) {
    def f() = 1
}

class D() { 
    def f() = 2
}


sealed trait T {
    def h() : Char
}

case class C1(x : Char) extends T {
    def h() = x
    def k(a : Int)(b : Int) : Int = 5
}
case class C2(x : Char) extends T {
    def h() = x
}

val x = {
    val c = C(1)
    def g(x : Int, y : Int) = x + y
    val x = 1
    
}
