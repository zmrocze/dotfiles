import scala.collection.mutable.ArrayBuffer

class UnionFind(n : Int):
    // union find on 0..n-1
    var parent : ArrayBuffer[Int] = ArrayBuffer.range(0, n)
    var size : ArrayBuffer[Int] = ArrayBuffer.fill(n)(1)

    def find(x : Int) : Int =
        if (parent(x) == x) then x else {
            val findx = find(parent(x))
            parent(x) = findx
            return findx
        }

    def union(x : Int, y : Int) = 
        val a = find(x)
        val b = find(y)
        if (a != b) then {
            if size(a) > size(b) then
                parent(b) = a
                size(a) = size(a) + size(b)
            else 
                parent(a) = b
                size(b) = size(a) + size(b)
        }


class Graph(n : Int, edgesInit : ArrayBuffer[Vector[Int]]):
    // graph on nodes 0..n-1, with nodes glued via union find
    // An efficient representation for a substitution
    val edges: ArrayBuffer[Vector[Int]] = edgesInit
    val unions : UnionFind = UnionFind(n)
    // func symbols
    val termSymbol : ArrayBuffer[Option[String]] = ArrayBuffer.fill(n)(None)

    def hasCycle() : Boolean = 
        var visited : Set[Int] = Set[Int]()

        def dfs(x: Int): Boolean = 
            visited + x
            // var res = false
            for (y <- edges(x)) {
                if visited(y) then true else {
                    if dfs(y) then true else {
                        
                    }
                }
            }
