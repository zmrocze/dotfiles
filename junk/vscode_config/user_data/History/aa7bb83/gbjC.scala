
trait Pretty {
    def pretty(): String = prettyPrio(-2)
  
    def prettyPrio(prio : Int): String
  }
  
  def textInterleaved[I <: Iterable[String]](sep : String, xs : I): String = 
    Iterator.unfold(xs.iterator){ ys => 
      if ! ys.hasNext then None
      else 
        val x = ys.next
        Some(x + (if ys.hasNext then sep else ""), ys)
    }
    .fold("")(_ + _)
  
  def commaInterleaved[I <: Iterable[String]](xs : I): String =
    textInterleaved(", ", xs)
  
  def prettySet[A <: Pretty](xs : Set[A]): String =
    "{" ++ commaInterleaved(xs.map(a => a.pretty())) ++ "}"

        
