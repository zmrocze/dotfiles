@main def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

def fib(n : Int ) : Int = 
  if (n > 0) {
    fib(n -1 ) + fib(n-2)
  } else {
    1
  }