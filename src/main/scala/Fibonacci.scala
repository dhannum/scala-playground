object Fibonacci {
  def fib(n: Int) : Int = {
    def go(n: Int) : Int = {
      if (n == 1 || n == 2) 1
      else go(n - 1) + go(n - 2)
    }

    go(n)
  }

  def main(args: Array[String]): Unit = {
    Array(1,2,3,4,5,6,7,8,9,10).foreach { it => println(fib(it)) }
  }
}
