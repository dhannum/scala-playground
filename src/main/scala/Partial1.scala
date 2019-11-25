object Partial1 {
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => partial1(a, f)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    def add1 = partial1(1, (a: Int, b: Int) => a + b)

    println(add1(5))

    println(curry((a: Int, b: Int) => a + b)(5)(6))

    println(compose((a: Int) => a * a, (a: Int) => a + a)(5))
  }
}
