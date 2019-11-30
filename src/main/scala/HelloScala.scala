object HelloScala {
  def main(args: Array[String]): Unit = {
    def l = Ch3.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    def f = (a : Int, b : Int) => a + b


     def l1 = Ch3.foldLeft(List(1, 2, 3), 0)(f)
     def l2 = Ch3.foldLeft(List(2, 3), f(0, 1))(f)
     def l3 = Ch3.foldLeft(List(3), f(f(0, 1), 2))(f)
     def l4 = Ch3.foldLeft(Nil, f(f(f(0, 1), 2), 3))(f)

//Ch3.foldLeft(List(2, 3), f(0, 1))(f)

    println(List.append(List(1,2,3,4), List(5,6,7,8)))
  }
}

