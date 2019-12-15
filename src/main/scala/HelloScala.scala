object HelloScala {
  def main(args: Array[String]): Unit = {
    def l = Ch3.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    def f = (a : Int, b : Int) => a + b


     def l1 = Ch3.foldLeft(List(1, 2, 3), 0)(f)
     def l2 = Ch3.foldLeft(List(2, 3), f(0, 1))(f)
     def l3 = Ch3.foldLeft(List(3), f(f(0, 1), 2))(f)
     def l4 = Ch3.foldLeft(Nil, f(f(f(0, 1), 2), 3))(f)

    println(List.filter(List(1,2,3,4,5,6,7,8,9))(a => a % 2 == 0))


    println(List.flatMap(List(1,2,3,4,5))(i => List(i,i)))
    println(List.flatMap_2(List(1,2,3,4,5))(i => List(i,i)))
    println(List.flatMap_3(List(1,2,3,4,5))(i => List(i,i)))
  }
}

