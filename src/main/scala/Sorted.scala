object Sorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    def loop(as: Array[A], index: Integer): Boolean = {
      if (index == as.length - 1) true
      else if (ordered(as(index), as(index + 1))) loop(as, index + 1)
      else false
    }

    loop(as, 0)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1,2,3,4,5,9,7,8,9,10), { (a : Int, b : Int) => a < b }))
  }
}
