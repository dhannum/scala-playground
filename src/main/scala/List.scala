import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def build[A](values: Array[A]): List[A] = {
    if (values.length > 0) Cons(values(0), build(values.tail)) else Nil
  }

  @tailrec
  def print[A](l: List[A]): Unit = l match {
    case Nil => ;
    case Cons(head, tail) => println(head); print(tail)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match { case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, tail) => Cons(newHead, tail)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 1)
      l
    else
      l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
      case _ => Nil
    }
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def sum2(ns: List[Int]) = Ch3.foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = Ch3.foldRight(ns, 1.0)(_ * _)

  def buildWithFold() = Ch3.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  def length[A](as: List[A]): Int = Ch3.foldRight(as, 0)((_, c) => c + 1)

  def sum3(ns: List[Int]) = Ch3.foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = Ch3.foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = Ch3.foldLeft(as, 0)((c, _) => c + 1)

  def concat[A](l: List[List[A]]): List[A] = Ch3.foldRight(l, Nil:List[A])(append)

  def reverse[A](as : List[A]): List[A] = Ch3.foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  def append[A](l : List[A], r : List[A]) : List[A] = Ch3.foldRight(l, r)(Cons(_, _))

  def add1(l: List[Int]): List[Int] = Ch3.foldRight(l, Nil:List[Int])((a, b) => Cons(a + 1, b))

  def double2String(l: List[Double]): List[String] = Ch3.foldRight(l, Nil:List[String])((a, b) => Cons(a.toString, b))

  def map[A,B](as: List[A])(f: A => B): List[B] = Ch3.foldRight(as, Nil:List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = Ch3.foldRight(as, Nil:List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def flatMap_2[A,B](as: List[A])(f: A => List[B]): List[B] =
    Ch3.foldRight(as, Nil:List[B])((h:A,t:List[B]) => append(f(h), t))

  def flatMap_3[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => map(f(h))(buf.+=); go(t)
    }
    go(as)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

}

