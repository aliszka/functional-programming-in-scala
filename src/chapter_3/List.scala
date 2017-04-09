package chapter_3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productFL(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int =
    foldLeft(l, 0)((b, a) => b + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((ls, x) => Cons(x, ls))

  def append[A](as: List[A], bs: List[A]): List[A] =
    foldLeft(reverse(as), bs)((ls, x) => Cons(x, ls))

  def concat[A](l: List[List[A]]): List[A] =
//    foldLeft(l, Nil: List[A])(append)
    foldLeft(reverse(l), Nil: List[A])((ls, xs) => append(xs, ls))

  def plus1(as: List[Int]): List[Int] =
    foldLeft(reverse(as), Nil: List[Int])((rs, a) => Cons(a + 1, rs))

  def double2string(ds: List[Double]): List[String] =
    foldLeft(reverse(ds), Nil: List[String])((rs, d) => Cons(d.toString, rs))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(reverse(as), Nil: List[B])((bs, a) => Cons(f(a), bs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse(as), Nil: List[A])((rs, a) => if (f(a)) Cons(a, rs) else rs)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(reverse(as), Nil: List[B])((rs, a) => append(f(a), rs))

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  def foldRightFL[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def zipAdd(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipAdd(xs, ys))
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

//  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean
}

object ListExercises {

  private def pd(name: String, value: Int) =
    println("%s: %d".format(name, value))

  private def pf(name: String, value: Double) =
    println("%s: %f".format(name, value))

  private def ps(name: String, value: String) =
    println("%s: %s".format(name, value))

  def exercise01(): Unit = {
    val result = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    println("switch: %s".format(result))
  }

  def exercise02(): Unit = {
    println("tail: %s".format(List.tail(List(1,2,3,4,5))))
  }

  def exercise03(): Unit = {
    println("setHead: %s".format(List.setHead(List(1,2,3,4,5), 10)))
  }

  def exercise04(): Unit = {
    println("drop: %s".format(List.drop(List(1,2,3,4,5), 3)))
  }

  def exercise05(): Unit = {
    println("dropWhile: %s".format(List.dropWhile(List(1,2,3,4,5), (a: Int) => a < 4)))
  }

  def exercise06(): Unit = {
    println("init: %s".format(List.init(List(1,2,3,4,5))))
  }

  def exercise07(): Unit = {
    //TODO
  }

  def exercise08(): Unit = {
    println("foldRight constructors: %s".format(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))))
  }

  def exercise09(): Unit = {
    println("length: %d".format(List.length(List(1,2,3,4,5))))
  }

  def exercise10(): Unit = {
    println("foldLeft sum: %d".format(List.foldLeft(List(1,2,3,4,5), 0)(_ + _)))
  }

  def exercise11(): Unit = {
    println("foldLeft sum: %d".format(List.sumFL(List(1,2,3,4,5))))
    println("foldLeft product %f".format(List.productFL(List(1,2,3,4,5))))
    println("foldLeft length %d".format(List.lengthFL(List(1,2,3,4,5))))
  }

  def exercise12(): Unit = {
    println("reverse: %s".format(List.reverse(List(1,2,3,4,5))))
  }

  def exercise13(): Unit = {
    //TODO foldLeft by foldRight
    ps("foldRight by foldLeft", List.foldRightFL(List(1,2,3,4,5), 0)(_ - _).toString)
  }

  def exercise14(): Unit = {
    println("append: %s".format(List.append(List(1,2,3), List(4,5))))
  }

  def exercise15(): Unit = {
    println("concat: %s".format(List.concat(List(List(1,2,3), List(4,5,6), List(7,8,9)))))
  }

  def exercise16(): Unit = {
    println("plus 1: %s".format(List.plus1(List(1,2,3,4,5))))
  }

  def exercise17(): Unit = {
    println("double 2 string: %s".format(List.double2string(List(1.1,2.2,3.3,4.4,5.5))))
  }

  def exercise18(): Unit = {
    println("plus 1: %s".format(List.map(List(1,2,3,4,5))(_ + 1)))
    println("double 2 string: %s".format(List.map(List(1.1,2.2,3.3,4.4,5.5))(_.toString)))
  }

  def exercise19(): Unit = {
    ps("filter", List.filter(List(1,2,3,4,5))(_ % 2 == 0).toString)
  }

  def exercise20(): Unit = {
    ps("flatMap", List.flatMap(List(1,2,3,4,5))(x => List(x, x*x)).toString)
  }

  def exercise21(): Unit = {
    ps("flatMap filter", List.filterFM(List(1,2,3,4,5))(_ % 2 == 0).toString)
  }

  def exercise22(): Unit = {
    ps("zipAdd", List.zipAdd(List(1,2,3,4,5), List(1,2,3,4,5,6)).toString)
  }

  def exercise23(): Unit = {
    ps("zipWith", List.zipWith(List(1,2,3,4,5), List(1,2,3,4,5,6))(_ + _).toString)
  }

  def exercise24(): Unit = {
    //TODO
  }
}