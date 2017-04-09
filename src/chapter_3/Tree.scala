package chapter_3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(fLeaf: A => B)(fBranch: (B, B) => B): B = t match {
    case Leaf(x) => fLeaf(x)
    case Branch(l, r) => fBranch(fold(l)(fLeaf)(fBranch), fold(r)(fLeaf)(fBranch))
  }

  def sizeF[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumF(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthF[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}

object TreeExercises {

  private def pd(name: String, value: Int) =
    println("%s: %d".format(name, value))

  private def pf(name: String, value: Double) =
    println("%s: %f".format(name, value))

  private def ps(name: String, value: String) =
    println("%s: %s".format(name, value))


  def exercise25(): Unit = {
    ps("size", Tree.size(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))).toString)
  }
  def exercise26(): Unit = {
    ps("maximum", Tree.maximum(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))).toString)
  }
  def exercise27(): Unit = {
    ps("depth", Tree.depth(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))).toString)
  }
  def exercise28(): Unit = {
    ps("map", Tree.map(Branch(Leaf(3), Branch(Leaf(2), Leaf(1))))(_ * 2).toString)
  }
  def exercise29(): Unit = {
    ps("size fold", Tree.sizeF(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))).toString)
    ps("maximum fold", Tree.maximumF(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))).toString)
    ps("depth fold", Tree.depthF(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))).toString)
    ps("map", Tree.mapF(Branch(Leaf(3), Branch(Leaf(2), Leaf(1))))(_ * 2).toString)
  }
}
