package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def treeSize(t: Tree[_]): Int = t match {
    case Leaf(_)      ⇒ 1
    case Branch(l, r) ⇒ 1 + treeSize(l) + treeSize(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      ⇒ v
    case Branch(l, r) ⇒ maximum(l) max maximum(r)
  }

  def depth(t: Tree[_]): Int = {
    def go(t: Tree[_], count: Int): Int = t match {
      case Leaf(_)      ⇒ count + 1
      case Branch(l, r) ⇒ go(l, count + 1) max go(r, count + 1)
    }
    go(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(a)      ⇒ Leaf(f(a))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }
}