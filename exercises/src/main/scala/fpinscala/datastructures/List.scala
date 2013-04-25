package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil         ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs)  ⇒ x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1, 2, 3)
  val total = sum(example)

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          ⇒ x
    case Nil                                   ⇒ 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
    case Cons(h, t)                            ⇒ h + sum(t)
    case _                                     ⇒ 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        ⇒ a2
      case Cons(h, t) ⇒ Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)(
      (acc, item) ⇒ Cons(item, acc))

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B = // Utility functions
    l match {
      case Nil         ⇒ z
      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil              ⇒ Nil
    case Cons(head, Nil)  ⇒ Nil
    case Cons(head, tail) ⇒ tail
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A])(f: A ⇒ Boolean): List[A] = l match {
    case Nil                         ⇒ Nil
    case Cons(head, tail) if f(head) ⇒ dropWhile(tail)(f)
    case _                           ⇒ l
  }

  def setHead[A](l: List[A])(h: A): List[A] = Cons(h, tail(l))

  def prepend[A](a: A, l: List[A]): List[A] = Cons(a, l)

  def init[A](l: List[A]): List[A] = {
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil              ⇒ acc
      case Cons(head, Nil)  ⇒ acc //head is last element
      case Cons(head, tail) ⇒ go(tail, append(acc, List(head)))
    }
    go(l, Nil)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) ⇒ 1 + b)

  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil              ⇒ acc
      case Cons(head, tail) ⇒ go(tail, prepend(head, acc))
    }

    go(l, Nil)
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B = l match {
    case Nil              ⇒ z
    case Cons(head, tail) ⇒ foldLeft(tail, f(z, head))(f)
  }

  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(append(_, _))

  def addOne(l: List[Int]): List[Int] = {
    @tailrec
    def go(l: List[Int], acc: List[Int]): List[Int] = l match {
      case Nil              ⇒ acc
      case Cons(head, tail) ⇒ go(tail, append(acc, Cons(head + 1, Nil)))
    }
    go(l, Nil)
  }

  def doubleToString(l: List[Double]): List[String] = {
    @tailrec
    def go(l: List[Double], acc: List[String]): List[String] = l match {
      case Nil              ⇒ reverse(acc)
      case Cons(head, tail) ⇒ go(tail, prepend(s"$head", acc))
    }
    go(l, Nil)
  }

  def map[A, B](l: List[A])(f: A ⇒ B): List[B] = {
    @tailrec
    def go(l: List[A], acc: List[B]): List[B] = l match {
      case Nil        ⇒ reverse(acc)
      case Cons(h, t) ⇒ go(t, prepend(f(h), acc))
    }
    go(l, Nil)
  }

  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] = {
    @tailrec
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil                ⇒ reverse(acc)
      case Cons(h, t) if f(h) ⇒ go(t, prepend(h, acc))
      case Cons(_, t)         ⇒ go(t, acc)
    }
    go(l, Nil)
  }
}