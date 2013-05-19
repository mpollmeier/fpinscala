package fpinscala.laziness

import Stream._
import scala.annotation.tailrec

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toList: List[A] = uncons match {
    case Some((h, t)) ⇒ h :: t.toList
    case None         ⇒ Nil
  }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = uncons match {
    case Some((h, t)) ⇒ f(h, t.foldRight(z)(f))
    case None         ⇒ z
  }

  def exists(p: A ⇒ Boolean): Boolean = foldRight(false)((a, b) ⇒ p(a) || b)

  def take(n: Int): Stream[A] = uncons match {
    case Some((h, t)) if n > 0 ⇒ cons(h, t.take(n - 1))
    case _                     ⇒ empty
  }

  def takeWhile(p: A ⇒ Boolean): Stream[A] = uncons match {
    case Some((h, t)) if p(h) ⇒ cons(h, t.takeWhile(p))
    case _                    ⇒ empty
  }

  def forAll(p: A ⇒ Boolean): Boolean =
    foldRight(true)((a, b) ⇒ p(a) && b)

  def takeWhile2(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) ⇒ if (p(a)) cons(a, b) else b)

  def map[B](fun: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a, b) ⇒ cons(fun(a), b))

}
object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z) match {
    case None         ⇒ empty[A]
    case Some((a, s)) ⇒ cons(a, unfold(s)(f))
  }

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = sys.error("todo")
}