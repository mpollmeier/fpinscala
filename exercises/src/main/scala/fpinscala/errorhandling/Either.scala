package fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case Right(r) ⇒ Right(f(r))
    case Left(l)  ⇒ Left(l)
  }

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Right(r) ⇒ f(r)
    case Left(l)  ⇒ Left(l)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(_) ⇒ b
    case right   ⇒ right
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) ⇒ Right(f(a, b))
    case (Left(a), _)         ⇒ Left(a)
    case (_, Left(b))         ⇒ Left(b)
  }
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception ⇒ Left(e)
    }

}