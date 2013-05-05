package fpinscala.errorhandling
import fpinscala.Spec

class ErrorHandlingSpec extends Spec {
  describe("Option") {

    object MyOption {

      sealed trait Option[+A] {
        def map[B](f: A ⇒ B): Option[B] = flatMap(x ⇒ Some(f(x)))

        def flatMap[B](f: A ⇒ Option[B]): Option[B] = this match {
          case Some(x) ⇒ f(x)
          case None    ⇒ None
        }

        def getOrElse[B >: A](default: ⇒ B): B = this match {
          case Some(x) ⇒ x
          case None    ⇒ default
        }

        def orElse[B >: A](ob: ⇒ Option[B]): Option[B] = this match {
          case Some(x) ⇒ this
          case None    ⇒ ob
        }

        def filter(f: A ⇒ Boolean): Option[A] = this match {
          case Some(x) if (f(x)) ⇒ this
          case _                 ⇒ None
        }
      }

      case class Some[+A](get: A) extends Option[A]
      case object None extends Option[Nothing]

    }

    import MyOption._

    it("maps") {
      Some(2).map(_ * 2) should be(Some(4))
    }

    it("flatMaps") {
      Some(2).flatMap(x ⇒ Some(x * 2)) should be(Some(4))
    }

    it("getOrElse") {
      Some(2).getOrElse(4) should be(2)
      None.getOrElse(4) should be(4)
    }

    it("orElse") {
      Some(2).orElse(Some(4)) should be(Some(2))
      None.orElse(Some(4)) should be(Some(4))
    }

  }
}