package fpinscala.errorhandling
import fpinscala.Spec

class ErrorHandlingSpec extends Spec {
  describe("MyOption") {

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

  describe("given option") {
    it("computes variance") {
      Option.mean(Seq(1.0, 3.0)) should be(Some(2.0))
      Option.variance(Seq(1.0, 3.0)) should be(Some(1.0))
    }

    it("map2") {
      Option.map2(Some(1), Some(2))(_ + _) should be(Some(3))
      Option.map2(None: Option[Int], Some(2))(_ + _) should be(None)
      Option.map2(Some(1), None: Option[Int])(_ + _) should be(None)
    }

    it("bothMatch_2") {
      Option.bothMatch_2("ab", "..", "ab") should be(Some(true))
    }

    it("sequences") {
      Option.sequence(List(Some(1), None, Some(2))) should be(None)
      Option.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    }

    it("sequences_2") {
      Option.sequence_2(List(Some(1), None, Some(2))) should be(None)
      Option.sequence_2(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    }
  }

  describe("Either") {
    it("maps") {
      Right(2).map(_ * 2) should be(Right(4))
    }

    it("flatMaps") {
      Right(2).flatMap(r ⇒ Right(r * 2)) should be(Right(4))
    }

    it("orElses") {
      Left("err").orElse(Right(2)) should be(Right(2))
    }

    it("maps2") {
      Right(2).map2(Right(2))(_ + _) should be(Right(4))
    }
  }
}
