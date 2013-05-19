package fpinscala.laziness

import scala.annotation.tailrec
import fpinscala.Spec

class StreamSpec extends Spec {

  describe("Stream") {
    import Stream._

    it("can get converted to a list") {
      cons(1, cons(2, cons(3, empty))).toList should be(List(1, 2, 3))
    }

    it("can take elements") {
      cons(1, cons(2, cons(3, empty))).take(2).toList should be(List(1, 2))
    }

    it("can take elements while < 3") {
      cons(1, cons(2, cons(3, empty))).takeWhile(_ < 3).toList should be(List(1, 2))
    }

    it("forAlls") {
      cons(1, cons(2, cons(3, empty))).forAll(_ > 0) should be(true)
      cons(1, cons(2, cons(3, empty))).forAll(_ < 2) should be(false)
    }

    it("can take elements while < 3 with foldRight") {
      cons(1, cons(2, cons(3, empty))).takeWhile2(_ < 3).toList should be(List(1, 2))
    }

    it("maps") {
      cons(1, cons(2, cons(3, empty))).map(_ * 2).toList should be(List(2, 4, 6))
    }

    it("unfolds") {
      def ones = unfold(1)(Some(1, _))
      ones.take(3).toList should be(List(1, 1, 1))

      def constant[A](a: A) = unfold(a)(Some(a, _))
      constant("a").take(3).toList should be(List("a", "a", "a"))

      def from(n: Int): Stream[Int] = unfold(n)(s ⇒ Some(s, s + 1))
      from(0).take(3).toList should be(List(0, 1, 2))

      //      def fib(z: Int) = unfold(z)(s ⇒ Some((s + z, s + 1)))
      //      def fib(z: Int) = unfold(cons(z, empty))(s ⇒ Some((s, s + z)))
      //      println(fib(0).take(5).toList)
    }

  }
}
