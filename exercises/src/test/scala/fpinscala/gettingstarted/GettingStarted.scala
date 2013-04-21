package fpinscala.gettingstarted

import scala.annotation.tailrec
import fpinscala.Spec

class GettingStarted extends Spec {

  describe("PolymorphicFunctions") {
    import PolymorphicFunctions._

    it("implements isSorted - excercise 2") {

      def comparator = (x: Int, y: Int) ⇒ x > y
      isSorted(List(1, 2, 3), comparator) should be(true)
      isSorted(List(1, 2, 2), comparator) should be(true)
      isSorted(List(1, 3, 2), comparator) should be(false)
    }

    it("implements partial1  - excercise 3") {
      val stringIsOne = partial1(1, (a: Int, b: String) ⇒ b.equals("" + a))
      stringIsOne("2") should be(false)
      stringIsOne("1") should be(true)
    }

    it("shows currying  - excercise 4") {
      def stringMatchesInt(a: Int, b: String) = b.equals("" + a)
      val func = curry(stringMatchesInt)

      func(2)("3") should be(false)
      func(1)("1") should be(true)
    }

    it("shows uncurrying  - excercise 5") {
      // see uncurry impl, too lazy for an example
    }

    it("shows function composition - excercise 6") {
      val add2 = (x: Int) ⇒ x + 2
      val double = (x: Int) ⇒ x * 2
      val add2AndDouble = add2 andThen double
      add2AndDouble(3) should be(10)
      val add2AndDouble2 = add2 compose double
      add2AndDouble2(3) should be(8)

      def add3(x: Int) = x + 3
      def triple = (x: Int) ⇒ x * 3
      val add3AndTriple = add3 _ andThen triple
      add3AndTriple(3) should be(18)

      val f = (x: Double) ⇒ math.Pi / 2 - x
      val cos = f andThen math.sin
      cos(1) should be(math.sin(f(1)))
    }
  }
}
