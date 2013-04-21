package fpinscala.datastructures

import scala.annotation.tailrec
import fpinscala.Spec

class ListSpec extends Spec {

  describe("Lists") {
    import List._

    it("gets the tail from the list") {
      tail(List(1, 2)) should be(List(2))
      tail(List(1, 2, 3)) should be(List(2, 3))
      tail(List(1)) should be(Nil)
      tail(Nil) should be(Nil)
    }

    it("drops elements") {
      drop(List(Nil), 1) should be(Nil)
      drop(List(1, 2), 2) should be(Nil)
      drop(List(1, 2), 1) should be(List(2))
      drop(List(1, 2, 3, 4), 2) should be(List(3, 4))

      dropWhile(List(1, 2, 3))(_ > 0) should be(Nil)
      dropWhile(List(1, 2, 3))(_ < 0) should be(List(1, 2, 3))
      dropWhile(List(1, 2, 3))(_ < 3) should be(List(3))
    }

    it("replaces the head") {
      setHead(Nil)(4) should be(List(4))
      setHead(List(1, 2, 3))(4) should be(List(4, 2, 3))
    }

    it("returns all but the last element with init") {
      init(Nil) should be(Nil)
      init(List(1)) should be(Nil)
      init(List(1, 2, 3, 4)) should be(List(1, 2, 3))
    }

  }
}
