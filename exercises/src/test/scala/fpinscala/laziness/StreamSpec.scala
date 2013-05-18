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

  }
}
