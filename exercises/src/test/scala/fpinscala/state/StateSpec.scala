package fpinscala.state

import scala.annotation.tailrec
import fpinscala.Spec
import State._
import RNG._

class StateSpec extends Spec {

  describe("RNG") {

    it("generates a positive random number") {
      var rng = simple(System.currentTimeMillis)
      (1 to 100) foreach { _ ⇒
        val (i, rng2) = positiveInt(rng)
        i should be >= (0)
        rng = rng2
      }
    }

    it("generates a positiveMax") {
      var rng = simple(System.currentTimeMillis)
      (1 to 100) foreach { _ ⇒
        val (i, rng2) = positiveMax(100)(rng)
        i should be >= (0)
        i should be < (100)
        rng = rng2
      }
    }

    it("generates random doubles") {
      var rng = simple(System.currentTimeMillis)
      (1 to 100) foreach { _ ⇒
        val (i, rng2) = double(rng)
        i should be >= (0d)
        i should be < (1d)
        rng = rng2
      }
    }

    it("sequences") {
      val r = simple(System.currentTimeMillis)
      sequence(List(unit(1), unit(2), unit(3)))(r)._1 should be(List(1, 2, 3))
    }

  }
}

object StateSpec extends App {
  val rng = simple(System.currentTimeMillis)
  println(ints(5)(rng))
  println(doubleWithMap(rng))
  println(intDouble2(rng))
}