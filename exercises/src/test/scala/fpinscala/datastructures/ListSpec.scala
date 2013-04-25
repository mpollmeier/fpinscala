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

    it("folds right") {
      foldRight(List(1, 2, 3), 1)(_ * _) should be(6)
      foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should be(List(1, 2, 3))

      foldRight(List("c", "b", "a"), "")(_ + _) should be("cba")
    }

    it("computes the length using foldRight") {
      List.length(Nil: List[Int]) should be(0)
      List.length(List(1, 2, 3)) should be(3)
    }

    @tailrec
    def createBigList(l: List[Int], i: Int): List[Int] =
      if (i > 500000) l
      else createBigList(prepend(i, l), i + 1)
    val bigList = createBigList(Nil, 0)

    it("throws a stackOverflow for large lists for foldRight") {
      intercept[StackOverflowError] {
        foldRight(bigList, 0)(_ + _)
      }
    }

    it("doesn't throw a stackOverflow for large lists for foldLeft") {
      foldLeft(bigList, 0)(_ + _) //doesn't fail
      foldLeft(List("c", "b", "a"), "")(_ + _) should be("cba")
    }

    it("sums with foldLeft") {
      foldLeft(List(1, 2, 3, 4), 0)(_ + _) should be(10)
      foldLeft(List(1, 2, 3, 4), 1)(_ * _) should be(24)
      foldLeft(List(1, 2, 3, 4), 0)((a, b) ⇒ a + 1) should be(4)
    }

    it("reverses the list") {
      reverse(Nil) should be(Nil)
      reverse(List(1, 2)) should be(List(2, 1))
    }

    it("appends via foldLeft") {
      append2(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4))
    }

    it("flattens") {
      flatten(List(List(1, 2), List(3, 4))) should be(List(1, 2, 3, 4))
      flatten(List(List(1), List(2, 3), List(4))) should be(List(1, 2, 3, 4))
    }

    it("adds one") {
      addOne(List(1, 2, 5)) should be(List(2, 3, 6))
    }

  }
}
