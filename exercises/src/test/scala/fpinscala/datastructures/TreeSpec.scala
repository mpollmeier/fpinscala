package fpinscala.datastructures

import scala.annotation.tailrec
import fpinscala.Spec

class TreeSpec extends Spec {
  describe("Trees") {
    import Tree._

    it("get's the size") {
      treeSize(Leaf(Unit)) should be(1)
      treeSize(Branch(Leaf(Unit), Leaf(Unit))) should be(3)
      treeSize(Branch(Leaf(Unit), Branch(Leaf(Unit), Leaf(Unit)))) should be(5)
    }

    it("get's the maximum") {
      maximum(Branch(Leaf(2), Branch(Leaf(5), Leaf(4)))) should be(5)
    }

    it("get's the maximum path length") {
      depth(Leaf(2)) should be(0)
      depth(Branch(Leaf(2), Branch(Leaf(5), Leaf(4)))) should be(2)
    }

    it("maps") {
      map(Leaf(2))(_ + 1) should be(Leaf(3))
      map(Branch(Leaf(2), Branch(Leaf(5), Leaf(4))))(_ * 2) should be(Branch(Leaf(4), Branch(Leaf(10), Leaf(8))))
    }
  }
}
