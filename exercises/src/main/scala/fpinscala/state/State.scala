package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
        ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
        simple(seed2))
    }
  }

  type Rand[+A] = RNG ⇒ (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng ⇒ (a, rng)

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    val i2 = i match {
      case i if i == Int.MinValue ⇒ Int.MaxValue
      case i                      ⇒ math.abs(i)
    }
    (i2, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = positiveInt(rng)
    val i2 = i.toDouble / Int.MaxValue.toDouble
    (i2, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, _) = double(rng)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) (acc, rng)
      else {
        val (i2, rng2) = rng.nextInt
        go(count - 1, rng2, acc :+ i2)
      }
    }
    go(count, rng, Nil)
  }

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ % 100)

  def doubleWithMap: Rand[Double] =
    map(positiveInt)(_.toDouble / Int.MaxValue.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    rng ⇒ {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def intDouble2: Rand[(Int, Double)] =
    map2(positiveInt, double)((i, d) ⇒ (i, d))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) ⇒ map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] = sys.error("todo")
}

case class State[S, +A](run: S ⇒ (A, S)) {
  def map[B](f: A ⇒ B): State[S, B] =
    sys.error("todo")
  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")
}