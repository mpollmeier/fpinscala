package fpinscala

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.reflect.runtime.universe._

class FutureCompositionSpec extends Spec {

  describe("Futures") {
    it("returns one future if true and falls back to other future if false") {

      def destiny(f: Future[Boolean]): Future[Int] =
        f.flatMap(if (_) Future { 42 } else Future { -1 })

      Await.result(destiny(Future { true }), 1 second) should be(42)
      Await.result(destiny(Future { false }), 1 second) should be(-1)

      // problem: calls both Futures
    }

    it("returns one future if true and falls back to other future if false 2") {
      def destiny(f: Future[Boolean]): Future[Int] = f.map(
        if (_) { println("a"); 42 }
        else { println("b"); Await.result(Future { -1 }, 1 second) })

      Await.result(destiny(Future { true }), 1 second) should be(42)
      Await.result(destiny(Future { false }), 1 second) should be(-1)
    }

  }
}
