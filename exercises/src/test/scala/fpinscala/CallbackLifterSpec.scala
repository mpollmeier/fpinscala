package fpinscala

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.reflect.runtime.universe._

class CallbackLifterSpec extends Spec {

  describe("callbackLifter") {
    def returnValueViaCallback(cb: (Int) ⇒ Any) { cb(3) }
    def withArgs(arg: Int, cb: (Int) ⇒ Any) { cb(42) }

    def callbackLifter[T](func: ((T ⇒ Any)) ⇒ Unit): T = {
      var retVal = None: Option[T]
      func((x: T) ⇒ retVal = Some(x))
      retVal.get
    }

    case class CallBackException[T](x: T) extends Exception
    def exceptionDrivenCallbackLifter[T](func: ((T ⇒ Any)) ⇒ Unit): T =
      try {
        func((x: T) ⇒ throw new CallBackException[T](x))
        null.asInstanceOf[T] //the compiler want's us to return a T ;)
      } catch {
        case CallBackException(x: T) ⇒ x
      }

    it("lifts a callback to it's return value") {
      callbackLifter(returnValueViaCallback) should be(3)
      exceptionDrivenCallbackLifter(returnValueViaCallback) should be(3)

      val partial = withArgs(3, _: (Int) ⇒ Any)
      callbackLifter(partial) should be(42)
      exceptionDrivenCallbackLifter(partial) should be(42)
    }
  }
}
