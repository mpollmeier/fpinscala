package fpinscala

import scala.concurrent.duration._
import scala.collection.mutable
import org.scalatest.{ BeforeAndAfter, BeforeAndAfterAll, FunSpec }
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.scalatest.matchers.BePropertyMatchResult
import org.scalatest.matchers.BePropertyMatcher
import org.mockito.Mockito
import org.mockito.verification.VerificationMode
import org.slf4j.LoggerFactory

trait Spec extends FunSpec
  with ShouldMatchers
  with Matchers
  with MockitoSugar
  with MockitoWrapper
  with BeforeAndAfter
  with BeforeAndAfterAll

trait Matchers {
  def anInstanceOf[T](implicit manifest: Manifest[T]) = {
    val clazz = manifest.runtimeClass.asInstanceOf[Class[T]]
    new BePropertyMatcher[AnyRef] {
      def apply(left: AnyRef) =
        BePropertyMatchResult(left.getClass.isAssignableFrom(clazz), "an instance of " + clazz.getName)
    }
  }
}

trait MockitoWrapper {
  def verify[T](mock: T) = Mockito.verify(mock)
  def verify[T](mock: T, mode: VerificationMode) = Mockito.verify(mock, mode)
  def when[T](methodCall: T) = Mockito.when(methodCall)
  def never = Mockito.never
  def times(wantedNumberOfInvocations: Int) = Mockito.times(wantedNumberOfInvocations)
}

