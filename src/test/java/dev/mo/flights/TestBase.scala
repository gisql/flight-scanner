package dev.mo.flights

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.Checkers
import org.scalatest.time.{Millis, Span}
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.{Await, Future}
import concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

abstract class TestBase extends WordSpec with Matchers with ScalaFutures with Checkers with Sys {
  implicit val pc = PatienceConfig(timeout = scaled(Span(15000, Millis)), interval = scaled(Span(10, Millis)))
  override implicit def system: ActorSystem = ActorSystem("test")
  //  override implicit val materializer = ActorMaterializer()
  protected def waitFor[T](test: => T, cond: T => Boolean) = Future {
    while (!cond(test)) {
      Thread.sleep(100)
    }
    test
  }
  protected def deFuture[T](f: Future[T], timeoutSec: Int = 10): T = Await.result(f, timeoutSec seconds)
  protected def src(res: String) = Source.fromInputStream(getClass.getResourceAsStream(res))
}
