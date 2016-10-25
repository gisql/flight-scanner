package dev.mo.flights

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Currency

import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.Materializer
import spray.json.{DefaultJsonProtocol, JsString, JsValue, RootJsonFormat}

import scala.concurrent.ExecutionContext

trait Sys {
  implicit def system: ActorSystem
  implicit def executor: ExecutionContext = system.dispatcher
  def createLogger = Logging.getLogger(system, this)
}

class SysProvider(implicit val system: ActorSystem) extends Sys

trait CommonProtocol extends DefaultJsonProtocol {
  private def str[T](json: JsValue, conv: String => T): T = json match {
    case JsString(v) => conv(v)
    case _ => throw new IllegalArgumentException(s"Cannot parse $json")
  }
  implicit val currencyFormat = new RootJsonFormat[Currency] {
    override def read(json: JsValue): Currency = str(json, Currency.getInstance)
    override def write(obj: Currency): JsValue = JsString(obj.getCurrencyCode)
  }
  implicit val dateTimeFormat = new RootJsonFormat[LocalDateTime] {
    private val fmt = DateTimeFormatter.ISO_DATE_TIME
    override def read(json: JsValue): LocalDateTime = str(json, LocalDateTime.parse(_, fmt))
    override def write(obj: LocalDateTime): JsValue = JsString(fmt.format(obj))
  }
}