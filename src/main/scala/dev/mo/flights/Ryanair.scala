package dev.mo.flights

import java.time.{LocalDate, LocalDateTime}
import java.util.Currency

import spray.client.pipelining._
import spray.json.{DefaultJsonProtocol, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

object RyanairProtocol extends DefaultJsonProtocol with CommonProtocol {
  // https://desktopapps.ryanair.com/en-gb/availability?ADT=1&CHD=0&DateIn=2016-12-03&DateOut=2016-11-28&Destination=OPO&FlexDaysIn=0&FlexDaysOut=0&INF=0&Origin=STN&RoundTrip=true&TEEN=0
  case class AvFare(_type: String, amount: Double, count: Int, hasDiscount: Boolean, publishedFare: Double)
  case class AvClassedFare(fareClass: String, fares: List[AvFare])
  case class AvFlight(flightNumber: String, time: List[LocalDateTime], faresLeft: Int, regularFare: Option[AvClassedFare])
  case class AvDate(dateOut: LocalDateTime, flights: List[AvFlight])
  case class Trip(origin: String, destination: String, dates: List[AvDate])
  case class Availability(currency: Currency, serverTimeUTC: LocalDateTime, trips: List[Trip])

  implicit val avFareFormat = new RootJsonFormat[AvFare] {
    def split(m: Map[String, JsValue]) = {
      def g(k: String) = m.getOrElse(k, JsNull)
      (g("type"), g("amount"), g("count"), g("hasDiscount"), g("publishedFare"))
    }
    override def read(json: JsValue): AvFare = json match {
      case JsObject(m) => split(m) match {
        case (JsString(t), JsNumber(a), JsNumber(c), JsBoolean(h), JsNumber(p)) =>
          AvFare(t, a.doubleValue(), c.intValue(), h, p.doubleValue())
        case _ =>
          throw new IllegalArgumentException(s"couldn't parse $json as fare")
      }
      case _ => throw new IllegalArgumentException(s"couldn't parse $json as fare")
    }
    override def write(f: AvFare): JsValue =
      JsObject(Map("type" -> JsString(f._type), "amount" -> JsNumber(f.amount), "count" -> JsNumber(f.count),
        "hasDiscount" -> JsBoolean(f.hasDiscount), "publishedFare" -> JsNumber(f.publishedFare)))
  }
  implicit val avClassedFareFormat = jsonFormat2(AvClassedFare)
  implicit val avFlightFormat = jsonFormat4(AvFlight)
  implicit val avDateFormat = jsonFormat2(AvDate)
  implicit val tripFormat = jsonFormat3(Trip)
  implicit val avFormat = jsonFormat3(Availability)
}

trait Ryanair extends Sys {

  import RyanairProtocol._
  import spray.httpx.SprayJsonSupport._

  private val logger = createLogger
  private val pipeline = sendReceive ~> unmarshal[Availability]

  def availability(origin: String, destination: String, date: LocalDate, flex: Int = 0, adults: Int = 2) = {
    val query = s"Origin=$origin&Destination=$destination&ADT=$adults&DateOut=$date&FlexDaysOut=$flex"
    logger.info(s"GET with query: $query")
    pipeline(Get(s"https://desktopapps.ryanair.com/en-gb/availability?$query"))
  }
}
