package dev.mo.flights

import java.time.{LocalDate, LocalDateTime}
import java.util.Currency

import spray.client.pipelining._
import spray.http.HttpRequest
import spray.json.{DefaultJsonProtocol, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Success, Try}

object RyanairProtocol extends DefaultJsonProtocol with CommonProtocol {
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

  //  https://api.ryanair.com/aggregate/3/common?embedded=airports&market=en-gb
  case class Coordinates(longitude: Double, latitude: Double)
  case class Airport(iataCode: String, name: String, coordinates: Coordinates, countryCode: String,
                     currencyCode: Currency, routes: List[String], priority: Int)
  implicit val coordinatesFormat = jsonFormat2(Coordinates)
  implicit val airportFormat = jsonFormat7(Airport)
}

trait Ryanair extends Sys {

  import RyanairProtocol._
  import spray.httpx.SprayJsonSupport._

  private val logger = createLogger
  private val avPipe = sendReceive ~> unmarshal[Availability]

  private lazy val airportCache: Map[String, Airport] = {
    import concurrent.duration._
    def cleanRoutes(in: Airport) = in.copy(routes = in.routes.filter(_.startsWith("airport:")).map(_.substring(8)))

    val airportPipe = sendReceive ~> unmarshal[Map[String, List[JsValue]]]
    val ftr = airportPipe(Get("https://api.ryanair.com/aggregate/3/common?embedded=airports&market=en-gb")) map { res =>
      val rv = res("airports").map(a => Try(a.convertTo[Airport])) collect {
        case Success(a) => a.iataCode -> cleanRoutes(a)
      }
      rv.toMap
    }
    Await.result(ftr, 10 seconds)
  }

  def airport(iata: String): Option[Airport] = airportCache.get(iata)

  def canFly(from: String): String => Boolean = {
    val fx = airport(from).map(_.routes.toSet) match {
      case Some(routes) => routes
      case None => Set.empty[String]
    }

    fx.contains
  }

  //curl 'https://desktopapps.ryanair.com/v4/en-ie/availability?
  // &CHD=0&INF=0&TEEN=0&IncludeConnectingFlights=true&RoundTrip=true&ToUs=AGREED&exists=false&promoCode=
  // ADT=1&DateIn=2018-04-12&DateOut=2018-04-12&Destination=WMI&FlexDaysIn=6&FlexDaysOut=4&Origin=STN'

  def availability(origin: String, destination: String, date: LocalDate, flex: Int = 0, adults: Int = 2): Future[Availability] = {
    val query = s"Origin=$origin&Destination=$destination&ADT=$adults&DateOut=$date&DateIn=$date&FlexDaysOut=$flex&FlexDaysIn=$flex"
    val uri = s"https://desktopapps.ryanair.com/v4/en-ie/availability?$query&CHD=0&INF=0&TEEN=0&IncludeConnectingFlights=true&RoundTrip=true&ToUs=AGREED&exists=false&promoCode="
    avPipe(Get(uri))
    //    println(uri)
    //    ???
  }

  def printTripsFrom(origin: String): Unit = {
    println(s"ITA,Country,City,Longitude,Latitude")
    airportCache(origin).routes.map(airportCache).sortBy(x => (x.countryCode, x.name)) foreach {
      case Airport(ita, name, c, cc, _, _, _) =>
        println(s"$ita,$cc,$name,${c.longitude},${c.latitude}")
    }
  }
}
