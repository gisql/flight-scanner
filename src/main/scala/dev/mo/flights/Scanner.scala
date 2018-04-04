package dev.mo.flights

import java.time.temporal.ChronoUnit.DAYS
import java.time.{DayOfWeek, Duration, LocalDate, LocalDateTime}

import dev.mo.flights.RyanairProtocol.{AvFlight, Availability}

import scala.concurrent.Future


case class Trip(from: String, to: String, price: Double, ticketCount: Int, outTime: LocalDateTime, backTime: LocalDateTime)
//curl 'https://desktopapps.ryanair.com/v4/en-ie/availability?ADT=1&CHD=0&DateIn=2018-04-12&DateOut=2018-04-12&Destination=WMI&FlexDaysIn=6&FlexDaysOut=4&INF=0&IncludeConnectingFlights=true&Origin=STN&RoundTrip=true&TEEN=0&ToUs=AGREED&exists=false&promoCode='
// -H 'Pragma: no-cache' -H 'Origin: https://www.ryanair.com' -H 'Accept-Encoding: gzip, deflate, br'
// -H 'Accept-Language: en-GB,en;q=0.9,pl;q=0.8'
// -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36'
// -H 'Accept: application/json, text/plain, */*'
// -H 'Referer: https://www.ryanair.com/gb/en/booking/home'
// -H 'Cookie: RYANSESSION=WrYgsAolAvMAAEMP8QAAAAA6; check=true; AMCVS_64456A9C54FA26ED0A4C98A5%40AdobeOrg=1; s_cc=true; AMCV_64456A9C54FA26ED0A4C98A5%40AdobeOrg=-894706358%7CMCIDTS%7C17616%7CMCMID%7C81850505495059374294553362537055146679%7CMCAAMLH-1522573083%7C6%7CMCAAMB-1522573083%7C6G1ynYcLPuiQxYZrsz_pkqfLG9yMXBpb2zX5dvJdYQJzPXImdj0y%7CMCOPTOUT-1521975483s%7CNONE%7CMCAID%7CNONE%7CMCSYNCSOP%7C411-17623%7CvVersion%7C2.3.0; agso=AaAHR2oBAAr3PKEuktVIDffrdfpSj6Y.; agsr=AeDPFTgBAAr3PKEuktVIDffrde9WArA.; s_nr2=1521968386170-New; s_sq=%5B%5BB%5D%5D; agss=ARSk3OMBAEBzbcMuktVIDffrdVOneCQ.; mbox=session#9744bcbeaa9944858e7361232eae0cc1#1521970249|PC#9744bcbeaa9944858e7361232eae0cc1.21_15#1585213085'
// -H 'Connection: keep-alive' -H 'Cache-Control: no-cache' --compressed
trait Scanner extends Sys with Ryanair {
  private def acceptableTrips(acceptable: (AvFlight, AvFlight) => Boolean, start: LocalDate, origin: String, destinations: Seq[String]): Future[List[Trip]] = {
    def mkTrip(out: AvFlight, in: AvFlight, dst: String) = {
      def bestFare(flight: AvFlight) = flight.regularFare.flatMap(_.fares.filter(_.count >= 2).sortBy(_.amount).headOption)

      val bestOut = bestFare(out)
      val bestIn = bestFare(in)
      bestOut.flatMap(o => bestIn.map(i =>
        Trip(origin, dst, i.amount + o.amount, Math.min(i.count, o.count), out.time.head, in.time.drop(1).head)
      ))
    }

    def flatten(in: Availability) = in.trips.map(_.dates.flatMap(_.flights).filter(_.regularFare.isDefined))
    def oneDest(dst: String) = availability(origin, dst, start, 1).map(flatten) map {
      case out :: in :: Nil => out.flatMap(o => in.filter(i => acceptable(o, i)).flatMap(i => mkTrip(o, i, dst)))
      case _ => Nil
    }

    val canFlyTo = canFly(origin)
    destinations.filter(canFlyTo).foldLeft(Future.successful(List.empty[Trip])) {
      case (acc, dest) =>
        for {
          a <- acc
          tr <- oneDest(dest)
        } yield a ++ tr
    }
    //    Future.sequence(destinations.filter(canFlyTo).map(oneDest)).map(x => x.toList.flatten)
  }

  def oneDayTrips(after: LocalDate, origin: String, destinations: String*): Future[List[Trip]] = {
    val nextSaturday = {
      val sat = DayOfWeek.SATURDAY.getValue
      val cur = after.getDayOfWeek.getValue
      if (sat == cur) after.plus(7, DAYS)
      else if (sat > cur) after.plus(sat - cur, DAYS)
      else after.plus(6, DAYS)
    }
    def acceptable(out: AvFlight, in: AvFlight) =
      out.time.drop(1).headOption.flatMap(ot => in.time.headOption.map(it =>
        ot.toLocalDate == it.toLocalDate &&
          Duration.between(ot, it).toMinutes >= 6 * 60
      )).getOrElse(false)

    acceptableTrips(acceptable, nextSaturday, origin, destinations)
  }
}
