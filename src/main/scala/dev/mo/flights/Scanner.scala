package dev.mo.flights

import java.time.temporal.ChronoUnit.DAYS
import java.time.{DayOfWeek, Duration, LocalDate, LocalDateTime}

import dev.mo.flights.RyanairProtocol.{AvFlight, Availability}

import scala.concurrent.Future


case class Trip(from: String, to: String, price: Double, ticketCount: Int, outTime: LocalDateTime, backTime: LocalDateTime)
trait Scanner extends Sys with Ryanair {
  private def acceptableTrips(acceptable: (AvFlight, AvFlight) => Boolean, start: LocalDate, origin: String, destinations: Seq[String]) = {
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
    Future.sequence(destinations.filter(canFlyTo).map(oneDest)).map(x => x.toList.flatten)
  }

  def oneDayTrips(after: LocalDate, origin: String, destinations: String*) = {
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
