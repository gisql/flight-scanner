package dev.mo.flights

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import akka.actor.ActorSystem

import scala.concurrent.Future

object Main extends App with Scanner {
  override implicit lazy val system = ActorSystem("Scanner")
  def weekIn2018(w: Int): Future[_] =
    oneDayTrips(
      LocalDate.of(2018, 1, 1).minus(2, ChronoUnit.DAYS).plus(w, ChronoUnit.WEEKS), "STN",
      "KRK", "GIB", "SZG", "PFO", "PRG", "SXF", "BRE", "ATH", "KGS", "RHO", "BUD",
      "GOA", "BLQ", "PSA", "FCO", "CIA", "VCE", "VRN", "LUX", "MLA", "RYG", "TRF", "OSL", "GDN",
      "LIS", "IBZ", "AGP", "PMI", "SVQ", "VLC", "EDI",
      "LNZ", "PUY", "RJK", "ZAD",
      "BOD", "CFE", "GNB", "LRH", "MRS", "NCE",
      "NUE", "WRO", "BSL",
      "CHQ", "CFU", "SKG",
      "TRN", "NAP", "FAO",
      "RIX"
    ).map(trips =>
      trips.sortBy(_.price).flatMap(t => airport(t.to).map(a => t -> a)).foreach({ case (t, a) =>
        println(f"${a.name} (${t.to}),${t.price}%2.2f,${t.outTime.toLocalDate},${t.outTime.toLocalTime},${t.backTime.toLocalTime}")
      })
    )

  println("Flights from STN")
  def retry(n: Int, cmd: => Future[_]): Future[_] = cmd recoverWith {
    case e =>
      if (n <= 0) throw e
      else {
        println(s"Retrying $n: $e")
        retry(n - 1, cmd)
      }
  }

  printTripsFrom("STN")
  /*
    retry(2, weekIn2018(20)) onComplete {
      e =>
        println("terminating...", e)
        system.terminate()
    }
  */
}
