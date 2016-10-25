package dev.mo.flights

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import akka.actor.ActorSystem

object Main extends App with Scanner {
  override implicit def system = ActorSystem("Scanner")
  /*
    private val destinations = List("SZG", "PFO", "PRG", "SXF", "HAM", "BRE", "ATH", "KGS", "RHO", "BUD", "DUB", "BGY",
      "MXP", "GOA", "BLQ", "PSA", "FCO", "CIA", "TSF", "VCE", "VRN", "LUX", "MLA", "EIN", "RYG", "TRF", "OSL", "GDN",
      "OPO", "LIS", "IBZ", "MAD", "AGP", "PMI", "SVQ", "VLC", "NYO", "EDI")
  */

  /*
    oneDayTrips(LocalDate.now(), "STN",
      "SZG", "PFO", "PRG", "SXF", "HAM", "BRE", "ATH", "KGS", "RHO", "BUD", "DUB", "BGY",
      "MXP", "GOA", "BLQ", "PSA", "FCO", "CIA", "TSF", "VCE", "VRN", "LUX", "MLA", "EIN", "RYG", "TRF", "OSL", "GDN",
      "OPO", "LIS", "IBZ", "MAD", "AGP", "PMI", "SVQ", "VLC", "NYO", "EDI").map(trips =>
      trips.sortBy(_.price).foreach(t => println(s"${t.to},${t.price},${t.outTime},${t.backTime}"))
    )
  */

  def fire(w: Int) = oneDayTrips(LocalDate.now().plus(w, ChronoUnit.WEEKS),
    "STN", "SZG", "PFO", "PRG", "SXF", "BRE", "ATH", "KGS", "RHO", "BUD",
    "GOA", "BLQ", "PSA", "FCO", "CIA", "VCE", "VRN", "LUX", "MLA", "EIN", "RYG", "TRF", "OSL", "GDN",
    "OPO", "LIS", "IBZ", "MAD", "AGP", "PMI", "SVQ", "VLC", "NYO", "EDI").map(trips =>
    trips.sortBy(_.price).foreach(t =>
      println(f"${t.to},${t.price}%2.2f,${t.outTime.toLocalDate},${t.outTime.toLocalTime},${t.backTime.toLocalTime}"))
  )

  println("Flights from STN")
  fire(35).andThen { case x =>
    println(s"Finished and then: $x")
  }
}
