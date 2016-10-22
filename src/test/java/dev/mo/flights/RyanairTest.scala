package dev.mo.flights

import java.time.LocalDate

class RyanairTest extends TestBase with Ryanair {
  "Ryanair availability" should {
    "return result with at least one flight from STN to OPO for next 5 days" in {
      whenReady(availability("STN", "OPO", LocalDate.now(), 5)) { av =>
        val trip = av.trips.last
        trip.dates should not be empty
        trip.origin shouldBe "STN"
        trip.destination shouldBe "OPO"
      }
    }
  }
}
