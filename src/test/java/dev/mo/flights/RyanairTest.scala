package dev.mo.flights

import java.time.LocalDate

class RyanairTest extends TestBase with Ryanair {
  "Ryanair availability" should {
    "return result with at least one flight from STN to OPO for next 5 days" in {
      whenReady(availability("STN", "OPO", LocalDate.now(), 5)) { av =>
        val trip = av.trips.head
        trip.dates should not be empty
        trip.origin shouldBe "STN"
        trip.destination shouldBe "OPO"
      }
    }

    "return result 2 results: there and back" in {
      whenReady(availability("STN", "OPO", LocalDate.now(), 5)) { av =>
        av.trips.size shouldBe 2
        val there = av.trips.head
        val back = av.trips.last
        there.origin shouldBe "STN"
        there.destination shouldBe "OPO"

        back.origin shouldBe "OPO"
        back.destination shouldBe "STN"
      }
    }
  }

  "Ryanair airport" should {
    "return information about STN airport" in {
      val r = airport("STN").get
      r.iataCode shouldBe "STN"
      r.name shouldBe "London Stansted"
    }
    "contain only airports available in the repository" in {
      val r = airport("STN").get
      r.routes should not be empty
      r.routes.map(airport).forall(_.isDefined)
    }
  }

  "Ryanair canFly" should {
    "return true for STN to ORK" in {
      canFly("STN")("ORK") shouldBe true
    }
    "return false for any destination if origin doesn't exit" in {
      canFly("XXX")("STN") shouldBe false
    }
    "return false for any origin if destination doesn't exit" in {
      canFly("STN")("XXX") shouldBe false
    }
    "work with partial functions" in {
      val fromSTNto = canFly("STN")
      fromSTNto("XXX") shouldBe false
      fromSTNto("ORK") shouldBe true
      fromSTNto("OPO") shouldBe true
    }
  }
}
