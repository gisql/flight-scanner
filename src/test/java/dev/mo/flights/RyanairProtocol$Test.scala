package dev.mo.flights

import java.util.Currency

class RyanairProtocol$Test extends TestBase {

  import spray.json._
  import RyanairProtocol._

  "Protocol's availability" should {
    "parse example json file without failing and yield currency as GBP" in {
      val json = src("/availability.json").mkString("").parseJson
      val av = json.convertTo[Availability]
      av.currency shouldBe Currency.getInstance("GBP")
    }
    "parse json file with missing fares" in {
      val json = src("/av-no-flights.json").mkString("").parseJson
      json.convertTo[Availability]
    }
  }
}
