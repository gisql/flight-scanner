package dev.mo.flights

import java.util.Currency

class RyanairProtocol$Test extends TestBase {

  import spray.json._
  import RyanairProtocol._

  "Availability" should {
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

  "Airport" should {
    "parse example json file without failing" in {
      val json = src("/dta.json").mkString("").parseJson
      val aps = json.asJsObject.fields("airports").convertTo[List[Airport]]
      aps should not be empty
      aps.head.iataCode shouldBe "PMF"
    }
  }
}
