package dev.mo.flights

import java.time.LocalDate.now
import java.time.{Duration, LocalDate, LocalDateTime}
import java.util.Currency
import java.util.concurrent.atomic.AtomicBoolean

import dev.mo.flights.RyanairProtocol.Availability

import scala.concurrent.Future

class ScannerTest extends TestBase with Scanner {
  "Scanner for one day trips" should {
    "make only one call for one destination" in {
      val service = new SysProvider with Scanner {
        private val executed = new AtomicBoolean()
        override def availability(origin: String, destination: String, date: LocalDate, flex: Int, adults: Int) = Future {
          if (executed.getAndSet(true)) throw new IllegalStateException("This service has already been called")
          Availability(Currency.getInstance("GBP"), LocalDateTime.now(), Nil)
        }
      }
      whenReady(service.oneDayTrips(now(), "STN", "ORK")) { _ => }
    }
    "return trips with out and return on the same day" in {
      whenReady(oneDayTrips(now(), "STN", "ORK")) { trips =>
        trips.forall(t => t.outTime.toLocalDate === t.backTime.toLocalDate) shouldBe true
      }
    }
    "leave at least 6h between flights" in {
      whenReady(oneDayTrips(now(), "STN", "ORK")) { trips =>
        trips.foreach(println)
        trips.map(t => Duration.between(t.outTime, t.backTime).toMinutes).forall(_ >= 60 * 6) shouldBe true
      }
    }
    "return trips no further than 8 days from requested date" in {
      whenReady(oneDayTrips(now(), "STN", "MAD")) { trips =>
        trips.map(t => Duration.between(LocalDateTime.now(), t.backTime).toDays).forall(_ <= 8) shouldBe true
      }
    }
    "return at least one trip from STN to DUB" in {
      whenReady(oneDayTrips(now(), "STN", "DUB")) { trips =>
        trips should not be empty
      }
    }
    "return empty list for non-existing airport" in {
      whenReady(oneDayTrips(now(), "XXX", "DUB")) { trips =>
        trips shouldBe empty
      }
      whenReady(oneDayTrips(now(), "STN", "XXX")) { trips =>
        trips shouldBe empty
      }
    }
  }
}
