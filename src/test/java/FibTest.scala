import dev.mo.flights.TestBase
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class FibTest extends TestBase with GeneratorDrivenPropertyChecks with Fib {
  "fib" should {
    "return 0 if n <= 0" in {
      fib(0) shouldBe 0
    }
    "return 1 if n = 1" in {
      fib(1) shouldBe 1
    }
    // 0 1 2 3 4 5 6
    // 0 1 1 2 3 5 8
    "return the sum of 2 previous terms for any legal n > 1" in {
      forAll(Gen.chooseNum(2, 200)) {
        n =>
          fib(n) shouldBe fib(n - 1) + fib(n - 2)
      }
    }
    "print" in {
      (1 to 10) map fib foreach println
      println(fib(200000))
      println(fib(200000).toString.length)
    }
  }
}
