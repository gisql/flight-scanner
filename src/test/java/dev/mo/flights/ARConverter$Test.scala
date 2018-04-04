package dev.mo.flights

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class ARConverter$Test extends WordSpec with GeneratorDrivenPropertyChecks with Matchers {
  import ARConverter.toRoman

  private val invalidNumbers = Gen.oneOf(Gen.chooseNum(Int.MinValue, 0), Gen.chooseNum(4000, Int.MaxValue))
  private val validNumbers = Gen.chooseNum(1, 3999)

  "Arabic to Roman converter" should {
    "return None for invalid input" in {
      forAll(invalidNumbers) {
        n =>
          toRoman(n) should not be defined
      }
    }

    "return a string containing at least one of the roman digits" in {
      forAll(validNumbers) {
        n =>
          toRoman(n).get should contain atLeastOneOf('I', 'V', 'X', 'L', 'C', 'D', 'M')
      }
    }

    "return different results for different numbers" in {
      forAll(validNumbers, validNumbers) {
        (m, n) =>
          whenever(m != n) {
            toRoman(n) should not be toRoman(m)
          }
      }
    }

    "produce result containing only roman digits" in {
      forAll(validNumbers) {
        n =>
          toRoman(n).get should fullyMatch regex "^[IVXLCDM]+$"
      }
    }

    "not repeat any digit more than thrice" in {
      forAll(validNumbers) {
        n =>
          toRoman(n).get should not(include regex "(I{4,}|V{4,}|X{4,}|L{4,}|C{4,}|D{4,}|M{4,})")
      }
    }

    "not have I before L, C D or M; nor V before V, X, L, C, D M; nor L before L, C, M; nor D before M or D" in {
      forAll(validNumbers) {
        n =>
          val roman = toRoman(n).get
          Seq(
            "I.*L", "I.*C", "I.*D", "I.*M",
            "V.*L", "V.*V", "V.*C", "V.*D", "V.*M",
            "X.*D", "X.*M",
            "L.*L", "L.*C", "L.*D", "L.*M",
            "D.*D", "D.*M"
          ) foreach {
            forbidden => roman should not(include regex forbidden)
          }
      }
    }

    "not have any letter more than 4 times" in {
      forAll(validNumbers) {
        n => {
          val letterCounts = toRoman(n).get.toCharArray.groupBy(x => x).map(_._2.length)
          letterCounts.max should be < 4
        }
      }
    }
  }
}
