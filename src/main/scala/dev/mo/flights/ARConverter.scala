package dev.mo.flights

object ARConverter {
  def toRoman(arabic: Int): Option[String] = {
    def convert = {
      val roman = "MCXI".toCharArray

      def itr(n: Int, acc: String): String = if (n == 0) acc else itr(n - 1, acc + roman(((arabic - n) / 3) % roman.size))

      itr(arabic, "")
    }

    if (arabic < 1 || arabic > 3999) None else Some(convert)
  }
}
