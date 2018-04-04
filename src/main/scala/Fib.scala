import scala.annotation.tailrec

trait Fib {
  def fib(n: Int): BigDecimal =
    if (n < 1) 0
    else fibTR(0, 1, n - 2)

  @tailrec
  private def fibTR(f_2: BigDecimal, f_1: BigDecimal, k: Int): BigDecimal =
    if (k <= 0) f_2 + f_1
    else fibTR(f_1, f_2 + f_1, k - 1)

  def fibonacci(n: Int): BigDecimal =
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
}
