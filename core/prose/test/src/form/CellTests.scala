package grit.prose.form

import utest.*

/** What reads as a number in a cell's words. */
object CellTests extends TestSuite {

  private def number(value: String, shown: String): Option[Cell.Number] =
    Some(Cell.Number(BigDecimal(value), Text.plain(shown)))

  val tests = Tests {

    test("a number reads with its sign, currency, thousands and decimals, shown as written") {
      (
        Cell.Number.read(" -$1,200.50 "),
        Cell.Number.read("+3"),
        Cell.Number.read("£7"),
        Cell.Number.read("1200")
      ) ==> (
        number("-1200.50", "-$1,200.50"),
        number("3", "+3"),
        number("7", "£7"),
        number("1200", "1200")
      )
    }

    test("a percentage's value is the number as written") {
      Cell.Number.read("12.5%") ==> number("12.5", "12.5%")
    }

    test("a scale suffix, a unit, a comma decimal and words read as no number") {
      Vector("1.2k", "3M", "3 kg", "1,5", "1,2345", "12,34,567", "", "five", "$", ".5", "5.", "--5")
        .map(Cell.Number.read) ==> Vector.fill(12)(None)
    }
  }
}
