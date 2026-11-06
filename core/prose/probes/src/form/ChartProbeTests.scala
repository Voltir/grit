package grit.prose.form

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects of a chart made outside prose: only [[Chart.of]] makes one, so every
  * series is as long as the categories and every renderer may chart it without checking.
  * Pinned by compiling probe sources against prose, as a package outside it.
  */
object ChartProbeTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.prose.form.{Cell, Chart, Text}
      |
      |def three: Cell.Number = Cell.Number(BigDecimal(3), Text.plain("3"))
      |""".stripMargin

  private val built =
    """def c: Chart = Chart(Chart.Kind.Bar, "t", "a", None, Vector("x"), Vector.empty)
      |""".stripMargin

  private val series =
    """def s: Chart.Series = Chart.Series("n", Vector(three))
      |""".stripMargin

  private val copied =
    """def f(c: Chart): Chart = c.copy(categories = Vector.empty)
      |""".stripMargin

  private val made =
    """def c = Chart.of(Chart.Kind.Bar, "t", None, Vector("a", "n"), Vector(Vector(three, three)))
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(built, series, copied, made))

  val tests = Tests {
    test("a chart built by its constructor outside prose is rejected") {
      compiled.errors(built) ==> List(
        "object Chart in package grit.prose.form does not take parameters"
      )
    }

    test("a series built outside prose is rejected") {
      compiled.errors(series) ==> List("object Series in object Chart does not take parameters")
    }

    test("a chart copied with other categories outside prose is rejected") {
      val errs = compiled.errors(copied)
      assert(errs.size == 1, errs.forall(_.contains("private method copy can only be accessed")))
    }

    test("a chart made by Chart.of is accepted") {
      compiled.errors(made) ==> Nil
    }
  }
}
