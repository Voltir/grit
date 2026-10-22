package grit.core.files

import grit.core.durable.Probes

import utest.*

/** A format reaches its file only through the [[OpenFile]] it is handed: one that holds a
  * capability, or a deployment's choice of formats that captures one, does not compile.
  */
object FormatCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.files.*
      |trait Tx extends caps.SharedCapability { def run(): Int }
      |""".stripMargin

  /* A format holding `field`; its type left to inference, so only `Format`'s pure self type can
   * refuse what it holds. */
  private def format(field: String, use: String): String =
    s"""def make($field): Unit = {
       |    val made = new Format {
       |      def name: FormatName = FormatName("probe")
       |      def version: Int = $use
       |      def read(file: OpenFile^, limits: ReadLimits): Either[Unread, Understood] =
       |        if (file.late(limits)) Left(Unread.TooSlow) else Left(Unread.NotMine)
       |    }
       |    val _ = made
       |  }""".stripMargin

  private val reading = format("n: Int", "n")
  private val holding = format("tx: Tx^", "tx.run()")
  private val choosing =
    """def choose(tx: Tx^, pdf: Format): Unit = {
      |    val chosen = Formats(kind => if (tx.run() > 0 && kind == Kind.Pdf) Vector(pdf) else Vector.empty)
      |    val _ = chosen
      |  }""".stripMargin
  private lazy val compiled = Probes.batch(prelude, List(reading, holding, choosing))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: a format reading only the file it is handed compiles") {
      compiled.errors(reading) ==> Nil
    }

    test("a format holding a capability is rejected") {
      assert(Probes.heldImpure(compiled.errors(holding)))
    }

    test("a choice of formats that captures a capability is rejected") {
      assert(Probes.flowsInto("{}")(compiled.errors(choosing)))
    }
  }
}
