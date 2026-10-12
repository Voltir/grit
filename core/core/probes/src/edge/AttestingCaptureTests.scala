package grit.core.edge

import grit.core.durable.Probes

import utest.*

/** An attesting keeps no source: it captures the jot and the report it is built with and
  * nothing else, so an edge's source never outlives the call it was handed to.
  */
object AttestingCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.edge.*
      |import grit.core.store.*
      |import grit.core.identity.Account
      |""".stripMargin

  private val attesting =
    """def make(v: Voucher, jot: Jot, report: Attesting.Report => Unit): Unit = {
      |    val made: Attesting^{jot, report} = new Attesting(v, jot, report)
      |    val _ = made
      |  }""".stripMargin

  private val keeping =
    """final class Keeping(v: Voucher, jot: Jot, report: Attesting.Report => Unit, s: RealmSource^) {
      |    def ask(a: Account): Asked = s.ask(a)
      |  }
      |  def make(v: Voucher, jot: Jot, report: Attesting.Report => Unit, s: RealmSource^): Unit = {
      |    val made: Keeping^{jot, report} = new Keeping(v, jot, report, s)
      |    val _ = made
      |  }""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(attesting, keeping))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: an attesting over a voucher, a jot and a report captures only those two") {
      compiled.errors(attesting) ==> Nil
    }

    test("one that keeps the source it was built with is rejected at that type") {
      val errs = compiled.errors(keeping)
      assert(
        errs.exists(e => e.contains("capability `s²` cannot flow into capture set {jot², report²}"))
      )
    }
  }
}
