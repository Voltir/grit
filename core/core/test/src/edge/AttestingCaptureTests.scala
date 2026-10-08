package grit.core.edge

import grit.core.durable.Probes

import utest.*

/** An attesting keeps no source: it captures the jot and the report it is built with and
  * nothing else, so an edge's source never outlives the call it was handed to.
  */
object AttestingCaptureTests extends TestSuite {

  private def probe(body: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.edge.*
         |import grit.core.store.*
         |import grit.core.identity.Account
         |object Probe {
         |  $body
         |}
         |""".stripMargin
    )

  val tests = Tests {
    test("control: an attesting over a voucher, a jot and a report captures only those two") {
      probe(
        """def make(v: Voucher, jot: Jot, report: Attesting.Report => Unit): Unit = {
          |    val made: Attesting^{jot, report} = new Attesting(v, jot, report)
          |    val _ = made
          |  }""".stripMargin
      ) ==> Nil
    }

    test("one that keeps the source it was built with is rejected at that type") {
      val errs = probe(
        """final class Keeping(v: Voucher, jot: Jot, report: Attesting.Report => Unit, s: RealmSource^) {
          |    def ask(a: Account): Asked = s.ask(a)
          |  }
          |  def make(v: Voucher, jot: Jot, report: Attesting.Report => Unit, s: RealmSource^): Unit = {
          |    val made: Keeping^{jot, report} = new Keeping(v, jot, report, s)
          |    val _ = made
          |  }""".stripMargin
      )
      assert(
        errs.exists(e => e.contains("capability `s²` cannot flow into capture set {jot², report²}"))
      )
    }

    test("one that keeps the last source it was handed in a field is rejected where declared") {
      val errs = probe(
        """final class Remembering(v: Voucher, jot: Jot, report: Attesting.Report => Unit) {
          |    private var last: Option[RealmSource^] = None
          |    def before(s: RealmSource^, a: Account): Unit = { last = Some(s) }
          |  }""".stripMargin
      )
      assert(errs.exists(_.contains("Mutable variable last")))
    }
  }
}
