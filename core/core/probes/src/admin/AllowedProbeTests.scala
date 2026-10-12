package grit.core.admin

import grit.core.durable.Probes

import utest.*

/** An allowed change comes only from [[Authority.decide]], so every change applied passes the
  * one rule. A constructor's access is not seen by `typeChecks`, so the probe compiles a source
  * of its own (docs/capture-checking.md).
  */
object AllowedProbeTests extends TestSuite {

  private def probe(body: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.admin.*
         |import grit.core.identity.Principal
         |import grit.core.place.Place
         |import grit.core.visibility.Compartments
         |object Probe {
         |  val quiet: Change = Change.Quiet(Place.Everywhere, true)
         |  $body
         |}
         |""".stripMargin
    )

  val tests = Tests {
    test("control: an allowed change is had by deciding it, and names its change") {
      probe(
        "val allowed: Either[Refusal, Change] = Authority.decide(quiet, Principal.Grit, None, " +
          "false, Set.empty, Compartments.Shipped, Set.empty).map(_.change)"
      ) ==> Nil
    }

    test("an allowed change made by anything but deciding it is rejected") {
      val errs = probe("val made: Authority.Allowed = new Authority.Allowed(quiet)")
      errs.map(_.linesIterator.toVector.headOption) ==> List(
        Some(
          "constructor Allowed cannot be accessed as a member of " +
            "grit.core.admin.Authority.Allowed from object Probe."
        )
      )
    }
  }
}
