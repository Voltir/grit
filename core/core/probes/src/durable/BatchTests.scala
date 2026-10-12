package grit.core.durable

import utest.*
import Probes.{heldImpure, options}

/** What [[Probes.batch]] promises: one compile serves every probe it can, each probe sees only
  * its own errors, and an error that stops the compiler before capture checking costs the batch
  * its speed, never another probe's verdict.
  */
object BatchTests extends TestSuite {

  private val prelude = "package probe\n"

  private val holds =
    "class Holds(c: caps.SharedCapability^) extends caps.Pure { val kept = c }"
  private val clean = "def id(s: String): String = s"
  private val typerError = "val wrong: Int = \"text\""
  private val setupError = "val wrong: Int^ = 1"

  val tests = Tests {
    test("an error in one probe is not reported against another") {
      val b = Probes.batch(prelude, List(holds, clean))
      assert(heldImpure(b.errors(holds)), b.errors(clean).isEmpty)
    }

    test("a typer error in one probe does not mask another's capture error") {
      val b = Probes.batch(prelude, List(typerError, holds))
      assert(heldImpure(b.errors(holds)), b.errors(typerError).exists(_.contains("Found:")))
    }

    test("a Setup-phase error in one probe does not mask another's capture error") {
      val b = Probes.batch(prelude, List(setupError, holds))
      assert(
        heldImpure(b.errors(holds)),
        b.errors(setupError).exists(_.contains("makes no sense to add a capture set"))
      )
    }

    test("a batch with no breach reaches capture checking") {
      val b = Probes.batch(prelude, List(clean, "def twice(s: String): String = s + s"))
      assert(b.batched, b.errors(clean).isEmpty)
    }

    test("a batch with capture checking off reaches the typer") {
      val off = options.filterNot(_.startsWith("-language:experimental."))
      val b = Probes.batch(prelude, List(clean, typerError), off)
      assert(b.batched, b.errors(clean).isEmpty, b.errors(typerError).nonEmpty)
    }
  }
}
