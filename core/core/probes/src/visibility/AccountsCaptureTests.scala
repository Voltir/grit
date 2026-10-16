package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A service's extension accounts for a result by the result alone: an [[Accounts]] that holds
  * a capability does not compile, since the turn's step runs it inside its transaction. And no
  * code outside core makes an [[Accounted.Items]] but through [[Accounted.of]], which bounds it.
  */
object AccountsCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.clock.Clock
      |import grit.core.place.Place
      |import grit.core.store.Tx
      |import grit.core.visibility.*
      |""".stripMargin

  private val byResult =
    """object ByResult extends Accounts[String] {
      |    def items(result: String): Accounted =
      |      if (result.isEmpty) Accounted.Unread
      |      else Accounted.of(Vector(Provenance(Place.Everywhere, Integrity.Member)))
      |  }""".stripMargin

  private val holdingClock =
    """final class Timed(clock: Clock^) extends Accounts[String] {
      |    def items(result: String): Accounted = { val _ = clock.now(); Accounted.Unread }
      |  }""".stripMargin

  private val closingOverTx =
    """def reading(tx: Tx^): Unit = {
      |    val made = new Accounts[String] {
      |      def items(result: String): Accounted = { val _ = Tx.visibility(tx); Accounted.Unread }
      |    }
      |    val _ = made.items("")
      |  }""".stripMargin

  private val madeOutside =
    """val made: Accounted = Accounted.Items(Vector.empty)"""

  private val copiedOutside =
    """def widened(p: Provenance): Accounted = Accounted.of(Vector(p)) match {
      |    case i: Accounted.Items => i.copy(Vector.fill(40)(p))
      |    case other => other
      |  }""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(byResult, holdingClock, closingOverTx))

  /* Apart, since an access error stops the compile before capture checking runs. */
  private lazy val reached = Probes.batch(prelude, List(madeOutside, copiedOutside))

  val tests = Tests {
    test("one compile answers each batch") {
      (compiled.runs, reached.runs) ==> (1, 1)
    }

    test("control: an Accounts reading the result alone compiles") {
      compiled.errors(byResult) ==> Nil
    }

    test("an Accounts holding a clock is rejected") {
      val errs = compiled.errors(holdingClock)
      assert(
        errs.exists(e =>
          e.contains(
            "Reference `Timed.this.clock` is not included in the allowed capture set {}"
          ) &&
            e.contains("self type of class Timed")
        )
      )
    }

    test("an Accounts closing over a transaction is rejected") {
      val errs = compiled.errors(closingOverTx)
      assert(
        errs.exists(e =>
          e.contains("Reference `tx` is not included in the allowed capture set {}") &&
            e.contains("Accounts")
        )
      )
    }

    test("Items is neither made nor copied outside core's visibility package") {
      val made = reached.errors(madeOutside)
      val copied = reached.errors(copiedOutside)
      assert(
        made.exists(_.contains("object Items in object Accounted does not take parameters")),
        copied.exists(e =>
          e.contains("method copy cannot be accessed") &&
            e.contains("can only be accessed from package grit.core.visibility")
        )
      )
    }
  }
}
