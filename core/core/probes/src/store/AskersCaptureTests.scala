package grit.core.store

import grit.core.durable.Probes

import utest.*

/** Askers reach the database only through the transaction they are given: one that holds a
  * capability does not compile, since the kit hands the engine's askers to a tool for as long
  * as the engine runs.
  */
object AskersCaptureTests extends TestSuite {

  /* Its type left to inference, so only the trait's own self type can refuse `p`. */
  private val prelude =
    """package outside
      |import grit.core.store.*
      |import grit.core.id.TurnRef
      |import grit.core.identity.Principal
      |trait Provider extends caps.SharedCapability { def call(): String }
      |""".stripMargin

  private def askers(answers: String): String =
    s"""def make(p: Provider^): Unit = {
       |    val made = new Askers {
       |      def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] =
       |        $answers
       |    }
       |    val _ = made
       |  }""".stripMargin

  private val throughTx = askers("Right(Some(Principal.Grit))")
  private val calling = askers("{ p.call(); Right(None) }")
  private lazy val compiled = Probes.batch(prelude, List(throughTx, calling))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: askers answering through the transaction they are given compile") {
      compiled.errors(throughTx) ==> Nil
    }

    test("askers that call a capability they hold are rejected") {
      val errs = compiled.errors(calling)
      assert(Probes.heldImpure(errs) && errs.exists(_.contains("Askers")))
    }
  }
}
