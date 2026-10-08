package grit.core.store

import grit.core.durable.Probes

import utest.*

/** Askers reach the database only through the transaction they are given: one that holds a
  * capability does not compile, since the kit hands the engine's askers to a tool for as long
  * as the engine runs.
  */
object AskersCaptureTests extends TestSuite {

  /* Its type left to inference, so only the trait's own self type can refuse `p`. */
  private def probe(answers: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.store.*
         |import grit.core.id.TurnRef
         |import grit.core.identity.Principal
         |trait Provider extends caps.SharedCapability { def call(): String }
         |object Probe {
         |  def make(p: Provider^): Unit = {
         |    val made = new Askers {
         |      def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] =
         |        $answers
         |    }
         |    val _ = made
         |  }
         |}
         |""".stripMargin
    )

  val tests = Tests {
    test("control: askers answering through the transaction they are given compile") {
      probe("Right(Some(Principal.Grit))") ==> Nil
    }

    test("askers that call a capability they hold are rejected") {
      val errs = probe("{ p.call(); Right(None) }")
      assert(Probes.heldImpure(errs) && errs.exists(_.contains("Askers")))
    }
  }
}
