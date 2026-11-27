package grit.kit.run

import grit.kit.deployment.{Deployments, Placer}
import grit.kit.environment.Secrets
import grit.tools.MachineGuard

import utest.*

/** [[Launch.guarding]]: the classifier a deployment's machine guard asks, by its placer. */
object GuardingTests extends TestSuite {

  private val db = Map("GRIT_DATABASE_URL" -> "jdbc:postgresql://localhost:5432/grit_test")

  private def guarding(placer: Placer): Either[String, grit.core.classify.Classifier^] = {
    val d = Deployments.accepted(placer = placer)
    Launch.guarding(d, Secrets.of(db, d).fold(r => sys.error(r.toString), identity))
  }

  val tests = Tests {
    test(
      "under the stub placer the guard asks the stub, which finds every harm likely, so it refuses whatever code does not clear"
    ) {
      val asked = guarding(Placer.Stub) match {
        case Right(c) =>
          val state = MachineGuard.State("find src -name x", "/co", None)
          c.ask(state, MachineGuard.questions(state))
            .map(_.value.most >= MachineGuard.Refuses)
        case Left(why) => Left(why)
      }
      asked ==> Right(true)
    }

    test("with the placer off, the guard has no classifier, saying why") {
      guarding(Placer.Off("quiet")).left.toOption ==> Some("quiet")
    }
  }
}
