package grit.app.main

import grit.core.allot.EffortRouting
import grit.kit.deployment.Offered

import utest.*

/** Whether the reference deployment's turns' routes lay an effort over their pins'
  * (`GRIT_EFFORT_ROUTING`).
  */
object EffortRoutingChoiceTests extends TestSuite {

  private def routing(env: (String, String)*): Either[String, EffortRouting] =
    Deploying
      .deployment(env.toMap, Offered.Read, Vector.empty, Vector.empty, java.time.ZoneOffset.UTC)
      .map(_.effort)

  val tests = Tests {
    test(
      "GRIT_EFFORT_ROUTING: unset or routed is Routed, held, in any spacing, Held; any other word an error naming the variable"
    ) {
      routing() ==> Right(EffortRouting.Routed)
      routing("GRIT_EFFORT_ROUTING" -> "routed") ==> Right(EffortRouting.Routed)
      routing("GRIT_EFFORT_ROUTING" -> " held ") ==> Right(EffortRouting.Held)
      routing("GRIT_EFFORT_ROUTING" -> "high") ==>
        Left("GRIT_EFFORT_ROUTING is neither routed nor held")
    }
  }
}
