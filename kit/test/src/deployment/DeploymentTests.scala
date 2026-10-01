package grit.kit.deployment

import scala.concurrent.duration.DurationInt

import grit.core.id.EdgeName

import utest.*

/** What [[Deployment.of]] refuses. */
object DeploymentTests extends TestSuite {
  import Deployments.edge

  private def of(edges: Vector[grit.core.edge.ServedEdge], tools: Offered = Offered.Read) =
    Deployments.of(edges = edges, tools = tools).map(_ => ())

  val tests = Tests {
    test("tools that ask first are refused when an edge served cannot answer an ask, naming it") {
      (
        of(Vector(edge("slack", asks = false), edge("pager", asks = true)), Offered.All),
        of(Vector(edge("pager", asks = true)), Offered.All),
        of(Vector(edge("slack", asks = false)), Offered.Read),
        of(Vector.empty, Offered.All)
      ) ==> (
        Left(DeploymentRefusal.AsksUnanswered(Vector(EdgeName("slack")))),
        Right(()),
        Right(()),
        Right(())
      )
    }

    test("two edges of one name are refused") {
      of(Vector(edge("slack", asks = false), edge("slack", asks = false))) ==>
        Left(DeploymentRefusal.EdgeRepeated(EdgeName("slack")))
    }

    test("speaking unprompted with topics off is refused: no classifier could judge a draft") {
      val limits = grit.core.speech.Limits.suggested(
        grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap"))
      )
      val off = Topics.Off("no key")
      (
        Deployments
          .of(topics = off, speaking = grit.core.speech.Speaking.Within(limits))
          .map(_ => ()),
        Deployments
          .of(topics = off, speaking = grit.core.speech.Speaking.Shadow(limits))
          .map(_ => ()),
        Deployments.of(topics = off).map(_ => ()),
        Deployments
          .of(topics = Topics.Stub, speaking = grit.core.speech.Speaking.Within(limits))
          .map(_ => ())
      ) ==> (
        Left(DeploymentRefusal.SpeaksUnjudged("no key")),
        Left(DeploymentRefusal.SpeaksUnjudged("no key")),
        Right(()),
        Right(())
      )
    }

    test("a sweep under a second is refused; a second is not") {
      (
        Deployments.of(sweep = 999.millis).map(_ => ()),
        Deployments.of(sweep = 1.second).map(_ => ())
      ) ==> (Left(DeploymentRefusal.SweepTooOften(999.millis)), Right(()))
    }
  }
}
