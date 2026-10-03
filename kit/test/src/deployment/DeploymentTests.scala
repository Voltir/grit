package grit.kit.deployment

import scala.concurrent.duration.DurationInt

import grit.core.id.EdgeName
import grit.core.triage.Gate

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
        grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap")),
        grit.lifecycle.triage.TriageQuestions.Shipped.speak
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

    test(
      "two shadows of one name are refused, and shadows with topics off: no classifier could be asked"
    ) {
      def variant(name: String) = grit.lifecycle.shadow.ShadowVariant(
        grit.core.id.ShadowName.of(name).getOrElse(sys.error("a name")),
        grit.lifecycle.triage.TriageQuestions.V1,
        None,
        grit.core.spend.DailyCap.of("0.01").getOrElse(sys.error("a cap")),
        java.time.Instant.EPOCH
      )
      def shadowed(topics: Topics, shadows: String*) =
        Deployments.of(topics = topics, shadows = shadows.toVector.map(variant)).map(_ => ())
      (
        shadowed(Topics.Jev, "words", "replica", "words"),
        shadowed(Topics.Off("no key"), "words"),
        shadowed(Topics.Off("no key")),
        shadowed(Topics.Jev, "words", "replica"),
        shadowed(Topics.Stub, "words")
      ) ==> (
        Left(
          DeploymentRefusal.ShadowRepeated(
            grit.core.id.ShadowName.of("words").getOrElse(sys.error("a name"))
          )
        ),
        Left(DeploymentRefusal.ShadowsUnasked("no key")),
        Right(()),
        Right(()),
        Right(())
      )
    }

    test(
      "a review is refused unless it names a declared shadow, whose set's gate it keeps, " +
        "or while speaking is off: live's gate would never be reached"
    ) {
      def name(s: String) = grit.core.id.ShadowName.of(s).getOrElse(sys.error("a name"))
      def variant(called: String, questions: grit.lifecycle.triage.TriageQuestions) =
        grit.lifecycle.shadow.ShadowVariant(
          name(called),
          questions,
          None,
          grit.core.spend.DailyCap.of("0.01").getOrElse(sys.error("a cap")),
          java.time.Instant.EPOCH
        )
      val shadows = Vector(
        variant("v2", grit.lifecycle.triage.TriageQuestions.V2),
        variant("v1", grit.lifecycle.triage.TriageQuestions.V1)
      )
      val speaking = grit.core.speech.Speaking.Shadow(
        grit.core.speech.Limits.suggested(
          grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap")),
          grit.lifecycle.triage.TriageQuestions.Shipped.speak
        )
      )
      def reviewing(of: String) =
        grit.core.review.Reviewing.of(name(of), 8, 20, 24.hours).getOrElse(sys.error("a review"))
      def reviewed(of: String, speaks: grit.core.speech.Speaking = speaking) =
        Deployments
          .of(speaking = speaks, shadows = shadows, review = Some(reviewing(of)))
          .map(_.review.map(r => (r.reviewing.shadow, r.gate)))
      (
        reviewed("undeclared"),
        reviewed("v1"),
        reviewed("v2", grit.core.speech.Speaking.Off),
        reviewed("v2"),
        Deployments.of(speaking = speaking, shadows = shadows).map(_.review)
      ) ==> (
        Left(DeploymentRefusal.ReviewUngated(name("undeclared"))),
        Right(Some((name("v1"), grit.lifecycle.triage.TriageQuestions.V1.speak))),
        Left(DeploymentRefusal.ReviewUnspoken),
        Right(Some((name("v2"), grit.lifecycle.triage.TriageQuestions.V2.speak))),
        Right(None)
      )
    }

    test(
      "speaking by a gate that reads a question live triage does not ask is refused, naming it"
    ) {
      def speaks(gate: grit.core.triage.Gate) = {
        val limits = grit.core.speech.Limits.suggested(
          grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap")),
          gate
        )
        (
          Deployments.of(speaking = grit.core.speech.Speaking.Within(limits)).map(_ => ()),
          Deployments.of(speaking = grit.core.speech.Speaking.Shadow(limits)).map(_ => ())
        )
      }
      val chatter = grit.core.triage.Reading.Chosen(grit.core.triage.Tags.V1.kind, "chatter")
      (
        speaks(grit.lifecycle.triage.TriageQuestions.V1.speak),
        speaks(grit.lifecycle.triage.TriageQuestions.Shipped.speak)
      ) ==> (
        (
          Left(DeploymentRefusal.SpeechUnread(chatter)),
          Left(DeploymentRefusal.SpeechUnread(chatter))
        ),
        (Right(()), Right(()))
      )
    }

    test(
      "live triage that does not ask durable as a yes/no is refused: every period would earn a closing"
    ) {
      import grit.lifecycle.triage.TriageQuestions
      import grit.lifecycle.triage.TriageQuestions.Item
      def asking(question: grit.core.classify.Question) =
        TriageQuestions
          .of(
            Item.One(grit.core.triage.Earning.Durable, question),
            Vector.empty,
            Gate.Open
          )
          .getOrElse(sys.error("a set"))
      val open = grit.core.id.QuestionName.of("open").getOrElse(sys.error("a name"))
      val unasked = TriageQuestions
        .of(
          Item.One(open, grit.core.classify.Question.YesNo("?", None, None)),
          Vector.empty,
          Gate.Open
        )
        .getOrElse(sys.error("a set"))
      val chosen = grit.core.classify.Question
        .choice(
          "?",
          grit.core.classify.Question.Key("yes", None),
          grit.core.classify.Question.Key("no", None)
        )
        .getOrElse(sys.error("a choice"))
      (
        Deployment.earning(TriageQuestions.Shipped),
        Deployment.earning(TriageQuestions.V1),
        Deployment.earning(asking(grit.core.classify.Question.YesNo("?", None, None))),
        Deployment.earning(unasked),
        Deployment.earning(asking(chosen))
      ) ==> (
        Right(()),
        Right(()),
        Right(()),
        Left(DeploymentRefusal.DurableUnasked),
        Left(DeploymentRefusal.DurableUnasked)
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
