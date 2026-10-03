package grit.core.recipe

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{KnowledgeSourceName, QuestionName}
import grit.core.period.Probability
import grit.core.place.Service
import grit.core.triage.{Bound, Gate, Reading, Tags}

import utest.*

/** What offering decides of each service: offered, withheld with what its gate found, or
  * unread, from the root's per-source answers.
  */
object OfferingTests extends TestSuite {

  private def p(x: Double): Probability = Probability.clamped(x)
  private def source(n: String): KnowledgeSourceName =
    KnowledgeSourceName.of(n).getOrElse(throw new java.lang.AssertionError(n))
  private def service(n: String): Service =
    Service.of(n).getOrElse(throw new java.lang.AssertionError(n))
  private def per(n: String): QuestionName = QuestionName.per(Tags.V2.sourcePrefix, source(n))
  private def yes(n: String, v: Double) = per(n) -> Answer.YesNo(v)

  private val github = service("github")
  private val docs = service("docs")
  private val linear = service("linear")

  // github is one source's; docs is two sources' (made up, to have one service of two); linear
  // is no source's.
  private val supplied: VectorMap[Service, Vector[KnowledgeSourceName]] = VectorMap(
    github -> Vector(source("github")),
    docs -> Vector(source("handbook"), source("wiki"))
  )
  private val services: Vector[Service] = Vector(linear, github, docs)

  private def at(name: String, v: Double) = Gate.Failed(
    Bound.AtLeast(Reading.Yes(per(name)), p(0.3)),
    p(v)
  )

  val tests = Tests {
    test(
      "by source, a service is withheld with every source's failure when each reads under its line"
    ) {
      val answers =
        VectorMap(yes("github", 0.25), yes("handbook", 0.1), yes("wiki", 0.2))
      Offering.decide(Offering.BySource(p(0.3)), supplied, services, Some(answers)) ==> Vector(
        ServiceOffer(linear, Vector.empty, ServiceOffer.Verdict.Ungated),
        ServiceOffer(
          github,
          Vector(source("github")),
          ServiceOffer.Verdict.Checked(Gate.Checked.Fails(at("github", 0.25), Vector.empty))
        ),
        ServiceOffer(
          docs,
          Vector(source("handbook"), source("wiki")),
          ServiceOffer.Verdict.Checked(
            Gate.Checked.Fails(at("handbook", 0.1), Vector(at("wiki", 0.2)))
          )
        )
      )
    }

    test("a service of two sources is offered when either reads at its line") {
      val answers = VectorMap(yes("github", 0.3), yes("handbook", 0.1), yes("wiki", 0.3))
      Offering
        .decide(Offering.BySource(p(0.3)), supplied, services, Some(answers))
        .map(o => o.service -> o.verdict) ==> Vector(
        linear -> ServiceOffer.Verdict.Ungated,
        github -> ServiceOffer.Verdict.Checked(Gate.Checked.Passes),
        docs -> ServiceOffer.Verdict.Checked(Gate.Checked.Passes)
      )
    }

    test(
      "a source the answers do not answer leaves its service unread, offered, unless another fails it"
    ) {
      // wiki unanswered and handbook under: docs could pass on wiki, so it is unread.
      val answers = VectorMap(yes("handbook", 0.1))
      val decided = Offering.decide(Offering.BySource(p(0.3)), supplied, services, Some(answers))
      decided.map(o => o.service -> o.verdict) ==> Vector(
        linear -> ServiceOffer.Verdict.Ungated,
        github -> ServiceOffer.Verdict.Checked(Gate.Checked.Unread(Reading.Yes(per("github")))),
        docs -> ServiceOffer.Verdict.Checked(Gate.Checked.Unread(Reading.Yes(per("wiki"))))
      )
      decided.filter(_.withheld) ==> Vector.empty
    }

    test("a root with no answers leaves every gated service unweighed, offered") {
      val decided = Offering.decide(Offering.BySource(p(0.3)), supplied, services, None)
      decided.map(o => o.service -> o.verdict) ==> Vector(
        linear -> ServiceOffer.Verdict.Ungated,
        github -> ServiceOffer.Verdict.Unweighed,
        docs -> ServiceOffer.Verdict.Unweighed
      )
      decided.filter(_.withheld) ==> Vector.empty
    }

    test("all offers every service ungated, whatever the answers") {
      val answers = VectorMap(yes("github", 0.0), yes("handbook", 0.0), yes("wiki", 0.0))
      Offering
        .decide(Offering.All, supplied, services, Some(answers))
        .map(o => o.service -> o.verdict) ==> services.map(_ -> ServiceOffer.Verdict.Ungated)
    }

    test("only a failed gate withholds") {
      def offer(v: ServiceOffer.Verdict) = ServiceOffer(github, Vector(source("github")), v)
      Vector(
        ServiceOffer.Verdict.Ungated,
        ServiceOffer.Verdict.Unweighed,
        ServiceOffer.Verdict.Checked(Gate.Checked.Passes),
        ServiceOffer.Verdict.Checked(Gate.Checked.Unread(Reading.Yes(per("github")))),
        ServiceOffer.Verdict.Checked(Gate.Checked.Fails(at("github", 0.1), Vector.empty))
      ).map(v => offer(v).withheld) ==> Vector(false, false, false, false, true)
    }
  }
}
