package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.{CorpusName, QuestionName}
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.place.Service
import grit.core.recipe.{Rooted, ServiceOffer}
import grit.core.store.Focus
import grit.core.tool.{ToolName, ToolSetId}
import grit.core.triage.Tags
import grit.eval.harness.corpus.{Offered, TurnCase}
import grit.turn.{TurnOffer, TurnShape}

import utest.*

/** What a recorded turn is given under each variant: the tools left once its recipe's offering
  * has withheld services, decided over the services and sources its shape recorded by its
  * root's answers, and its window's width.
  */
object TurnVariantTests extends TestSuite {

  import Fixtures.right

  private val assembled = Assembled.Shipped.copy(window = Tokens(4000), hits = 5)
  private def variant(name: String) = right(TurnVariant.named(name, assembled))
  private def source(n: String) = right(CorpusName.of(n))
  private def yes(n: String, p: Double) = right(QuestionName.read(n)) -> Answer.YesNo(p)
  private val github = right(Service.of("github"))
  private val slack = right(Service.of("slack"))

  // Worked in the github service, reaching slack's post; read is grit's own, no service's.
  private val read = ToolName("read")
  private val searchCode = ToolName("github_search_code")
  private val getFile = ToolName("github_get_file")
  private val post = ToolName("slack_post")
  private val whole: Vector[ToolName] = Vector(read, searchCode, getFile, post)
  private val wholeId = right(ToolSetId.of("0123456789abcdef"))

  private def took(
      service: Service,
      via: TurnShape.Via,
      tools: Vector[ToolName],
      sources: Vector[CorpusName],
      verdict: ServiceOffer.Verdict = ServiceOffer.Verdict.Ungated
  ) = TurnShape.Took(ServiceOffer(service, sources, verdict), via, tools)

  // As recorded: github supplied by github; slack by docs and wiki (made up, to have one
  // service of two sources); nothing withheld.
  private val shape = TurnShape(
    Width.Deployed,
    wholeId,
    Vector(
      took(github, TurnShape.Via.Workspace, Vector(searchCode, getFile), Vector(source("github"))),
      took(slack, TurnShape.Via.Reached, Vector(post), Vector(source("docs"), source("wiki")))
    )
  )

  /** A turn rooted as `rooted`, offered `offered` of `whole`, recorded with `recorded`. */
  private def turn(
      rooted: Rooted,
      recorded: Option[TurnShape] = Some(shape),
      offered: Vector[ToolName] = whole
  ): TurnCase = {
    val t = rooted match {
      case Rooted.Heard(focus) => Fixtures.turn(root = TurnOffer.Root.Heard).copy(focus = focus)
      case Rooted.Addressed => Fixtures.turn(root = TurnOffer.Root.Addressed)
    }
    t.copy(offered =
      Some(
        Offered(
          offered,
          wholeId,
          Tokens(10),
          VectorMap.empty,
          Some(github.place),
          Vector(slack.place),
          recorded
        )
      )
    )
  }

  private val heard = Rooted.Heard(Focus.Open)
  private val roots: Vector[Rooted] = Vector(heard, Rooted.Heard(Focus.Focused), Rooted.Addressed)

  private def shaped(v: String, t: TurnCase, answers: Option[VectorMap[QuestionName, Answer]]) =
    TurnVariant.shape(variant(v), t, answers)

  /** The tools of `whole` `t` is offered under `v`. */
  private def tools(
      v: String,
      t: TurnCase,
      answers: Option[VectorMap[QuestionName, Answer]]
  ): Vector[ToolName] = whole.filter(shaped(v, t, answers).offers)

  val tests = Tests {
    test(
      "offer-x withholds the tools of a service whose every recorded source reads under x, on every root"
    ) {
      val answers = Some(
        VectorMap(
          yes("source:conversations", 0.1),
          yes("source:github", 0.25),
          yes("source:docs", 0.1),
          yes("source:wiki", 0.4)
        )
      )
      // github at 0.25 is under 0.3 but not 0.2; slack's wiki at 0.4 holds it offered at 0.3,
      // and at 0.5 both its sources are under.
      Vector("offer-0.2", "offer-0.3", "offer-0.5").flatMap(v =>
        roots.map(r => tools(v, turn(r), answers))
      ) ==> Vector.fill(3)(whole) ++ Vector.fill(3)(Vector(read, post)) ++
        Vector.fill(3)(Vector(read))
      shaped("offer-0.3", turn(heard), answers).services ==> Some(Set(slack))
    }

    test("offer-0.3-0.1 gates a heard turn at 0.3 and one said to grit at 0.1") {
      val answers = Some(VectorMap(yes("source:github", 0.25), yes("source:docs", 0.05)))
      tools("offer-0.3-0.1", turn(heard), answers) ==> Vector(read, post)
      tools("offer-0.3-0.1", turn(Rooted.Addressed), answers) ==> whole
    }

    test("offer-heard-0.2 gates a heard turn at 0.2 and offers a turn said to grit everything") {
      val answers = Some(VectorMap(yes("source:github", 0.1), yes("source:docs", 0.1)))
      tools("offer-heard-0.2", turn(heard), answers) ==> Vector(read, post)
      tools("offer-heard-0.2", turn(Rooted.Addressed), answers) ==> whole
    }

    test("a turn with no answers, or none for a service's other source, is offered every tool") {
      tools("offer-0.5", turn(heard), None) ==> whole
      // docs under, wiki unasked: slack is unread, github unasked; both offered.
      tools("offer-0.5", turn(heard), Some(VectorMap(yes("source:docs", 0.1)))) ==> whole
    }

    test("a wide variant doubles its root's window and leaves the other's as deployed") {
      val wide = Width.Within(Tokens(8000), 5)
      Vector("shipped", "wide-heard", "wide-addressed").map(v =>
        roots.map(r => shaped(v, turn(r), None).width)
      ) ==> Vector(
        Vector(Width.Deployed, Width.Deployed, Width.Deployed),
        Vector(wide, wide, Width.Deployed),
        Vector(Width.Deployed, Width.Deployed, wide)
      )
    }

    test("a recorded turn is rooted as heard at its focus, or as said to grit") {
      roots.map(r => TurnVariant.rooted(turn(r))) ==> roots
    }

    test("a turn is decided over the services, sources and tools its shape recorded") {
      // Recorded: github supplied by repo alone, and only github_search_code taken from it.
      val recorded = shape.copy(services =
        Vector(took(github, TurnShape.Via.Workspace, Vector(searchCode), Vector(source("repo"))))
      )
      val answers = Some(VectorMap(yes("source:repo", 0.1), yes("source:github", 0.9)))
      val t = turn(heard, Some(recorded))
      (tools("offer-0.3", t, answers), shaped("offer-0.3", t, answers).services) ==>
        (Vector(read, getFile, post), Some(Set.empty))
    }

    test(
      "a turn recorded before shapes is offered as recorded under every variant, its services unknown"
    ) {
      val answers = Some(VectorMap(yes("source:github", 0.0), yes("source:docs", 0.0)))
      val t = turn(heard, None, Vector(read, searchCode))
      TurnVariant
        .all(assembled)
        .map(v => (tools(v.name, t, answers), shaped(v.name, t, answers).services)) ==>
        Vector.fill(TurnVariant.all(assembled).size)((Vector(read, searchCode), None))
    }

    test("a service withheld live is offered under shipped, from the shape's whole set") {
      val answers = Some(VectorMap(yes("source:github", 0.1)))
      val withheld = shape.copy(services =
        shape.services.map(s =>
          if (s.offer.service == github)
            s.copy(offer =
              s.offer.copy(verdict =
                ServiceOffer.Verdict.Checked(
                  Tags.V2.source(source("github"), Probability.clamped(0.2)).check(answers.get)
                )
              )
            )
          else s
        )
      )
      val t = turn(heard, Some(withheld), Vector(read, post))
      (tools("shipped", t, answers), tools("offer-0.2", t, answers)) ==>
        (whole, Vector(read, post))
    }

    test(
      "the variant whose recipe a turn ran under decides its services as recorded, and another does not"
    ) {
      // Recorded under offer-heard-0.2's recipe, by these answers: github withheld, slack
      // offered (docs passes; wiki unasked).
      val answers = VectorMap(yes("source:github", 0.1), yes("source:docs", 0.3))
      def checked(sources: Vector[CorpusName]) =
        sources.map(s => Tags.V2.source(s, Probability.clamped(0.2))) match {
          case first +: rest =>
            ServiceOffer.Verdict.Checked(
              (if (rest.isEmpty) first else grit.core.triage.Gate.AnyOf(first, rest)).check(answers)
            )
          case _ => ServiceOffer.Verdict.Ungated
        }
      val recorded =
        shape.copy(services =
          shape.services.map(s => s.copy(offer = s.offer.copy(verdict = checked(s.offer.sources))))
        )
      val t = turn(heard, Some(recorded))
      Vector("offer-heard-0.2", "offer-0.5", "shipped").map(v =>
        TurnVariant.asRecorded(variant(v), t, Some(answers))
      ) ==> Vector(Some(true), Some(false), Some(false))
      TurnVariant.asRecorded(variant("offer-heard-0.2"), turn(heard, None), Some(answers)) ==> None
    }
  }
}
