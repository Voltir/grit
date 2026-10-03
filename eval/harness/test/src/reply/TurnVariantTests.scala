package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.{KnowledgeSourceName, QuestionName}
import grit.core.message.Tokens
import grit.core.place.{Place, Service}
import grit.core.recipe.Rooted
import grit.core.store.Focus
import grit.core.tool.{Retry, ToolName, ToolSet, ToolSetId}
import grit.core.triage.{KnowledgeSource, KnowledgeSources}
import grit.turn.TurnOffer

import utest.*

/** What a recorded turn is given under each variant: the tools left once its recipe's offering
  * has withheld services by its root's per-source answers, and its window's width.
  */
object TurnVariantTests extends TestSuite {

  import Fixtures.right

  private val assembled = Assembled.Shipped.copy(window = Tokens(4000), hits = 5)
  private def variant(name: String) = right(TurnVariant.named(name, assembled))
  private def yes(n: String, p: Double) = right(QuestionName.read(n)) -> Answer.YesNo(p)
  private val github = right(Service.of("github"))
  private val slack = right(Service.of("slack"))

  // conversations is no service's; github is the github service's; docs and wiki both the
  // slack service's (made up, to have one service of two sources).
  private val knowledge: KnowledgeSources = {
    val within = right(Place.read("slack:"))
    def source(n: String, supplies: Option[Service]) =
      KnowledgeSource(right(KnowledgeSourceName.of(n)), s"the $n", within, supplies)
    KnowledgeSources
      .of(
        Vector(
          source("conversations", None),
          source("github", Some(github)),
          source("docs", Some(slack)),
          source("wiki", Some(slack))
        )
      )
      .fold(n => throw new java.lang.AssertionError(n), identity)
  }

  // Worked in the github service, reaching slack's post; read is grit's own, no service's.
  private val read = ToolName("read")
  private val searchCode = ToolName("github_search_code")
  private val getFile = ToolName("github_get_file")
  private val post = ToolName("slack_post")
  private val offer = TurnOffer.Recorded(
    Some(github.place),
    right(ToolSetId.of("0123456789abcdef")),
    Vector.empty,
    TurnOffer.Root.Heard,
    Vector(searchCode, getFile),
    Map(post -> slack.place)
  )
  private val set: Vector[ToolSet.Entry] = Vector(read, searchCode, getFile, post).map(n =>
    ToolSet.Entry(n, "Does.", ujson.Obj("type" -> "object"), false, Retry.Rerun)
  )

  private val heard = Rooted.Heard(Focus.Open)

  private def shaped(
      v: String,
      rooted: Rooted,
      answers: Option[VectorMap[QuestionName, Answer]]
  ): Shaped = TurnVariant.shape(variant(v), rooted, Some(offer), set, answers, knowledge)

  private def tools(v: String, rooted: Rooted, answers: Option[VectorMap[QuestionName, Answer]]) =
    shaped(v, rooted, answers).tools.map(_.name)

  val tests = Tests {
    test(
      "offer-x withholds the tools of a service whose every source reads under x, on every root"
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
        Vector(heard, Rooted.Heard(Focus.Focused), Rooted.Addressed).map(r => tools(v, r, answers))
      ) ==> Vector.fill(3)(Vector(read, searchCode, getFile, post)) ++
        Vector.fill(3)(Vector(read, post)) ++ Vector.fill(3)(Vector(read))
      shaped("offer-0.3", heard, answers).services.map(s => s.service -> s.withheld) ==>
        Vector(github -> true, slack -> false)
    }

    test("offer-0.3-0.1 gates a heard turn at 0.3 and one said to grit at 0.1") {
      val answers = Some(VectorMap(yes("source:github", 0.25), yes("source:docs", 0.05)))
      tools("offer-0.3-0.1", heard, answers) ==> Vector(read, post)
      tools("offer-0.3-0.1", Rooted.Addressed, answers) ==> Vector(read, searchCode, getFile, post)
    }

    test("offer-heard-0.2 gates a heard turn at 0.2 and offers a turn said to grit everything") {
      val answers = Some(VectorMap(yes("source:github", 0.1), yes("source:docs", 0.1)))
      tools("offer-heard-0.2", heard, answers) ==> Vector(read, post)
      tools("offer-heard-0.2", Rooted.Addressed, answers) ==>
        Vector(read, searchCode, getFile, post)
    }

    test("a turn with no answers, or none for a service's other source, is offered every tool") {
      tools("offer-0.5", heard, None) ==> Vector(read, searchCode, getFile, post)
      // docs under, wiki unasked: slack is unread, github unasked; both offered.
      tools("offer-0.5", heard, Some(VectorMap(yes("source:docs", 0.1)))) ==>
        Vector(read, searchCode, getFile, post)
    }

    test("a wide variant doubles its root's window and leaves the other's as deployed") {
      val wide = Width.Within(Tokens(8000), 5)
      Vector("shipped", "wide-heard", "wide-addressed").map(v =>
        Vector(heard, Rooted.Heard(Focus.Focused), Rooted.Addressed).map(shaped(v, _, None).width)
      ) ==> Vector(
        Vector(Width.Deployed, Width.Deployed, Width.Deployed),
        Vector(wide, wide, Width.Deployed),
        Vector(Width.Deployed, Width.Deployed, wide)
      )
    }

    test("a recorded turn is rooted as heard at its focus, or as said to grit") {
      val t = Fixtures.turn(root = TurnOffer.Root.Heard)
      Vector(
        TurnVariant.rooted(t),
        TurnVariant.rooted(t.copy(focus = Focus.Focused)),
        TurnVariant.rooted(Fixtures.turn(root = TurnOffer.Root.Addressed))
      ) ==> Vector(heard, Rooted.Heard(Focus.Focused), Rooted.Addressed)
    }

    test(
      "a turn that recorded its shape is decided over the services, sources and tools it recorded"
    ) {
      // Recorded: github supplied by repo alone, and only github_search_code taken from it;
      // the knowledge passed in says github's source is github, and infers both tools.
      val repo = right(KnowledgeSourceName.of("repo"))
      val recorded = offer.copy(shaped =
        Some(
          grit.turn.TurnShape(
            Width.Deployed,
            offer.tools,
            Vector(
              grit.turn.TurnShape.Took(
                grit.core.recipe.ServiceOffer(
                  github,
                  Vector(repo),
                  grit.core.recipe.ServiceOffer.Verdict.Ungated
                ),
                grit.turn.TurnShape.Via.Workspace,
                Vector(searchCode)
              )
            )
          )
        )
      )
      val answers = Some(VectorMap(yes("source:repo", 0.1), yes("source:github", 0.9)))
      val decided =
        TurnVariant.shape(variant("offer-0.3"), heard, Some(recorded), set, answers, knowledge)
      (decided.tools.map(_.name), decided.services.map(s => (s.service, s.sources, s.withheld))) ==>
        (Vector(read, getFile, post), Vector((github, Vector(repo), true)))
    }

    test("a service's tools are those taken from its advert, as workspace or reached") {
      TurnVariant.toolsOf(offer, github) ==> Set(searchCode, getFile)
      TurnVariant.toolsOf(offer, slack) ==> Set(post)
      TurnVariant.toolsOf(offer.copy(workspace = Some(right(Place.read("fs:/x")))), github) ==>
        Set.empty
    }
  }
}
