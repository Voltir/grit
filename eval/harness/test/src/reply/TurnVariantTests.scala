package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.QuestionName
import grit.core.message.Tokens
import grit.core.place.{Place, Service}
import grit.core.tool.{ToolName, ToolSetId}
import grit.turn.TurnOffer

import utest.*

/** A variant's width per turn and the services it withholds by triage's per-source answers. */
object TurnVariantTests extends TestSuite {

  import Fixtures.right

  private def variant(name: String) = TurnVariant.named(name).getOrElse(sys.error(name))
  private def name(n: String) = right(QuestionName.read(n))
  private def yes(n: String, p: Double) = name(n) -> Answer.YesNo(p)
  private val github = right(Service.of("github"))
  private val slack = right(Service.of("slack"))

  // conversations is no service's; github is the github service's; docs and wiki both the
  // slack service's (made up, to have one service of two sources).
  private val supplies = right(
    Supplies.read(
      """{"sources": [
        |  {"name": "conversations", "line": "the team's past conversations", "within": "slack:", "service": null},
        |  {"name": "github", "line": "the team's repository", "within": "slack:", "service": "github"},
        |  {"name": "docs", "line": "the docs", "within": "slack:", "service": "slack"},
        |  {"name": "wiki", "line": "the wiki", "within": "slack:", "service": "slack"}
        |]}""".stripMargin
    )
  )

  private val assembled = Assembled.Shipped.copy(window = Tokens(4000), hits = 5)

  val tests = Tests {
    test("a gated variant withholds a service whose every source triage answered below its line") {
      val answers = VectorMap(
        yes("source:conversations", 0.1),
        yes("source:github", 0.25),
        yes("source:docs", 0.1),
        yes("source:wiki", 0.4)
      )
      // github at 0.25 is below 0.3 but not 0.2; slack's wiki at 0.4 holds it offered under
      // 0.3, and under 0.5 both its sources are below.
      TurnVariant.withheld(variant("offer-0.2"), answers, supplies) ==> Set.empty
      TurnVariant.withheld(variant("offer-0.3"), answers, supplies) ==> Set(github)
      TurnVariant.withheld(variant("offer-0.5"), answers, supplies) ==> Set(github, slack)
    }

    test("a source triage did not ask about holds its service offered") {
      val answers = VectorMap(yes("source:docs", 0.1))
      TurnVariant.withheld(variant("offer-0.5"), answers, supplies) ==> Set.empty
      TurnVariant.withheld(variant("shipped"), VectorMap(yes("source:github", 0.0)), supplies) ==>
        Set.empty
    }

    test("narrow-when-answered halves a heard turn's window only when triage reads it answered") {
      val v = variant("narrow-when-answered")
      val half = Width.Within(Tokens(2000), 5)
      TurnVariant.width(v, TurnOffer.Root.Heard, VectorMap(yes("open", 0.4)), assembled) ==> half
      TurnVariant.width(v, TurnOffer.Root.Heard, VectorMap(yes("open", 0.6)), assembled) ==>
        Width.Deployed
      TurnVariant.width(v, TurnOffer.Root.Heard, VectorMap.empty, assembled) ==> Width.Deployed
      TurnVariant.width(v, TurnOffer.Root.Addressed, VectorMap(yes("open", 0.4)), assembled) ==>
        Width.Deployed
    }

    test("a wide variant doubles its root's window and leaves the other's") {
      val v = variant("wide-heard")
      TurnVariant.width(v, TurnOffer.Root.Heard, VectorMap.empty, assembled) ==>
        Width.Within(Tokens(8000), 5)
      TurnVariant.width(v, TurnOffer.Root.Addressed, VectorMap.empty, assembled) ==> Width.Deployed
    }

    test("a variant is past the reply model's context only by a budget over it") {
      TurnVariant.pastContext(variant("wide-addressed"), assembled, Tokens(7999)) ==>
        Some(Tokens(8000))
      TurnVariant.pastContext(variant("wide-addressed"), assembled, Tokens(8000)) ==> None
      TurnVariant.pastContext(variant("narrow-when-answered"), assembled, Tokens(3999)) ==>
        Some(Tokens(4000))
    }

    test("a service's tools are those taken from its advert, as workspace or reached") {
      val offer = TurnOffer.Recorded(
        Some(github.place),
        right(ToolSetId.of("0123456789abcdef")),
        Vector.empty,
        TurnOffer.Root.Addressed,
        Vector(ToolName("github_search_code")),
        Map(ToolName("slack_post") -> slack.place)
      )
      TurnVariant.toolsOf(offer, github) ==> Set(ToolName("github_search_code"))
      TurnVariant.toolsOf(offer, slack) ==> Set(ToolName("slack_post"))
      TurnVariant.toolsOf(offer.copy(workspace = Some(right(Place.read("fs:/x")))), github) ==>
        Set.empty
    }

    test("supplies refuse a source declared twice") {
      Supplies.read(
        """{"sources": [{"name": "a", "line": "x", "within": "slack:", "service": null},
          |{"name": "a", "line": "y", "within": "slack:", "service": null}]}""".stripMargin
      ) ==> Left("supplies: a is declared twice")
    }
  }
}
