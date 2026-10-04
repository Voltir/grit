package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.{KnowledgeSourceName, QuestionName}
import grit.core.message.{Tokens, Usage}
import grit.core.place.{Place, Service}
import grit.core.recipe.ServiceOffer
import grit.core.tool.{ToolName, ToolSetId}
import grit.core.triage.{KnowledgeSource, KnowledgeSources, Tags, Weighing}
import grit.eval.harness.corpus.{Live, Offered, TurnCase}
import grit.turn.{TurnOffer, TurnRecord, TurnShape, TurnWeighing}

import utest.*

/** Where the answers a variant decides a recorded turn's offering by come from: what the turn
  * recorded, or the harness's asking of a message said to grit that its recipe did not read.
  */
object TurnAnswersTests extends TestSuite {

  import Fixtures.right

  private val assembled = Assembled.Shipped
  private def variant(name: String) = right(TurnVariant.named(name, assembled))
  private val repo = right(KnowledgeSourceName.of("repo"))
  private val github = right(Service.of("github"))
  private def answers(p: Double): VectorMap[QuestionName, Answer] =
    VectorMap(QuestionName.per(Tags.V2.sourcePrefix, repo) -> Answer.YesNo(p))
  private def tags(p: Double): Tags.Weighed = Tags.Weighed(answers(p), "jev", Usage.Zero)

  /** Recorded: the turn's own (0.1), live triage's row (0.5), the harness's asking (0.9). */
  private val own = answers(0.1)
  private val live = Live.Named(answers(0.5), "jev", None)
  private val asked = Some(answers(0.9))

  private def shape(sources: Vector[KnowledgeSourceName]) = TurnShape(
    Width.Deployed,
    right(ToolSetId.of("0123456789abcdef")),
    Vector(
      TurnShape.Took(
        ServiceOffer(github, sources, ServiceOffer.Verdict.Ungated),
        TurnShape.Via.Workspace,
        Vector(ToolName("github_search_code"))
      )
    )
  )

  /** A turn rooted at `root`, recorded with `weighed` and `recorded`, live triage's row kept. */
  private def turn(
      root: TurnOffer.Root,
      weighed: TurnRecord.Weigh,
      recorded: Option[TurnShape] = Some(shape(Vector(repo)))
  ): TurnCase =
    Fixtures
      .turn(root = root)
      .copy(
        triage = Some(live),
        weighed = weighed,
        offered = Some(
          Offered(
            Vector(ToolName("github_search_code")),
            right(ToolSetId.of("0123456789abcdef")),
            Tokens(10),
            VectorMap.empty,
            Some(github.place),
            Vector.empty,
            recorded
          )
        )
      )

  private def recorded(w: TurnWeighing.Weighed) = TurnRecord.Weigh.Recorded(Some(w))
  private val gating: Vector[TurnVariant] = Vector(variant("offer-0.2"))

  val tests = Tests {
    test(
      "the answers a turn recorded, kept or asked, are its own, whatever live triage kept or the harness asked"
    ) {
      Vector(
        turn(TurnOffer.Root.Heard, recorded(TurnWeighing.Weighed.Kept(tags(0.1)))),
        turn(
          TurnOffer.Root.Addressed,
          recorded(TurnWeighing.Weighed.Asked(Weighing.Weighed(tags(0.1), Tokens(5))))
        )
      ).map(t => (TurnAnswers.of(t, asked), TurnAnswers.asks(t, gating))) ==>
        Vector.fill(2)((Some(own), false))
    }

    test(
      "a failed weighing, a heard root that weighed nothing and a turn with no weigh step have no answers, and are never asked"
    ) {
      Vector(
        turn(
          TurnOffer.Root.Addressed,
          recorded(TurnWeighing.Weighed.Failed(Weighing.Unweighed.Unread))
        ),
        turn(TurnOffer.Root.Heard, recorded(TurnWeighing.Weighed.Kept(Tags.Unanswered("down")))),
        turn(TurnOffer.Root.Heard, TurnRecord.Weigh.Recorded(None)),
        turn(TurnOffer.Root.Heard, TurnRecord.Weigh.Unrecorded),
        turn(TurnOffer.Root.Addressed, TurnRecord.Weigh.Unrecorded)
      ).map(t => (TurnAnswers.of(t, asked), TurnAnswers.asks(t, gating))) ==>
        Vector.fill(5)((None, false))
    }

    test(
      "a root said to grit that weighed nothing takes the harness's answers, asked only when a variant gates a service its shape recorded"
    ) {
      val unweighed = turn(TurnOffer.Root.Addressed, TurnRecord.Weigh.Recorded(None))
      TurnAnswers.of(unweighed, asked) ==> asked
      Vector(
        TurnAnswers.asks(unweighed, gating),
        TurnAnswers.asks(unweighed, Vector(TurnVariant.Shipped, variant("offer-heard-0.2"))),
        // A service no source supplies is never gated; a turn with no shape is offered as
        // recorded.
        TurnAnswers.asks(
          turn(
            TurnOffer.Root.Addressed,
            TurnRecord.Weigh.Recorded(None),
            Some(shape(Vector.empty))
          ),
          gating
        ),
        TurnAnswers.asks(
          turn(TurnOffer.Root.Addressed, TurnRecord.Weigh.Recorded(None), None),
          gating
        )
      ) ==> Vector(true, false, false, false)
    }

    test(
      "knowledge is the turn's declaration only when it declares each source its shape recorded, each service supplied as recorded"
    ) {
      def knowledge(sources: (String, Option[Service])*) = right(
        KnowledgeSources
          .of(
            sources.toVector.map((n, s) =>
              KnowledgeSource(right(KnowledgeSourceName.of(n)), s"the $n", Place.Everywhere, s)
            )
          )
          .left
          .map(KnowledgeSourceName.value)
      )
      val t = turn(TurnOffer.Root.Addressed, TurnRecord.Weigh.Recorded(None))
      Vector(
        TurnAnswers.declared(t, knowledge("repo" -> Some(github), "chat" -> None)),
        TurnAnswers.declared(t, knowledge("chat" -> None)),
        TurnAnswers.declared(t, knowledge("repo" -> Some(github), "wiki" -> Some(github))),
        TurnAnswers.declared(t, knowledge("repo" -> None))
      ) ==> Vector(
        Right(()),
        Left("knowledge.json does not declare repo, which c1:2's shape records"),
        Left("knowledge.json says github is supplied by repo, wiki; c1:2's shape records repo"),
        Left("knowledge.json says github is supplied by nothing; c1:2's shape records repo")
      )
    }
  }
}
