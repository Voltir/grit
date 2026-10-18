package grit.turn

import grit.core.durable.{History, InMemoryDurable, StepRecord}
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.speech.Outcome
import grit.core.tool.ToolSetId
import grit.core.visibility.{Label, Level}

import utest.*

/** A turn read after the fact from what it recorded: recorded histories, real step outputs,
  * read through [[TurnRecord]]. A reader meets turns of every epoch, so shapes only an earlier
  * epoch could record are read from its directory ([[Earlier]]).
  */
object TurnRecordTests extends TestSuite {

  /** The epoch whose histories hold the shapes the current one no longer records. */
  private val Earlier = "2026-10-02"

  private def history(name: String, epoch: String): History =
    History
      .read(ujson.read(os.read(os.Path(sys.env("GRIT_HISTORIES")) / epoch / s"$name.json")))
      .fold(why => throw new java.lang.AssertionError(s"$name: $why"), identity)

  /** `name`'s steps, recorded under `epoch`, as a reader of DBOS sees them: a marker or a
    * throw records no output.
    */
  private def steps(name: String, epoch: String = Turn.Epoch): Vector[StepRecord] =
    history(name, epoch).steps.map(s =>
      StepRecord(
        s.name,
        s.outcome match {
          case InMemoryDurable.Outcome.Output(value) => Some(value)
          case InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker => None
        },
        None
      )
    )

  /** The tool set id `name`'s offer step names, read from its JSON as written. */
  private def setOf(name: String): Option[ToolSetId] =
    steps(name)
      .find(_.name == Turn.Step.Offer)
      .flatMap(_.output)
      .flatMap(o => ujson.read(o).obj.get("ok"))
      .flatMap(_.obj.get("tools"))
      .flatMap(_.strOpt)
      .flatMap(ToolSetId.of(_).toOption)

  private val turn = TurnRef(ConversationId("c1"), TurnSeq(3))

  val tests = Tests {
    test("an offer reads back with the root and the tool set its step recorded") {
      val read = Vector("replied", "heard-posted").map(n =>
        TurnRecord.offer(steps(n)).map(_.map(o => (o.root, Some(o.tools))))
      )
      read ==> Vector(
        Right(Some((TurnOffer.Root.Addressed, setOf("replied")))),
        Right(Some((TurnOffer.Root.Heard, setOf("heard-posted"))))
      )
      assert(setOf("replied").nonEmpty)
    }

    test("no offer step reads as none, and an offer this build cannot read names its step") {
      TurnRecord.offer(Vector.empty) ==> Right(None)
      TurnRecord
        .offer(Vector(StepRecord(Turn.Step.Offer, Some("{}"), None)))
        .left
        .map(
          _.takeWhile(_ != ':')
        ) ==> Left(Turn.Step.Offer)
    }

    test("the first failure is named by its step's family and kind, and a replied turn has none") {
      def failed(name: String) =
        TurnRecord
          .failure(steps(name))
          .map((step, f) =>
            step -> (f match {
              case TurnFailure.Assembly(_) => "assembly"
              case TurnFailure.Model(_) => "model"
              case TurnFailure.Store(_) => "store"
              case TurnFailure.Uncleared(_) => "uncleared"
              case TurnFailure.ToolsChanged(_) => "tools-changed"
            })
          )
      Vector("model-failed", "summary-failed", "replied").map(failed) ==> Vector(
        Some(Turn.Step.Ask -> "model"),
        Some(Turn.Step.Summarise -> "model"),
        None
      )
    }

    test("a heard turn's judgement reads from its judge step; a named or replied turn has none") {
      def scores(name: String, root: TurnOffer.Root, epoch: String = Turn.Epoch) = TurnRecord
        .judged(steps(name, epoch), root)
        .map(_.map {
          case TurnJudge.Judgement.Scored(j, estimated) =>
            (
              grit.core.period.Probability.value(j.grounded),
              grit.core.period.Probability.value(j.worth),
              grit.core.message.Tokens.value(estimated)
            )
          case other => other
        })
      Vector(
        scores("heard-posted", TurnOffer.Root.Heard),
        scores("named-posted", TurnOffer.Root.Named, Earlier),
        scores("by-name-replied", TurnOffer.Root.ByName)
      ) ==> Vector(Right(Some((0.9, 0.9, 67L))), Right(None), Right(None))
    }

    test("what became of a heard turn's draft reads from its record-speech step") {
      def kind(name: String) = TurnRecord
        .speech(steps(name))
        .map(_.map {
          case Outcome.Posted(_) => "posted"
          case Outcome.Shadowed(_) => "shadowed"
          case other => other.toString
        })
      Vector("heard-posted", "heard-shadowed", "replied").map(kind) ==> Vector(
        Right(Some("posted")),
        Right(Some("shadowed")),
        Right(None)
      )
    }

    test(
      "a named turn's record-speech, recorded with the named judge's score, reads as posted unjudged"
    ) {
      TurnRecord.speech(steps("named-judged", Earlier)) ==>
        Right(Some(Outcome.Posted(grit.core.speech.Cleared.Named)))
    }

    test(
      "a call no edge claimed is expired, one claimed and left silent abandoned, and one answered late neither"
    ) {
      Vector("hosted-expired", "hosted-orphaned", "hosted-slow").map(n =>
        TurnRecord.givenUp(steps(n))
      ) ==> Vector(
        Map((0, 0) -> TurnRecord.GivenUp.Expired),
        Map((0, 0) -> TurnRecord.GivenUp.Abandoned),
        Map.empty
      )
    }

    test(
      "what a turn's root was weighed with reads from its weigh step: kept, asked, failed and why, or nothing; a turn with no weigh step is told from one that weighed nothing"
    ) {
      def model(name: String, epoch: String = Turn.Epoch) = TurnRecord
        .weighed(steps(name, epoch))
        .map {
          case TurnRecord.Weigh.Unrecorded => "no step"
          case TurnRecord.Weigh.Recorded(w) =>
            w.fold("nothing") {
              case TurnWeighing.Weighed.Failed(why) => s"failed $why"
              case TurnWeighing.Weighed.Kept(_) => "kept"
              case TurnWeighing.Weighed.Asked(asked) =>
                s"asked ${asked.tags.model} ${asked.estimate}"
            }
        }
      Vector(
        model("heard-withheld"),
        model("addressed-weighed"),
        model("addressed-unweighed", Earlier),
        model("addressed-failed"),
        model("pinned-before-weigh", Earlier)
      ) ==> Vector(
        Right("kept"),
        Right("asked jev 321"),
        // Recorded before a failure was kept: read as not weighed, as it was written.
        Right("nothing"),
        Right("failed PlacementLate"),
        Right("no step")
      )
    }

    test("a ledger row's role is read from the id its turn keeps it under, and only its turn's") {
      val other = TurnRef(ConversationId("c1"), TurnSeq(4))
      val ids = Vector(
        turn.replyId,
        turn.draftId,
        Turn.queryId(turn, 0),
        Turn.queryId(turn, 2),
        TurnTools.callId(turn, TurnLoop.Round.at(1)),
        TurnTopics.placedId(turn),
        TurnVerdict.verdictId(turn),
        TurnJudge.id(turn),
        TurnSummary.id(turn),
        TurnWeighing.id(turn),
        TurnWeighing.id(other),
        other.replyId,
        Turn.queryId(other, 0),
        TurnTools.callId(other, TurnLoop.Round.at(1)),
        EntryId("call:c1:3:x")
      )
      ids.map(TurnRecord.role(turn, _)) ==> Vector(
        Some(TurnRecord.Role.Reply),
        Some(TurnRecord.Role.Reply),
        Some(TurnRecord.Role.Query),
        Some(TurnRecord.Role.Query),
        Some(TurnRecord.Role.Round(1)),
        Some(TurnRecord.Role.Topic),
        Some(TurnRecord.Role.Topic),
        Some(TurnRecord.Role.Judge),
        Some(TurnRecord.Role.Summary),
        Some(TurnRecord.Role.Weigh),
        None,
        None,
        None,
        None,
        None
      )
    }

    test(
      "a move's ledger row reads as the turn's query or the batch it judged, and only its turn's"
    ) {
      val other = TurnRef(ConversationId("c1"), TurnSeq(4))
      val (mine, theirs) = (WorkflowId.value(turn.workflowId), WorkflowId.value(other.workflowId))
      Vector(
        s"move:$mine:query",
        s"move:$mine:relevance-0",
        s"move:$mine:relevance-2",
        s"move:$mine:relevance-x",
        s"move:$theirs:relevance-0"
      ).map(id => TurnRecord.role(turn, EntryId(id))) ==> Vector(
        Some(TurnRecord.Role.Query),
        Some(TurnRecord.Role.Relevance(0)),
        Some(TurnRecord.Role.Relevance(2)),
        None,
        None
      )
    }

    test(
      "a recorded route step reads back as the label the turn's reads were routed for, and no route step reads as none"
    ) {
      val routed = Vector(
        StepRecord(
          Turn.Step.Route,
          Some("""{"ok":{"classification":"internal","turn":1}}"""),
          None
        )
      )
      (TurnRecord.routedAt(routed), TurnRecord.routedAt(Vector.empty)) ==> (
        Right(Some(Label.at(Level.Internal))),
        Right(None)
      )
    }
  }
}
