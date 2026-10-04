package grit.turn

import grit.core.durable.{History, InMemoryDurable, StepRecord}
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.speech.Outcome
import grit.core.tool.ToolSetId

import utest.*

/** A turn read after the fact from what it recorded: the current epoch's recorded histories,
  * real step outputs, read through [[TurnRecord]].
  */
object TurnRecordTests extends TestSuite {

  private def history(name: String): History =
    History
      .read(ujson.read(os.read(os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch / s"$name.json")))
      .fold(why => throw new java.lang.AssertionError(s"$name: $why"), identity)

  /** `name`'s steps as a reader of DBOS sees them: a marker or a throw records no output. */
  private def steps(name: String): Vector[StepRecord] =
    history(name).steps.map(s =>
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
            })
          )
      Vector("model-failed", "summary-failed", "replied").map(failed) ==> Vector(
        Some(Turn.Step.CallModel -> "model"),
        Some(Turn.Step.Summarise -> "model"),
        None
      )
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
      def model(name: String) = TurnRecord
        .weighed(steps(name))
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
        "heard-withheld",
        "addressed-weighed",
        "addressed-unweighed",
        "addressed-failed",
        "pinned-before-weigh"
      ).map(model) ==> Vector(
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
  }
}
