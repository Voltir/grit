package grit.turn

import java.time.Instant

import grit.core.classify.{Answered, ClassifierError, StateJson}
import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.stitch.{Said, Strand}
import grit.core.store.{Entry, Payload, Speakers}
import grit.core.visibility.Integrity

import utest.*

object TurnNeedsTests extends TestSuite {
  import TurnRelevance.Verdict
  import TurnOffer.Root

  private def answered(p: Double): Either[ClassifierError, Answered[Probability]] =
    Right(Answered(Probability.clamped(p), Usage(Tokens(40), Tokens(2), Tokens.Zero, None), "j"))

  private def entry(n: Long, payload: Payload): Entry =
    Entry(
      EntryId(s"e$n"),
      ConversationId("c"),
      TurnSeq(n),
      None,
      EntrySeq(n),
      payload,
      Instant.EPOCH,
      Integrity.Member
    )

  val tests = Tests {
    test(
      "the gate passes a message at its pass mark, rejects one under it, and leaves none unjudged as said"
    ) {
      val at = Probability.value(TurnNeeds.Pass)
      (
        TurnNeeds.verdict(answered(at)),
        TurnNeeds.verdict(answered(at - 0.01)),
        TurnNeeds.verdict(Left(ClassifierError.Unavailable("no service")))
      ) ==> (
        Verdict.Passed(TurnNeeds.Pass),
        Verdict.Rejected(Probability.clamped(at - 0.01)),
        Verdict.Unjudged(ClassifierError.Unavailable("no service").said)
      )
    }

    test(
      "after a rejection a heard or named turn ends and an addressed or by-name one goes unsearched; a pass or no verdict searches"
    ) {
      val no = Verdict.Rejected(Probability.clamped(0.1))
      val roots = Vector(Root.Heard, Root.Named, Root.Addressed, Root.ByName)
      roots.map(TurnNeeds.after(no, _)) ==> Vector(
        TurnNeeds.After.Ends(Root.Heard),
        TurnNeeds.After.Ends(Root.Named),
        TurnNeeds.After.Unsearched,
        TurnNeeds.After.Unsearched
      )
      roots.map(TurnNeeds.after(Verdict.Unjudged("down"), _)) ==>
        Vector.fill(4)(TurnNeeds.After.Searches)
      roots.map(TurnNeeds.after(Verdict.Passed(Probability.clamped(0.9)), _)) ==>
        Vector.fill(4)(TurnNeeds.After.Searches)
    }

    test(
      "the gate is shown the judge's message and thread: the strand's excerpt, then the thread"
    ) {
      val heard = entry(2, Payload.Heard("is it still on?"))
      val all = Vector(entry(1, Payload.Heard("when is the freeze?")), heard)
      val place = Place.read("slack:T1/C1/1.0").fold(e => sys.error(e), identity)
      val opening = Said(
        ConversationId("a"),
        place,
        entry(0, Payload.Heard("the freeze is Thursday")).copy(conversationId = ConversationId("a"))
      )
      val strand =
        Strand.Read(Some(opening), Vector(opening), Vector.empty, Set(opening.conversation))
      val names =
        Speakers(Map(EntryId("e0") -> "Cy", EntryId("e1") -> "Ana", EntryId("e2") -> "Ben"))
      val state =
        TurnNeeds.state(TurnJudge.told(all, Vector(heard), strand, 800, names))
      state ==> TurnNeeds.State(
        "Ben: is it still on?",
        "Cy: the freeze is Thursday\nAna: when is the freeze?"
      )
      summon[StateJson[TurnNeeds.State]].json(state) ==> ujson.Obj(
        "message" -> "Ben: is it still on?",
        "thread" -> "Cy: the freeze is Thursday\nAna: when is the freeze?"
      )
    }
  }
}
