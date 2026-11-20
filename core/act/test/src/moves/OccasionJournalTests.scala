package grit.act.moves

import java.time.Instant

import grit.core.act.Occasioned
import grit.core.durable.Journaled
import grit.core.id.{ConversationId, DocumentVersionId, TurnRef, TurnSeq}
import grit.core.speech.Silence
import grit.core.visibility.Integrity

import utest.*

/** The recorded forms of an occasion move's three steps ([[OccasionMove]]): a run in flight
  * replays by them, so each `Occasioned` case's bytes are pinned, and each reads back as it was.
  */
object OccasionJournalTests extends TestSuite {

  private val turn = TurnRef(ConversationId("c"), TurnSeq.First)
  private val about = DocumentVersionId.of(7L).getOrElse(throw new java.lang.AssertionError("7"))
  private val until = Instant.parse("2026-10-09T12:00:00Z")

  /** Each case, and the bytes it is recorded as. */
  private val cases: Vector[(Occasioned, String)] = Vector(
    Occasioned.Started(turn) -> """{"started":"c:0"}""",
    Occasioned.Held(None, Silence.NoAddress) -> """{"held":{"kind":"no_address"}}""",
    Occasioned.Held(Some(turn), Silence.Lapsed(until)) ->
      """{"held":{"kind":"lapsed","until":"2026-10-09T12:00:00Z"},"turn":"c:0"}""",
    Occasioned.Already -> """{"already":true}""",
    Occasioned.Mislabelled(MovesContract.Channel.room) -> """{"mislabelled":"slack:T1/C1"}""",
    Occasioned.Unread(about) -> """{"unread":7}""",
    Occasioned.Unstarted(turn, "down") -> """{"unstarted":"c:0","why":"down"}"""
  )

  /** `a` recorded by `j`, then read back. */
  private def round[A](a: A)(using j: Journaled[A]): Either[String, A] = j.decode(j.encode(a))

  val tests = Tests {
    test("each case is recorded as its pinned bytes, and reads back as it was") {
      val j = OccasionMove.occasioned
      (cases.map((o, _) => j.encode(o)), cases.map((o, _) => j.decode(j.encode(o)))) ==> (
        cases.map(_._2),
        cases.map((o, _) => Right(o))
      )
    }

    test(
      "the first step records an end as its case's bytes, or the turn recorded, its address and what it is believed at"
    ) {
      val j = OccasionMove.recorded
      val ended: Either[Occasioned, (TurnRef, String, Integrity)] = Left(Occasioned.Already)
      val made: Either[Occasioned, (TurnRef, String, Integrity)] =
        Right((turn, "C1", Integrity.Outside))
      (j.encode(ended), j.encode(made), round(ended)(using j), round(made)(using j)) ==> (
        """{"ended":{"already":true}}""",
        """{"recorded":"c:0","to":"C1","believed":"outside"}""",
        Right(ended),
        Right(made)
      )
    }

    test("the decide step records an end as its case's bytes, or that its turn is to start") {
      val j = OccasionMove.decision
      val ended: Either[Occasioned, Unit] = Left(Occasioned.Held(Some(turn), Silence.Off))
      val drafting: Either[Occasioned, Unit] = Right(())
      (j.encode(ended), j.encode(drafting), round(ended)(using j), round(drafting)(using j)) ==> (
        """{"ended":{"held":{"kind":"off"},"turn":"c:0"}}""",
        """{"drafting":true}""",
        Right(ended),
        Right(drafting)
      )
    }
  }
}
