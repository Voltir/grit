package grit.turn

import grit.core.context.{AssemblyNote, Window}
import grit.core.durable.Journaled
import grit.core.id.{EntryId, TurnSeq}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.provider.ModelRequest
import grit.models.StubProvider

import utest.*

object TurnJournalTests extends TestSuite {

  private def roundTrip[A](a: A)(using j: Journaled[A]): Either[String, A] =
    j.decode(j.encode(a))

  val tests = Tests {
    import TurnJournal.given

    test("outputs and failures read back as written") {
      val window: Either[TurnFailure, Window] = Right(Window(Vector(EntryId("a"), EntryId("b"))))
      roundTrip(window) ==> Right(window)
      val reply: Either[TurnFailure, Message.Assistant] =
        new StubProvider()
          .complete(ModelRequest("s", Vector(Message.User("hi"))))
          .left
          .map(_ => TurnFailure.Model("unreachable"))
      roundTrip(reply) ==> Right(reply)
      for (
        failure <- Seq(TurnFailure.Assembly("a"), TurnFailure.Model("m"), TurnFailure.Store("s"))
      ) {
        val id: Either[TurnFailure, EntryId] = Left(failure)
        roundTrip(id) ==> Right(id)
      }
    }

    test("a window's notes read back, and one without notes keeps the bare array") {
      val noted: Either[TurnFailure, Window] = Right(
        Window(
          Vector(EntryId("a")),
          Vector(
            AssemblyNote.Queried(
              "postgres sqlite",
              "m",
              Usage(Tokens(3), Tokens(2), Tokens.Zero, Some(BigDecimal("0.00001"))),
              Tokens(4)
            ),
            AssemblyNote.FellBack("why"),
            AssemblyNote.Recalled(Vector(TurnSeq(0), TurnSeq(4)))
          )
        )
      )
      roundTrip(noted) ==> Right(noted)
      val j = summon[Journaled[Either[TurnFailure, Window]]]
      j.encode(Right(Window(Vector(EntryId("a"))))) ==> """{"ok":["a"]}"""
      j.encode(Right(Window(Vector(EntryId("a")), Vector(AssemblyNote.FellBack("why"))))) ==>
        """{"ok":{"entries":["a"],"notes":[{"fellBack":"why"}]}}"""
      j.encode(
        Right(Window(Vector(EntryId("a")), Vector(AssemblyNote.Recalled(Vector(TurnSeq(2))))))
      ) ==>
        """{"ok":{"entries":["a"],"notes":[{"recalled":[2]}]}}"""
      assert(j.decode("""{"ok":{"entries":["a"],"notes":[{"lunch":1}]}}""").isLeft)
      assert(j.decode("""{"ok":{"entries":["a"],"notes":[{"recalled":[-1]}]}}""").isLeft)
    }

    test("a classification reads back as written") {
      import grit.core.topic.{Placement, TopicEvent, TopicId, Weights}
      val (a, b) = (TopicId("a"), TopicId("b"))
      val choice = Vector(Placement.Chance(None, 0.9), Placement.Chance(Some(a), 0.1))
      val full = TurnTopics.Classification(
        Vector(
          TopicEvent.Opened(b),
          TopicEvent.Placed(
            TurnSeq(3),
            Weights.changed(a, 0.1, choice, Some(b)),
            Placement.Classified(0.1, Placement.Outcome.Changed(choice))
          )
        ),
        Some(TurnTopics.Shown(a, "Knots")),
        Vector(TurnTopics.Shown(b, "new topic")),
        Some(
          (
            "jev-1",
            Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.00001"))),
            Tokens(35)
          )
        ),
        None
      )
      val empty =
        TurnTopics.Classification(Vector.empty, None, Vector.empty, None, Some("store: down"))
      roundTrip(full) ==> Right(full)
      roundTrip(empty) ==> Right(empty)
      assert(summon[Journaled[TurnTopics.Classification]].decode("""{"events":[]}""").isLeft)
    }

    test("the recorded form is pinned") {
      val j = summon[Journaled[Either[TurnFailure, EntryId]]]
      j.encode(Right(EntryId("reply:c1:0"))) ==> """{"ok":"reply:c1:0"}"""
      j.encode(Left(TurnFailure.Model("down"))) ==> """{"failed":"model","reason":"down"}"""
    }

    test("a record in neither shape is rejected") {
      val j = summon[Journaled[Either[TurnFailure, EntryId]]]
      assert(j.decode("""{"ok":"x","failed":"model","reason":"r"}""").isLeft)
      assert(j.decode("""{"failed":"lunch","reason":"r"}""").isLeft)
      assert(j.decode("""[1]""").isLeft)
    }
  }
}
