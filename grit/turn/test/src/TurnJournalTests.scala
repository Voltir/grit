package grit.turn

import grit.core.context.{AssemblyNote, Window}
import grit.core.durable.Journaled
import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.place.Place
import grit.core.provider.ModelRequest
import grit.core.store.Nearby
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

    test("a window's nearby sections are recorded after its notes, and read back") {
      val api = Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val near: Either[TurnFailure, Window] = Right(
        Window(
          Vector(EntryId("a")),
          Vector.empty,
          Vector(Nearby.Open(ConversationId("c9"), api, Vector(EntryId("x"))))
        )
      )
      val j = summon[Journaled[Either[TurnFailure, Window]]]
      j.encode(near) ==>
        """{"ok":{"entries":["a"],"notes":[],""" +
        """"nearby":[{"conversation":"c9","place":"fs:/home/nick/api","entries":["x"]}]}}"""
      roundTrip(near) ==> Right(near)
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

    test(
      "an offer's root: heard is written, and an offer recorded before roots reads as addressed"
    ) {
      val j = summon[Journaled[Either[TurnFailure, TurnOffer.Recorded]]]
      val tools = grit.core.tool.ToolSet.Empty.id
      val heard: Either[TurnFailure, TurnOffer.Recorded] =
        Right(TurnOffer.Recorded(None, tools, Vector.empty, TurnOffer.Root.Heard))
      val set = grit.core.tool.ToolSetId.value(tools)
      j.encode(heard) ==> s"""{"ok":{"workspace":null,"tools":"$set","prompt":[],"root":"heard"}}"""
      j.decode(j.encode(heard)) ==> Right(heard)
      // Every offer recorded before this field was an addressed turn's.
      j.decode(s"""{"ok":{"workspace":null,"tools":"$set","prompt":[]}}""") ==>
        Right(Right(TurnOffer.Recorded(None, tools, Vector.empty, TurnOffer.Root.Addressed)))
    }

    test(
      "an offer's advertised tools are written by name, and an offer recorded before them reads as none"
    ) {
      // Pinned: replay reads this key to rebuild the tools a turn took from an edge's advert.
      val j = summon[Journaled[Either[TurnFailure, TurnOffer.Recorded]]]
      val tools = grit.core.tool.ToolSet.Empty.id
      val set = grit.core.tool.ToolSetId.value(tools)
      val workspace = grit.core.place.Place.read("service:github").toOption
      val took: Either[TurnFailure, TurnOffer.Recorded] = Right(
        TurnOffer.Recorded(
          workspace,
          tools,
          Vector.empty,
          TurnOffer.Root.Addressed,
          Vector(grit.core.tool.ToolName("github_search"))
        )
      )
      j.encode(took) ==>
        s"""{"ok":{"workspace":"service:github","tools":"$set","prompt":[],"advertised":["github_search"]}}"""
      j.decode(j.encode(took)) ==> Right(took)
      j.decode(s"""{"ok":{"workspace":null,"tools":"$set","prompt":[]}}""")
        .map(_.map(_.advertised)) ==>
        Right(Right(Vector.empty))
    }

    test(
      "an offer's reached tools are written with their places, and an offer recorded before them reads as none"
    ) {
      // Pinned: replay reads this key to address each reached tool's calls to its place.
      val j = summon[Journaled[Either[TurnFailure, TurnOffer.Recorded]]]
      val tools = grit.core.tool.ToolSet.Empty.id
      val set = grit.core.tool.ToolSetId.value(tools)
      val workspace = grit.core.place.Place.read("service:github").toOption
      val elsewhere = grit.core.place.Place
        .read("service:elsewhere")
        .fold(e => throw new java.lang.AssertionError(e), identity)
      val took: Either[TurnFailure, TurnOffer.Recorded] = Right(
        TurnOffer.Recorded(
          workspace,
          tools,
          Vector.empty,
          TurnOffer.Root.Addressed,
          reached = Map(grit.core.tool.ToolName("post_x") -> elsewhere)
        )
      )
      j.encode(took) ==>
        s"""{"ok":{"workspace":"service:github","tools":"$set","prompt":[],"reached":{"post_x":"service:elsewhere"}}}"""
      j.decode(j.encode(took)) ==> Right(took)
      j.decode(s"""{"ok":{"workspace":null,"tools":"$set","prompt":[]}}""")
        .map(_.map(_.reached)) ==>
        Right(Right(Map.empty))
    }

    test("a record in neither shape is rejected") {
      val j = summon[Journaled[Either[TurnFailure, EntryId]]]
      assert(j.decode("""{"ok":"x","failed":"model","reason":"r"}""").isLeft)
      assert(j.decode("""{"failed":"lunch","reason":"r"}""").isLeft)
      assert(j.decode("""[1]""").isLeft)
    }
  }
}
