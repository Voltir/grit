package grit.lifecycle.triage

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.StateJson
import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, TriageRef, TurnRef}
import grit.core.period.LifecycleSettings
import grit.core.place.{Locality, Namespace, Place, Scope, Weight}
import grit.core.recipe.{Pool, Section, Source}
import grit.core.stitch.Tuning
import grit.core.store.Focus
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

/** The state JSON triage's question is shown, pinned byte for byte for each way its thread is
  * cut: it is what the classifier is sent, and a capture's recorded request digests are over it.
  */
object TriageInputTests extends TestSuite {
  import TriageFixtures.*

  /** The state `triage` is asked about, as the classifier is sent it. */
  private def sent(
      w: World,
      triage: TriageRef,
      tuning: Tuning = Tuning.Default,
      recipe: TriageRecipe = TriageRecipe.Shipped
  ) =
    TriageInput
      .build(w.reads, w.rooms, FakeDb.as(Subject.Public), triage.message, tuning, recipe)
      .map((_, state) => ujson.write(StateJson[TriageQuestion.State].json(state)))

  private def json(message: String, author: String, thread: String): String =
    ujson.write(ujson.Obj("new_message" -> message, "author" -> author, "thread" -> thread))

  /** Thread b of channel C, its opening `opening` by David at minute `minutes`, stitched to c. */
  private def stitched(w: World, opening: String, minutes: Long) = {
    val b = w.thread("2.0")
    val t = w.hear(opening, "David", minutes, in = b)
    // Exchange 1 (c) at 0.9: its opening follows c.
    val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
    new InMemoryDurable().run(t.workflowId)(w.body(classifier, minutes + 1))
    b
  }

  val tests = Tests {
    test("a person's message said to grit builds the state the same message heard does") {
      val heard = new World
      heard.hear("when is standup?", "Ben", 0)
      val h = heard.hear("is standup at 10:00?", "Ana", 1)
      val said = new World
      said.hear("when is standup?", "Ben", 0)
      val s = said.say("is standup at 10:00?", 1, Some("Ana"))
      val state = (w: World, t: TurnRef) =>
        TriageInput
          .build(
            w.reads,
            w.rooms,
            FakeDb.as(Subject.Public),
            t,
            Tuning.Default,
            TriageRecipe.Shipped
          )
          .map((_, state) => ujson.write(StateJson[TriageQuestion.State].json(state)))
      state(said, s) ==> Right(json("is standup at 10:00?", "Ana", "Ben: when is standup?"))
      state(said, s) ==> state(heard, h.message)
    }

    test("with no strand, the thread is its conversation's messages before it, whole") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      w.say("unrelated", 1)
      val t = w.hear("standup moves to 10:00", "Ana", 2)
      sent(w, t) ==> Right(
        """{"new_message":"standup moves to 10:00","author":"Ana",""" +
          """"thread":"Ben: when is standup?\n\nUser: unrelated"}"""
      )
    }

    test("with no strand, a thread over ThreadChars keeps its end") {
      val w = new World
      // Two lines of 1,000 characters and the blank line between: 2,002, two over.
      w.hear("a" * 995, "Ben", 0)
      w.hear("b" * 995, "Ben", 1)
      val t = w.hear("the last", "Ana", 2)
      sent(w, t) ==> Right(json("the last", "Ana", "n: " + "a" * 995 + "\n\nBen: " + "b" * 995))
    }

    test("with a strand, the thread is its excerpt, then its conversation's messages") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = stitched(w, "Is this a real question", 1)
      val t = w.hear("and another", "David", 3, in = b)
      sent(w, t) ==> Right(
        json(
          "and another",
          "David",
          "Nick: where did we land on the Engine contract term?\nDavid: Is this a real question"
        )
      )
    }

    test("a strand over ThreadChars is shown alone, its start") {
      val w = new World
      // Ten lines of "Nick: mN " and 291 x's, each 300 characters, the most a strand line shows.
      (0 until 10).foreach(i => w.hear(s"m$i " + "x" * 291, "Nick", i.toLong))
      val b = stitched(w, "Is this a real question", 11)
      val t = w.hear("and another", "David", 13, in = b)
      // Within 3,000 characters the strand shows its opening, a `…` line, then the last eight.
      val line = (i: Int) => s"Nick: m$i " + "x" * 291
      val excerpt = (Vector(line(0), "…") ++ (2 until 10).map(line)).mkString("\n")
      sent(w, t, Tuning.Default.copy(strandChars = 3_000)) ==>
        Right(json("and another", "David", excerpt.take(TriageQuestion.ThreadChars)))
    }

    test("a recipe whose sources find nothing outside the thread gives the shipped JSON") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      w.hear("I asked before", "Ana", 1)
      val t = w.hear("standup moves to 10:00", "Ana", 2)
      val every =
        Pool(Vector(Source.Channel(1.hour, 5), Source.Author(1.hour, 5), Source.Exchanges), 600)
      sent(w, t, recipe = TriageRecipe(every, every)) ==> sent(w, t)
      sent(w, t) ==> Right(
        json("standup moves to 10:00", "Ana", "Ben: when is standup?\n\nAna: I asked before")
      )
    }

    test("a state's sections follow its thread under their keys, an empty one left out") {
      val state = TriageQuestion.State(
        "standup moves to 10:00",
        "Ana",
        "Ben: when is standup?",
        VectorMap(Section.Exchanges -> "", Section.Nearby -> "Cy, 5 minutes before: lunch?")
      )
      ujson.write(StateJson[TriageQuestion.State].json(state)) ==>
        """{"new_message":"standup moves to 10:00","author":"Ana","thread":"Ben: when is standup?",""" +
        """"nearby_in_channel":"Cy, 5 minutes before: lunch?"}"""
    }

    test("a pool shows nothing when the scope in force does not hold the message's room") {
      val w = new World
      w.hear("lunch?", "Ben", 0, in = w.thread("2.0"))
      val t = w.hear("standup moves to 10:00", "Ana", 1)
      val nearby = Pool(Vector(Source.Channel(1.hour, 5)), 600)
      val recipe = TriageRecipe(nearby, nearby)
      sent(w, t, recipe = recipe) ==> Right(
        ujson.write(
          ujson.Obj(
            "new_message" -> "standup moves to 10:00",
            "author" -> "Ana",
            "thread" -> "",
            "nearby_in_channel" -> "Ben, 1 minute before: lunch?"
          )
        )
      )
      val d = LifecycleSettings.Default
      val off = LifecycleSettings.of(
        d.windows,
        d.balance,
        d.settle,
        d.resolveAt,
        d.asks,
        Locality(Scope.Off, Weight.Default)
      )
      off.flatMap(o => w.lifecycle.set(o)(using TestTx.fake).left.map(_.toString)) ==> Right(())
      sent(w, t, recipe = recipe) ==> sent(w, t)
    }

    test("a Slack thread's opening is read at its open focus, and a reply in it focused") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val opening = w.hear("Is this a real question", "David", 1, in = b)
      val reply = w.hear("and another", "David", 2, in = b)
      Vector(opening, reply).map(t =>
        TriageInput
          .read(
            w.reads,
            w.rooms,
            FakeDb.as(Subject.Public),
            t.message,
            Tuning.Default,
            TriageRecipe.Shipped
          )
          .map(_.focus)
      ) ==> Vector(Right(Focus.Open), Right(Focus.Focused))
    }

    test("a read is at its conversation's own place, and at none when that is not found") {
      val w = new World
      val here = w.hear("who owns the deploy?", "Ana", 0)
      val nowhere = w.hear("and the rollback?", "Ana", 1, in = ConversationId("gone"))
      Vector(here, nowhere).map(t =>
        TriageInput
          .read(
            w.reads,
            w.rooms,
            FakeDb.as(Subject.Public),
            t.message,
            Tuning.Default,
            TriageRecipe.Shipped
          )
          .map(_.place)
      ) ==> Vector(
        Right(Some(Place.under(Namespace.Slack, Vector("T", "C", "1.0")))),
        Right(None)
      )
    }
  }
}
