package grit.lifecycle.stitch

import grit.core.classify.Classifier
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.id.{ConversationId, EntryId, StitchRef, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.spend.DailyCap
import grit.core.stitch.{Link, Placed, Stitching, Tuning}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx
import grit.lifecycle.triage.TriageFixtures

import utest.*

object StitchTests extends TestSuite {
  import TriageFixtures.*

  /** Exchange 1 at 0.9. */
  private def follows = new Scripted(Vector(0.9, 0.1), Vector.empty)

  /** The placement's body over `w`, asking `classifier`, at minute 2. */
  private def body(w: World, classifier: Classifier^)(id: WorkflowId)(using Durable^): String =
    w.placement(classifier, 2)(id)

  val tests = Tests {
    test(
      "an opening is placed in ask:stitch, its call's cost kept in ask:stitch:record, and its placement in record-stitch"
    ) {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val id = StitchRef(TurnRef(b, t.turn), at(1)).workflowId
      val durable = new InMemoryDurable
      durable.run(id)(body(w, follows)) ==> "stitched: follows"
      durable.recordedSteps(id) ==> Vector("ask:stitch", "ask:stitch:record", "record-stitch")
      w.stitches.links(Vector(b))(using TestTx.fake) ==> Right(Vector(Link(b, c)))
    }

    test(
      "a classifier whose service is not cleared for what the placement read is not asked: unread"
    ) {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val classifier = follows
      new InMemoryDurable().run(StitchRef(TurnRef(b, t.turn), at(1)).workflowId)(
        w.placement(classifier, 2, grit.core.classify.TestClassifiers.above())
      ) ==> "stitched: unread"
      (w.stitches.links(Vector(b))(using TestTx.fake), classifier.calls) ==> (
        Right(Vector.empty),
        0
      )
    }

    test("a placement resumed after it asked reads its answer back, and asks nothing again") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val id = StitchRef(TurnRef(b, t.turn), at(1)).workflowId
      val first = new InMemoryDurable
      first.run(id)(body(w, follows))
      val again = follows
      new InMemoryDurable().replay(id, first.history(id))(body(w, again)) ==>
        Right("stitched: follows")
      again.calls ==> 0
    }

    test("an opening placed already is not asked again, though offered still shows its offer") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val turn = TurnRef(b, t.turn)
      new InMemoryDurable().run(StitchRef(turn, at(1)).workflowId)(body(w, follows))
      val again = follows
      val other = new InMemoryDurable
      val id = StitchRef(turn, at(1)).workflowId
      other.run(id)(body(w, again)) ==> "nothing asked"
      other.recordedSteps(id) ==> Vector("ask:stitch")
      again.calls ==> 0
      assert(
        Stitching
          .offered(w.reads, FakeDb.as(Subject.Public), turn, Tuning.Default)
          .exists(_.nonEmpty)
      )
    }

    test("a placement over the day's cap asks no classifier: unread, for the cap") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      w.ledger.now = at(2)
      w.ledger.record(
        EntryId("elsewhere"),
        TurnRef(ConversationId("c2"), TurnSeq(0)),
        WorkflowId("c2:0"),
        "m",
        None,
        None,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal(1))),
        Tokens(1)
      )(using TestTx.fake)
      val classifier = follows
      new InMemoryDurable().run(StitchRef(TurnRef(b, t.turn), at(1)).workflowId)(
        w.placement(classifier, 2, cap = DailyCap.of("1").toOption)
      ) ==> "stitched: unread"
      val why = for {
        all <- w.entries.list(b)(using TestTx.fake).toOption
        root <- all.minByOption(_.seq)
        placed <- w.stitches.placed(root.id)(using TestTx.fake).toOption.flatten
      } yield placed match {
        case Placed.Unread(why, _) => why
        case other => s"not unread: $other"
      }
      (why, classifier.calls) ==> (
        Some("unavailable: The day's spend reached its cap, so no model was asked."),
        0
      )
    }

    test("a placement's classifier call is kept in the ledger once, under its own workflow") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val id = StitchRef(TurnRef(b, t.turn), at(1)).workflowId
      new InMemoryDurable().run(id)(body(w, follows))
      w.ledger.of(id)(using TestTx.fake).map(_.map(r => (r.entry, r.model, r.usage))) ==>
        Right(Vector((EntryId(s"move:${WorkflowId.value(id)}:stitch"), "jev-1.13.0", Spent)))
    }

    test("a reply's placement, or an id that is not a placement's, asks nothing") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val reply = w.hear("and another", "David", 1)
      val classifier = follows
      val durable = new InMemoryDurable
      val id = StitchRef(TurnRef(c, reply.turn), at(1)).workflowId
      durable.run(id)(body(w, classifier)) ==> "nothing asked"
      durable.run(WorkflowId("triage:c1:1:0"))(body(w, classifier)) ==>
        "not a stitch: triage:c1:1:0"
      classifier.calls ==> 0
    }
  }
}
