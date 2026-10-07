package grit.lifecycle.stitch

import grit.core.classify.Classifier
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.id.{StitchRef, TurnRef, WorkflowId}
import grit.core.stitch.{Link, Stitching, Tuning}
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
    test("an opening is placed in stitch, and its placement kept in record-stitch") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val id = StitchRef(TurnRef(b, t.turn), at(1)).workflowId
      val durable = new InMemoryDurable
      durable.run(id)(body(w, follows)) ==> "stitched: follows"
      durable.recordedSteps(id) ==> Vector("stitch", "record-stitch")
      w.stitches.links(Vector(b))(using TestTx.fake) ==> Right(Vector(Link(b, c)))
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
      other.recordedSteps(id) ==> Vector("stitch")
      again.calls ==> 0
      assert(
        Stitching
          .offered(w.reads, FakeDb.as(Subject.Public), turn, Tuning.Default)
          .exists(_.nonEmpty)
      )
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
