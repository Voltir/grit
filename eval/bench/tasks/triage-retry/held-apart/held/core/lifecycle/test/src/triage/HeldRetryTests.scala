package grit.lifecycle.triage

import java.time.ZoneOffset

import grit.core.durable.InMemoryDurable
import grit.core.id.TurnRef
import grit.core.speech.{Decision, Limits, Speaking, Venue}
import grit.core.spend.{Budget, DailyCap}
import grit.core.stitch.Tuning
import grit.core.store.Tx
import grit.dbos.sql.TestTx

import utest.*

/** Triage's `consider` step asked again for one heard message after its decision was kept, as
  * DBOS runs a step whose transaction committed and whose result was not recorded.
  */
object HeldRetryTests extends TestSuite {

  import TriageFixtures.*

  /** v7 answered: gap asks at 1.0; open 0.9, to 0.125, to-grit 0.125, durable 0.5,
    * records-hold 0.875, for-room 0: past its gate, and placed in its thread.
    */
  private def asking =
    new Scripted(Vector(1, 0, 0, 0, 0), Vector(0.9, 0.125, 0.125, 0.5, 0.875, 0.0))

  private val within = Speaking.Within(
    Limits.suggested(
      DailyCap.of("0.25").getOrElse(sys.error("a cap")),
      TriageQuestions.ShippedSpeak
    )
  )

  /** The stores [[World.body]] gives the triage. */
  private def records(w: World): TriageRecords =
    TriageRecords(
      w.entries,
      w.triage,
      w.principals,
      w.conversations,
      w.speech,
      w.ledger,
      w.acknowledgements,
      w.deliveries,
      w.stitches,
      w.search,
      w.rooms,
      w.periods,
      w.documents
    )

  val tests = Tests {
    test("a consider asked again after its decision was kept answers the decision it kept") {
      val w = new World
      val t = w.hear("what did we decide about the refi page?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(w.body(asking, 1, within))
      val turn = TurnRef(c, t.turn)
      given Tx = TestTx.fake
      val entry = w.entries
        .list(c)
        .toOption
        .flatMap(_.find(_.turnSeq == t.turn))
        .fold(sys.error("no entry"))(_.id)
      val tags = w.tags(t).getOrElse(sys.error("no tags"))
      // As the flow's step asks it: the body's budget and tuning, at its clock's start.
      val again = Speak.consider(
        records(w),
        within,
        Budget(ZoneOffset.UTC, None),
        Tuning.Default.horizon,
        t,
        entry,
        tags,
        at(1)
      )
      (again, w.speech.decisions.map(_._2)) ==> (
        Right(Decision.Drafting(turn, Venue.Thread)),
        Vector(Decision.Drafting(turn, Venue.Thread))
      )
    }

    test("a triage whose consider step runs again after its decision was kept starts its turn") {
      val w = new World
      val t = w.hear("what did we decide about the refi page?", "Ana", 0)
      val durable = new InMemoryDurable
      val first = durable.run(t.workflowId)(w.body(asking, 1, within))
      val turn = TurnRef(c, t.turn)
      // The step committed its transaction and was not recorded: DBOS runs it again.
      val recorded = durable.history(t.workflowId).takeWhile(_.name != "consider")
      new InMemoryDurable().replay(t.workflowId, recorded)(w.body(asking, 1, within)) ==>
        Right(first)
      (w.started, w.speech.decisions.map(_._2)) ==> (
        Vector(turn, turn),
        Vector(Decision.Drafting(turn, Venue.Thread))
      )
    }
  }
}
