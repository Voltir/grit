package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.{TurnRef, TurnSeq}
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger, Payload}
import grit.core.topic.{Band, Placement, TopicEvent, TopicId, Topics}
import grit.models.StubClassifier

import utest.*

object TurnTopicsTests extends TestSuite {
  import TurnFixtures.*

  /** Each of `said` answered in turn, in one conversation, placed by `classifier`. */
  private def converse(
      said: Vector[String],
      classifier: CountingClassifier = new CountingClassifier,
      entries: InMemoryEntryStore = new InMemoryEntryStore,
      ledger: InMemoryUsageLedger = new InMemoryUsageLedger
  ): (InMemoryEntryStore, Vector[TurnRef]) = {
    val durable = new InMemoryDurable
    val turns = said.map { text =>
      val turn = say(entries, text)
      runTurn(durable, entries, new RecordingProvider, turn, ledger, classifier = classifier)
      turn
    }
    (entries, turns)
  }

  private def last(t: Topics, turn: TurnRef): Option[TopicEvent.Placed] =
    t.placements.get(turn.turnSeq).flatMap(_.lastOption)

  private def close(x: Double, y: Double): Boolean = math.abs(x - y) < 1e-9

  val tests = Tests {
    test("the first message opens a topic, and nothing is asked") {
      val classifier = new CountingClassifier
      val (entries, turns) = converse(Vector("hello"), classifier)
      classifier.calls ==> 0
      val t = topics(entries)
      t.topics.map(_.id) ==> Vector(TopicId.openedBy(turns(0)))
      last(t, turns(0)).map(p => (p.weights, p.elsewhere, p.by)) ==>
        Some((Vector(TopicId.openedBy(turns(0)) -> 1.0), 0.0, Placement.First))
    }

    test("sure it is the same: it stays, weighed p(same), the rest elsewhere; its cost recorded") {
      val ledger = new InMemoryUsageLedger
      val classifier = new CountingClassifier
      val (entries, turns) =
        converse(Vector("hello", "and more ~0.85"), classifier, ledger = ledger)
      classifier.calls ==> 1
      val t = topics(entries)
      t.topics.size ==> 1
      val placed = last(t, turns(1))
      placed.map(_.by) ==> Some(Placement.Classified(0.85, Band.Same, Vector.empty))
      placed.map(_.weights) ==> Some(Vector(TopicId.openedBy(turns(0)) -> 0.85))
      assert(placed.exists(p => close(p.elsewhere, 0.15)))
      ledger.rows.collect { case r if r._1 == TurnTopics.placedId(turns(1)) => r._3 } ==>
        Vector(StubClassifier.Model)
    }

    test("unsure: it stays for now, in the uncertain band") {
      val (entries, turns) = converse(Vector("hello", "hm ~0.5"))
      val t = topics(entries)
      t.placed(turns(1).turnSeq) ==> Some(TopicId.openedBy(turns(0)))
      last(t, turns(1)).map(_.by) ==> Some(Placement.Classified(0.5, Band.Uncertain, Vector.empty))
    }

    test("changed, with nowhere earlier to go: a new topic, and no second question") {
      val classifier = new CountingClassifier
      val (entries, turns) = converse(Vector("hello", "knots? ~0.1"), classifier)
      classifier.calls ==> 1
      val t = topics(entries)
      t.current.map(_.id) ==> Some(TopicId.openedBy(turns(1)))
      t.earlier.map(_.id) ==> Vector(TopicId.openedBy(turns(0)))
      last(t, turns(1)).map(_.by) ==>
        Some(Placement.Classified(0.1, Band.Changed, Vector(None -> 1.0)))
    }

    test("changed, back to an earlier topic: the choice scaled by 1 - p(same)") {
      val classifier = new CountingClassifier
      val (entries, turns) =
        converse(Vector("hello", "knots? ~0.1", "back ~0.1 ~back:new topic (2)"), classifier)
      classifier.calls ==> 3
      val t = topics(entries)
      val (first, second) = (TopicId.openedBy(turns(0)), TopicId.openedBy(turns(1)))
      t.placed(turns(2).turnSeq) ==> Some(first)
      t.current.map(_.id) ==> Some(first)
      val placed = last(t, turns(2)).getOrElse(sys.error("placed"))
      placed.weights.map(_._1) ==> Vector(second, first)
      assert(
        close(placed.weights(0)._2, 0.1),
        close(placed.weights(1)._2, 0.81),
        close(placed.elsewhere, 0.09)
      )
      // No new topic was opened: only the two.
      t.topics.size ==> 2
    }

    test("changed, to something new: a new topic among earlier ones") {
      val (entries, turns) = converse(Vector("hello", "knots? ~0.1", "sailing? ~0.1"))
      val t = topics(entries)
      t.topics.map(_.id) ==> turns.reverse.map(TopicId.openedBy)
      t.placed(turns(2).turnSeq) ==> Some(TopicId.openedBy(turns(2)))
    }

    test("the classifier down: unclassified, in the current topic, and the turn replies") {
      val (entries, turns) =
        converse(Vector("hello", "more"), new CountingClassifier(fail = true))
      val t = topics(entries)
      t.placed(turns(1).turnSeq) ==> Some(TopicId.openedBy(turns(0)))
      last(t, turns(1)).map(_.by) ==>
        Some(Placement.Unclassified("unavailable: HTTP 529: overloaded"))
      texts(entries).lastOption.exists(_.startsWith("summary")) ==> true
    }

    test("the placement is recorded as an entry, never sent to the model") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      runTurn(durable, entries, provider, turn, classifier = new CountingClassifier)
      provider.requests.flatMap(_.messages).size ==> 1
      entries
        .get(TurnTopics.placedId(turn))(using grit.dbos.sql.TestTx.fake)
        .toOption
        .flatten
        .map(_.payload.isInstanceOf[Payload.Topic]) ==> Some(true)
    }

    test("a crash recording the placement resumes there, and asks nothing twice") {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val classifier = new CountingClassifier
      converse(Vector("hello"), classifier, store)
      val turn = say(store, "more")
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Topic])
      assertThrows[InMemoryDurable.Crash](
        runTurn(durable, entries, new RecordingProvider, turn, classifier = classifier)
      )
      durable.recordedSteps(turn.workflowId) ==> Vector("DBOS.patch-topics", "classify")
      runTurn(durable, entries, new RecordingProvider, turn, classifier = classifier)
      classifier.calls ==> 1
      topics(store).placements.get(turn.turnSeq).map(_.size) ==> Some(1)
    }

    test("a conversation from before topics: its next message opens the first topic") {
      // An earlier turn with no topic events, as one that ran before the patch left it.
      val entries = new InMemoryEntryStore
      val _ = say(entries, "from before")
      val classifier = new CountingClassifier
      val (_, turns) = converse(Vector("after"), classifier, entries)
      classifier.calls ==> 0
      last(topics(entries), turns(0)).map(_.by) ==> Some(Placement.First)
      TurnSeq.value(turns(0).turnSeq) ==> 1L
    }
  }
}
