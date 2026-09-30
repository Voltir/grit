package grit.turn

import grit.assembly.estimate.CharEstimate
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, PeriodSeq, TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.store.{Entry, InMemoryEntryStore, InMemoryUsageLedger, Payload}
import grit.core.topic.{Placement, TopicEvent, TopicId, Topics, Weights}
import grit.dbos.sql.TestTx
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

  /** The keys of topics named `names`, in order (turnless topics fold newest first). */
  private def named(names: String*): Vector[String] = {
    val events = names.toVector.zipWithIndex.reverse.flatMap { (name, i) =>
      val id = TopicId(s"t$i")
      Vector(TopicEvent.Opened(id), TopicEvent.Described(id, name, "about"))
    }
    TurnTopics.keys(Topics.fold(Vector.empty, events).topics).map(_._2.key)
  }

  val tests = Tests {
    test("after the purge, the first message of a period is weighed against the carried topic") {
      import grit.core.period.{CloseReason, Closing, Flows, Section, TestClosings}
      val photo = TestClosings.line(Section.Topics, "Photo Rename Script", 1, 1)
      val at = java.time.Instant.parse("2026-09-20T10:00:00Z")
      // Period 1 (turns 0 and 1) is purged: only its closing entry is left.
      val all = Vector(
        Entry(
          EntryId("closing:c1:1"),
          conversation,
          TurnSeq(1),
          None,
          5,
          Payload.Closed(
            PeriodSeq.First,
            CloseReason.Lapsed,
            Closing(
              Flows.of("p", None, Vector()).getOrElse(sys.error("flows")),
              TestClosings.balance(photo)
            )
          ),
          at
        ),
        Entry(
          EntryId("u2"),
          conversation,
          TurnSeq(2),
          None,
          6,
          Payload.Message(Message.User("more photos ~0.9")),
          at
        )
      )
      val placed = TurnTopics.place(
        new CountingClassifier,
        CharEstimate,
        TurnRef(conversation, TurnSeq(2)),
        all
      )
      placed.current.map(_.id) ==> Some(TopicId.carried(photo.id))
      placed.events ==> Vector(
        TopicEvent.Placed(
          TurnSeq(2),
          Weights.same(TopicId.carried(photo.id), 0.9),
          Placement.Classified(0.9, Placement.Outcome.Same)
        )
      )
    }

    test("a turn rooted on a heard message is placed by what was heard") {
      val at = java.time.Instant.parse("2026-09-20T10:00:00Z")
      val all = Vector(
        Entry(
          EntryId("u0"),
          conversation,
          TurnSeq(0),
          None,
          0,
          Payload.Message(Message.User("photos")),
          at
        ),
        Entry(
          EntryId("h1"),
          conversation,
          TurnSeq(1),
          None,
          1,
          Payload.Heard("more photos ~0.85"),
          at
        )
      )
      val opened = TopicEvent.Opened(TopicId.openedBy(TurnRef(conversation, TurnSeq(0))))
      val first = TopicEvent.Placed(
        TurnSeq(0),
        Weights.whole(TopicId.openedBy(TurnRef(conversation, TurnSeq(0)))),
        Placement.First
      )
      val withTopic = all.patch(
        1,
        Vector(
          Entry(
            EntryId("topic0"),
            conversation,
            TurnSeq(0),
            None,
            0,
            Payload.Topic(Vector(opened, first)),
            at
          )
        ),
        0
      )
      TurnTopics
        .place(new CountingClassifier, CharEstimate, TurnRef(conversation, TurnSeq(1)), withTopic)
        .events
        .collect { case p: TopicEvent.Placed => p.by } ==>
        Vector(Placement.Classified(0.85, Placement.Outcome.Same))
    }

    test("the first message opens a topic, and nothing is asked") {
      val classifier = new CountingClassifier
      val (entries, turns) = converse(Vector("hello"), classifier)
      classifier.calls ==> 0
      val t = topics(entries)
      t.topics.map(_.id) ==> Vector(TopicId.openedBy(turns(0)))
      last(t, turns(0)).map(p => (p.weights, p.by)) ==>
        Some((Weights.whole(TopicId.openedBy(turns(0))), Placement.First))
      // Recorded as its own entry; that no request carries it is TurnTests' (only messages).
      entries.get(TurnTopics.placedId(turns(0)))(using TestTx.fake).map(_.map(_.payload)) ==>
        Right(
          Some(
            Payload.Topic(
              Vector(
                TopicEvent.Opened(TopicId.openedBy(turns(0))),
                TopicEvent.Placed(
                  turns(0).turnSeq,
                  Weights.whole(TopicId.openedBy(turns(0))),
                  Placement.First
                )
              )
            )
          )
        )
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
      placed.map(_.by) ==> Some(Placement.Classified(0.85, Placement.Outcome.Same))
      placed.map(_.weights.byTopic) ==>
        Some(Vector(Weights.Share(TopicId.openedBy(turns(0)), 0.85)))
      assert(placed.exists(p => close(p.weights.elsewhere, 0.15)))
      ledger.rows.collect { case r if r._1 == TurnTopics.placedId(turns(1)) => r._3 } ==>
        Vector(StubClassifier.Model)
    }

    test("unsure: it stays for now, in the uncertain band") {
      val (entries, turns) = converse(Vector("hello", "hm ~0.5"))
      val t = topics(entries)
      t.placed(turns(1).turnSeq) ==> Some(TopicId.openedBy(turns(0)))
      // The classifier's placement; the model's verdict follows it (TurnVerdictTests).
      t.placements.get(turns(1).turnSeq).flatMap(_.headOption).map(_.by) ==>
        Some(Placement.Classified(0.5, Placement.Outcome.Uncertain))
    }

    test("changed, with nowhere earlier to go: a new topic, and no second question") {
      val classifier = new CountingClassifier
      val (entries, turns) = converse(Vector("hello", "knots? ~0.1"), classifier)
      classifier.calls ==> 1
      val t = topics(entries)
      t.current.map(_.id) ==> Some(TopicId.openedBy(turns(1)))
      t.earlier.map(_.id) ==> Vector(TopicId.openedBy(turns(0)))
      last(t, turns(1)).map(_.by) ==>
        Some(
          Placement.Classified(
            0.1,
            Placement.Outcome.Changed(Vector(Placement.Chance(None, 1.0)))
          )
        )
    }

    test("changed, back to an earlier topic: the choice scaled by 1 - p(same)") {
      val classifier = new CountingClassifier
      val (entries, turns) =
        converse(Vector("hello", "knots? ~0.1", "back ~0.1 ~back:new topic (2)"), classifier)
      classifier.calls ==> 3
      // The states go as they always have: the questions' instructions name these fields.
      def shape(v: ujson.Value): Vector[String] = v.obj.keys.toVector
      classifier.states.map(shape) ==> Vector(
        Vector("current_topic", "recent_messages", "new_message"),
        Vector("current_topic", "recent_messages", "new_message"),
        Vector("left_topic", "recent_messages", "new_message")
      )
      classifier.states.lastOption.map(s => shape(s("left_topic"))) ==> Some(
        Vector("name", "summary")
      )
      classifier.states.lastOption.map(_("recent_messages").arr.map(shape).toVector) ==>
        Some(Vector(Vector("from", "text"), Vector("from", "text")))
      classifier.states.lastOption.map(_("new_message").str) ==>
        Some("back ~0.1 ~back:new topic (2)")
      val t = topics(entries)
      val (first, second) = (TopicId.openedBy(turns(0)), TopicId.openedBy(turns(1)))
      t.placed(turns(2).turnSeq) ==> Some(first)
      t.current.map(_.id) ==> Some(first)
      val placed = last(t, turns(2)).getOrElse(sys.error("placed"))
      placed.weights.byTopic.map(_.topic) ==> Vector(second, first)
      assert(
        placed.weights.byTopic.map(_.weight).zip(Vector(0.1, 0.81)).forall((x, y) => close(x, y)),
        close(placed.weights.elsewhere, 0.09)
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
      durable
        .recordedSteps(turn.workflowId) ==> Vector(
        "pin-models",
        "offer",
        "stitch",
        "DBOS.patch-topics",
        "classify"
      )
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

    test("the summary names a new topic and says what it covers; the name then stays") {
      val summaries = Vector(
        "Summary: Asked about knots.\nTopic: **Knots and hitches under load**\nAbout: which knot holds.",
        "Summary: Asked about bowlines.\nTopic: Bowlines\nAbout: knots, the bowline most."
      )
      val summarizer =
        new Scripted((_, n) => Right(said(summaries.lift(n).getOrElse("no labels here"))))
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      Vector("knots?", "and bowlines?", "and sheet bends?").foreach { text =>
        runTurn(
          durable,
          entries,
          new RecordingProvider,
          say(entries, text),
          summarizer = summarizer
        )
      }
      texts(entries).filter(_.startsWith("summary:")) ==> Vector(
        "summary: Asked about knots.",
        "summary: Asked about bowlines.",
        "summary: no labels here"
      )
      val topic = topics(entries).current.getOrElse(sys.error("no topic"))
      (topic.name, topic.summary) ==> (
        Some("Knots and hitches under"),
        Some("knots, the bowline most.")
      )
      // The second summary was asked with the name the first gave.
      summarizer.requests.lift(1).flatMap(_.messages.headOption) match {
        case Some(Message.User(text)) =>
          assert(
            text.startsWith("Topic so far: Knots and hitches under (about: which knot holds.).")
          )
        case other => assert(other.toString == "a user message")
      }
    }

    test("keys are distinct: a repeated name is numbered") {
      val (a, b, c) = (TopicId("a"), TopicId("b"), TopicId("c"))
      val ts = Topics
        .fold(
          Vector.empty,
          Vector(
            TopicEvent.Opened(a),
            TopicEvent.Opened(b),
            TopicEvent.Described(b, "X", "x"),
            TopicEvent.Opened(c)
          )
        )
        .topics
      TurnTopics.keys(ts).map(_._2.key) ==> Vector("new topic", "X", "new topic (2)")
    }

    test("keys are distinct when a number would repeat a shown name") {
      named("x", "x", "x (2)") ==> Vector("x", "x (3)", "x (2)")
    }

    test("a topic shown as the new-topic key is numbered") {
      named(TurnTopics.NewKey, "y") ==> Vector(s"${TurnTopics.NewKey} (1)", "y")
    }

    test("keys are pairwise distinct and never the new-topic key") {
      val n = TurnTopics.NewKey
      val cases = Vector(
        Vector("x", "x", "x (2)"),
        Vector("x (2)", "x", "x"),
        Vector("x", "x (2)", "x", "x (3)", "x"),
        Vector("x (2)", "x (2)", "x", "x", "x (2) (2)"),
        Vector(n, n, s"$n (1)", s"$n (2)"),
        Vector("x (1)", "x", "x", "x (3)", "x (2)", "x"),
        Vector.fill(5)("x") ++ Vector("x (2)", "x (4)")
      )
      cases.foreach { shown =>
        val keys = named(shown*)
        assert(keys.distinct.size == keys.size, !keys.contains(n))
      }
    }
  }
}
