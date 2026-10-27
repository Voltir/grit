package grit.turn

import grit.assembly.estimate.CharEstimate
import grit.core.agenda.{Agenda, Placement, Weights}
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, EntrySeq, PeriodSeq, TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.store.{Entry, InMemoryEntryStore, InMemoryUsageLedger, Payload}
import grit.core.visibility.Integrity
import grit.dbos.sql.TestTx
import grit.models.StubClassifier

import utest.*

object TurnAgendaTests extends TestSuite {
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

  private def last(t: Agenda, turn: TurnRef): Option[Agenda.Event.Placed] =
    t.placements.get(turn.turnSeq).flatMap(_.lastOption)

  private def close(x: Double, y: Double): Boolean = math.abs(x - y) < 1e-9

  /** The keys of items named `names`, in order (turnless items fold newest first). */
  private def named(names: String*): Vector[String] = {
    val events = names.toVector.zipWithIndex.reverse.flatMap { (name, i) =>
      val id = Agenda.ItemId(s"t$i")
      Vector(Agenda.Event.Opened(id), Agenda.Event.Described(id, name, "about"))
    }
    TurnAgenda.keys(Agenda.fold(Vector.empty, events).items).map(_._2.key)
  }

  val tests = Tests {
    test("after the purge, the first message of a period is weighed against the carried item") {
      import grit.core.period.{CloseReason, Closing, Flows, Section, TestClosings}
      val photo = TestClosings.line(Section.Agenda, "Photo Rename Script", 1, 1)
      val at = java.time.Instant.parse("2026-09-20T10:00:00Z")
      // Period 1 (turns 0 and 1) is purged: only its closing entry is left.
      val all = Vector(
        Entry(
          EntryId("closing:c1:1"),
          conversation,
          TurnSeq(1),
          None,
          EntrySeq(5),
          Payload.Closed(
            PeriodSeq.First,
            CloseReason.Lapsed,
            Closing(
              Flows.of("p", None, Vector()).getOrElse(sys.error("flows")),
              TestClosings.balance(photo)
            )
          ),
          at,
          Integrity.Member
        ),
        Entry(
          EntryId("u2"),
          conversation,
          TurnSeq(2),
          None,
          EntrySeq(6),
          Payload.Message(Message.User("more photos ~0.9")),
          at,
          Integrity.Member
        )
      )
      val placed = TurnAgenda.place(
        new CountingClassifier,
        CharEstimate,
        TurnRef(conversation, TurnSeq(2)),
        all
      )
      placed.current.map(_.id) ==> Some(Agenda.ItemId.carried(photo.id))
      placed.events ==> Vector(
        Agenda.Event.Placed(
          TurnSeq(2),
          Weights.same(Agenda.ItemId.carried(photo.id), 0.9),
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
          EntrySeq(0),
          Payload.Message(Message.User("photos")),
          at,
          Integrity.Member
        ),
        Entry(
          EntryId("h1"),
          conversation,
          TurnSeq(1),
          None,
          EntrySeq(1),
          Payload.Heard("more photos ~0.85"),
          at,
          Integrity.Member
        )
      )
      val opened = Agenda.Event.Opened(Agenda.ItemId.openedBy(TurnRef(conversation, TurnSeq(0))))
      val first = Agenda.Event.Placed(
        TurnSeq(0),
        Weights.whole(Agenda.ItemId.openedBy(TurnRef(conversation, TurnSeq(0)))),
        Placement.First
      )
      val withItem = all.patch(
        1,
        Vector(
          Entry(
            EntryId("topic0"),
            conversation,
            TurnSeq(0),
            None,
            EntrySeq(0),
            Payload.Agenda(Vector(opened, first)),
            at,
            Integrity.Member
          )
        ),
        0
      )
      TurnAgenda
        .place(new CountingClassifier, CharEstimate, TurnRef(conversation, TurnSeq(1)), withItem)
        .events
        .collect { case p: Agenda.Event.Placed => p.by } ==>
        Vector(Placement.Classified(0.85, Placement.Outcome.Same))
    }

    test("the first message opens an item, and nothing is asked") {
      val classifier = new CountingClassifier
      val (entries, turns) = converse(Vector("hello"), classifier)
      classifier.calls ==> 0
      val t = agenda(entries)
      t.items.map(_.id) ==> Vector(Agenda.ItemId.openedBy(turns(0)))
      last(t, turns(0)).map(p => (p.weights, p.by)) ==>
        Some((Weights.whole(Agenda.ItemId.openedBy(turns(0))), Placement.First))
      // Recorded as its own entry; that no request carries it is TurnTests' (only messages).
      entries.get(TurnAgenda.placedId(turns(0)))(using TestTx.fake).map(_.map(_.payload)) ==>
        Right(
          Some(
            Payload.Agenda(
              Vector(
                Agenda.Event.Opened(Agenda.ItemId.openedBy(turns(0))),
                Agenda.Event.Placed(
                  turns(0).turnSeq,
                  Weights.whole(Agenda.ItemId.openedBy(turns(0))),
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
      val t = agenda(entries)
      t.items.size ==> 1
      val placed = last(t, turns(1))
      placed.map(_.by) ==> Some(Placement.Classified(0.85, Placement.Outcome.Same))
      placed.map(_.weights.byItem) ==>
        Some(Vector(Weights.Share(Agenda.ItemId.openedBy(turns(0)), 0.85)))
      assert(placed.exists(p => close(p.weights.elsewhere, 0.15)))
      ledger.rows.collect { case r if r._1 == TurnAgenda.placedId(turns(1)) => r._3 } ==>
        Vector(StubClassifier.Model)
      // Served by the classifier's service, as its clearance names it.
      ledger
        .of(turns(1).workflowId)(using TestTx.fake)
        .map(_.collect { case r if r.entry == TurnAgenda.placedId(turns(1)) => r.upstream }) ==>
        Right(Vector(Some("service:test")))
    }

    test("unsure: it stays for now, in the uncertain band") {
      val (entries, turns) = converse(Vector("hello", "hm ~0.5"))
      val t = agenda(entries)
      t.placed(turns(1).turnSeq) ==> Some(Agenda.ItemId.openedBy(turns(0)))
      // The classifier's placement; the model's verdict follows it (TurnVerdictTests).
      t.placements.get(turns(1).turnSeq).flatMap(_.headOption).map(_.by) ==>
        Some(Placement.Classified(0.5, Placement.Outcome.Uncertain))
    }

    test("changed, with nowhere earlier to go: a new item, and no second question") {
      val classifier = new CountingClassifier
      val (entries, turns) = converse(Vector("hello", "knots? ~0.1"), classifier)
      classifier.calls ==> 1
      val t = agenda(entries)
      t.current.map(_.id) ==> Some(Agenda.ItemId.openedBy(turns(1)))
      t.earlier.map(_.id) ==> Vector(Agenda.ItemId.openedBy(turns(0)))
      last(t, turns(1)).map(_.by) ==>
        Some(
          Placement.Classified(
            0.1,
            Placement.Outcome.Changed(Vector(Placement.Chance(None, 1.0)))
          )
        )
    }

    test("changed, back to an earlier item: the choice scaled by 1 - p(same)") {
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
      val t = agenda(entries)
      val (first, second) = (Agenda.ItemId.openedBy(turns(0)), Agenda.ItemId.openedBy(turns(1)))
      t.placed(turns(2).turnSeq) ==> Some(first)
      t.current.map(_.id) ==> Some(first)
      val placed = last(t, turns(2)).getOrElse(sys.error("placed"))
      placed.weights.byItem.map(_.item) ==> Vector(second, first)
      assert(
        placed.weights.byItem.map(_.weight).zip(Vector(0.1, 0.81)).forall((x, y) => close(x, y)),
        close(placed.weights.elsewhere, 0.09)
      )
      // No new item was opened: only the two.
      t.items.size ==> 2
    }

    test("changed, to something new: a new item among earlier ones") {
      val (entries, turns) = converse(Vector("hello", "knots? ~0.1", "sailing? ~0.1"))
      val t = agenda(entries)
      t.items.map(_.id) ==> turns.reverse.map(Agenda.ItemId.openedBy)
      t.placed(turns(2).turnSeq) ==> Some(Agenda.ItemId.openedBy(turns(2)))
    }

    test("the classifier down: unclassified, in the current item, and the turn replies") {
      val (entries, turns) =
        converse(Vector("hello", "more"), new CountingClassifier(fail = true))
      val t = agenda(entries)
      t.placed(turns(1).turnSeq) ==> Some(Agenda.ItemId.openedBy(turns(0)))
      last(t, turns(1)).map(_.by) ==>
        Some(Placement.Unclassified("unavailable: HTTP 529: overloaded"))
      texts(entries).lastOption.exists(_.startsWith("summary")) ==> true
    }

    test(
      "a classifier whose service is not cleared for what the turn read: unclassified, and not asked"
    ) {
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      val durable = new InMemoryDurable
      val turns = Vector("hello", "and more").map { text =>
        val turn = say(entries, text)
        durable.run(turn.workflowId)(
          turnBodyWith(
            entries,
            new RecordingProvider,
            new Before(entries),
            new InMemoryUsageLedger,
            classifier,
            aboveTheClassifier = true
          )
        )
        turn
      }
      (last(agenda(entries), turns(1)).map(_.by), classifier.calls) ==> (
        Some(
          Placement.Unclassified("unavailable: " + grit.core.classify.TestClassifiers.Uncleared)
        ),
        0
      )
    }

    test("a crash recording the placement resumes there, and asks nothing twice") {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val classifier = new CountingClassifier
      converse(Vector("hello"), classifier, store)
      val turn = say(store, "more")
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Agenda])
      assertThrows[InMemoryDurable.Crash](
        runTurn(durable, entries, new RecordingProvider, turn, classifier = classifier)
      )
      durable
        .recordedSteps(turn.workflowId) ==> Vector(
        "pin-models",
        "weigh",
        "route",
        "offer",
        "stitched",
        "classify"
      )
      runTurn(durable, entries, new RecordingProvider, turn, classifier = classifier)
      classifier.calls ==> 1
      agenda(store).placements.get(turn.turnSeq).map(_.size) ==> Some(1)
    }

    test("a conversation from before agendas: its next message opens the first item") {
      // An earlier turn with no agenda events, as one that ran before the patch left it.
      val entries = new InMemoryEntryStore
      val _ = say(entries, "from before")
      val classifier = new CountingClassifier
      val (_, turns) = converse(Vector("after"), classifier, entries)
      classifier.calls ==> 0
      last(agenda(entries), turns(0)).map(_.by) ==> Some(Placement.First)
      TurnSeq.value(turns(0).turnSeq) ==> 1L
    }

    test("the summary names a new item and says what it covers; the name then stays") {
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
      val item = agenda(entries).current.getOrElse(sys.error("no topic"))
      (item.name, item.summary) ==> (
        Some("Knots and hitches under"),
        Some("knots, the bowline most.")
      )
      // The second summary was asked with the name the first gave.
      summarizer.requests.lift(1).flatMap(_.messages.headOption) match {
        case Some(Message.User(text)) =>
          assert(
            text.startsWith(
              "Topic so far:\n<document>\nKnots and hitches under (about: which knot holds.)\n" +
                "</document>\n\n<transcript>\n"
            )
          )
        case other => assert(other.toString == "a user message")
      }
    }

    test("keys are distinct: a repeated name is numbered") {
      val (a, b, c) = (Agenda.ItemId("a"), Agenda.ItemId("b"), Agenda.ItemId("c"))
      val ts = Agenda
        .fold(
          Vector.empty,
          Vector(
            Agenda.Event.Opened(a),
            Agenda.Event.Opened(b),
            Agenda.Event.Described(b, "X", "x"),
            Agenda.Event.Opened(c)
          )
        )
        .items
      TurnAgenda.keys(ts).map(_._2.key) ==> Vector("new topic", "X", "new topic (2)")
    }

    test("keys are distinct when a number would repeat a shown name") {
      named("x", "x", "x (2)") ==> Vector("x", "x (3)", "x (2)")
    }

    test("an item shown as the new-item key is numbered") {
      named(TurnAgenda.NewKey, "y") ==> Vector(s"${TurnAgenda.NewKey} (1)", "y")
    }

    test("keys are pairwise distinct and never the new-item key") {
      val n = TurnAgenda.NewKey
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
