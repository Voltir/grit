package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{Provider, ToolUse}
import grit.core.store.{Entry, EntryStore, InMemoryEntryStore, InMemoryUsageLedger, StoreError, Tx}
import grit.core.topic.{Placement, TopicEvent, TopicId, Verdict}

import utest.*

/** The verdict of a turn whose classifier was unsure ([[TurnVerdict]]), asked in its tool
  * loop.
  */
object TurnVerdictTests extends TestSuite {
  import TurnFixtures.*

  /** `underlying`, refusing to insert the entry `id`. */
  final class Refusing(underlying: EntryStore, id: EntryId) extends EntryStore {
    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
      if (entry.id == id) Left(StoreError.DatabaseError("no room for a verdict"))
      else underlying.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = underlying.get(id)
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = underlying.list(c)
    def at(c: ConversationId, seqs: Vector[EntrySeq])(using
        Tx^
    ): Either[StoreError, Vector[Entry]] =
      underlying.at(c, seqs)
    def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] =
      underlying.ofTurn(turn)
    def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      underlying.lockNext(c)
  }

  /** Two earlier turns, the second a new topic; then `asked` answered by `provider`, with
    * the stub classifier. The last turn, its conversation's entries and ledger.
    */
  private def third(
      asked: String,
      provider: Provider^,
      durable: InMemoryDurable = new InMemoryDurable
  ): (TurnRef, InMemoryEntryStore, InMemoryUsageLedger) = {
    val entries = new InMemoryEntryStore
    val ledger = new InMemoryUsageLedger
    val classifier = new CountingClassifier
    Vector("hello", "knots? ~0.1").foreach { text =>
      val t = say(entries, text)
      runTurn(
        new InMemoryDurable,
        entries,
        new RecordingProvider,
        t,
        ledger,
        classifier = classifier
      )
    }
    val turn = say(entries, asked)
    runTurn(durable, entries, provider, turn, ledger, classifier = classifier)
    (turn, entries, ledger)
  }

  private def placements(entries: InMemoryEntryStore, turn: TurnRef): Vector[TopicEvent.Placed] =
    topics(entries).placements.getOrElse(turn.turnSeq, Vector.empty)

  private def lastBy(entries: InMemoryEntryStore, turn: TurnRef): Option[Placement] =
    placements(entries, turn).lastOption.map(_.by)

  val tests = Tests {
    test("sure of the topic: no tool, no note") {
      val provider = new RecordingProvider
      val (_, _, _) = third("more ~0.9", provider)
      provider.requests.lastOption.map(r => (r.tools, r.messages.lastOption)) ==>
        Some((Vector.empty, Some(Message.User("more ~0.9"))))
    }

    test("unsure: the tool offered and the note on the message, in the request only") {
      val provider = new RecordingProvider
      val (turn, entries, _) = third("hm ~0.5", provider)
      val first = provider.requests.lift(provider.requests.size - 1)
      first.map(_.tools.map(_.name)) ==> Some(Vector("topic"))
      first.map(_.use) ==> Some(ToolUse.Auto)
      val note = first.flatMap(_.messages.lastOption) match {
        case Some(Message.User(text)) => text
        case other => other.toString
      }
      assert(
        note.startsWith("hm ~0.5\n\n[grit: the topic may have changed."),
        note.contains("Current topic: new topic."),
        note.contains("Earlier topics: new topic (2).")
      )
      texts(entries).filter(_.startsWith("user: hm")) ==> Vector("user: hm ~0.5")
    }

    test("the verdict names an earlier topic: placed back in it") {
      val (turn, entries, _) =
        third(
          """back ~0.5 #call:{"about":"earlier","earlier":"new topic (2)"}""",
          new RecordingProvider
        )
      val t = topics(entries)
      t.placed(turn.turnSeq) ==> Some(
        TopicId.openedBy(TurnRef(conversation, grit.core.id.TurnSeq(0)))
      )
      t.topics.size ==> 2
    }

    test("reading a call: the earlier key an enum, and each refusal's message") {
      val none = TurnTopics.Classification(Vector.empty, None, Vector.empty, None, None)
      val two = none.copy(
        earlier = Vector(
          TurnTopics.Shown(TopicId("a"), "Knots"),
          TurnTopics.Shown(TopicId("b"), "Knots (2)")
        )
      )
      TurnVerdict.read(two, ujson.Obj("about" -> "earlier", "earlier" -> "Knots (2)")) ==>
        Right(Verdict.Earlier("Knots (2)"))
      TurnVerdict.read(two, ujson.Obj("about" -> "new", "name" -> "  Sailing ")) ==>
        Right(Verdict.New(Some("Sailing")))
      TurnVerdict.read(two, ujson.Obj("about" -> "new", "name" -> " ")) ==> Right(Verdict.New(None))
      TurnVerdict.read(two, ujson.Obj("about" -> "current", "earlier" -> "Knots")) ==>
        Right(Verdict.Current)
      TurnVerdict.read(two, ujson.Obj("about" -> "earlier")).left.map(_.message) ==>
        Left("`earlier` is missing: it takes one of `Knots`, `Knots (2)`.")
      TurnVerdict
        .read(two, ujson.Obj("about" -> "earlier", "earlier" -> "knots"))
        .left
        .map(_.message) ==>
        Left("`earlier` takes one of `Knots`, `Knots (2)`, not \"knots\".")
      // No earlier topics: no `earlier` field to send, and no `earlier` to say.
      TurnVerdict.topic(none).schema(strict = false).parameters("properties").obj.keys.toVector ==>
        Vector("about", "name")
      TurnVerdict.read(none, ujson.Obj("about" -> "earlier")).left.map(_.message) ==>
        Left("`about` takes one of `current`, `new`, not \"earlier\".")
      TurnVerdict.read(none, ujson.Obj("about" -> "new", "earlier" -> "x")).left.map(_.message) ==>
        Left("There is no argument `earlier`; the arguments there are `about`, `name`.")
    }

    test("a loop's anomalies: none, one, then each said in order") {
      val topic = topicCall("current")
      val other = AssistantBlock.ToolCall(grit.core.id.ToolCallId("p"), "peek", ujson.Obj())
      val table: Vector[(Vector[Message.Assistant], Option[String])] = Vector(
        Vector(said("", topic, other), said("done")) -> None,
        Vector(said("hi")) -> Some("no topic call in the first reply"),
        Vector.empty -> Some("no topic call in the first reply"),
        Vector(said("", topic, topic), said("done")) ->
          Some("topic called 2 times; only the first counts"),
        Vector(said("", topic), said("", topic), said("done")) ->
          Some("topic called 2 times; only the first counts"),
        Vector(said("", other), said("", other), said("", topic), said("done")) -> Some(
          "no topic call in the first reply; topic called only in a later round (2); not read"
        ),
        Vector(said("", other), said("", topic), said("", topic)) -> Some(
          "no topic call in the first reply; topic called only in a later round (1, 2); not " +
            "read; topic called 2 times; only the first counts"
        )
      )
      table.foreach { (replies: Vector[Message.Assistant], expected: Option[String]) =>
        (replies.size, TurnVerdict.anomaly(replies)) ==> (replies.size, expected)
      }
    }

    test("the verdict says current: it stays") {
      val (turn, entries, _) =
        third("""hm ~0.5 #call:{"about":"current"}""", new RecordingProvider)
      val t = topics(entries)
      // The current topic is the one the second turn opened; none is opened for this one.
      t.placed(turn.turnSeq) ==> Some(TopicId.openedBy(TurnRef(conversation, TurnSeq(1))))
      t.topics.size ==> 2
      lastBy(entries, turn) ==> Some(Placement.Asked(Verdict.Current, None))
    }

    test("the earlier topics are an enum in the tool's schema, offered per call") {
      val provider = new RecordingProvider
      val (_, _, _) = third("hm ~0.5", provider)
      val shown = provider.requests.lift(provider.requests.size - 1).flatMap(_.tools.headOption)
      shown.map(_.parameters("properties")("earlier")("enum")) ==>
        Some(ujson.Arr("new topic (2)"))
      shown.map(_.parameters("required")) ==> Some(ujson.Arr("about"))
    }

    test("the verdict not recorded: the turn replies, and its log says so") {
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach { text =>
        val t = say(entries, text)
        runTurn(new InMemoryDurable, entries, new RecordingProvider, t, classifier = classifier)
      }
      val turn = say(entries, """hm ~0.5 #call:{"about":"current"}""")
      val refusing = new Refusing(entries, TurnVerdict.verdictId(turn))
      val log = runTurn(
        new InMemoryDurable,
        refusing,
        new RecordingProvider,
        turn,
        classifier = classifier
      )
      assert(
        log.startsWith("replied: "),
        log.endsWith("; verdict not recorded: Store(no room for a verdict)")
      )
    }

    test("the verdict's rounds are built and recorded without listing the conversation") {
      val store = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach { text =>
        val t = say(store, text)
        runTurn(new InMemoryDurable, store, new RecordingProvider, t, classifier = classifier)
      }
      val turn = say(store, """hm ~0.5 #call:{"about":"new","name":"Sailing"}""")
      val entries = new UnlistedWhileAnswering(store)
      runTurn(
        new InMemoryDurable,
        entries,
        new RecordingProvider,
        turn,
        classifier = classifier
      ) ==>
        "replied: reply:c1:2; summarised: summary:c1:2"
      entries.windows ==> 1
      lastBy(store, turn) ==> Some(Placement.Asked(Verdict.New(Some("Sailing")), None))
    }
  }
}
