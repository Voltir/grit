package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.act.{Asked, Dreaming, MoveError, ScriptedDreaming}
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  Document,
  DocumentTerms,
  InMemoryDocuments,
  Pointer
}
import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, CorpusName, DocKey, PluginName}
import grit.core.job.JobRun
import grit.core.message.Message
import grit.core.need.{Exported, Needs, PluginDocs, PluginReads}
import grit.core.place.Place
import grit.core.stitch.StrandWorld
import grit.core.store.{Conversation, StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label, Level, Recorded}
import grit.dbos.sql.TestTx
import grit.knowledge.card.{Card, CardShelf, CardSummary, UnitVersion}
import grit.knowledge.ref.{Ref, TopicKey}

import utest.*

/** What an ask of [[TopicDreamTests]]' runs was: `attach` or `write`, and the message it was
  * shown.
  */
private final case class Ask(kind: String, shown: String)

/** The topics job's runs over rooms of threads in memory ([[StrandWorld]]) and cards kept by a
  * knowledge plugin beside it, their moves made by [[ScriptedDreaming]] over one document
  * store, so a later run reads what an earlier one kept. Unless a test says, the attach ask
  * opens a topic for everything it is shown, and each write ask answers [[Lead]] and
  * [[Passage]].
  */
object TopicDreamTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val Plugin: PluginName = right(PluginName.of("topics"))
  private val Knowledge: PluginName = right(PluginName.of("knowledge"))

  private val Terms: DocumentTerms =
    right(DocLabel.of("topic").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 500)))
  private val CardTerms: DocumentTerms = right(
    DocLabel.of("knowledge card").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 100))
  )

  private val Cards: Exported[CardShelf] = new Exported[CardShelf] {
    val name: PluginName = Knowledge
    def service(own: PluginReads): CardShelf = CardShelf.over(own.documents)
  }

  private val Day: LocalDate = LocalDate.parse("2026-10-12")

  /** When a run is made, unless a test says: an hour after the world's first message. */
  private val Now: Instant = StrandWorld.at(3600)

  private val Lead = "The deploy pipeline and what breaks it."
  private val Passage = "Ana traced the flaky deploy to the shared database."

  /** A strand of two threads in `channel`: the root saying `says`, a follower `reply`. */
  private def strand(w: StrandWorld, channel: String, says: String, at: Long = 0): Conversation = {
    val root = w.thread(channel, "1.0")
    w.say(root, says, at)
    w.follow(w.say(w.thread(channel, "2.0"), s"reply on $says", at + 10), root)
    root
  }

  private def key(root: Conversation): String = s"topic:strand-${ConversationId.value(root.id)}"

  /** The asks a run made, and the documents it left. */
  private final class Runs(w: StrandWorld) {
    val documents = new InMemoryDocuments
    private val heard = new Heard
    ok(documents.declare(Vector(Plugin -> Terms, Knowledge -> CardTerms))(using TestTx.fake))

    def asked: Vector[Ask] = heard.asked

    /** One run for `day`, made `now`, of `perRun`; its attach ask answered by `attach` of what
      * it was told and shown, its write asks by `write`; its rooms labelled as `rooms` records
      * them: its reply.
      */
    def run(
        perRun: Int = 10,
        day: Option[LocalDate] = Some(Day),
        now: Instant = Now,
        rooms: Recorded = Recorded.Empty,
        attach: (String, String) -> ujson.Value = everyNew,
        write: String -> ujson.Value = _ => wrote(Lead, Passage)
    ): String = {
      val dream = new TopicDream(10, ZoneOffset.UTC, Some(Cards))
      heard.run()
      val needs = Needs.over(
        Plugin,
        Vector(Knowledge -> PluginReads(NoCache, documents.shelf(Knowledge)))
      )
      val answer: (String, Vector[Message]) -> Either[MoveError, Asked[ujson.Value]] =
        (system, messages) => {
          val shown = messages.collect { case Message.User(t) => t }.mkString
          val isAttach = system.startsWith("You sort")
          heard.ask(Ask(if (isAttach) "attach" else "write", shown))
          val reply = if (isAttach) attach(system, shown) else write(shown)
          Right(Asked(TestTainted.at(reply, Integrity.Authority), Label.Public))
        }
      val moves = new ScriptedDreaming(
        dream.limits,
        Label.Public,
        Plugin,
        Terms,
        Dreaming.Reads(w.shelf, _ => needs, w.excerpts, documents),
        answer,
        documents = documents,
        at = now,
        rooms = rooms
      )
      dream.run(
        TestTainted.trusted(JobRun(TopicParams(perRun, 2.days, 2.days, day), now, now)),
        moves
      )
    }

    private given Tx = TestTx.fake

    /** The current topics: each key and text, by key. */
    def kept: Vector[(String, String)] =
      topics.map(d => (DocKey.value(d.key), DocText.value(d.text))).sortBy(_._1)

    def topics: Vector[Document] =
      ok(documents.shelf(Plugin).newest(Int.MaxValue))

    /** The keys of the topics that refer to `r`. */
    def referring(r: Ref): Vector[String] =
      ok(documents.shelf(Plugin).referring(right(Ref.written(r)), 10)).map(d => DocKey.value(d.key))

    /** A card of the Drive file `id`, kept by the knowledge plugin `at`, believed at
      * `integrity`: the key of the topic it would open.
      */
    def card(
        id: String,
        says: String,
        at: Instant = Now,
        integrity: Integrity = Integrity.Member
    ): String = {
      val c = Card.file(
        right(CorpusName.of("shared")),
        id,
        right(UnitVersion.of("2026-01-01T00:00:00Z")),
        right(CardSummary.of(says, Vector.empty)),
        right(Pointer.of(right(ToolName.of("drive_read")), "file" -> ujson.Str(id))),
        Some(s"File $id")
      )
      val _ = ok(
        documents
          .keeper(Knowledge, CardTerms)
          .write(
            c.docKey,
            Label.Public,
            integrity,
            Place.Everywhere,
            c.text,
            c.data,
            Some(c.pointer),
            at
          )
      )
      TopicKey.value(TopicKey.card(c.key))
    }

    /** The topics put up for review, each key and day, as an edge at a public room is offered
      * them.
      */
    def reviewed: Vector[(String, LocalDate)] = {
      val room = right(Place.read("slack:T/CREVIEW"))
      given Tx =
        TestTx.fake(Clearance.of(Label.Public), recorded = labelled(Label.Public, "CREVIEW"))
      ok(documents.reviews.of(Plugin).unposted(room)).map(r => (DocKey.value(r.key), r.day))
    }
  }

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(x => throw new java.lang.AssertionError(s"store failed: $x"), identity)

  private val NoCache: PluginDocs = new PluginDocs {
    def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]] = Right(None)
    def newest(prefix: String, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[(String, ujson.Value)]] =
      Right(Vector.empty)
  }

  /** What a fake's asks were shown, across runs. */
  private final class Heard {
    // Appended by the fake's script, on the test's one thread; read after each run.
    @caps.unsafe.untrackedCaptures
    var asked: Vector[Ask] = Vector.empty

    def run(): Unit = ()

    def ask(a: Ask): Unit = asked = asked :+ a
  }

  private def wrote(lead: String, passage: String): ujson.Value =
    ujson.Obj("lead" -> lead, "passage" -> passage)

  /** How many of `what` the attach ask was told it is shown. */
  private def told(system: String, what: String): Int =
    s"(\\d+) ${what}s?\\b".r.findFirstMatchIn(system).fold(0)(_.group(1).toInt)

  /** Everything shown placed `place`. */
  private def placing(place: String): (String, String) -> ujson.Value = (system, _) =>
    ujson.Obj(
      "discussions" -> ujson.Arr.from(Vector.fill(told(system, "discussion"))(ujson.Str(place))),
      "cards" -> ujson.Arr.from(Vector.fill(told(system, "card"))(ujson.Str(place)))
    )

  private val everyNew: (String, String) -> ujson.Value = placing("new")

  /** Each of `channels` kept at `label`. */
  private def labelled(label: Label, channels: String*): Recorded =
    Recorded(
      channels
        .map(c => right(Place.read(s"slack:T/$c")) -> Recorded.Kept(None, Some(label), false, None))
        .toMap,
      Map.empty
    )

  private val Internal: Label = Label.at(Level.Internal)

  val tests = Tests {
    test("an unreferred strand goes to the attach ask, and opens a topic or joins one") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val first = runs.run()
      val b = strand(w, "C2", "deploy rollback", 100)
      val c = strand(w, "C3", "lunch order", 200)
      val second = runs.run(
        now = Now.plusSeconds(600),
        attach = (_, _) =>
          // Two discussions, then one topic: the first joins it, the second opens its own.
          ujson.Obj("discussions" -> ujson.Arr("1", "new"), "cards" -> ujson.Arr())
      )
      (
        first,
        second,
        runs.asked.map(_.kind),
        runs.kept.map(_._1),
        runs.referring(Ref.Strand(b.id)),
        runs.referring(Ref.Strand(c.id))
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 left for the next run",
        "topics: 2 written (1 opened), 0 withdrawn, 1 unchanged, 0 left for the next run",
        Vector("attach", "write", "attach", "write", "write"),
        Vector(key(a), key(c)).sorted,
        Vector(key(a)),
        Vector(key(c))
      )
    }

    test("a strand a topic already refers to is attached to it without the attach ask") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      w.say(a, "the rollback works now", 50)
      val again = runs.run(now = Now.plusSeconds(600))
      (
        again,
        runs.asked.map(_.kind),
        runs.asked.last.shown.contains("the rollback works now")
      ) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 0 unchanged, 0 left for the next run",
        Vector("attach", "write", "write"),
        true
      )
    }

    test(
      "a changed card a topic links to attaches to it by its unit; one no topic refers to goes to the attach ask"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run(write =
        _ => wrote(Lead, "See [the plan](https://docs.google.com/document/d/plan1/edit).")
      )
      runs.card("plan1", "The deploy plan.", Now.plusSeconds(60))
      runs.card("menu2", "The lunch menu.", Now.plusSeconds(60))
      val second = runs.run(now = Now.plusSeconds(600), attach = placing("none"))
      val asks = runs.asked.drop(2)
      (
        second,
        asks.map(_.kind),
        asks.map(a => (a.shown.contains("The deploy plan."), a.shown.contains("The lunch menu.")))
      ) ==> (
        // The strand, in the topic's lineage at its digest, and the card set aside.
        "topics: 1 written (0 opened), 0 withdrawn, 2 unchanged, 0 left for the next run",
        Vector("attach", "write"),
        Vector((false, true), (true, false))
      )
    }

    test(
      "each topic touched gets one write ask and one keep, its passage under the run's day, its lead first"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val b = strand(w, "C2", "hiring plan", 100)
      val runs = new Runs(w)
      val reply = runs.run()
      (reply, runs.asked.map(_.kind), runs.kept) ==> (
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 0 left for the next run",
        Vector("attach", "write", "write"),
        Vector(key(a), key(b)).sorted.map(_ -> s"$Lead\n\n## 2026-10-12\n\n$Passage")
      )
    }

    test(
      "a passage whose strand's room is at another label now is withdrawn, and a topic left with none goes"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val _ = strand(w, "C2", "deploy rollback", 86400)
      val day2 = Some(Day.plusDays(1))
      val later = StrandWorld.at(86400 + 3600)
      val _ = runs.run(
        day = day2,
        now = later,
        attach = (_, _) => ujson.Obj("discussions" -> ujson.Arr("1"), "cards" -> ujson.Arr())
      )
      val joined = runs.kept
      val moved = runs.run(day = day2, now = later, rooms = labelled(Internal, "C1"))
      val left = runs.kept
      val gone = runs.run(day = day2, now = later, rooms = labelled(Internal, "C1", "C2"))
      (
        joined.map(_._2.linesIterator.count(_.startsWith("## "))),
        moved,
        left,
        gone,
        runs.kept
      ) ==> (
        Vector(2),
        "topics: 0 written (0 opened), 1 withdrawn, 1 unchanged, 0 left for the next run",
        Vector(key(a) -> s"$Lead\n\n## 2026-10-13\n\n$Passage"),
        "topics: 0 written (0 opened), 1 withdrawn, 0 unchanged, 0 left for the next run",
        Vector()
      )
    }

    test("past perRun, the rest are left and counted") {
      val w = new StrandWorld
      (1 to 3).foreach(i => strand(w, s"C$i", s"subject $i", i * 100L))
      val runs = new Runs(w)
      val reply = runs.run(perRun = 2)
      (reply, runs.kept.size) ==> (
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 1 left for the next run",
        2
      )
    }

    test("at most Reviewed of the topics written are put up for review, new ones first") {
      val w = new StrandWorld
      val a = strand(w, "C0", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      w.say(a, "the rollback works now", 86400)
      val roots =
        (1 to TopicDream.Reviewed).map(i => strand(w, s"C$i", s"subject $i", 86400 + i * 100L))
      val day2 = Day.plusDays(1)
      val reply = runs.run(day = Some(day2), now = StrandWorld.at(86400 + 3600))
      (reply, runs.reviewed.sortBy(_._1)) ==> (
        "topics: 9 written (8 opened), 0 withdrawn, 0 unchanged, 0 left for the next run",
        ((key(a) -> Day) +: roots.map(r => key(r) -> day2).toVector).sortBy(_._1)
      )
    }

    test("a rerun over the same digests asks nothing and counts them unchanged") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val first = runs.run()
      val again = runs.run()
      (first, again, runs.asked.size) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 0 left for the next run",
        2
      )
    }

    test("a run for a past day dates its passages that day and reads strands as of its end") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      w.say(a, "said the next day", 86400)
      val runs = new Runs(w)
      val reply = runs.run(day = Some(Day), now = StrandWorld.at(2 * 86400))
      (
        reply,
        runs.asked.map(_.shown.contains("said the next day")),
        runs.kept.map(_._2.linesIterator.filter(_.startsWith("## ")).toVector)
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 left for the next run",
        Vector(false, false),
        Vector(Vector("## 2026-10-12"))
      )
    }

    test("a link too long to keep as a ref is counted, and stays in the topic's text") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val long = "https://example.com/" + ("a" * 10000)
      val reply = runs.run(write = _ => wrote(Lead, s"See [the log]($long)."))
      (reply, runs.kept.map(_._2.contains(long))) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 left for the next run; 1 ref too long to keep",
        Vector(true)
      )
    }

    test("a topic written from a card believed at Outside is believed no more than that") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val opened = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60), Integrity.Outside)
      val reply = runs.run()
      (reply, runs.topics.map(d => (DocKey.value(d.key), d.integrity))) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 left for the next run",
        Vector((opened, Integrity.Outside))
      )
    }
  }
}
