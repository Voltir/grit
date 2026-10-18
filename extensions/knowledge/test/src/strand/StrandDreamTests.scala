package grit.knowledge.strand

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.{Asked, Dreaming, MoveError, ScriptedDreaming}
import grit.core.document.{DocLabel, DocText, DocWeight, Document, DocumentTerms, InMemoryDocuments}
import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, DocKey, PluginName}
import grit.core.job.JobRun
import grit.core.message.Message
import grit.core.need.Needs
import grit.core.place.Place
import grit.core.stitch.StrandWorld
import grit.core.store.Conversation
import grit.core.visibility.{Clearance, Integrity, Label, Level, Recorded}
import grit.dbos.sql.TestTx

import utest.*

/** The dream job's runs over rooms of threads in memory ([[StrandWorld]]), their moves made by
  * [[ScriptedDreaming]] over one document store, so a second run reads what the first kept.
  * Every summary is answered [[Summary]] with [[Terms]], unless a test says.
  */
object StrandDreamTests extends TestSuite {

  private val Plugin: PluginName = PluginName.of("strands").fold(sys.error, identity)

  private val Terms: DocumentTerms =
    DocLabel
      .of("room discussion")
      .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 200))
      .fold(sys.error, identity)

  private val Internal: Label = Label.at(Level.Internal)

  /** When every run is made: an hour after the world's first message. */
  private val Now: Instant = StrandWorld.at(3600)

  private val Summary = "Ana and Bo traced the flaky deploy to the shared database."

  /** A message counted whole at its most ([[grit.core.stitch.Stitching.MessageChars]]). */
  private def long(word: String): String = (word + " ") * 60

  /** A world of one room, `C1`, and the strand there: `members` threads, the root first, each
    * after the root saying one long message, the root saying `rootSays`. Its root.
    */
  private def strand(w: StrandWorld, channel: String, members: Int, rootSays: Int): Conversation = {
    val root = w.thread(channel, "1.0")
    (0 until rootSays).foreach(i => w.say(root, long(s"deploy$i"), i.toLong))
    (1 until members).foreach { m =>
      val t = w.thread(channel, s"${m + 1}.0")
      w.follow(w.say(t, long(s"reply$m"), (10 * m).toLong), root)
    }
    root
  }

  /** The asks a run's summaries made, and the documents it left, in order. */
  private final class Runs(w: StrandWorld, floor: Label = Label.Public) {
    val documents = new InMemoryDocuments
    private val heard = new Heard

    /** Each summary's message, in order, across runs. */
    def asked: Vector[String] = heard.asked

    /** One run with `perRun`, its summaries answered by `answer` (the ask's index in the run),
      * its rooms labelled as `rooms` records them, of a plugin whose schedules are at `labels`:
      * its reply.
      */
    def run(
        perRun: Int = 5,
        rooms: Recorded = Recorded.Empty,
        labels: Vector[Label] = Vector(floor),
        answer: Int -> Either[MoveError, ujson.Value] = _ => Right(Answered)
    ): String = {
      val dream = new StrandDream(5, labels)
      heard.run()
      val needs = Needs.over(Plugin, Vector.empty)
      val moves = new ScriptedDreaming(
        dream.limits,
        floor,
        Plugin,
        Terms,
        Dreaming.Reads(w.shelf, _ => needs, w.excerpts, documents),
        (_, messages) =>
          answer(heard.ask(messages.collect { case Message.User(t) => t }))
            .map(v => Asked(TestTainted.at(v, Integrity.Member), floor)),
        documents = documents,
        at = Now,
        rooms = rooms
      )
      dream.run(TestTainted.trusted(JobRun(DreamParams(perRun, 2.days), Now, Now)), moves)
    }

    /** The current documents a run at `floor` reads: each key, label and text. */
    def kept: Vector[(String, String, String)] =
      documents
        .keeper(Plugin, Terms)
        .newest(Int.MaxValue)(using TestTx.fake(Clearance.of(floor)))
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
        .map(d => (DocKey.value(d.key), Label.written(d.label), DocText.value(d.text)))
  }

  /** What a fake's summaries were shown, across runs, and how many the current run asked. */
  private final class Heard {
    // Appended by the fake's script, on the test's one thread; read after each run.
    @caps.unsafe.untrackedCaptures
    var asked: Vector[String] = Vector.empty
    // As `asked`: the current run's asks, counted.
    @caps.unsafe.untrackedCaptures
    private var n = 0

    def run(): Unit = n = 0

    /** Records `shown`: the ask's index in its run. */
    def ask(shown: Vector[String]): Int = {
      asked = asked ++ shown
      n += 1
      n - 1
    }
  }

  private val Answered: ujson.Value =
    ujson.Obj("summary" -> Summary, "terms" -> ujson.Arr("flaky deploy", "shared database"))

  private def labelled(channel: String, label: Label): Recorded =
    Recorded(
      Map(
        Place.read(s"slack:T/$channel").fold(sys.error, identity) ->
          Recorded.Kept(None, Some(label), false, None)
      ),
      Map.empty
    )

  val tests = Tests {
    test(
      "a strand of three threads and enough said is kept as one document at the run's floor, keyed by its root"
    ) {
      val w = new StrandWorld
      val root = strand(w, "C1", members = 3, rootSays = 3)
      val runs = new Runs(w)
      val reply = runs.run()
      (reply, runs.kept.map((k, l, t) => (k, l, t.linesIterator.toVector))) ==> (
        "strands: 1 written, 0 withdrawn, 0 unchanged, 0 left for the next run",
        Vector(
          (
            s"strand:${ConversationId.value(root.id)}",
            "public",
            Vector(
              "3 threads in this room, 5 messages, from 2026-10-12 to 2026-10-12.",
              "",
              Summary,
              "",
              "Terms: flaky deploy, shared database"
            )
          )
        )
      )
    }

    test(
      "a strand of two threads, or of three with too little said, gets no document and asks nothing"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", members = 2, rootSays = 3)
      val r = w.thread("C2", "1.0")
      w.say(r, "short", 0)
      (1 to 2).foreach(m => w.follow(w.say(w.thread("C2", s"${m + 1}.0"), "brief", m.toLong), r))
      val runs = new Runs(w)
      (runs.run(), runs.asked, runs.kept) ==> (
        "strands: 0 written, 0 withdrawn, 0 unchanged, 0 left for the next run",
        Vector(),
        Vector()
      )
    }

    test(
      "a strand unchanged since its document asks nothing; one more message summarises it again and replaces it"
    ) {
      val w = new StrandWorld
      val root = strand(w, "C1", members = 3, rootSays = 3)
      val runs = new Runs(w)
      val first = runs.run()
      val again = runs.run()
      val asks = runs.asked.size
      w.say(root, long("later"), 100)
      val changed = runs.run()
      (first, again, asks, changed, runs.asked.size, runs.kept.map(_._3.linesIterator.next())) ==> (
        "strands: 1 written, 0 withdrawn, 0 unchanged, 0 left for the next run",
        "strands: 0 written, 0 withdrawn, 1 unchanged, 0 left for the next run",
        1,
        "strands: 1 written, 0 withdrawn, 0 unchanged, 0 left for the next run",
        2,
        Vector("3 threads in this room, 6 messages, from 2026-10-12 to 2026-10-12.")
      )
    }

    test(
      "a document whose strand's messages are gone is withdrawn, and so is one whose data does not read"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      val keeper = runs.documents.keeper(Plugin, Terms)
      given grit.core.store.Tx = TestTx.fake
      def write(key: String, data: ujson.Value): Unit = {
        val _ = keeper.write(
          DocKey.of(key).fold(sys.error, identity),
          Label.Public,
          Integrity.Member,
          Place.read("slack:T/C1").fold(sys.error, identity),
          DocText.of("old").fold(sys.error, identity),
          data,
          None,
          Now
        )
      }
      write("strand:gone", ujson.Obj("root" -> "gone", "digest" -> "0123456789abcdef"))
      write("strand:odd", ujson.Obj("root" -> "odd"))
      (runs.run(), runs.kept) ==> (
        "strands: 0 written, 2 withdrawn, 0 unchanged, 0 left for the next run",
        Vector()
      )
    }

    test(
      "a room relabelled away from the run's floor loses its document there, and none is written"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", members = 3, rootSays = 3)
      val runs = new Runs(w, Internal)
      val first = runs.run(rooms = labelled("C1", Internal))
      val kept = runs.kept.map(_._2)
      val moved =
        runs.run(rooms = labelled("C1", Label.Public), labels = Vector(Label.Public, Internal))
      (first, kept, moved, runs.kept) ==> (
        "strands: 1 written, 0 withdrawn, 0 unchanged, 0 left for the next run",
        Vector("internal"),
        "strands: 0 written, 1 withdrawn, 0 unchanged, 0 left for the next run",
        Vector()
      )
    }

    test("a room at a label no schedule declares gets no document, and the reply counts it") {
      val w = new StrandWorld
      val _ = strand(w, "C1", members = 3, rootSays = 3)
      val runs = new Runs(w, Internal)
      (runs.run(labels = Vector(Internal)), runs.kept) ==> (
        "strands: 0 written, 0 withdrawn, 0 unchanged, 0 left for the next run; 1 room at a label no schedule declares",
        Vector()
      )
    }

    test("past perRun, strands are left for the next run, oldest touched first summarised") {
      val w = new StrandWorld
      val _ = strand(w, "C1", members = 3, rootSays = 3)
      val older = strand(w, "C2", members = 3, rootSays = 3)
      w.say(w.thread("C1", "2.0"), "and more", 200)
      val runs = new Runs(w)
      (runs.run(perRun = 1), runs.kept.map(_._1)) ==> (
        "strands: 1 written, 0 withdrawn, 0 unchanged, 1 left for the next run",
        Vector(s"strand:${ConversationId.value(older.id)}")
      )
    }

    test(
      "a summary refused leaves its strand unwritten, naming why, and the next is still summarised"
    ) {
      val w = new StrandWorld
      val second = strand(w, "C1", members = 3, rootSays = 3)
      val first = strand(w, "C2", members = 3, rootSays = 3)
      w.say(w.thread("C1", "2.0"), "and more", 200)
      val runs = new Runs(w)
      val reply =
        runs.run(answer = i => if (i == 0) Left(MoveError.Model("busy")) else Right(Answered))
      (reply, runs.kept.map(_._1)) ==> (
        s"strands: 1 written, 0 withdrawn, 0 unchanged, 1 left for the next run; not written: ${ConversationId.value(first.id)} (busy)",
        Vector(s"strand:${ConversationId.value(second.id)}")
      )
    }

    test(
      "a summary is shown the strand's messages, its root's first, under their speakers' names, in the order said"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", members = 3, rootSays = 3)
      val runs = new Runs(w)
      val _ = runs.run()
      runs.asked.map(_.linesIterator.map(_.split(' ').take(2).mkString(" ")).toVector) ==> Vector(
        Vector(
          "Someone: deploy0",
          "Someone: deploy1",
          "Someone: deploy2",
          "Someone: reply1",
          "Someone: reply2"
        )
      )
    }

    test(
      "its parameters read back as written; a reach that is no positive duration does not read"
    ) {
      val dream = new StrandDream(5, Vector(Label.Public))
      val params = DreamParams(3, 2.days)
      (
        dream.write(params),
        dream.read(dream.write(params)),
        dream.read(ujson.Obj("perRun" -> 3, "reach" -> "PT0S")),
        dream.limits.asks
      ) ==> (
        ujson.Obj("perRun" -> 3, "reach" -> "PT48H"),
        Right(params),
        Left(
          """a dream's parameters hold reach, a positive ISO-8601 duration: {"perRun":3,"reach":"PT0S"}"""
        ),
        5
      )
    }

    test("a document's text says one thread and one message in the singular") {
      val w = new StrandWorld
      val root = w.thread("C1", "1.0")
      w.say(root, "alone", 0)
      val seen = StrandWorld.right(w.shelf.of(Vector(root.id), Now)(using TestTx.fake))
      val summary =
        grit.knowledge.card.CardSummary.of("A note.", Vector()).fold(sys.error, identity)
      seen.map(s => DocText.value(StrandDocument.text(s, summary))) ==>
        Vector("1 thread in this room, 1 message, from 2026-10-12 to 2026-10-12.\n\nA note.")
    }

    test("a document's data names its root, room, members, digest and counts, and reads back") {
      val w = new StrandWorld
      val root = strand(w, "C1", members = 3, rootSays = 3)
      val seen = StrandWorld.right(w.shelf.of(Vector(root.id), Now)(using TestTx.fake))
      val data = seen.map(StrandDocument.data)
      val doc = (d: ujson.Value) =>
        Document(
          grit.core.id.DocumentVersion.of(1).getOrElse(sys.error("no version")),
          Plugin,
          StrandDocument.key(root.id),
          Label.Public,
          Integrity.Member,
          Place.read("slack:T/C1").fold(sys.error, identity),
          DocText.of("t").fold(sys.error, identity),
          d,
          None,
          Now,
          grit.core.document.Placement(0, Now),
          None
        )
      (
        data.map(_.value.keys.toVector),
        data.map(d => StrandDocument.held(doc(d))),
        StrandDocument.held(doc(ujson.Obj()))
      ) ==> (
        Vector(Vector("root", "room", "members", "digest", "said", "since", "touched")),
        seen.map(s => Right((s.root, s.digest))),
        Left("the document strand:c1 names no root")
      )
    }
  }
}
