package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.act.{Asked, Dreaming, MoveError, ScriptedDreaming}
import grit.core.document.{
  DocLabel,
  DocRef,
  DocText,
  DocWeight,
  Document,
  DocumentSearch,
  DocumentTerms,
  InMemoryDocuments,
  Pointer,
  Shelved
}
import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, CorpusName, DocKey, DocumentVersion, PluginName}
import grit.core.job.JobRun
import grit.core.message.Message
import grit.core.need.{Exported, Needs, PluginDocs, PluginReads}
import grit.core.place.Place
import grit.core.stitch.StrandWorld
import grit.core.store.{Conversation, StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label, Level, Recorded}
import grit.dbos.sql.TestTx
import grit.knowledge.card.{Card, CardShelf, CardSummary, Scope, UnitVersion}
import grit.knowledge.ref.{Ref, TopicKey}

import utest.*

/** What an ask of [[TopicDreamTests]]' runs was: `attach` or `write`, what it was told, and the
  * message it was shown.
  */
private final case class Ask(kind: String, system: String, shown: String)

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

  /** A link to the Drive file `plan1`, as a person pastes it. */
  private val PlanLink = "https://docs.google.com/document/d/plan1/edit."

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

    /** One run for `day`, made `now`, of `perRun`, by a job writing at most `most`; its attach ask answered by `attach` of what
      * it was told and shown, its write asks by `write`; its rooms labelled as `rooms` records
      * them; its asks reading no document `unread` holds to: its reply.
      */
    def run(
        perRun: Int = 10,
        most: Int = 10,
        day: Option[LocalDate] = Some(Day),
        seedUntil: Option[LocalDate] = None,
        now: Instant = Now,
        rooms: Recorded = Recorded.Empty,
        attach: (String, String) -> ujson.Value = everyNew,
        write: String -> ujson.Value = _ => wrote(Lead, Passage),
        unread: Document -> Boolean = _ => false
    ): String = {
      val dream = new TopicDream(most, ZoneOffset.UTC, Some(Cards))
      heard.run()
      val needs = Needs.over(
        Plugin,
        Vector(Knowledge -> PluginReads(NoCache, documents.shelf(Knowledge)))
      )
      val answer: (String, Vector[Message]) -> Either[MoveError, Asked[ujson.Value]] =
        (system, messages) => {
          val shown = messages.collect { case Message.User(t) => t }.mkString
          val isAttach = system.startsWith("You sort")
          heard.ask(Ask(if (isAttach) "attach" else "write", system, shown))
          val reply = if (isAttach) attach(system, shown) else write(shown)
          Right(Asked(TestTainted.at(reply, Integrity.Authority), Label.Public))
        }
      val moves = new ScriptedDreaming(
        dream.limits,
        Label.Public,
        Plugin,
        Terms,
        Dreaming.Reads(w.shelf, _ => needs, w.excerpts, reading(unread)),
        answer,
        documents = documents,
        at = now,
        rooms = rooms
      )
      dream.run(
        TestTainted.trusted(JobRun(TopicParams(perRun, 2.days, 2.days, day, seedUntil), now, now)),
        moves
      )
    }

    /** [[documents]] as a summary reads them, but for those `unread` holds to. */
    private def reading(unread: Document -> Boolean): DocumentSearch = new DocumentSearch {
      def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
        documents.declared()
      def shelved(plugins: Vector[PluginName], at: Instant)(using
          Tx^
      ): Either[StoreError, Vector[Shelved]] = documents.shelved(plugins, at)
      def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
          Tx^
      ): Either[StoreError, Vector[DocumentSearch.Hit]] =
        documents.search(shelves, query, limit, at)
      def read(versions: Vector[DocumentVersion])(using
          Tx^
      ): Either[StoreError, Vector[Document]] =
        documents.read(versions).map(_.filterNot(unread))
    }

    private given Tx = TestTx.fake

    /** The current topics: each key and text, by key. */
    def kept: Vector[(String, String)] =
      topics.map(d => (DocKey.value(d.key), DocText.value(d.text))).sortBy(_._1)

    def topics: Vector[Document] =
      ok(documents.shelf(Plugin).newest(Int.MaxValue)).filter(d => TopicKey.read(d.key).nonEmpty)

    /** The roots of the strands and the versions of the cards set aside at the public label. */
    def aside: (Vector[ConversationId], Vector[DocumentVersion]) = {
      val tags = ok(Aside.read(documents.keeper(Plugin, Terms), Label.Public)).tags
      (
        tags.collect { case Lineage.Tag.Strand(root, _, _) => root },
        tags.collect { case Lineage.Tag.Card(v) => v }
      )
    }

    /** The keys of the topics that refer to `r`. */
    def referring(r: Ref): Vector[String] =
      ok(documents.shelf(Plugin).referring(right(Ref.written(r)), 10)).map(d => DocKey.value(d.key))

    /** A card of the Drive file `id`, kept by the knowledge plugin `at`, believed at
      * `integrity`.
      */
    def card(
        id: String,
        says: String,
        at: Instant = Now,
        integrity: Integrity = Integrity.Member
    ): Unit = {
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
    }

    /** A card of the repository `o/r`'s directory `path`, kept by the knowledge plugin `at`. */
    def directory(path: String, says: String, at: Instant = Now): Unit = {
      val c = Card.directory(
        right(CorpusName.of("repo")),
        path,
        right(UnitVersion.of("2026-01-01T00:00:00Z")),
        Scope.of(Set("**")),
        right(CardSummary.of(says, Vector.empty)),
        right(
          Pointer.of(
            right(ToolName.of("github_get_file_contents")),
            "owner" -> ujson.Str("o"),
            "repo" -> ujson.Str("r"),
            "path" -> ujson.Str(path)
          )
        )
      )
      val _ = ok(
        documents
          .keeper(Knowledge, CardTerms)
          .write(
            c.docKey,
            Label.Public,
            Integrity.Member,
            Place.Everywhere,
            c.text,
            c.data,
            Some(c.pointer),
            at
          )
      )
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

  /** How many discussions, topics and cards an attach ask's message shows: each discussion a
    * block of lines before the first document, each document a block under its label.
    */
  private def counted(shown: String): (Int, Int, Int) = {
    val blocks = shown.split("\n\n").toVector
    (
      blocks.takeWhile(b => !b.startsWith("topic:\n") && !b.startsWith("knowledge card:\n")).size,
      blocks.count(_.startsWith("topic:\n")),
      blocks.count(_.startsWith("knowledge card:\n"))
    )
  }

  /** A topic group of an attach reply: `topic`, a number or `new`, and the discussions and cards
    * it holds, by number.
    */
  private def group(topic: String, discussions: Seq[Int], cards: Seq[Int] = Nil): ujson.Value =
    ujson.Obj(
      "topic" -> topic,
      "discussions" -> ujson.Arr.from(discussions.map(ujson.Num(_))),
      "cards" -> ujson.Arr.from(cards.map(ujson.Num(_)))
    )

  /** An attach reply of `groups`, with the discussions and cards it sets aside. */
  private def placed(
      groups: Seq[ujson.Value],
      asideDiscussions: Seq[Int] = Nil,
      asideCards: Seq[Int] = Nil
  ): ujson.Value =
    ujson.Obj(
      "topics" -> ujson.Arr.from(groups),
      "aside" -> ujson.Obj(
        "discussions" -> ujson.Arr.from(asideDiscussions.map(ujson.Num(_))),
        "cards" -> ujson.Arr.from(asideCards.map(ujson.Num(_)))
      )
    )

  /** Everything shown placed `place`: each in a new topic of its own for `new`, set aside for
    * `none`, else all in the topic of that number.
    */
  private def placing(place: String): (String, String) -> ujson.Value = (_, shown) => {
    val (discussions, _, cards) = counted(shown)
    val ds = 1 to discussions
    val cs = 1 to cards
    place match {
      case "new" =>
        placed(ds.map(i => group("new", Seq(i))) ++ cs.map(j => group("new", Nil, Seq(j))))
      case "none" => placed(Nil, ds, cs)
      case n => placed(Seq(group(n, ds, cs)))
    }
  }

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
          placed(Seq(group("1", Seq(1)), group("new", Seq(2))))
      )
      (
        first,
        second,
        runs.asked.map(_.kind),
        runs.kept.map(_._1),
        runs.referring(Ref.Strand(b.id)),
        runs.referring(Ref.Strand(c.id))
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        "topics: 2 written (1 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector("attach", "write", "attach", "write", "write"),
        Vector(key(a), key(c)).sorted,
        Vector(key(a)),
        Vector(key(c))
      )
    }

    test(
      "a lead or card the attach ask cannot read is not shown or counted to it; the rest are placed as numbered in the order shown, and it is told how many discussions it is shown"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val b = strand(w, "C2", "hiring plan", 100)
      val runs = new Runs(w)
      val _ = runs.run()
      val _ = runs.card("plan1", "The deploy plan.", Now.plusSeconds(60))
      val c = strand(w, "C3", "deploy rollback", 300)
      // b's topic, the newest and so the first lead, and the card are not read: a's is topic 1.
      val reply = runs.run(
        now = Now.plusSeconds(600),
        unread =
          d => DocKey.value(d.key) == key(b) || DocText.value(d.text).contains("The deploy plan."),
        attach = (_, shown) =>
          placed(
            group("1", 1 to counted(shown)._1) +:
              (1 to counted(shown)._3).map(j => group("new", Nil, Seq(j)))
          )
      )
      val attach = runs.asked.filter(_.kind == "attach").last
      (
        reply,
        counted(attach.shown),
        "\\d+ (discussion|topic|card)s?\\b".r.findAllIn(attach.system).toVector,
        runs.referring(Ref.Strand(c.id))
      ) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 2 unchanged, 0 set aside, 1 left for the next run",
        (1, 1, 0),
        Vector("1 discussion"),
        Vector(key(a))
      )
    }

    test("a topic a strand attaches to by ref is among those the attach ask shows") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      // A full page of newer topics, so a's is not shown as one of the newest.
      (1 to TopicScan.Leads).foreach(i => strand(w, s"N$i", s"matter $i", 20))
      val _ = runs.run(perRun = TopicScan.Leads, most = TopicScan.Leads, now = Now.plusSeconds(300))
      w.say(a, "the rollback works now", 50)
      val b = strand(w, "C2", "deploy rollback", 100)
      val reply = runs.run(
        now = Now.plusSeconds(600),
        attach = (_, _) => placed(Seq(group("1", Seq(1))))
      )
      val attach = runs.asked.filter(_.kind == "attach").last
      (reply, counted(attach.shown), runs.referring(Ref.Strand(b.id))) ==> (
        s"topics: 1 written (0 opened), 0 withdrawn, ${TopicScan.Leads} unchanged, 0 set aside, 0 left for the next run",
        (1, TopicScan.Leads + 1, 0),
        Vector(key(a))
      )
    }

    test(
      "a discussion and a card one new topic group holds open one topic, written from both"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val reply = runs.run(attach = (_, _) => placed(Seq(group("new", Seq(1), Seq(1)))))
      val lineage = runs.topics.flatMap(d =>
        TopicKey
          .read(d.key)
          .toVector
          .flatMap(k => TopicWrite.read(k, d.data).toOption.toVector)
          .flatMap(_.topic.passages.flatMap(_.from.tags))
      )
      (
        reply,
        runs.kept.map(_._1),
        lineage.map {
          case Lineage.Tag.Strand(root, _, _) => s"strand ${ConversationId.value(root)}"
          case Lineage.Tag.Card(_) => "card"
          case Lineage.Tag.Topic(_) => "topic"
        }
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(key(a)),
        Vector(s"strand ${a.id}", "card")
      )
    }

    test(
      "an attach reply's numbers and topics not shown are dropped and counted, the rest placed, not asked again"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      // Card 2 and discussion 2 were not shown, nor was topic 3.
      val reply = runs.run(attach =
        (_, _) => placed(Seq(group("new", Seq(1), Seq(1, 2)), group("3", Seq(2))))
      )
      (reply, runs.asked.map(_.kind), runs.kept.map(_._1)) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run; 3 named twice or not shown, dropped",
        Vector("attach", "write"),
        Vector(key(a))
      )
    }

    test(
      "a discussion or card an attach reply names in two places is dropped from both and offered again, the rest placed"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val b = strand(w, "C2", "lunch order", 10)
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val first = runs.run(attach =
        (_, _) =>
          placed(
            Seq(group("new", Seq(1, 1), Seq(1)), group("new", Seq(2))),
            asideDiscussions = Seq(1),
            asideCards = Seq(1)
          )
      )
      val _ = runs.run(now = Now.plusSeconds(600), attach = placing("none"))
      (first, runs.kept.map(_._1), runs.asked.map(x => (x.kind, counted(x.shown)))) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run; 2 unnamed, offered again; 2 named twice or not shown, dropped",
        Vector(key(b)),
        Vector(("attach", (2, 0, 1)), ("write", (1, 0, 0)), ("attach", (1, 1, 1)))
      )
    }

    test(
      "an attach reply placing nothing it was shown is asked once more, the second placed and counted asked again, within the job's limits"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val reply = runs.run(
        most = 1,
        attach = (system, _) =>
          if (system.contains("Your previous reply")) placed(Seq(group("new", Seq(1), Seq(1))))
          else placed(Nil, asideDiscussions = Seq(2), asideCards = Seq(2))
      )
      (reply, runs.asked.map(_.kind), runs.kept.map(_._1)) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run; 1 asked again",
        Vector("attach", "attach", "write"),
        Vector(key(a))
      )
    }

    test(
      "an attach reply placing nothing it was shown, twice over, leaves all it was shown"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val reply = runs.run(attach = (_, _) => placed(Seq(group("2", Seq(1)))))
      (reply, runs.asked.map(_.kind), runs.kept, runs.aside._1) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run; not written: attach (its reply placed nothing shown: each it named was named twice, or not shown, or in a topic not shown)",
        Vector("attach", "attach"),
        Vector(),
        Vector()
      )
    }

    test("an attach ask whose scan holds no topic or no card is told it is shown none") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val _ = runs.card("plan1", "The deploy plan.", Now.plusSeconds(60))
      val _ = strand(w, "C2", "deploy rollback", 100)
      val _ = runs.run(now = Now.plusSeconds(600))
      runs.asked
        .filter(_.kind == "attach")
        .map(a =>
          (a.system.contains("No topic is shown yet"), a.system.contains("No card is shown"))
        ) ==> Vector((true, true), (false, false))
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
        "topics: 1 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector("attach", "write", "write"),
        true
      )
    }

    test(
      "a changed card a topic links to attaches to it by its unit; one no topic refers to goes to the attach ask, and placed in none is counted set aside"
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
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 1 set aside, 0 left for the next run",
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
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
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
        attach = (_, _) => placed(Seq(group("1", Seq(1))))
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
        "topics: 0 written (0 opened), 1 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector(key(a) -> s"$Lead\n\n## 2026-10-13\n\n$Passage"),
        "topics: 0 written (0 opened), 1 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector()
      )
    }

    test("past perRun, the rest are left and counted") {
      val w = new StrandWorld
      val roots = (1 to 3).map(i => strand(w, s"C$i", s"subject $i", i * 100L))
      val runs = new Runs(w)
      val reply = runs.run(perRun = 2)
      (reply, runs.kept.map(_._1)) ==> (
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run",
        roots.take(2).map(key).toVector
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
        "topics: 9 written (8 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        ((key(a) -> Day) +: roots.map(r => key(r) -> day2).toVector).sortBy(_._1)
      )
    }

    test(
      "a strand or card set aside is not asked of again, until the strand has new messages"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val first = runs.run(attach = placing("none"))
      val again = runs.run(now = Now.plusSeconds(600))
      w.say(a, "the rollback works now", 700)
      val changed = runs.run(now = Now.plusSeconds(1200), attach = placing("none"))
      (first, again, changed, runs.asked.map(a => (a.kind, counted(a.shown)))) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 2 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 2 unchanged, 0 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 1 set aside, 0 left for the next run",
        Vector(("attach", (1, 0, 1)), ("attach", (1, 0, 0)))
      )
    }

    test("the aside keeps only the strands and cards still within a run's recent") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val _ = runs.run(attach = placing("none"))
      val first = runs.aside
      val b = strand(w, "C2", "lunch order", 3 * 86400)
      val later = StrandWorld.at(3 * 86400 + 3600)
      val _ = runs.run(day = Some(Day.plusDays(3)), now = later, attach = placing("none"))
      (first._1, first._2.size, runs.aside) ==> (Vector(a.id), 1, (Vector(b.id), Vector()))
    }

    test("a rerun over the same digests asks nothing and counts them unchanged") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val first = runs.run()
      val again = runs.run()
      (first, again, runs.asked.size) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
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
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(false, false),
        Vector(Vector("## 2026-10-12"))
      )
    }

    test("a run for a past day is shown no card written after that day's end") {
      // The card shown goes to a new entry of its own, so is set aside: no topic writes it.
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", StrandWorld.at(3600))
      val _ = runs.card("menu2", "The lunch menu.", StrandWorld.at(86400))
      val _ = runs.run(day = Some(Day), now = StrandWorld.at(2 * 86400))
      runs.asked
        .map(a =>
          (a.kind, a.shown.contains("The deploy plan."), a.shown.contains("The lunch menu."))
        ) ==>
        Vector(("attach", true, false), ("write", false, false))
    }

    test("a link too long to keep as a ref is counted, and stays in the topic's text") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val long = "https://example.com/" + ("a" * 10000)
      val reply = runs.run(write = _ => wrote(Lead, s"See [the log]($long)."))
      (reply, runs.kept.map(_._2.contains(long))) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run; 1 ref too long to keep",
        Vector(true)
      )
    }

    test("a directory's card is never shown to the attach ask") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      runs.directory("skills", "The skills, a folder each.", Now.minusSeconds(60))
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val _ = runs.run(attach = placing("none"))
      runs.asked.map(x =>
        (counted(x.shown), x.shown.contains("The skills"), x.shown.contains("The deploy plan."))
      ) ==> Vector(((1, 0, 1), false, true))
    }

    test("a directory's card attaches when a strand links its folder") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.directory("skills", "The skills, a folder each.", Now.minusSeconds(60))
      val a = strand(w, "C1", "the skills are in https://github.com/o/r/tree/main/skills")
      val _ = runs.run()
      (
        runs.asked.map(x => (x.kind, x.shown.contains("The skills"))),
        runs.referring(Ref.GitHub("o", "r", "skills"))
      ) ==> (Vector(("attach", false), ("write", true)), Vector(key(a)))
    }

    test("a run shows up to Cards cards; the next shows those it did not place, not those it did") {
      val w = new StrandWorld
      val runs = new Runs(w)
      (1 to TopicScan.Cards + 5).foreach(i =>
        runs.card(s"f$i", s"File number $i.", Now.minusSeconds(3000L - i))
      )
      val first = runs.run(attach = placing("none"))
      val second = runs.run(now = Now.plusSeconds(600), attach = placing("none"))
      val files = runs.asked.map(x => "File number \\d+\\.".r.findAllIn(x.shown).toSet)
      val shown = files.headOption.getOrElse(Set.empty)
      val next = files.lift(1).getOrElse(Set.empty)
      (
        first,
        second,
        runs.asked.map(x => counted(x.shown)._3),
        (next, next.intersect(shown))
      ) ==> (
        s"topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, ${TopicScan.Cards} set aside, 0 left for the next run",
        s"topics: 0 written (0 opened), 0 withdrawn, ${TopicScan.Cards} unchanged, 5 set aside, 0 left for the next run",
        Vector(TopicScan.Cards, 5),
        ((1 to 5).map(i => s"File number $i.").toSet, Set.empty[String])
      )
    }

    test("a seeding run shows Cards file cards past newer directories' cards and cards set aside") {
      val w = new StrandWorld
      val runs = new Runs(w)
      (1 to 5).foreach(i => runs.card(s"a$i", s"Aside number $i.", Now.minusSeconds(100L - i)))
      val _ = runs.run(attach = placing("none"))
      (1 to TopicScan.Cards + 5).foreach(i =>
        runs.card(s"f$i", s"File number $i.", Now.minusSeconds(3000L - i))
      )
      // Within the 100 documents the plugin's terms keep.
      (1 to TopicScan.Cards - 15).foreach(i =>
        runs.directory(s"d$i", s"Folder number $i.", Now.minusSeconds(1000L - i))
      )
      val seeding =
        runs.run(now = Now.plusSeconds(600), seedUntil = Some(Day), attach = placing("none"))
      val shown = runs.asked.lift(1).map(_.shown).getOrElse("")
      (
        seeding,
        counted(shown)._3,
        "File number (\\d+)\\.".r.findAllMatchIn(shown).map(_.group(1).toInt).toVector,
        shown.contains("Folder number") || shown.contains("Aside number")
      ) ==> (
        s"topics: 0 written (0 opened), 0 withdrawn, 5 unchanged, ${TopicScan.Cards} set aside, 0 left for the next run",
        TopicScan.Cards,
        (6 to TopicScan.Cards + 5).toVector,
        false
      )
    }

    test("a run for a day up to seedUntil reads the cards written within SeedReach") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.card("old1", "An old plan.", Now.minusSeconds(30 * 86400))
      val before = runs.run(seedUntil = Some(Day.minusDays(1)), attach = placing("none"))
      val seeding = runs.run(seedUntil = Some(Day), attach = placing("none"))
      (before, seeding, runs.asked.map(_.shown.contains("An old plan."))) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        Vector(true)
      )
    }

    test("a new entry of cards alone sets them aside and opens no topic") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val reply =
        runs.run(attach = (_, _) => placed(Seq(group("new", Seq(1)), group("new", Nil, Seq(1)))))
      (reply, runs.kept.map(_._1), runs.aside._2.size, runs.asked.map(_.kind)) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        Vector(key(a)),
        1,
        Vector("attach", "write")
      )
    }

    test(
      "a reply that misses a card places the rest, not asked again, and the card unnamed is offered again"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val first = runs.run(attach = (_, _) => placed(Seq(group("new", Seq(1)))))
      val again = runs.run(now = Now.plusSeconds(600), attach = placing("none"))
      (first, again, runs.kept.map(_._1), runs.asked.map(x => (x.kind, counted(x.shown)))) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run; 1 unnamed, offered again",
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 1 set aside, 0 left for the next run",
        Vector(key(a)),
        Vector(("attach", (1, 0, 1)), ("write", (1, 0, 0)), ("attach", (0, 1, 1)))
      )
    }

    test("a card a strand links goes to the strand's topic and is not shown to the attach ask") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val a = strand(w, "C1", s"the plan is at $PlanLink")
      val reply = runs.run()
      (
        reply,
        runs.asked.map(x => (x.kind, counted(x.shown)._3, x.shown.contains("The deploy plan."))),
        runs.kept.map(_._1)
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(("attach", 0, false), ("write", 1, true)),
        Vector(key(a))
      )
    }

    test("a card a strand links, written before cardRecent, is attached all the same") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(5 * 86400))
      val a = strand(w, "C1", s"the plan is at $PlanLink")
      val _ = runs.run()
      (runs.asked.map(_.shown.contains("The deploy plan.")), runs.referring(Ref.Drive("plan1"))) ==>
        (Vector(false, true), Vector(key(a)))
    }

    test("a card set aside is paired when a strand later links it, and leaves the aside") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val first = runs.run(attach = placing("none"))
      val setAside = runs.aside._2.size
      val b = strand(w, "C2", s"the plan is at $PlanLink", 100)
      val _ = runs.run(now = Now.plusSeconds(600))
      (
        first,
        setAside,
        runs.asked.drop(1).map(x => (x.kind, x.shown.contains("The deploy plan."))),
        runs.aside,
        runs.referring(Ref.Drive("plan1"))
      ) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        1,
        Vector(("attach", false), ("write", true)),
        (Vector(), Vector()),
        Vector(key(b))
      )
    }

    test(
      "a strand's topic refers to the units it links, so a card of one written later attaches by ref"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", s"the plan is at $PlanLink")
      val runs = new Runs(w)
      val _ = runs.run()
      val _ = runs.card("plan1", "The deploy plan.", Now.plusSeconds(60))
      val second = runs.run(now = Now.plusSeconds(600))
      (second, runs.asked.map(_.kind), runs.referring(Ref.Drive("plan1"))) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector("attach", "write", "write"),
        Vector(key(a))
      )
    }

    test("a strand set aside takes the cards it links to the aside") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val a = strand(w, "C1", s"the plan is at $PlanLink")
      val reply = runs.run(attach = placing("none"))
      (reply, runs.asked.map(x => counted(x.shown)), runs.aside._1, runs.aside._2.size) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 2 set aside, 0 left for the next run",
        Vector((1, 0, 0)),
        Vector(a.id),
        1
      )
    }

    test("a card paired by a guest's link is written no higher than the guest's strand") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val root = w.thread("C1", "1.0")
      w.say(root, s"the plan is at $PlanLink", 0, Integrity.Outside)
      w.follow(w.say(w.thread("C1", "2.0"), "thanks", 10), root)
      val _ = runs.run()
      (
        runs.topics.map(d => (DocKey.value(d.key), d.integrity)),
        runs.referring(Ref.Drive("plan1"))
      ) ==> (Vector((key(root), Integrity.Outside)), Vector(key(root)))
    }

    test("a topic over DocRef.Most refs keeps its strands and drops its oldest units first") {
      val w = new StrandWorld
      val runs = new Runs(w)
      given Tx = TestTx.fake
      val a = strand(w, "C1", "deploy pipeline")
      val units = (1 to DocRef.Most + 5).map(i => Ref.Drive(s"f$i")).toVector
      val tags = w.of(a, Now).map(s => Lineage.Tag.Strand(s.root, s.room, s.digest))
      val topic = Topic.of(
        TopicWrite.prose(Lead),
        Vector(grit.knowledge.topic.Passage(Day, Lineage.of(tags), TopicWrite.prose(Passage)))
      )
      val k = ok(
        TopicWrite.kept(
          runs.documents.keeper(Plugin, Terms),
          right(TopicKey.strand(a.id)),
          Label.Public,
          TopicWrite.Held(topic, units, None),
          Now
        )
      )
      (
        k.skipped,
        runs.referring(Ref.Strand(a.id)).size,
        runs.referring(Ref.Drive("f1")).size,
        runs.referring(Ref.Drive(s"f${DocRef.Most + 5}")).size
      ) ==> (6, 1, 0, 1)
    }

    test("a topic written from a card believed at Outside is believed no more than that") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val a = strand(w, "C1", "deploy pipeline")
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(60), Integrity.Outside)
      val reply = runs.run(attach = (_, _) => placed(Seq(group("new", Seq(1), Seq(1)))))
      (reply, runs.topics.map(d => (DocKey.value(d.key), d.integrity))) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector((key(a), Integrity.Outside))
      )
    }
  }
}
