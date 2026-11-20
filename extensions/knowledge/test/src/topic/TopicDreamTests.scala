package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.act.ScriptedMoves.Replied
import grit.core.act.{Dreaming, MoveError, ScriptedDreaming, ScriptedKeeping}
import grit.core.classify.{Answer as Judged, Question, Request}
import grit.core.document.{
  DocLabel,
  DocRef,
  DocText,
  DocWeight,
  Document,
  DocumentKeeper,
  DocumentSearch,
  DocumentTerms,
  InMemoryDocuments,
  Pointer,
  Shelved
}
import grit.core.files.InMemoryPieces
import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, CorpusName, DocKey, DocumentVersionId, PluginName, ScheduleId}
import grit.core.identity.{Account, TestAccounts}
import grit.core.job.{Grace, InMemorySchedules, JobRun, Schedule, SlotRule}
import grit.core.message.Message
import grit.core.need.{Exported, Needs, PluginDocs, PluginReads}
import grit.core.place.Place
import grit.core.stitch.{StrandDigest, StrandWorld}
import grit.core.store.{Conversation, InMemoryPrincipals, StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label, Level, Recorded, TestLabels, Visibility}
import grit.dbos.sql.TestTx
import grit.knowledge.card.{Card, CardShelf, CardSummary, Scope, UnitVersion}
import grit.knowledge.ref.{Ref, TopicKey}
import grit.prose.form.{Block, Doc, Span, Text}

import utest.*

/** What an ask of [[TopicDreamTests]]' runs was: a `sort`, what kind of item it placed
  * (`discussion` or `document`) and its item's text then each topic's, one a line; or a
  * `write`, what it was told, and the message it was shown.
  */
private final case class Ask(kind: String, system: String, shown: String)

/** The topics job's runs over rooms of threads in memory ([[StrandWorld]]) and cards kept by a
  * knowledge plugin beside it, their moves made by [[ScriptedDreaming]] over one document
  * store, so a later run reads what an earlier one kept. Unless a test says, each discussion's
  * sort chooses a topic of its own and each card's places it in none, and each write ask
  * answers [[Lead]] and [[Passage]].
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

  /** A strand of two threads in `channel`: the root saying `says`, a follower `reply`, each
    * believed at `integrity`.
    */
  private def strand(
      w: StrandWorld,
      channel: String,
      says: String,
      at: Long = 0,
      integrity: Integrity = Integrity.Member
  ): Conversation = {
    val root = w.thread(channel, "1.0")
    w.say(root, says, at, integrity)
    w.follow(w.say(w.thread(channel, "2.0"), s"reply on $says", at + 10, integrity), root)
    root
  }

  private def key(root: Conversation): String = s"topic:strand-${ConversationId.value(root.id)}"

  /** The asks a run made, and the documents it left: each run believed at `believed` under
    * `visibility`, as a schedule's run is believed at grit's own.
    */
  private final class Runs(
      w: StrandWorld,
      visibility: Visibility = Visibility.Shipped,
      believed: Integrity = Integrity.Authority
  ) {
    val documents = new InMemoryDocuments
    private val heard = new Heard
    ok(documents.declare(Vector(Plugin -> Terms, Knowledge -> CardTerms))(using TestTx.fake))

    def asked: Vector[Ask] = heard.asked

    /** The schedules its runs book. */
    private val schedules = new InMemorySchedules()

    /** The schedule its runs' booking `name` wrote, if any. */
    def booked(name: String): Option[Schedule] =
      ok(schedules.read(ScheduleId.booked(ScriptedKeeping.Turn, name))(using TestTx.fake))

    /** One run for `day`, made `now`, of `perRun`, by a job writing at most `most`, kept at
      * `floor`; its sorts judged by `sort`, its write asks answered by `write`; its rooms
      * labelled as `rooms` records them; its asks reading no document `unread` holds to; sorting
      * at most `merges` topics to merge, none unless a test of merges says, folding those at
      * `foldAt` or more, the topic `meanwhile` names rewritten with its lead as the first merge
      * sort these runs make is judged; speaking ahead of meetings in `aheadIn`: its reply.
      */
    def run(
        perRun: Int = 64,
        most: Int = 64,
        day: Option[LocalDate] = Some(Day),
        seedUntil: Option[LocalDate] = None,
        now: Instant = Now,
        rooms: Recorded = Recorded.Empty,
        sort: Judge = everyNew,
        write: String -> ujson.Value = _ => wrote(Lead, Passage),
        unread: Document -> Boolean = _ => false,
        floor: Label = Label.Public,
        asideFor: FiniteDuration = TopicParams.AsideFor,
        merges: Int = 0,
        sorts: Int = TopicParams.Sorts,
        foldAt: Option[Double] = None,
        meanwhile: Option[(String, String)] = None,
        aheadIn: Vector[Place] = Vector.empty,
        people: Int = PersonCards.PerRun,
        person: String -> ujson.Value = _ => ujson.Obj("paragraph" -> Paragraph)
    ): String = {
      val dream = new TopicDream(most, ZoneOffset.UTC, Some(Cards))
      heard.run()
      val needs = Needs.over(
        Plugin,
        Vector(Knowledge -> PluginReads(NoCache, documents.shelf(Knowledge)))
      )
      val answer: (String, Vector[Message]) -> Either[MoveError, Replied[ujson.Value]] =
        (system, messages) => {
          val shown = messages.collect { case Message.User(t) => t }.mkString
          if (system.contains(TopicDream.PersonSystem)) {
            heard.ask(Ask("person", system, shown))
            Right(Replied(person(shown), Label.Public))
          } else {
            heard.ask(Ask("write", system, shown))
            Right(Replied(write(shown), Label.Public))
          }
        }
      val judged: Request -> Either[MoveError, Replied[Vector[Judged]]] = r => {
        val ask = sorted(r.state, r.questions)
        if (
          ask.system.startsWith("document") && !heard.asked.exists(_.system.startsWith("document"))
        )
          meanwhile.foreach((name, lead) => topic(name = name, lead = lead))
        heard.ask(ask)
        Right(Replied(sort.answers(r.questions), Label.Public))
      }
      val pieces = new InMemoryPieces
      val moves = new ScriptedDreaming(
        dream.limits,
        floor,
        Plugin,
        Terms,
        Dreaming.Reads(w.shelf, _ => needs, w.excerpts, reading(unread), pieces),
        answer,
        documents = documents,
        at = now,
        integrity = believed,
        rooms = rooms,
        judgments = judged,
        visibility = visibility,
        booked = schedules
      )
      dream.run(
        TestTainted.trusted(
          JobRun(
            TopicParams(
              perRun,
              2.days,
              2.days,
              day,
              seedUntil,
              asideFor,
              TopicParams.KeepFor,
              merges,
              sorts = sorts,
              foldAt = foldAt,
              aheadIn = aheadIn,
              people = people
            ),
            now,
            now
          ),
          believed
        ),
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
      def rank(versions: Vector[DocumentVersionId], query: String, limit: Int)(using
          Tx^
      ): Either[StoreError, Vector[DocumentSearch.Hit]] =
        documents.rank(versions, query, limit).map(_.filterNot(h => unread(h.document)))
      def briefOf(holder: grit.core.id.ClaimHolder, at: Instant)(using
          Tx^
      ): Either[StoreError, Option[Document]] = documents.briefOf(holder, at)
      def read(versions: Vector[DocumentVersionId])(using
          Tx^
      ): Either[StoreError, Vector[Document]] =
        documents.read(versions).map(_.filterNot(unread))
    }

    private given Tx = TestTx.fake

    /** The current topics: each key and text, by key. */
    def kept: Vector[(String, String)] =
      topics.map(d => (DocKey.value(d.key), DocText.value(d.text))).sortBy(_._1)

    /** The card of `who` current at the public label, when there is one. */
    def personal(who: Account): Option[Document] =
      ok(
        documents
          .keeper(Plugin, Terms)
          .current(PersonKey.doc(right(PersonKey.of(InMemoryPrincipals.person(who)))), Label.Public)
      )

    def topics: Vector[Document] =
      ok(documents.shelf(Plugin).newest(Int.MaxValue)).filter(d => TopicKey.read(d.key).nonEmpty)

    /** The roots of the strands and the versions of the cards set aside at the public label. */
    def aside: (Vector[ConversationId], Vector[DocumentVersionId]) = {
      val tags = ok(Aside.read(documents.keeper(Plugin, Terms), Label.Public)).map(_.tag)
      (
        tags.collect { case Lineage.Tag.Strand(root, _, _, _) => root },
        tags.collect { case Lineage.Tag.Card(v) => v }
      )
    }

    /** What the aside at the public label is believed at, when there is one. */
    def asideBelieved: Option[Integrity] =
      ok(documents.keeper(Plugin, Terms).current(Aside.key, Label.Public)).map(_.integrity)

    /** The keys of the topics that refer to `r`. */
    def referring(r: Ref): Vector[String] =
      ok(documents.shelf(Plugin).referring(right(Ref.written(r)), 10)).map(d => DocKey.value(d.key))

    /** A topic of `lead` keyed by the strand `name`, not opened by any strand here, kept at
      * `floor` with `passages`, or when none, one passage of [[Day]] written from strands of the
      * roots `from` (none by default), so a run at `floor` sorts among it; of `kind`, believed
      * at no more than `believed`.
      */
    def topic(
        floor: Label = Label.Public,
        name: String = "held",
        lead: String = Lead,
        from: Vector[String] = Vector.empty,
        passages: Vector[grit.knowledge.topic.Passage] = Vector.empty,
        kind: Topic.Kind = Topic.Kind.Subject,
        believed: Integrity = Integrity.Authority
    ): Unit = {
      val tags = from.map(r =>
        Lineage.Tag.Strand(
          ConversationId(r),
          right(Place.read("slack:T/C1")),
          right(StrandDigest.read("0123456789abcdef"))
        )
      )
      val held = TopicWrite.Held(
        Topic.of(
          TopicWrite.prose(lead),
          if (passages.nonEmpty) passages
          else
            Vector(grit.knowledge.topic.Passage(Day, Lineage.of(tags), TopicWrite.prose(Passage))),
          kind
        ),
        Vector.empty,
        None
      )
      val _ = ok(
        TopicWrite.kept(
          DocumentKeeper.believedAtMost(documents.keeper(Plugin, Terms), believed),
          right(TopicKey.strand(ConversationId(name))),
          floor,
          held,
          Now.minusSeconds(3600)
        )
      )
    }

    /** A card of the Drive file `id`, kept by the knowledge plugin `at`, believed at
      * `integrity`, at `label`.
      */
    def card(
        id: String,
        says: String,
        at: Instant = Now,
        integrity: Integrity = Integrity.Member,
        label: Label = Label.Public
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
            label,
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

    /** The text of each topic put up for review, as [[reviewed]] offers it, by key. */
    def reviewedText: Vector[(String, String)] = {
      val room = right(Place.read("slack:T/CREVIEW"))
      given Tx =
        TestTx.fake(Clearance.of(Label.Public), recorded = labelled(Label.Public, "CREVIEW"))
      ok(documents.reviews.of(Plugin).unposted(room))
        .map(r => (DocKey.value(r.key), DocText.value(r.document.text)))
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

  /** A sort's request as [[Ask]] holds it: its item's kind and how many topics it asks about,
    * then its item's text and each topic's, a line each.
    */
  private def sorted(state: ujson.Value, questions: Vector[Question]): Ask = {
    val item = state.obj.get("item").flatMap(_.objOpt)
    def field(k: String) = item.flatMap(_.get(k)).flatMap(_.strOpt).getOrElse("")
    val topics = questions.flatMap {
      case c: Question.Choice =>
        c.keys.filter(_.name.forall(_.isDigit)).flatMap(_.description)
      case Question.YesNo(instructions, _, _) => instructions.split("\n\n", 2).toVector.drop(1)
      case _ => Vector.empty
    }
    Ask("sort", s"${field("kind")} ${topics.size}", (field("text") +: topics).mkString("\n"))
  }

  /** How many discussions, topics and cards a sort was shown: its one discussion or card, and
    * the topics it asked about.
    */
  private def counted(a: Ask): (Int, Int, Int) = a.system.split(' ').toVector match {
    case Vector("discussion", n) => (1, n.toInt, 0)
    case Vector("document", n) => (0, n.toInt, 1)
    case _ => (0, 0, 0)
  }

  /** A classifier's answers to a sort's questions: a value, not a bare function, so a run's
    * defaulted argument does not infer a capturing `Vector` (docs/capture-checking.md).
    */
  private final case class Judge(answers: Vector[Question] -> Vector[Judged])

  /** Every sort judged alike: a discussion's choice `choice` (a topic's number from 1, `new` or
    * `aside`) at `p`, the rest on `new_topic` (on `aside` when `choice` is `new_topic`); each topic a card is
    * asked about, yes at `yes`.
    */
  private def judging(
      choice: String,
      p: Double = 1.0,
      yes: Double = 0.0
  ): Judge = Judge(qs =>
    qs.map {
      case c: Question.Choice =>
        val rest = if (choice == "new_topic") "aside" else "new_topic"
        Judged.Choice(
          choice,
          c.keys.map(k =>
            Judged.Weight(k.name, if (k.name == choice) p else if (k.name == rest) 1 - p else 0.0)
          ),
          1.0
        )
      case _ => Judged.YesNo(yes)
    }
  )

  /** Each discussion in a topic of its own, each card in none. */
  private val everyNew: Judge = judging("new_topic")

  /** Each discussion and card set aside. */
  private val asideAll: Judge = judging("aside")

  /** Each of `channels` kept at `label`. */
  private def labelled(label: Label, channels: String*): Recorded =
    Recorded(
      channels
        .map(c => right(Place.read(s"slack:T/$c")) -> Recorded.Kept(None, Some(label), false, None))
        .toMap,
      Map.empty
    )

  /** The room [[Runs.topic]]'s strands are of. */
  private val AheadRoom: Place = right(Place.read("slack:T/C1"))

  /** A meeting of two planned for [[Day]] at 15:00, five hours after [[Now]]. */
  private val Ahead: Topic.Kind = Topic.Kind.Meeting(
    Meeting.of(
      Meeting.State.Planned(Meeting.When(Day, Some(java.time.LocalTime.of(15, 0)))),
      Vector("Ana", "Ben")
    )
  )

  private val Internal: Label = Label.at(Level.Internal)

  private val Paragraph = "Appears to run the releases."

  private val PersonLine = "Dana Ito: a profile inferred from their messages in the team's channels"

  /** The card a person's ask was shown: the text inside its one `<document>` fence. */
  private def cardShown(a: Ask): Option[String] =
    "(?s)<document[^>]*>\n(.*?)\n</document>".r.findFirstMatchIn(a.shown).map(_.group(1))

  private val Dana: Account = TestAccounts.account("slack:T/U-dana")
  private val Bo: Account = TestAccounts.account("slack:T/U-bo")

  /** `n` threads, one a channel from `C{from}`, each with a message `by` said `at` seconds after
    * the world's start, the first with one by [[Bo]] after it; `by` named `name` first.
    */
  private def talk(
      w: StrandWorld,
      n: Int,
      by: Account = Dana,
      name: String = "Dana Ito",
      from: Int = 1,
      at: Long = 0,
      integrity: Integrity = Integrity.Member
  ): Vector[Conversation] = {
    ok(w.principals.name(by, name)(using TestTx.fake))
    ok(w.principals.name(Bo, "Bo Lund")(using TestTx.fake))
    (from until from + n).toVector.map { i =>
      val c = w.thread(s"C$i", "1.0")
      w.say(c, s"the release train, part $i", at, integrity, by = Some(by))
      if (i == from) w.say(c, s"noted, part $i", at + 10, integrity, by = Some(Bo))
      c
    }
  }

  val tests = Tests {
    test("a chosen topic is joined at any probability") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val first = runs.run()
      val b = strand(w, "C2", "deploy rollback", 100)
      val second = runs.run(now = Now.plusSeconds(600), sort = judging("1", 0.55))
      (
        first,
        second,
        runs.asked.map(x => (x.kind, counted(x))),
        runs.kept.map(_._1),
        runs.referring(Ref.Strand(b.id))
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector(
          ("sort", (1, 0, 0)),
          ("write", (0, 0, 0)),
          ("sort", (1, 1, 0)),
          ("write", (0, 0, 0))
        ),
        Vector(key(a)),
        Vector(key(a))
      )
    }

    test("a discussion chosen new opens a topic of its own") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val b = strand(w, "C2", "lunch order", 100)
      val second = runs.run(now = Now.plusSeconds(600), sort = judging("new_topic", 0.9))
      (second, runs.kept.map(_._1), runs.referring(Ref.Strand(b.id))) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector(key(a), key(b)).sorted,
        Vector(key(b))
      )
    }

    test("two new discussions open two topics") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val b = strand(w, "C2", "deploy rollback", 100)
      val runs = new Runs(w)
      val reply = runs.run()
      (reply, runs.kept.map(_._1), runs.asked.map(_.kind)) ==> (
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(key(a), key(b)).sorted,
        Vector("sort", "write", "sort", "write")
      )
    }

    test("a strand's sort is shown a topic of its room, though newer topics fill the newest") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val newer = TopicDream.Newest + 2
      (1 to newer).foreach(i => strand(w, s"N$i", s"matter $i", 20))
      val _ = runs.run(perRun = 2 * newer, most = 2 * newer, now = Now.plusSeconds(300))
      val b = w.thread("C1", "3.0")
      val _ = w.say(b, "lunch " * 40, 100)
      w.follow(w.say(w.thread("C1", "4.0"), "lunch " * 40, 110), b)
      val _ = runs.run(now = Now.plusSeconds(600), sort = asideAll)
      val shown = runs.asked.filter(_.kind == "sort").last
      (counted(shown), shown.shown.split("\n").count(_ == Lead)) ==>
        ((1, 1 + TopicDream.Newest, 0), 1 + TopicDream.Newest)
    }

    test(
      "a strand's sort is filled from every topic by its text, past the newest, to Fill in all"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      val topics = TopicDream.Fill + 4
      (1 to topics).foreach(i => strand(w, s"N$i", s"matter $i", 20))
      val _ = runs.run(perRun = 2 * topics, most = 2 * topics)
      val _ = strand(w, "C0", "the deploy pipeline breaks " * 8, 100)
      val _ = runs.run(now = Now.plusSeconds(600), sort = asideAll)
      counted(runs.asked.filter(_.kind == "sort").last) ==> (1, TopicDream.Fill, 0)
    }

    test(
      "a short strand's sort is shown no topic for being newest, and is filled by text to ShortFill"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      val topics = TopicDream.ShortFill + 4
      (1 to topics).foreach(i => strand(w, s"N$i", s"matter $i", 20))
      val _ = runs.run(perRun = 2 * topics, most = 2 * topics)
      val _ = strand(w, "C0", "lunch", 100)
      val _ = strand(w, "C9", "the deploy", 110)
      val _ = runs.run(now = Now.plusSeconds(600), sort = asideAll)
      runs.asked.filter(_.kind == "sort").takeRight(2).map(counted) ==>
        Vector((1, 0, 0), (1, TopicDream.ShortFill, 0))
    }

    test("a discussion chosen aside is set aside, and opens no topic") {
      val w = new StrandWorld
      val a = strand(w, "C1", "hello all")
      val runs = new Runs(w)
      val reply = runs.run(sort = asideAll)
      (reply, runs.kept, runs.aside._1) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        Vector(),
        Vector(a.id)
      )
    }

    test(
      "a topic a sort cannot read is not asked about, the rest numbered in the order shown; a card it cannot read is left"
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
        sort = judging("1", yes = 1.0)
      )
      (reply, runs.asked.drop(4).map(counted), runs.referring(Ref.Strand(c.id))) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 2 unchanged, 0 set aside, 1 left for the next run",
        Vector((1, 1, 0), (0, 0, 0)),
        Vector(key(a))
      )
    }

    test("a card goes to the topic its sort says yes to at 0.72 or more") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic()
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val reply = runs.run(sort = judging("new_topic", yes = TopicDream.CardAt))
      (
        reply,
        runs.asked.map(x => (x.kind, counted(x))),
        runs.asked.filter(_.kind == "write").map(_.shown.contains("The deploy plan.")),
        runs.aside._2
      ) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(("sort", (0, 1, 1)), ("write", (0, 0, 0))),
        Vector(true),
        Vector()
      )
    }

    test("a card no topic takes is set aside") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic()
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val reply = runs.run(sort = judging("new_topic", yes = 0.71))
      (reply, runs.asked.map(_.kind), runs.aside._2.size) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        Vector("sort"),
        1
      )
    }

    test("with no topic kept, cards are left and nothing is asked") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val reply = runs.run(sort = judging("new_topic", yes = 1.0))
      (reply, runs.asked, runs.aside._2) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run",
        Vector(),
        Vector()
      )
    }

    test("a topic a strand attaches to by ref is among those its sort asks about") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      // More newer topics than the text fills, so a's is shown first for its ref alone.
      val newer = TopicDream.ShortFill + 4
      (1 to newer).foreach(i => strand(w, s"N$i", s"matter $i", 20))
      val _ = runs.run(perRun = 2 * newer, most = 2 * newer, now = Now.plusSeconds(300))
      w.say(a, "the rollback works now", 50)
      val b = strand(w, "C2", "deploy rollback", 100)
      val reply = runs.run(
        now = Now.plusSeconds(600),
        sort = judging("1", yes = 1.0)
      )
      val sorted = runs.asked.filter(_.kind == "sort").last
      (reply, counted(sorted), runs.referring(Ref.Strand(b.id))) ==> (
        s"topics: 1 written (0 opened), 0 withdrawn, $newer unchanged, 0 set aside, 0 left for the next run",
        (1, 1 + TopicDream.ShortFill, 0),
        Vector(key(a))
      )
    }

    test("a strand a topic already refers to is attached to it without a sort") {
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
        Vector("sort", "write", "write"),
        true
      )
    }

    test(
      "a write ask is told to date days absolutely, to write the matter not the assistant's conduct, to name no day atop the passage, and to doubt nothing a card states"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val system = runs.asked.find(_.kind == "write").fold("")(_.system)
      Vector(
        "Write about the matter, not the exchange: never about the assistant's own conduct, " +
          "such as a message it sent twice, its corrections of its own earlier answers, or a " +
          "test of it.",
        "Write each day as its date (yyyy-mm-dd), never as \"today\", \"tomorrow\", " +
          "\"yesterday\" or a weekday's name: a message's relative day is reckoned from the day " +
          "it was said, 2026-10-12 for that day's messages.",
        "The passage is shown under its day, so it begins with what it adds, with no line " +
          "naming its day.",
        "Never call something unverified, unknown or unconfirmed that a card shown states."
      ).filterNot(system.contains) ==> Vector()
    }

    test("a write ask names the fences it is shown in, and ends with the rule") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val systems = runs.asked.filter(_.kind == "write").map(_.system)
      systems.map(s =>
        (
          Vector("`<discussion>`", "`<document label=\"topic\">`", "`<document>`").filter(
            s.contains
          ),
          s.endsWith("\n\n" + grit.core.prompt.Fenced.Rule)
        )
      ) ==> Vector(
        // An open is shown no topic, so it names no topic's fence.
        (Vector("`<discussion>`", "`<document>`"), true)
      )
    }

    test("a write ask shows each discussion's messages under the day they were said") {
      val w = new StrandWorld
      val root = w.thread("C1", "1.0")
      w.say(root, "can we meet Monday?", 0)
      w.follow(w.say(w.thread("C1", "2.0"), "Monday works", 86_400), root)
      val runs = new Runs(w)
      val _ = runs.run(day = Some(Day.plusDays(1)), now = StrandWorld.at(86_400 + 3600))
      runs.asked
        .filter(_.kind == "write")
        .map(
          _.shown.linesIterator.filter(_.matches("[A-Z][a-z]+day \\d{4}-\\d\\d-\\d\\d:")).toVector
        ) ==>
        Vector(Vector("Monday 2026-10-12:", "Tuesday 2026-10-13:"))
    }

    test("a topic's write ask is shown every card it holds, not only those attached that day") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val _ = runs.card("runbook", "The rollback runbook.", Now.plusSeconds(60))
      val _ = runs.run(now = Now.plusSeconds(300), sort = judging("new_topic", yes = 1.0))
      w.say(a, "the rollback works now", 50)
      val _ = runs.run(now = Now.plusSeconds(600))
      runs.asked.map(x => (x.kind, x.shown.contains("The rollback runbook."))) ==>
        Vector(("sort", false), ("write", false), ("sort", true), ("write", true), ("write", true))
    }

    test(
      "a changed card a topic links to attaches to it by its unit; one no topic refers to is sorted, and placed in none is counted set aside"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run(write =
        _ => wrote(Lead, "See [the plan](https://docs.google.com/document/d/plan1/edit).")
      )
      runs.card("plan1", "The deploy plan.", Now.plusSeconds(60))
      runs.card("menu2", "The lunch menu.", Now.plusSeconds(60))
      val second = runs.run(now = Now.plusSeconds(600), sort = asideAll)
      val asks = runs.asked.drop(2)
      (
        second,
        asks.map(_.kind),
        asks.map(a => (a.shown.contains("The deploy plan."), a.shown.contains("The lunch menu.")))
      ) ==> (
        // The strand, in the topic's lineage at its digest, and the card set aside.
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 1 set aside, 0 left for the next run",
        Vector("sort", "write"),
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
        Vector("sort", "write", "sort", "write"),
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
        sort = judging("1", yes = 1.0)
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

    test("a passage whose strand's messages are gone is kept while within keepFor") {
      val w = new StrandWorld
      val root = w.thread("C1", "1.0")
      val first = w.say(root, "deploy pipeline", 0)
      val reply = w.say(w.thread("C1", "2.0"), "reply on deploy pipeline", 10)
      w.follow(reply, root)
      val runs = new Runs(w)
      val _ = runs.run()
      val opened = runs.kept
      w.purge(first, 86400)
      w.purge(reply, 86400)
      val later = StrandWorld.at(86400 + 3600)
      val read = w.of(root, later)(using TestTx.fake)
      val purged = runs.run(day = Some(Day.plusDays(1)), now = later)
      (read, purged, runs.kept) ==> (
        Vector(),
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        opened
      )
    }

    test(
      "a passage dated more than keepFor before the run's day is withdrawn, and a topic left with none goes"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      val _ = strand(w, "C2", "deploy rollback", 86400)
      val _ = runs.run(
        day = Some(Day.plusDays(1)),
        now = StrandWorld.at(86400 + 3600),
        sort = judging("1")
      )
      // A run whose day is `days` after Day, an hour into it.
      def after(days: Int) = runs.run(
        day = Some(Day.plusDays(days.toLong)),
        now = StrandWorld.at(86400L * days + 3600)
      )
      val firstOld = after(91)
      val left = runs.kept
      val bothOld = after(92)
      (firstOld, left, bothOld, runs.kept) ==> (
        "topics: 0 written (0 opened), 1 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(key(a) -> s"$Lead\n\n## 2026-10-13\n\n$Passage"),
        "topics: 0 written (0 opened), 1 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector()
      )
    }

    test(
      "a topic whose data no longer reads, an earlier version's, is withdrawn and opened afresh"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      // The topic as an earlier version of the plugin kept it: data this reader refuses.
      runs.topics.foreach { d =>
        val old = ujson.copy(d.data)
        old("v") = 1
        val _ = ok(
          runs.documents
            .keeper(Plugin, Terms)
            .write(d.key, d.label, d.integrity, d.place, d.text, old, d.pointer, Now)(using
              TestTx.fake
            )
        )
      }
      val reply = runs.run(now = Now.plusSeconds(60))
      (reply, runs.kept) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run, 1 unreadable withdrawn",
        Vector(key(a) -> s"$Lead\n\n## 2026-10-12\n\n$Passage")
      )
    }

    test(
      "a topic of a later version than this reader's is left as it is and counted, and the strands it refers to are asked of no more"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run()
      // The topic as a later version of the plugin keeps it, referring to its strand as this
      // version does: data this reader must not wipe.
      runs.topics.foreach { d =>
        val later = ujson.copy(d.data)
        later("v") = 4
        val _ = ok(
          runs.documents
            .keeper(Plugin, Terms)
            .write(
              d.key,
              d.label,
              d.integrity,
              d.place,
              d.text,
              later,
              d.pointer,
              Now,
              Set(right(Ref.written(Ref.Strand(a.id))))
            )(using TestTx.fake)
        )
      }
      val reply = runs.run(now = Now.plusSeconds(60))
      (reply, runs.asked.map(_.kind), runs.topics.map(_.data("v").num)) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run, 1 of a later version left",
        Vector("sort", "write"),
        Vector(4.0)
      )
    }

    test("a run in a lineage is not offered again") {
      val w = new StrandWorld
      val room = w.room("C1")
      w.say(room, "standup notes are in the doc", 0)
      w.say(room, "thanks", 60)
      val runs = new Runs(w)
      val first = runs.run()
      val again = runs.run(now = Now.plusSeconds(600))
      (first, again, runs.asked.map(_.kind), runs.kept.map(_._1)) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector("sort", "write"),
        Vector(s"topic:run-${ConversationId.value(room.id)}:0")
      )
    }

    test("a run attaches to no topic by ref") {
      val w = new StrandWorld
      val room = w.room("C1")
      w.say(room, "standup notes are in the doc", 0)
      val runs = new Runs(w)
      val _ = runs.run()
      (runs.kept.size, runs.referring(Ref.Strand(room.id))) ==> (1, Vector.empty)
    }

    test("a grown thread attaches to its topic") {
      val w = new StrandWorld
      val room = w.room("C1")
      w.say(room, "who reviews the deploy script?", 0, source = Some("1.0"))
      val thread = w.under("C1", "1.0")
      w.say(thread, "I can", 30)
      val runs = new Runs(w)
      val _ = runs.run()
      w.say(thread, "done, it is merged", 900)
      val again = runs.run(now = Now.plusSeconds(600))
      // The first run sorted the top-level run and the thread, and wrote each.
      (again, runs.asked.map(_.kind).drop(4)) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector("write")
      )
    }

    test(
      "a dated meeting from Grace days before its strand's day to 14 after is pinned; an undated one never is"
    ) {
      def live(name: String, day: Option[String]) = TopicScan.Live(
        right(TopicKey.strand(ConversationId(name))),
        DocumentVersionId.of(1).getOrElse(throw new java.lang.AssertionError("version")),
        day.map(d => Meeting.When(LocalDate.parse(d), None))
      )
      val scan = TopicScan.Scanned(
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        0,
        0,
        0,
        Vector.empty,
        0,
        0,
        Vector.empty,
        Vector(
          live("back2", Some("2026-11-17")),
          live("back3", Some("2026-11-16")),
          live("ahead14", Some("2026-12-03")),
          live("ahead15", Some("2026-12-04")),
          live("undated", None),
          live("bare", None)
        )
      )
      scan.meetingsNear(LocalDate.parse("2026-11-19")).map(TopicKey.value) ==>
        Vector("back2", "ahead14").map(n => s"topic:strand-$n")
    }

    test(
      "a meeting is offered to a strand said Grace days after its day, and a held reply holds it; one said later is not offered it"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "the review, tuesday at nine")
      val runs = new Runs(w)
      def meeting(state: String) = ujson.Obj(
        "lead" -> Lead,
        "passage" -> Passage,
        "meeting" -> ujson.Obj(
          "state" -> state,
          "day" -> "2026-10-13",
          "start" -> "09:00",
          "who" -> ujson.Arr("Ana")
        )
      )
      val _ = runs.run(sort = judging("new_meeting"), write = _ => meeting("planned"))
      // Said 2026-10-15, two days after the meeting.
      val _ = strand(w, "C2", "notes from the review", 3 * 86_400)
      val _ = runs.run(
        sort = judging("1"),
        write = _ => meeting("held"),
        day = Some(Day.plusDays(3)),
        now = StrandWorld.at(3 * 86_400 + 3600)
      )
      val held = runs.kept.map((_, text) => text.linesIterator.next())
      // Said 2026-10-16, three days after it.
      val _ = strand(w, "C3", "more notes from the review", 4 * 86_400)
      val _ = runs.run(
        sort = asideAll,
        day = Some(Day.plusDays(4)),
        now = StrandWorld.at(4 * 86_400 + 3600)
      )
      (
        held,
        runs.asked
          .filter(a => a.kind == "sort" && a.shown.contains("notes from the review"))
          .map(a => (a.shown.linesIterator.next(), a.shown.contains("Meeting, ")))
      ) ==> (
        Vector("Meeting, held Tuesday 2026-10-13, with Ana."),
        Vector(
          ("Someone: notes from the review", true),
          ("Someone: more notes from the review", false)
        )
      )
    }

    test("at most sorts units") {
      val w = new StrandWorld
      (1 to 3).foreach(i => strand(w, s"C$i", s"subject $i", i * 100L))
      val runs = new Runs(w)
      val reply = runs.run(sorts = 2)
      (reply, runs.asked.count(_.kind == "sort")) ==> (
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run",
        2
      )
    }

    test("opens take at most half of perRun; the left are counted") {
      val w = new StrandWorld
      val roots = (1 to 3).map(i => strand(w, s"C$i", s"subject $i", i * 100L))
      val runs = new Runs(w)
      val reply = runs.run(perRun = 4)
      // Two opens; the third is left, not set aside, and offered again the next night.
      val again = runs.run(perRun = 4, now = Now.plusSeconds(600))
      (reply, again, runs.kept.map(_._1)) ==> (
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run; 1 new topics left for the next run",
        "topics: 1 written (1 opened), 0 withdrawn, 2 unchanged, 0 set aside, 0 left for the next run",
        roots.map(key).toVector
      )
    }

    test("a job writing at most 0 opens nothing; the new strand is left") {
      val w = new StrandWorld
      strand(w, "C1", "subject 1")
      val runs = new Runs(w)
      (runs.run(most = 0), runs.kept) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run; 1 new topics left for the next run",
        Vector()
      )
    }

    test(
      "a meeting's write is told to name a day by said and as, and its head is the day they reckon"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "the review, tomorrow at nine?")
      val runs = new Runs(w)
      val named = ujson.Obj(
        "lead" -> Lead,
        "passage" -> Passage,
        "meeting" -> ujson.Obj(
          "state" -> "planned",
          "day" -> ujson.Null,
          "said" -> "2026-10-12",
          "as" -> "tomorrow",
          "start" -> "09:00",
          "who" -> ujson.Arr("Ana")
        )
      )
      val _ = runs.run(sort = judging("new_meeting"), write = _ => named)
      (
        runs.kept.map((_, text) => text.linesIterator.next()),
        runs.asked
          .filter(_.kind == "write")
          .map(
            _.system.contains("`said`: the date on the day line above the message that names it")
          )
      ) ==> (Vector("Meeting, planned for Tuesday 2026-10-13 at 09:00, with Ana."), Vector(true))
    }

    test("new_meeting opens a proposed meeting") {
      val w = new StrandWorld
      val a = strand(w, "C1", "a call on the budget, monday or tuesday?")
      val b = strand(w, "C2", "the review, tuesday at nine", 100)
      val runs = new Runs(w)
      val planned = ujson.Obj(
        "lead" -> Lead,
        "passage" -> Passage,
        "meeting" -> ujson.Obj(
          "state" -> "planned",
          "day" -> "2026-10-13",
          "start" -> "09:00",
          "who" -> ujson.Arr("Ana", "Ben")
        )
      )
      val reply = runs.run(
        sort = judging("new_meeting"),
        write = shown => if (shown.contains("review")) planned else wrote(Lead, Passage)
      )
      (
        reply,
        runs.kept.map((k, text) => (k, text.linesIterator.next())),
        runs.asked.filter(_.kind == "write").map(_.system.contains("This topic is one meeting."))
      ) ==> (
        "topics: 2 written (2 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(
          (key(a), "Meeting: proposed, day not yet known."),
          (key(b), "Meeting, planned for Tuesday 2026-10-13 at 09:00, with Ana and Ben.")
        ),
        Vector(true, true)
      )
    }

    test(
      "an open's write ask is told the topic is new; a later write is told it is shown its text"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "the deploy pipeline is flaky")
      val runs = new Runs(w)
      val _ = runs.run(sort = everyNew)
      w.say(a, "fixed by pinning the runner", 900)
      val _ = runs.run(now = Now.plusSeconds(600))
      runs.asked
        .filter(_.kind == "write")
        .map(x =>
          (
            x.system.contains("This topic is new"),
            x.system.contains("then the topic's text"),
            x.system.contains("when the topic exists")
          )
        ) ==> Vector((true, false, false), (false, true, false))
    }

    test("a write reply with call markup it cannot read keeps nothing and is named not written") {
      val w = new StrandWorld
      val a = strand(w, "C1", "the deploy pipeline is flaky")
      val runs = new Runs(w)
      val reply =
        runs.run(sort = everyNew, write = _ => wrote("A lead.<parameter name=\"x\">", Passage))
      (reply.contains(s"not written: ${key(a)}"), runs.kept) ==> (true, Vector())
    }

    test("a meeting's second write keeps its head line once, though its lead repeats it") {
      val w = new StrandWorld
      val a = strand(w, "C1", "a call on the budget, monday or tuesday?")
      val runs = new Runs(w)
      val _ = runs.run(sort = judging("new_meeting"))
      w.say(a, "monday suits me", 900)
      val head = "Meeting: proposed, day not yet known."
      // As a write ask shown the topic's text answers: its lead opens with the head it saw.
      val _ = runs.run(now = Now.plusSeconds(600), write = _ => wrote(s"$head\n\n$Lead", Passage))
      runs.kept.filter(_._1 == key(a)).lastOption.map(_._2.linesIterator.count(_ == head)) ==>
        Some(1)
    }

    test("a topic opened tonight is a candidate for the next unit") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val _ = strand(w, "C2", "deploy rollback", 100)
      val runs = new Runs(w)
      // The first opens its own; the second is shown it, and chooses it.
      val first = Judge(qs =>
        qs.map {
          case c: Question.Choice if c.keys.exists(_.name == "1") =>
            Judged.Choice(
              "1",
              c.keys.map(k => Judged.Weight(k.name, if (k.name == "1") 1.0 else 0.0)),
              1.0
            )
          case c: Question.Choice =>
            Judged.Choice(
              "new_topic",
              c.keys.map(k => Judged.Weight(k.name, if (k.name == "new_topic") 1.0 else 0.0)),
              1.0
            )
          case _ => Judged.YesNo(0.0)
        }
      )
      val reply = runs.run(sort = first)
      (
        reply,
        runs.asked.map(x => (x.kind, counted(x)._2)),
        runs.kept.map(_._1)
      ) ==> (
        "topics: 2 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(("sort", 0), ("write", 0), ("sort", 1), ("write", 0)),
        Vector(key(a))
      )
    }

    test("a subject ignores a meeting object") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.run(write =
        _ =>
          ujson.Obj(
            "lead" -> Lead,
            "passage" -> Passage,
            "meeting" -> ujson.Obj("state" -> "held", "day" -> "2026-10-12")
          )
      )
      (
        runs.kept.map((k, text) => (k, text.linesIterator.next())),
        runs.asked.filter(_.kind == "write").map(_.system.contains("Give `meeting` null."))
      ) ==> (Vector((key(a), Lead)), Vector(true))
    }

    test("joins take the writes opens leave") {
      val w = new StrandWorld
      val roots = (1 to 3).map(i => strand(w, s"C$i", s"subject $i", i * 100L))
      val runs = new Runs(w)
      val _ = runs.run(perRun = 6)
      (1 to 3).foreach(i =>
        w.follow(w.say(w.thread(s"C$i", "8.0"), s"and $i", 950L + i), roots(i - 1))
      )
      // Each grown strand attaches by ref: none opens, so both writes go to joins.
      val again = runs.run(perRun = 2, now = Now.plusSeconds(1200))
      again ==>
        "topics: 2 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run"
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
      runs.topic()
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val first = runs.run(sort = asideAll)
      val again = runs.run(now = Now.plusSeconds(600))
      w.say(a, "the rollback works now", 700)
      val changed = runs.run(now = Now.plusSeconds(1200), sort = asideAll)
      (first, again, changed, runs.asked.map(a => (a.kind, counted(a)))) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 2 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 2 unchanged, 0 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 1 set aside, 0 left for the next run",
        Vector(("sort", (1, 1, 0)), ("sort", (0, 1, 1)), ("sort", (1, 1, 0)))
      )
    }

    test("the aside keeps only the strands and cards still within a run's recent") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      runs.topic()
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val _ = runs.run(sort = asideAll)
      val first = runs.aside
      val b = strand(w, "C2", "lunch order", 3 * 86400)
      val later = StrandWorld.at(3 * 86400 + 3600)
      val _ = runs.run(day = Some(Day.plusDays(3)), now = later, sort = asideAll)
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
      // The card sorted is set aside: no topic writes it.
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      runs.topic()
      val _ = runs.card("plan1", "The deploy plan.", StrandWorld.at(3600))
      val _ = runs.card("menu2", "The lunch menu.", StrandWorld.at(86400))
      val _ = runs.run(day = Some(Day), now = StrandWorld.at(2 * 86400))
      runs.asked
        .map(a =>
          (a.kind, a.shown.contains("The deploy plan."), a.shown.contains("The lunch menu."))
        ) ==>
        Vector(("sort", false, false), ("write", false, false), ("sort", true, false))
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

    test("a directory's card is never sorted") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      runs.topic()
      runs.directory("skills", "The skills, a folder each.", Now.minusSeconds(60))
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val _ = runs.run(sort = asideAll)
      runs.asked.map(x =>
        (counted(x), x.shown.contains("The skills"), x.shown.contains("The deploy plan."))
      ) ==> Vector(((1, 1, 0), false, false), ((0, 1, 1), false, true))
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
      ) ==> (Vector(("sort", false), ("write", true)), Vector(key(a)))
    }

    test(
      "a run is offered only the cards kept at its own label; a lower one reaches its topic by a strand's link"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(Internal)
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      runs.card("menu2", "The lunch menu.", Now.minusSeconds(60))
      runs.card("budget3", "The budget.", Now.minusSeconds(60), label = Internal)
      val _ = strand(w, "C1", s"the plan is at $PlanLink")
      val _ = runs.run(floor = Internal, rooms = labelled(Internal, "C1"))
      val said = Seq("The deploy plan.", "The lunch menu.", "The budget.")
      runs.asked.map(x => (x.kind, said.filter(x.shown.contains))) ==>
        Vector(("sort", Seq()), ("write", Seq("The deploy plan.")), ("sort", Seq("The budget.")))
    }

    test("a card kept below a run's label attaches by ref to a topic that refers to it") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = strand(w, "C1", s"the plan is at $PlanLink")
      val _ = runs.run(floor = Internal, rooms = labelled(Internal, "C1"))
      runs.card("plan1", "The deploy plan.", Now.plusSeconds(60))
      val second =
        runs.run(now = Now.plusSeconds(600), floor = Internal, rooms = labelled(Internal, "C1"))
      // The topic is written again only when the card attached to it.
      (second, runs.asked.map(x => (x.kind, x.shown.contains("The deploy plan.")))) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector(("sort", false), ("write", false), ("write", true))
      )
    }

    test(
      "a run with no discussion and no topic to show asks nothing, and leaves its cards for a later run"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val first = runs.run(sort = asideAll)
      runs.topic()
      val second = runs.run(now = Now.plusSeconds(600), sort = asideAll)
      (first, second, runs.asked.map(x => (x.kind, counted(x)))) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 1 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        Vector(("sort", (0, 1, 1)))
      )
    }

    test("a run shows up to Cards cards; the next shows those it did not place, not those it did") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic()
      (1 to TopicScan.Cards + 5).foreach(i =>
        runs.card(s"f$i", s"File number $i.", Now.minusSeconds(3000L - i))
      )
      val first = runs.run(sort = asideAll)
      val second = runs.run(now = Now.plusSeconds(600), sort = asideAll)
      // Each sort is one card's: the first run's are its first Cards.
      val files = runs.asked.map(x => "File number \\d+\\.".r.findAllIn(x.shown).toSet)
      val (shown, next) = files.splitAt(TopicScan.Cards) match {
        case (a, b) => (a.flatten.toSet, b.flatten.toSet)
      }
      (first, second, files.size, (next, next.intersect(shown))) ==> (
        s"topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, ${TopicScan.Cards} set aside, 0 left for the next run",
        s"topics: 0 written (0 opened), 0 withdrawn, ${TopicScan.Cards} unchanged, 5 set aside, 0 left for the next run",
        TopicScan.Cards + 5,
        ((1 to 5).map(i => s"File number $i.").toSet, Set.empty[String])
      )
    }

    test("a run shows Cards file cards past newer directories' cards and cards set aside") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic()
      (1 to 5).foreach(i => runs.card(s"a$i", s"Aside number $i.", Now.minusSeconds(100L - i)))
      val _ = runs.run(sort = asideAll)
      (1 to TopicScan.Cards + 5).foreach(i =>
        runs.card(s"f$i", s"File number $i.", Now.minusSeconds(3000L - i))
      )
      // Within the 100 documents the plugin's terms keep.
      (1 to TopicScan.Cards - 15).foreach(i =>
        runs.directory(s"d$i", s"Folder number $i.", Now.minusSeconds(1000L - i))
      )
      val ran = runs.run(now = Now.plusSeconds(600), sort = asideAll)
      // The first run sorted the five cards it set aside; each sort of the second is one card's.
      val shown = runs.asked.drop(5).map(_.shown)
      (
        ran,
        shown.size,
        shown.flatMap("File number (\\d+)\\.".r.findAllMatchIn(_).map(_.group(1).toInt)).sorted,
        shown.exists(s => s.contains("Folder number") || s.contains("Aside number"))
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
      runs.topic()
      runs.card("old1", "An old plan.", Now.minusSeconds(30 * 86400))
      val before = runs.run(seedUntil = Some(Day.minusDays(1)), sort = asideAll)
      val seeding = runs.run(seedUntil = Some(Day), sort = asideAll)
      (before, seeding, runs.asked.map(_.shown.contains("An old plan."))) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        Vector(true)
      )
    }

    test(
      "a card opens no topic, though a new discussion beside it does, and is sorted against it"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val reply =
        runs.run(sort = everyNew)
      (reply, runs.kept.map(_._1), runs.aside._2.size, runs.asked.map(_.kind)) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
        Vector(key(a)),
        1,
        Vector("sort", "write", "sort")
      )
    }

    test("an aside entry kept before entries were dated is dated when its aside was last written") {
      val runs = new Runs(new StrandWorld)
      given Tx = TestTx.fake
      val keeper = runs.documents.keeper(Plugin, Terms)
      val written = Now.minusSeconds(7200)
      val card = DocumentVersionId.of(7L).getOrElse(throw new java.lang.AssertionError("7"))
      val _ = ok(
        keeper.write(
          Aside.key,
          Label.Public,
          Integrity.Authority,
          Place.Everywhere,
          right(DocText.of("…")),
          ujson.Obj("v" -> 1, "aside" -> ujson.Arr(ujson.Obj("card" -> 7))),
          None,
          written
        )
      )
      ok(Aside.read(keeper, Label.Public)) ==> Vector(Aside.Item(Lineage.Tag.Card(card), written))
    }

    test(
      "a run with nothing to offer drains from its aside a card kept below its label, unchanged"
    ) {
      val runs = new Runs(new StrandWorld)
      given Tx = TestTx.fake(Clearance.of(Internal))
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(7200))
      val card = ok(runs.documents.shelf(Knowledge).newest(1)).map(_.version)
      val keeper = runs.documents.keeper(Plugin, Terms)
      val aside = card.map(v => Aside.Item(Lineage.Tag.Card(v), Now.minusSeconds(3600)))
      val _ = ok(Aside.kept(keeper, Internal, aside, Now.minusSeconds(3600)))
      val line = runs.run(floor = Internal)
      (card.size, line, runs.asked.size, ok(keeper.current(Aside.key, Internal))) ==> (
        1,
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        0,
        None
      )
    }

    test("a card set aside more than asideFor ago is offered again") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic()
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val runs3 = Vector(0L, 1800L, 4000L).map(after =>
        runs.run(now = Now.plusSeconds(after), sort = asideAll, asideFor = 1.hour)
      )
      (runs3, runs.asked.map(x => counted(x)._3)) ==> (
        Vector(
          "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run",
          "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
          "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 1 set aside, 0 left for the next run"
        ),
        Vector(1, 1)
      )
    }

    test("a card a strand links goes to the strand's topic and is not sorted") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val a = strand(w, "C1", s"the plan is at $PlanLink")
      val reply = runs.run()
      (
        reply,
        runs.asked.map(x => (x.kind, counted(x)._3, x.shown.contains("The deploy plan."))),
        runs.kept.map(_._1)
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector(("sort", 0, false), ("write", 0, true)),
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
      runs.topic()
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val first = runs.run(sort = asideAll)
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
        Vector(("sort", false), ("write", true)),
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
        Vector("sort", "write", "write"),
        Vector(key(a))
      )
    }

    test("a strand set aside takes the cards it links to the aside") {
      val w = new StrandWorld
      val runs = new Runs(w)
      val _ = runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val a = strand(w, "C1", s"the plan is at $PlanLink")
      val reply = runs.run(sort = asideAll)
      (reply, runs.asked.map(x => counted(x)), runs.aside._1, runs.aside._2.size) ==> (
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
      val _ = runs.run()
      runs.card("plan1", "The deploy plan.", Now.plusSeconds(60), Integrity.Outside)
      val reply = runs.run(now = Now.plusSeconds(600), sort = judging("new_topic", yes = 1.0))
      (reply, runs.topics.map(d => (DocKey.value(d.key), d.integrity))) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector((key(a), Integrity.Outside))
      )
    }

    test("a topic written from a strand and a card believed in a compartment is believed in it") {
      val w = new StrandWorld
      val runs = new Runs(w, TestLabels.Trialled, TestLabels.TrialledOwn)
      val a = strand(w, "C1", "deploy pipeline", integrity = TestLabels.Endorsed)
      val _ = runs.run()
      runs.card("plan1", "The deploy plan.", Now.plusSeconds(60), TestLabels.Endorsed)
      val reply = runs.run(now = Now.plusSeconds(600), sort = judging("new_topic", yes = 1.0))
      (reply, runs.topics.map(d => (DocKey.value(d.key), d.integrity))) ==> (
        "topics: 1 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector((key(a), TestLabels.Endorsed))
      )
    }

    test("a topic a scan prunes is believed as it was, never raised") {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline", integrity = Integrity.Outside)
      val runs = new Runs(w)
      val _ = runs.run()
      val _ = strand(w, "C2", "deploy rollback", 86400, Integrity.Outside)
      val day2 = Some(Day.plusDays(1))
      val later = StrandWorld.at(86400 + 3600)
      val _ = runs.run(day = day2, now = later, sort = judging("1", yes = 1.0))
      val joined = runs.topics.map(_.integrity)
      val moved = runs.run(day = day2, now = later, rooms = labelled(Internal, "C1"))
      (joined, moved, runs.topics.map(_.integrity)) ==> (
        Vector(Integrity.Outside),
        "topics: 0 written (0 opened), 1 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector(Integrity.Outside)
      )
    }

    test("the aside a scan keeps is believed as it was, never raised") {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline", integrity = Integrity.Outside)
      val runs = new Runs(w)
      runs.topic()
      runs.card("plan1", "The deploy plan.", Now.minusSeconds(60))
      val _ = runs.run(sort = asideAll)
      val first = (runs.aside._1, runs.aside._2.size, runs.asideBelieved)
      given Tx = TestTx.fake
      val card = ok(runs.documents.shelf(Knowledge).newest(1)).map(_.key)
      card.foreach(k =>
        ok(runs.documents.keeper(Knowledge, CardTerms).withdraw(k, Label.Public, Now))
      )
      val _ = runs.run(now = Now.plusSeconds(1800), sort = asideAll)
      (first, runs.aside, runs.asideBelieved) ==> (
        (Vector(a.id), 1, Some(Integrity.Outside)),
        (Vector(a.id), Vector()),
        Some(Integrity.Outside)
      )
    }

    test(
      "a topic of one discussion not written that night is sorted against the floor's others; a yes at FoldAt or more is proposed, nothing withdrawn, and it is not sorted again"
    ) {
      val w = new StrandWorld
      val a = strand(w, "C1", "deploy pipeline")
      val b = strand(w, "C2", "deploy pipeline rollback")
      val runs = new Runs(w)
      val fold = judging("new_topic", yes = TopicDream.FoldAt)
      val m = TopicParams.Merges
      val _ = runs.run(sort = fold, merges = m)
      val opened = runs.asked.size
      val proposed = runs.run(now = Now.plusSeconds(600), sort = fold, merges = m)
      val sorted = runs.asked.size
      val again = runs.run(now = Now.plusSeconds(1200), sort = fold, merges = m)
      (
        runs.asked.take(opened).map(_.kind),
        runs.asked.slice(opened, sorted).map(x => (x.kind, counted(x))),
        proposed,
        again,
        runs.asked.size - sorted,
        runs.kept.map(_._1)
      ) ==> (
        // Both topics are written the night they open, so neither is sorted to merge then.
        Vector("sort", "write", "sort", "write"),
        // The older is sorted against the other; the other, its proposed keeper, is not.
        Vector(("sort", (0, 1, 1))),
        "topics: 0 written (0 opened), 0 withdrawn, 2 unchanged, 0 set aside, 0 left for the next run" +
          s"; would fold ${key(a)} into ${key(b)} (0.90)",
        "topics: 0 written (0 opened), 0 withdrawn, 2 unchanged, 0 set aside, 0 left for the next run",
        0,
        Vector(key(a), key(b))
      )
    }

    test(
      "a topic its merge sort says yes to below FoldAt is set aside at its version, and sorted again only past asideFor"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "one", lead = "The deploy pipeline, first.", from = Vector("s-1"))
      runs.topic(name = "two", lead = "The deploy pipeline, second.", from = Vector("s-2"))
      val unsure = judging("new_topic", yes = TopicDream.FoldAt - 0.01)
      def run(now: Instant) =
        runs.run(now = now, sort = unsure, asideFor = 1.hour, merges = TopicParams.Merges)
      val first = run(Now)
      val sorted = runs.asked.size
      val _ = run(Now.plusSeconds(600))
      val within = runs.asked.size - sorted
      val _ = run(Now.plusSeconds(7200))
      (first, sorted, within, runs.asked.size - sorted) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        2,
        0,
        2
      )
    }

    test(
      "a topic of two discussions is never sorted to merge, though a topic of one may be proposed into it"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "two", lead = "The deploy pipeline, twice.", from = Vector("s-1", "s-2"))
      runs.topic(name = "one", lead = "The deploy pipeline, once.", from = Vector("s-3"))
      val reply = runs.run(sort = judging("new_topic", yes = 0.95), merges = TopicParams.Merges)
      (
        reply,
        runs.asked.map(x => (x.kind, counted(x))),
        runs.asked.flatMap(_.shown.linesIterator.toVector.headOption)
      ) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run" +
          "; would fold topic:strand-one into topic:strand-two (0.95)",
        Vector(("sort", (0, 1, 1))),
        Vector("The deploy pipeline, once.")
      )
    }

    test(
      "a keep that pushes out the oldest passage, a newer claim bearing on a claim in it, still reads back"
    ) {
      val w = new StrandWorld
      val _ = strand(w, "C1", "deploy pipeline")
      val runs = new Runs(w)
      val held = right(TopicKey.strand(ConversationId("held")))
      def claimed(n: Int, bears: Vector[ClaimRef]): Doc[TopicNote] = Doc(
        Vector(
          Block.Paragraph(
            Text(
              Vector(
                Span(
                  s"claim $n",
                  Set.empty,
                  Vector(
                    TopicNote.Claimed(
                      Claim(
                        right(ClaimId.of(s"c$n")),
                        None,
                        Certainty.Firm,
                        bears.map(Relation(Relation.Kind.Supersedes, _)),
                        Lineage.of(Vector.empty)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      // Topic.Passages days before the run's, the oldest holding c1, the next c2 bearing on it.
      val passages = (1 to Topic.Passages).toVector.map(i =>
        grit.knowledge.topic.Passage(
          Day.minusDays((Topic.Passages + 1 - i).toLong),
          Lineage.of(Vector.empty),
          i match {
            case 1 => claimed(1, Vector.empty)
            case 2 => claimed(2, Vector(ClaimRef(held, right(ClaimId.of("c1")))))
            case _ => TopicWrite.prose(Passage)
          }
        )
      )
      runs.topic(passages = passages)
      val _ = runs.run(sort = judging("1"))
      val again = runs.run(now = Now.plusSeconds(600))
      (again, runs.topics.map(d => TopicWrite.read(held, d.data).map(_.topic.passages.size))) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 1 unchanged, 0 set aside, 0 left for the next run",
        Vector(Right(Topic.Passages))
      )
    }

    test("a merge sort is shown only topics of its run's floor") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(floor = Internal, name = "one", lead = "The deploy pipeline, inside.")
      runs.topic(name = "public", lead = "The deploy pipeline, outside.")
      val reply =
        runs.run(
          sort = judging("new_topic", yes = 0.95),
          floor = Internal,
          merges = TopicParams.Merges
        )
      (reply, runs.asked) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run",
        Vector()
      )
    }

    /** A meeting planned for `day`, at `start` when given, no one named. */
    def planned(day: LocalDate, start: Option[java.time.LocalTime] = None): Topic.Kind =
      Topic.Kind.Meeting(Meeting.of(Meeting.State.Planned(Meeting.When(day, start)), Vector.empty))

    test(
      "a meeting's merge sort is shown no meeting of its day at another start, but one with no start"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      val nine = Some(java.time.LocalTime.of(9, 0))
      val two = Some(java.time.LocalTime.of(14, 0))
      runs.topic(name = "nine", lead = "The review at nine.", kind = planned(Day, nine))
      runs.topic(name = "two", lead = "The review at two.", kind = planned(Day, two))
      runs.topic(name = "any", lead = "The review, some time.", kind = planned(Day))
      val _ = runs.run(sort = judging("new_topic"), merges = TopicParams.Merges)
      runs.asked.map(_.shown.linesIterator.filter(_.startsWith("The ")).toVector) ==> Vector(
        Vector("The review at nine.", "The review, some time."),
        Vector("The review at two.", "The review, some time."),
        Vector("The review, some time.", "The review at two.", "The review at nine.")
      )
    }

    test(
      "a subject's merge sort is shown no meeting, and a meeting's only the meetings of its day"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "one", lead = "The deploy pipeline, once.", from = Vector("s-1"))
      runs.topic(name = "two", lead = "The deploy pipeline, again.", from = Vector("s-2"))
      runs.topic(name = "tue", lead = "The review, Tuesday.", kind = planned(Day))
      runs.topic(name = "tue2", lead = "The review again, Tuesday.", kind = planned(Day))
      runs.topic(name = "wed", lead = "The review, Wednesday.", kind = planned(Day.plusDays(1)))
      val _ = runs.run(sort = judging("new_topic"), merges = TopicParams.Merges)
      runs.asked.map(_.shown.linesIterator.filter(_.startsWith("The ")).toVector) ==> Vector(
        Vector("The deploy pipeline, once.", "The deploy pipeline, again."),
        Vector("The deploy pipeline, again.", "The deploy pipeline, once."),
        Vector("The review, Tuesday.", "The review again, Tuesday."),
        Vector("The review again, Tuesday.", "The review, Tuesday.")
      )
    }

    test(
      "a meeting is not sorted to merge the night a meeting is written, and is sorted the next"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "tue", lead = "The review, Tuesday.", kind = planned(Day))
      runs.topic(name = "tue2", lead = "The review again, Tuesday.", kind = planned(Day))
      val _ = strand(w, "C1", "the review moved")
      val merges = TopicParams.Merges
      val _ = runs.run(sort = judging("new_meeting"), merges = merges)
      val first = runs.asked.count(_.system.startsWith("document"))
      val _ = runs.run(now = Now.plusSeconds(600), sort = judging("new_meeting"), merges = merges)
      (first, runs.asked.count(_.system.startsWith("document"))) ==> (0, 2)
    }

    /** The roots of the strands each current topic's passages were written from, by key. */
    def roots(runs: Runs): Vector[(String, Vector[String])] = runs.topics
      .map(d =>
        (
          DocKey.value(d.key),
          TopicWrite
            .read(right(TopicKey.read(d.key).toRight("no topic")), d.data)
            .fold(u => sys.error(u.why), identity)
            .topic
            .passages
            .flatMap(_.from.tags)
            .collect { case Lineage.Tag.Strand(root, _, _, _) => ConversationId.value(root) }
        )
      )
      .sortBy(_._1)

    test(
      "a proposal at foldAt or more is folded: its keeper holds both topics' passages, the other is withdrawn, and the reply names it"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "one", lead = "The deploy pipeline, once.", from = Vector("s-1"))
      runs.topic(name = "two", lead = "The deploy pipeline.", from = Vector("s-2"))
      val reply = runs.run(
        sort = judging("new_topic", yes = 0.95),
        merges = TopicParams.Merges,
        foldAt = Some(0.95)
      )
      (reply, roots(runs)) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run" +
          "; folded topic:strand-one into topic:strand-two (0.95)",
        Vector("topic:strand-two" -> Vector("s-2", "s-1"))
      )
    }

    test("a proposal below foldAt is not folded, only proposed") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "one", lead = "The deploy pipeline, once.", from = Vector("s-1"))
      runs.topic(name = "two", lead = "The deploy pipeline.", from = Vector("s-2"))
      val reply = runs.run(
        sort = judging("new_topic", yes = 0.95),
        merges = TopicParams.Merges,
        foldAt = Some(0.96)
      )
      (reply, roots(runs).map(_._1)) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run" +
          "; would fold topic:strand-one into topic:strand-two (0.95)",
        Vector("topic:strand-one", "topic:strand-two")
      )
    }

    test("three topics of one subject fold into one keeper in one run") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "a", lead = "The deploy pipeline, first.", from = Vector("s-1"))
      runs.topic(name = "b", lead = "The deploy pipeline, second.", from = Vector("s-2"))
      runs.topic(name = "c", lead = "The deploy pipeline, third.", from = Vector("s-3"))
      val reply = runs.run(
        sort = judging("new_topic", yes = 0.95),
        merges = TopicParams.Merges,
        foldAt = Some(0.95)
      )
      (reply, roots(runs).map((k: String, rs: Vector[String]) => (k, rs.toSet))) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run" +
          "; folded topic:strand-a into topic:strand-c (0.95), folded topic:strand-b into topic:strand-c (0.95)",
        Vector("topic:strand-c" -> Set("s-1", "s-2", "s-3"))
      )
    }

    test("a fold's keeper is believed at no more than the topic folded into it") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "one", from = Vector("s-1"), believed = Integrity.Outside)
      runs.topic(name = "two", from = Vector("s-2"))
      val _ = runs.run(
        sort = judging("new_topic", yes = 0.95),
        merges = TopicParams.Merges,
        foldAt = Some(0.95)
      )
      runs.topics.map(d => (DocKey.value(d.key), d.integrity)) ==>
        Vector("topic:strand-two" -> Integrity.Outside)
    }

    test(
      "a fold refused, its keeper written since its sort, stays a proposal named not written; nothing is withdrawn or set aside"
    ) {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "one", lead = "The deploy pipeline, once.", from = Vector("s-1"))
      runs.topic(name = "two", lead = "The deploy pipeline.", from = Vector("s-2"))
      val reply = runs.run(
        sort = judging("new_topic", yes = 0.95),
        merges = TopicParams.Merges,
        foldAt = Some(0.95),
        meanwhile = Some("two" -> "The deploy pipeline, rewritten.")
      )
      val aside = ok(
        Aside.read(runs.documents.keeper(Plugin, Terms), Label.Public)(using TestTx.fake)
      ).map(_.tag)
      (reply, runs.kept.map(_._1), aside) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run" +
          "; would fold topic:strand-one into topic:strand-two (0.95)" +
          "; not written: fold-0 (a topic was written since its merge sort)",
        Vector("topic:strand-one", "topic:strand-two"),
        Vector()
      )
    }

    test("a fold of a subject into a meeting is refused, and writes nothing") {
      val w = new StrandWorld
      val runs = new Runs(w)
      runs.topic(name = "one", lead = "The review, a subject.", from = Vector("s-1"))
      runs.topic(name = "tue", lead = "The review, Tuesday.", kind = planned(Day))
      def at(name: String) = runs.topics
        .find(d => DocKey.value(d.key) == s"topic:strand-$name")
        .fold(sys.error(s"no topic $name"))(_.version)
      def keyed(name: String) = right(TopicKey.strand(ConversationId(name)))
      val folded = TopicDream.fold(
        Gathered.Fold(keyed("one"), keyed("tue"), 0.95),
        at("one"),
        at("tue"),
        runs.documents.keeper(Plugin, Terms),
        Now
      )(using TestTx.fake)
      (folded, runs.kept.map(_._1)) ==> (
        Left(
          StoreError.Invalid(
            "a subject and a meeting, or two meetings not of one day and start, never fold"
          )
        ),
        Vector("topic:strand-one", "topic:strand-tue")
      )
    }

    test(
      "a keeper put up for review the night it opens is still offered once a topic is folded into it, as the text its write put up"
    ) {
      val w = new StrandWorld
      val c = strand(w, "C1", "deploy pipeline rollback")
      val runs = new Runs(w)
      runs.topic(
        name = "one",
        lead = "The deploy pipeline, once.",
        passages = Vector(
          grit.knowledge.topic
            .Passage(
              Day,
              Lineage.of(Vector.empty),
              TopicWrite.prose("Bo moved deploys to Fridays.")
            )
        )
      )
      // Of two discussions, so never sorted itself, and one subject the other may fold with.
      runs.topic(name = "two", lead = "The office move.", from = Vector("s-2", "s-3"))
      val base = judging("new_topic")
      // Yes only for the topic tonight's strand opens, written with Lead.
      val onlyOpened = Judge(qs =>
        base
          .answers(qs)
          .zip(qs)
          .map {
            case (_, Question.YesNo(about, _, _)) if about.contains(Lead) => Judged.YesNo(0.95)
            case (a, _) => a
          }
      )
      val reply =
        runs.run(sort = onlyOpened, merges = TopicParams.Merges, foldAt = Some(0.95))
      (
        reply,
        runs.kept.map((k, text) => (k, text.contains("Fridays"))),
        runs.reviewed,
        runs.reviewedText.map((k, text) => (k, text.contains("Fridays")))
      ) ==> (
        "topics: 1 written (1 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the next run" +
          s"; folded topic:strand-one into ${key(c)} (0.95)",
        Vector(key(c) -> true, "topic:strand-two" -> false),
        Vector(key(c) -> Day),
        // The review is a verdict on the night's write, so it shows that write, not the fold.
        Vector(key(c) -> false)
      )
    }

    test(
      "a run given rooms sorts a planned meeting of two due in its strands' room, and books meeting-ahead Lead before its start when judged unsettled"
    ) {
      val runs = new Runs(new StrandWorld)
      runs.topic(name = "m", from = Vector("s1"), kind = Ahead)
      val reply = runs.run(sort = judging("unsettled"), aheadIn = Vector(AheadRoom))
      val start = Day.atTime(15, 0).toInstant(ZoneOffset.UTC)
      (
        reply,
        runs.asked.map(_.kind),
        runs.booked("later-0").map(s => (s.job, s.rule)),
        runs
          .booked("later-0")
          .map(s => new MeetingAhead(ZoneOffset.UTC).read(s.params).map(d => (d.room, d.until)))
      ) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the " +
          "next run; meetings ahead: 1 in its rooms within 24 hours, 0 naming fewer than two " +
          "people, 0 past the 3 a run asks about; 0 judged settled, 1 booked",
        Vector("sort"),
        Some(
          (
            MeetingAhead.Name,
            SlotRule.Once(
              start.minusSeconds(600),
              Grace.of(MeetingsAhead.Lead).getOrElse(Grace.Zero)
            )
          )
        ),
        Some(Right((AheadRoom, start)))
      )
    }

    test("a meeting judged settled books no run") {
      val runs = new Runs(new StrandWorld)
      runs.topic(name = "m", from = Vector("s1"), kind = Ahead)
      (
        runs.run(sort = judging("settled"), aheadIn = Vector(AheadRoom)),
        runs.booked("later-0")
      ) ==> (
        "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, 0 left for the " +
          "next run; meetings ahead: 1 in its rooms within 24 hours, 0 naming fewer than two " +
          "people, 0 past the 3 a run asks about; 1 judged settled, 0 booked",
        None
      )
    }

    test(
      "a run given no rooms has no ahead phase, and one made after its day's end finds no meeting due"
    ) {
      val next = Topic.Kind.Meeting(
        Meeting.of(
          Meeting.State.Planned(Meeting.When(Day.plusDays(1), Some(java.time.LocalTime.of(10, 0)))),
          Vector("Ana", "Ben")
        )
      )
      def asks(now: Instant, rooms: Vector[Place]): (Option[String], Int) = {
        val runs = new Runs(new StrandWorld)
        runs.topic(name = "m", from = Vector("s1"), kind = next)
        val reply = runs.run(now = now, sort = judging("unsettled"), aheadIn = rooms)
        (reply.split("; meetings ahead: ").lift(1).map(_.take(14)), runs.asked.size)
      }
      (
        asks(Now, Vector.empty),
        asks(
          Day.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant.plusSeconds(1),
          Vector(AheadRoom)
        )
      ) ==> ((None, 0), (Some("0 in its rooms"), 0))
    }

    test("a person who spoke in five strands within 30 days has a card opened; four open none") {
      val five = new StrandWorld
      talk(five, 5)
      val runs = new Runs(five)
      runs.run()
      val four = new StrandWorld
      talk(four, 4)
      val none = new Runs(four)
      none.run()
      (
        runs.personal(Dana).map(d => DocText.value(d.text).linesIterator.toVector.headOption),
        none.personal(Dana)
      ) ==> (Some(Some(PersonLine)), None)
    }

    test("a first card's ask is shown its name's line alone; the card keeps the paragraph") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run(people = 1)
      (
        runs.asked.filter(_.kind == "person").map(cardShown),
        runs.personal(Dana).map(d => DocText.value(d.text))
      ) ==> (
        Vector(Some(PersonLine)),
        Some(
          (Vector(PersonLine, Paragraph, "") ++
            Vector(Vector.fill(5)(Lead).mkString("Talks about: ", "; ", "")) ++
            Vector(
              "Works most with: Bo Lund",
              "Active in 5 rooms on 1 day in the last 30 days; last spoke 2026-10-12."
            )).mkString("\n")
        )
      )
    }

    test("a card's next ask is shown its name's line and its paragraph, none of its facts") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run(people = 1)
      talk(w, 5, from = 6, at = 6 * 86400)
      runs.run(day = Some(Day.plusDays(7)), now = Now.plusSeconds(7 * 86400), people = 1)
      runs.asked.filter(_.kind == "person").map(cardShown) ==> Vector(
        Some(PersonLine),
        Some(PersonLine + "\n" + Paragraph)
      )
    }

    test("the paragraph ask's system text holds no speaker's name") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run()
      val systems = runs.asked.filter(_.kind == "person").map(_.system)
      (systems.size, systems.exists(t => t.contains("Dana") || t.contains("Bo"))) ==> (1, false)
    }

    test(
      "a card written six days ago is not written again; at seven, with five fresh strands, it is"
    ) {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run(people = 1)
      val first = runs.personal(Dana).map(_.version)
      talk(w, 5, from = 6, at = 6 * 86400)
      runs.run(day = Some(Day.plusDays(6)), now = Now.plusSeconds(6 * 86400), people = 1)
      val sixth = runs.personal(Dana).map(_.version)
      runs.run(day = Some(Day.plusDays(7)), now = Now.plusSeconds(7 * 86400), people = 1)
      val seventh = runs.personal(Dana).map(_.version)
      (first.nonEmpty, sixth == first, seventh != first) ==> (true, true, true)
    }

    test("a card is still current after the next run, and no sort or merge is shown it") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run()
      talk(w, 1, by = Bo, name = "Bo Lund", from = 9, at = 600)
      runs.run(now = Now.plusSeconds(1200), merges = TopicParams.Merges)
      val sorts = runs.asked.filter(_.kind == "sort")
      (
        runs.personal(Dana).nonEmpty,
        sorts.nonEmpty,
        sorts.exists(a => (a.system + a.shown).contains("a profile inferred"))
      ) ==> (true, true, false)
    }

    test("a card whose person last spoke more than 90 days before the run's day is withdrawn") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run()
      val kept = runs.personal(Dana).nonEmpty
      runs.run(day = Some(Day.plusDays(90)), now = Now.plusSeconds(90 * 86400))
      val ninety = runs.personal(Dana).nonEmpty
      runs.run(day = Some(Day.plusDays(91)), now = Now.plusSeconds(91 * 86400))
      (kept, ninety, runs.personal(Dana)) ==> (true, true, None)
    }

    test("a card written from a room now at another label is withdrawn") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run()
      val kept = runs.personal(Dana).nonEmpty
      runs.run(now = Now.plusSeconds(600), rooms = labelled(Internal, "C1"))
      (kept, runs.personal(Dana)) ==> (true, None)
    }

    test("a run's reply counts the person cards it wrote") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run() ==> "topics: 5 written (5 opened), 0 withdrawn, 0 unchanged, 0 set aside, " +
        "0 left for the next run; 1 person card"
    }

    test("people 0 writes no card and asks nothing of one") {
      val w = new StrandWorld
      talk(w, 5)
      val runs = new Runs(w)
      runs.run(people = 0)
      (runs.personal(Dana), runs.asked.count(_.kind == "person")) ==> (None, 0)
    }

    test("a card's facts quoting a topic believed at Outside are believed at no more than that") {
      val w = new StrandWorld
      val threads = talk(w, 5)
      val runs = new Runs(w)
      runs.topic(
        name = "outside",
        lead = "The vendor's pricing.",
        from = threads.map(c => ConversationId.value(c.id)),
        believed = Integrity.Outside
      )
      // A reply that does not read keeps the facts' version alone.
      runs.run(people = 1, person = _ => ujson.Obj())
      runs.personal(Dana).map(_.integrity) ==> Some(Integrity.Outside)
    }
  }
}
