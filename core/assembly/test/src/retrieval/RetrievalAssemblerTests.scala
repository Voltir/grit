package grit.assembly.retrieval

import java.time.Instant

import scala.concurrent.duration.*

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.AssemblyFixtures.{
  FakeDb,
  World,
  c1,
  closed,
  closingOf,
  elsewhere,
  store
}
import grit.assembly.linear.LinearAssembler
import grit.core.context.{
  AssemblyNote,
  AssemblyRequest,
  ContextAssembler,
  JudgeText,
  Packing,
  Prepared,
  Querying,
  Results,
  Retrieved,
  Shown,
  Width,
  Window
}
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  Document,
  DocumentSearch,
  DocumentTerms,
  InMemoryDocuments,
  Shelved,
  Written
}
import grit.core.files.{
  FormatName,
  InMemoryPieces,
  Locus,
  Piece,
  PieceKeeper,
  PieceSearch,
  TestPieces
}
import grit.core.id.{
  CallSlot,
  CloseRef,
  ConversationId,
  DocKey,
  DocumentVersionId,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  PieceId,
  PluginName,
  TurnRef,
  TurnSeq
}
import grit.core.identity.{Account, TestAccounts}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.period.{CloseReason, LifecycleSettings, Probability, TestClosings}
import grit.core.place.{Place, Weight}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.stitch.{InMemoryStitchStore, StitchJson, StitchStore, Tuning}
import grit.core.store.{
  ClosingEntry,
  Conversation,
  Entry,
  EntrySearch,
  EntryStore,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryLifecycleStore,
  Nearby,
  OpenPeriod,
  Origin,
  Payload,
  Speakers,
  StoreError,
  Tx
}
import grit.core.visibility.Integrity
import grit.core.visibility.Label
import grit.dbos.sql.TestTx

import utest.*

object RetrievalAssemblerTests extends TestSuite {

  private def reply(text: String, model: String = "test"): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens(40), Tokens(6), Tokens.Zero, None),
      model
    )

  /** Answers every request with `answer`, or fails; keeps what it was sent. */
  private final class Writer(answer: Option[String], upstream: Option[String] = None)
      extends Provider {
    // Written and read only on the test's own thread, through the calls it makes.
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      requests = requests :+ request
      answer
        .map(reply(_, "writer").copy(upstream = upstream))
        .toRight(ProviderError.Unavailable("down"))
    }
  }

  /** One nearby search the assembler made: the conversations searched, and the query. */
  private final case class NearAsked(conversations: List[ConversationId], query: String)

  /** One search the assembler made. */
  private final case class Asked(
      conversation: ConversationId,
      from: TurnSeq,
      before: TurnSeq,
      query: String,
      limit: Int
  )

  /** Answers every search with the entries `ids` (`t{turn}:{seq}`), best first in the order
    * given, scored by `scores` where the test states them, whatever it is asked; keeps what
    * it was asked. The ranking is the test's, not a scorer's whose ties would decide it. A
    * room search finds nothing: the assembler never makes one.
    */
  private final class Scripted(ids: String*) extends EntrySearch {
    def room(
        room: grit.core.place.Place,
        from: java.time.Instant,
        until: java.time.Instant,
        query: String,
        limit: Int
    )(using Tx^): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)

    // What nearby answers: each conversation's hits, as (conversation, id, score), best
    // first, ids as `{name}:t{turn}:{seq}`, only those from its open period's first turn on;
    // and every nearby search it was asked for, as the periods and query.
    // Replaced whole by the test's own calls, on its one thread; nothing else holds it.
    @caps.unsafe.untrackedCaptures
    var near = Vector.empty[(ConversationId, String, Double)]
    @caps.unsafe.untrackedCaptures
    var nearAsked = Vector.empty[NearAsked]
    // What search scores its hits, best first, when the test states them; unstated, the
    // hits score by rank, the last 1.
    @caps.unsafe.untrackedCaptures
    var scores = Vector.empty[Double]
    // Only ever holds immutable vectors; nothing reads it but the test that owns it.
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[Asked]

    def search(
        conversation: ConversationId,
        from: TurnSeq,
        before: TurnSeq,
        query: String,
        limit: Int
    )(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = {
      asked = asked :+ Asked(conversation, from, before, query, limit)
      Right(ids.toVector.zipWithIndex.map { (id, rank) =>
        val turn = id.drop(1).takeWhile(_ != ':').toLongOption.getOrElse(sys.error(s"id $id"))
        EntrySearch.Hit(
          EntryId(id),
          TurnRef(conversation, TurnSeq(turn)),
          scores.lift(rank).getOrElse((ids.size - rank).toDouble)
        )
      })
    }

    def nearby(open: Vector[OpenPeriod], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = {
      nearAsked = nearAsked :+ NearAsked(open.map(_.conversation).toList, query)
      Right(near.flatMap { (c, id, score) =>
        val turn = Option
          .when(id.contains(":t"))(id.drop(id.lastIndexOf(":t") + 2).takeWhile(_ != ':'))
          .flatMap(_.toLongOption)
          .getOrElse(sys.error(s"not a nearby id: $id"))
        open
          .find(o => o.conversation == c && TurnSeq.value(o.first) <= turn)
          .map(_ => EntrySearch.Hit(EntryId(id), TurnRef(c, TurnSeq(turn)), score))
      })
    }

    // What closings answers: closing entries as (conversation, id, score), best first, only
    // those of the conversations asked, at most `limit`; and every closings search, as its
    // conversations.
    // Replaced whole by the test's own calls, on its one thread; nothing else holds it.
    @caps.unsafe.untrackedCaptures
    var closed = Vector.empty[(ConversationId, String, Double)]
    @caps.unsafe.untrackedCaptures
    var closedAsked = Vector.empty[List[ConversationId]]

    def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = {
      closedAsked = closedAsked :+ conversations.toList
      Right(
        closed
          .collect {
            case (c, id, score) if conversations.contains(c) =>
              EntrySearch.Hit(EntryId(id), TurnRef(c, TurnSeq(0)), score)
          }
          .take(limit)
      )
    }
  }

  /** One document search the assembler made. */
  private final case class DocsAsked(
      shelves: Vector[Shelved],
      query: String,
      limit: Int,
      at: Instant
  )

  /** The documents `under` keeps, as windows draw on them; a search's hits rescored by
    * `scores`, by key, where the test states them, then best first, so the ranking is the
    * test's. Keeps every search it was asked for.
    */
  private final class ScriptedDocs(val under: InMemoryDocuments) extends DocumentSearch {
    // Replaced whole by the test's own calls, on its one thread; nothing else reads it.
    @caps.unsafe.untrackedCaptures
    var scores = Map.empty[String, Double]
    // Only ever holds immutable vectors; nothing reads it but the test that owns it.
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[DocsAsked]

    def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
      under.declared()
    def shelved(plugins: Vector[PluginName], at: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Shelved]] = under.shelved(plugins, at)
    def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
        Tx^
    ): Either[StoreError, Vector[DocumentSearch.Hit]] = {
      asked = asked :+ DocsAsked(shelves, query, limit, at)
      under
        .search(shelves, query, limit, at)
        .map(
          _.map(h => h.copy(score = scores.getOrElse(DocKey.value(h.document.key), h.score)))
            .sortBy(-_.score)
        )
    }
    def read(
        versions: Vector[DocumentVersionId]
    )(using Tx^): Either[StoreError, Vector[Document]] = {
      reads = reads :+ versions
      under.read(versions)
    }
    // Only ever holds immutable vectors; nothing reads it but the test that owns it.
    @caps.unsafe.untrackedCaptures
    var reads = Vector.empty[Vector[DocumentVersionId]]
  }

  /** `under`'s entries, keeping which conversation each `list` asked for. */
  private final class Listing(under: InMemoryEntryStore) extends EntryStore {
    // Only ever holds immutable vectors; nothing reads it but the test that owns it.
    @caps.unsafe.untrackedCaptures
    var listed = Vector.empty[ConversationId]

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = under.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = under.get(id)
    def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = {
      listed = listed :+ conversation
      under.list(conversation)
    }
    def at(conversation: ConversationId, seqs: Vector[EntrySeq])(using
        Tx^
    ): Either[StoreError, Vector[Entry]] = under.at(conversation, seqs)
    def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] = under.ofTurn(turn)
    def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      under.lockNext(conversation)
  }

  private def got[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  private def plugin(name: String): PluginName = got(PluginName.of(name))

  private def termsOf(weight: Double, label: String = "Notes"): DocumentTerms =
    got(DocumentTerms.of(got(DocLabel.of(label)), got(DocWeight.of(weight)), 30.days, 10))

  /** Documents, `plugins` declared the ones in force, each at its weight. */
  private def shelf(plugins: (String, Double)*): ScriptedDocs = {
    val docs = new InMemoryDocuments
    docs
      .declare(plugins.toVector.map((p, w) => plugin(p) -> termsOf(w)))(using TestTx.fake)
      .getOrElse(sys.error("in-memory store"))
    new ScriptedDocs(docs)
  }

  /** A minute before the turns every world here holds: before any turn's root. */
  private val Before: Instant = Instant.EPOCH.minusSeconds(60)

  /** `text`, `p`'s document under `key` at `place`, written `at`; its version. */
  private def write(
      docs: ScriptedDocs,
      p: String,
      key: String,
      place: Place,
      text: String,
      at: Instant = Before
  ): DocumentVersionId =
    docs.under
      .keeper(plugin(p), termsOf(1))
      .write(
        got(DocKey.of(key)),
        grit.core.visibility.Label.Public,
        Integrity.Member,
        place,
        got(DocText.of(text)),
        ujson.Obj(),
        None,
        at
      )(using
        TestTx.fake
      ) match {
      case Right(Written.Versioned(v, _, _)) => v
      case other => sys.error(s"not written: $other")
    }

  /** What the version `v` of `docs` costs as the window shows it, under the label "Notes". */
  private def docCost(docs: ScriptedDocs, v: DocumentVersionId): Long =
    docs.under
      .read(Vector(v))(using TestTx.fake)
      .toOption
      .flatMap(_.headOption)
      .map(d => Tokens.value(CharEstimate.message(Shown.document(d, got(DocLabel.of("Notes"))))))
      .getOrElse(sys.error("no document"))

  /** `texts` kept as the pieces of `p`'s file `ref`, written before every turn's root; each one's
    * id, in order.
    */
  private def kept(
      pieces: InMemoryPieces,
      p: String,
      ref: String,
      texts: String*
  ): Vector[PieceId] = {
    val all = texts.toVector.map(t =>
      Piece(
        Vector("Policy"),
        grit.prose.form.Doc(
          Vector(
            grit.prose.form.Block.Paragraph(
              grit.prose.form.Text(
                Vector(grit.prose.form.Span(t, Set.empty, Vector(Locus.Page(1))))
              )
            )
          )
        ),
        TestPieces.anchor(t)
      )
    )
    pieces
      .keeper(plugin(p), PieceKeeper.Bound)
      .keep(
        TestPieces.file(ref, s"$ref.pdf", FormatName("test")),
        placeOf("files"),
        Integrity.Member,
        all,
        0,
        Before
      )(using TestTx.fake)
      .getOrElse(sys.error("in-memory store"))
    all.flatMap(piece =>
      pieces
        .search(Vector(plugin(p)), piece.text, 100, Instant.EPOCH)(using TestTx.fake)
        .getOrElse(Vector.empty)
        .find(_.piece.piece == piece)
        .map(_.piece.id)
    )
  }

  /** How many hits the assembler is told to ask for. */
  private val Hits = 7

  private def exchange(question: String, answer: String): Vector[Payload] =
    Vector(Payload.Message(Message.User(question)), Payload.Message(reply(answer)))

  /** A four-character question and an eight-character answer: 11 estimated tokens. */
  private def filler(n: Int): Vector[Payload] = exchange(f"q$n%03d", f"answer$n%02d")

  private def ask: Vector[Payload] = Vector(
    Payload.Message(Message.User("Where should my probe point?"))
  )

  /** The fact at turn 0 (22 estimated tokens), five fillers, and the ask at turn 6. */
  private def buried: Vector[Vector[Payload]] =
    Vector(exchange("Which database do probes use?", "grit_agent, never grit.")) ++
      (1 to 5).map(filler) :+ ask

  private def pin(model: String): Pinned = {
    val ref = ModelRef(ModelId.of(model).getOrElse(sys.error(model)), None)
    val a = Assignment(ref, 100, None)
    Catalog.of(Policy.one(a, a, a, a), Vector.empty).pin.query.first
  }

  /** The pin a window's query is asked to be written under. */
  private val QueryPin = pin("query/writer")

  /** Models whose calls under `pin` alone `writer` answers; under any other, each call fails. */
  private final class Writing(writer: Provider^, pin: Pinned) extends Models {
    def catalog(): Either[String, Catalog] = Left("no catalog")
    def provider(pinned: Pinned): Provider^ =
      if (pinned == pin) writer else new Failing(s"asked under ${pinned.assignment.ref}")
  }

  private final class Failing(why: String) extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      Left(ProviderError.Unavailable(why))
  }

  /** Turn 6's window. With a 25-token tail, the recent turns are 4 and 5. */
  private def assemble(
      world: World,
      writer: Provider^,
      budget: Long,
      search: EntrySearch = new Scripted(),
      at: Long = 6,
      weight: Weight = Weight.Default,
      origin: Origin = Origin.Task("conversation", "c1"),
      stitches: Option[StitchStore] = None,
      tuning: Tuning = Tuning.Default,
      others: Vector[Conversation] = Vector.empty,
      posted: Option[CallSlot] = None,
      width: Width = Width.Deployed,
      documents: DocumentSearch = new InMemoryDocuments,
      query: Option[Pinned] = Some(QueryPin),
      admit: Results -> Results = r => Results(r.ranked, r.beyond),
      pieces: PieceSearch = new InMemoryPieces
  ): Window = {
    val models = new Writing(writer, QueryPin)
    ContextAssembler
      .draw(
        retrieval(
          world,
          budget,
          search,
          weight,
          origin,
          stitches,
          tuning,
          others,
          posted,
          documents,
          pieces = pieces
        ),
        AssemblyRequest(TurnRef(c1, TurnSeq(at)), width)
      )(
        asked =>
          query.fold(Left(Prepared.NoPin))(models.provider(_).complete(asked).left.map(_.cause)),
        admit
      )(using new FakeDb)
      .getOrElse(sys.error("in-memory store"))
  }

  /** A query written for the stages a test runs one by one. */
  private val WrittenQuery: Querying.Written = Querying.Written(
    "database for probes",
    AssemblyNote.Queried(
      "database for probes",
      "writer",
      Usage(Tokens(40), Tokens(6), Tokens.Zero, None),
      Tokens.Zero
    )
  )

  /** The assembler [[assemble]] draws with. */
  private def retrieval(
      world: World,
      budget: Long,
      search: EntrySearch,
      weight: Weight = Weight.Default,
      origin: Origin = Origin.Task("conversation", "c1"),
      stitches: Option[StitchStore] = None,
      tuning: Tuning = Tuning.Default,
      others: Vector[Conversation] = Vector.empty,
      posted: Option[CallSlot] = None,
      documents: DocumentSearch = new InMemoryDocuments,
      entries: Option[EntryStore] = None,
      pieces: PieceSearch = new InMemoryPieces
  ): RetrievalAssembler = {
    val conversations = new InMemoryConversationStore
    conversations.all =
      Conversation(c1, origin, Account.Local, Instant.EPOCH, Label.Public) +: others
    posted.foreach(slot => conversations.posts = Map(c1 -> slot))
    val lifecycle = new InMemoryLifecycleStore
    lifecycle
      .set(
        LifecycleSettings
          .of(
            LifecycleSettings.Default.windows,
            4096,
            LifecycleSettings.Default.settle,
            LifecycleSettings.Default.resolveAt,
            3,
            weight
          )
          .getOrElse(sys.error("settings"))
      )(using TestTx.fake)
      .getOrElse(sys.error("in-memory store"))
    new RetrievalAssembler(
      entries.getOrElse(world.entries),
      conversations,
      world.periods,
      world.principals,
      lifecycle,
      search,
      documents,
      pieces,
      stitches.getOrElse(
        new InMemoryStitchStore(
          world.entries,
          c => Origin.Task("conversation", ConversationId.value(c))
        )
      ),
      CharEstimate,
      Tokens(budget),
      Tokens(25),
      tuning,
      Hits
    )
  }

  /** The ids of the entries `world`'s conversation `c` has at `seqs`, in their order. */
  private def idsAt(world: World, c: ConversationId, seqs: Vector[EntrySeq]): Vector[String] = {
    val bySeq = world.entries
      .list(c)(using TestTx.fake)
      .getOrElse(sys.error("in-memory store"))
      .map(e => e.seq -> EntryId.value(e.id))
      .toMap
    seqs.map(s => bySeq.getOrElse(s, s"<no entry at ${EntrySeq.value(s)}>"))
  }

  /** The ids of the entries of `world`'s own conversation `w` names. */
  private def ids(world: World, w: Window): Vector[String] = idsAt(world, c1, w.entries)

  private def turnsOf(world: World, w: Window): Vector[String] =
    ids(world, w).map(_.takeWhile(_ != ':')).distinct

  /** The seqs of the entries `ids`, which `world` holds. */
  private def seqs(world: World, ids: String*): Vector[EntrySeq] =
    ids.toVector.map(id =>
      world.entries
        .get(EntryId(id))(using TestTx.fake)
        .toOption
        .flatten
        .fold(sys.error(s"no entry $id"))(_.seq)
    )

  private def linear(world: World, budget: Long): Window =
    new LinearAssembler(
      world.entries,
      world.periods,
      world.principals,
      CharEstimate,
      Tokens(budget)
    )
      .assemble(
        AssemblyRequest(TurnRef(c1, TurnSeq(6))),
        Packing.Drawn(Querying.Drawn(Vector.empty))
      )(using
        new FakeDb
      )
      .getOrElse(sys.error("in-memory store"))

  private def placeOf(name: String): Place = Origin.Task("conversation", name).place

  /** Slack thread `ts` of channel C; c1 is thread 2.0. */
  private def thread(ts: String): Origin = Origin.Slack("T", "C", ts)

  /** Where each conversation of a strand world is: c1 thread 2.0, any other its name's thread. */
  private def threads(c: ConversationId): Origin =
    if (c == c1) thread("2.0") else thread(ConversationId.value(c))

  /** Heard `texts` in conversation `name`, each said its seconds before the epoch, oldest
    * first; ids `{name}:{i}`.
    */
  private def said(world: World, name: String, texts: (String, Long)*): ConversationId = {
    val c = ConversationId(name)
    given Tx = TestTx.fake
    texts.zipWithIndex.foreach { case ((text, ago), i) =>
      val next = world.entries.lockNext(c).getOrElse(sys.error("in-memory store"))
      val at = Instant.EPOCH.minusSeconds(ago)
      val _ = world.periods.openFor(c, next.turnSeq, at)
      val _ = world.entries.insert(
        Entry(
          EntryId(s"$name:$i"),
          c,
          next.turnSeq,
          None,
          next.seq,
          Payload.Heard(text),
          at,
          Integrity.Member
        )
      )
    }
    c
  }

  /** c1's first message placed as following `root`'s exchange. */
  private def follow(world: World, root: ConversationId): StitchStore = {
    val stitches = new InMemoryStitchStore(world.entries, threads)
    val placed = StitchJson
      .read(
        ujson.Obj(
          "kind" -> "follows",
          "root" -> ConversationId.value(root),
          "p" -> 0.9,
          "model" -> "jev",
          "usage" -> grit.core.store.PayloadJson.writeUsage(Usage.Zero),
          "seen" -> ujson.Obj(
            "state" -> ujson.Obj(),
            "offered" -> ujson.Arr(),
            "tuning" -> ujson.Obj(
              "horizon_seconds" -> 604800,
              "recent" -> 2,
              "lexical" -> 2,
              "follows_at" -> 0.6,
              "window_tokens" -> 1500,
              "strand_chars" -> 800
            )
          )
        )
      )
      .fold(e => sys.error(e), identity)
    stitches.record(EntryId("t0:0"), placed, Instant.EPOCH)(using TestTx.fake)
    stitches
  }

  /** Closes `c`'s period `n` in `world` after one more turn saying `said`, its prose `prose`;
    * its closing entry's id.
    */
  private def closeAgain(
      world: World,
      c: ConversationId,
      n: Long,
      said: String,
      prose: String
  ): String = {
    given Tx = TestTx.fake
    val first = world.entries.lockNext(c).getOrElse(sys.error("in-memory store"))
    val _ = world.periods.openFor(c, first.turnSeq, Instant.EPOCH)
    val _ = world.entries.insert(
      Entry(
        EntryId(s"${ConversationId.value(c)}:t${TurnSeq.value(first.turnSeq)}:${first.seq}"),
        c,
        first.turnSeq,
        None,
        first.seq,
        Payload.Message(Message.User(said)),
        Instant.EPOCH,
        Integrity.Member
      )
    )
    val period = PeriodRef(c, PeriodSeq.of(n).getOrElse(sys.error("period")))
    val _ = world.periods.seal(
      CloseRef(period, first.turnSeq, Instant.EPOCH),
      CloseReason.Resolved(Probability.One),
      TestClosings.prose(prose),
      Instant.EPOCH
    )
    EntryId.value(period.closingId)
  }

  /** The closing entry of `c`'s first period. */
  private def firstClosing(c: ConversationId): String =
    EntryId.value(PeriodRef(c, PeriodSeq.First).closingId)

  val tests = Tests {

    test("a period's first turn, with a period open elsewhere, writes a query and shows its turn") {
      val world = store(ask)
      val api = elsewhere(
        world,
        "api",
        close = false,
        exchange("the invoice test is flaky", "Pin TZ=UTC in the test JVM."),
        exchange("lunch?", "later")
      )
      val writer = new Writer(Some("invoice test flaky fix"))
      val search = new Scripted()
      search.near = Vector((api, "api:t0:1", 2.0))
      val w = assemble(world, writer, budget = 1000, search, at = 0)
      writer.requests.size ==> 1
      search.nearAsked.map(_.query) ==> Vector("invoice test flaky fix")
      w.entries ==> Vector()
      w.nearby ==> Vector(
        Nearby.Open(api, placeOf("api"), seqs(world, "api:t0:0", "api:t0:1"))
      )
    }

    test(
      "a closing elsewhere is a candidate: a first turn writes a query, and a match shows the newest kept record"
    ) {
      val world = store(ask)
      val ops = elsewhere(world, "ops", close = true, exchange("when is the freeze?", "Friday"))
      val newest = closeAgain(world, ops, 2, "and hotfixes?", "Hotfixes skip it.")
      val writer = new Writer(Some("deploy freeze"))
      val search = new Scripted()
      // The older closing matches; the section shows the newest, whose balance is current.
      search.closed = Vector((ops, firstClosing(ops), 2.0))
      val w = assemble(world, writer, budget = 1000, search, at = 0)
      writer.requests.size ==> 1
      search.closedAsked ==> Vector(List(ops))
      w.nearby ==> Vector(Nearby.Closed(ops, placeOf("ops"), seqs(world, newest)(0)))
    }

    test("one section per conversation: its open turns win over its closing") {
      val world = store(ask)
      val api = elsewhere(world, "api", close = true, exchange("flaky?", "TZ"))
      val reopened = world.entries.lockNext(api)(using TestTx.fake).getOrElse(sys.error("store"))
      val _ = world.periods.openFor(api, reopened.turnSeq, Instant.EPOCH)(using TestTx.fake)
      val _ = world.entries.insert(
        Entry(
          EntryId(s"api:t${TurnSeq.value(reopened.turnSeq)}:${reopened.seq}"),
          api,
          reopened.turnSeq,
          None,
          reopened.seq,
          Payload.Message(Message.User("still flaky")),
          Instant.EPOCH,
          Integrity.Member
        )
      )(using TestTx.fake)
      val open = s"api:t${TurnSeq.value(reopened.turnSeq)}:${reopened.seq}"
      val search = new Scripted()
      // The closing ranks first; the open turn still wins the conversation's one section.
      search.closed = Vector((api, firstClosing(api), 3.0))
      search.near = Vector((api, open, 1.0))
      assemble(world, new Writer(Some("flaky")), budget = 1000, search, at = 0).nearby ==>
        Vector(Nearby.Open(api, placeOf("api"), seqs(world, open)))
    }

    test("a record that does not fit is passed over for the next") {
      val world = store(ask)
      val big = elsewhere(world, "big", close = false, exchange("x", "y"))
      val _ = closeAgain(world, big, 1, "freeze?", "freeze " * 200)
      val small = elsewhere(world, "small", close = true, exchange("freeze?", "Friday"))
      val search = new Scripted()
      search.closed = Vector((big, firstClosing(big), 2.0), (small, firstClosing(small), 1.0))
      val smallCost = world.entries
        .get(EntryId(firstClosing(small)))(using TestTx.fake)
        .toOption
        .flatten
        .flatMap(ClosingEntry.of)
        .map(e => CharEstimate.message(Shown.recorded(placeOf("small"), e)))
        .getOrElse(sys.error("small's closing"))
      assemble(
        world,
        new Writer(Some("freeze")),
        Tokens.value(smallCost) + 5,
        search,
        at = 0
      ).nearby ==>
        Vector(Nearby.Closed(small, placeOf("small"), seqs(world, firstClosing(small))(0)))
    }

    test("a direct message's window draws on an open channel thread's period") {
      val dm = Origin.Direct(TestAccounts.sourced("slack:T1/U1"), "1.0")
      val origins = (c: ConversationId) =>
        ConversationId.value(c) match {
          case "c1" => dm
          case other => Origin.Slack("T1", "C1", other)
        }
      val world = closed(Vector(Vector(ask)), Vector.empty, origins)
      val standup = elsewhere(world, "standup", close = false, exchange("deploy when?", "at four"))
      val search = new Scripted()
      search.near = Vector((standup, "standup:t0:1", 2.0))
      val w = assemble(
        world,
        new Writer(Some("deploy")),
        budget = 1000,
        search,
        at = 0,
        origin = dm
      )
      assert(
        w.nearby.contains(
          Nearby.Open(
            standup,
            Origin.Slack("T1", "C1", "standup").place,
            seqs(world, "standup:t0:0", "standup:t0:1")
          )
        )
      )
    }

    test(
      "a stitched conversation's window shows its strand before its own turns: the opening, then the nearest that fit windowTokens"
    ) {
      val world = closed(Vector(Vector(ask)), Vector.empty, threads)
      val a = said(
        world,
        "1.0",
        "where did we land on the Engine contract term?" -> 120,
        "an old aside, long enough that it will not fit" -> 90,
        "lol" -> 30
      )
      val stitches = follow(world, a)
      val strand = seqs(world, "1.0:0", "1.0:1", "1.0:2")
      def shown(tokens: Long) = assemble(
        world,
        new Writer(Some("unused")),
        budget = 1000,
        at = 0,
        origin = thread("2.0"),
        stitches = Some(stitches),
        tuning = Tuning.Default.copy(windowTokens = Tokens(tokens))
      ).nearby
      shown(1000) ==> Vector(Nearby.Along(a, thread("1.0").place, strand))
      // 60 tokens: the section with all three costs 75, with the opening and "lol" 54.
      shown(60) ==> Vector(Nearby.Along(a, thread("1.0").place, Vector(strand(0), strand(2))))
    }

    test("a strand's root whose first message is purged is shown by its newest kept record") {
      val world = closed(Vector(Vector(ask)), Vector.empty, threads)
      val a =
        elsewhere(world, "1.0", close = true, exchange("the Engine contract term?", "12 months"))
      world.periods.purge(PeriodRef(a, PeriodSeq.First), Instant.EPOCH)(using TestTx.fake)
      val stitches = follow(world, a)
      val w = assemble(
        world,
        new Writer(Some("unused")),
        budget = 1000,
        at = 0,
        origin = thread("2.0"),
        stitches = Some(stitches)
      )
      w.nearby ==> Vector(
        Nearby.Closed(
          a,
          thread("1.0").place,
          seqs(world, EntryId.value(PeriodRef(a, PeriodSeq.First).closingId))(0)
        )
      )
    }

    // A thread begun by grit's post (c1, thread 2.0 of C), asked for in another channel.
    val askedIn: Origin = Origin.Slack("T", "C2", "9.0")
    val asker = ConversationId("asker")
    def askedFor(slot: Long = 0): CallSlot =
      CallSlot.of(TurnRef(asker, TurnSeq(slot)), 0, 0).getOrElse(sys.error("slot"))
    def postThread(periods: Vector[Vector[Vector[Payload]]], prose: Vector[String]): World = {
      val world = closed(periods, prose, c => if (c == asker) askedIn else thread("2.0"))
      elsewhere(
        world,
        "asker",
        close = false,
        exchange("post the open issues in #skynet", "Posted in #skynet.")
      )
      world
    }
    val posted = Vector(Payload.Posted("The engine's open issues."))
    val why = Vector(Payload.Message(Message.User("why this?")))
    def askerAt(origin: Origin): Conversation =
      Conversation(asker, origin, Account.Local, Instant.EPOCH, Label.Public)

    test(
      "a thread begun by grit's post is shown first the turn that asked for it, and the asker is not also ranked"
    ) {
      val world = postThread(Vector(Vector(posted, why)), Vector.empty)
      val search = new Scripted()
      search.near = Vector((asker, "asker:t0:0", 5.0))
      val w = assemble(
        world,
        new Writer(Some("open issues")),
        budget = 1000,
        search,
        at = 1,
        origin = thread("2.0"),
        others = Vector(askerAt(askedIn)),
        posted = Some(askedFor())
      )
      w.nearby ==> Vector(
        Nearby.Asked(asker, askedIn.place, seqs(world, "asker:t0:0", "asker:t0:1"))
      )
      ids(world, w) ==> Vector("t0:0")
      assemble(
        world,
        new Writer(Some("unused")),
        budget = 1000,
        at = 1,
        origin = thread("2.0"),
        others = Vector(askerAt(askedIn)),
        posted = Some(askedFor())
      ).nearby.map(_.conversation) ==> Vector(asker)
    }

    test("a terminal's or a task's asker is never shown into a Slack thread") {
      val world = postThread(Vector(Vector(posted, why)), Vector.empty)
      val tui = Origin.Tui(
        grit.core.place.Directory.of("/home/nick/api").getOrElse(sys.error("a directory")),
        "s1"
      )
      Vector(tui, Origin.Task("nightly", "r1")).map { origin =>
        assemble(
          world,
          new Writer(Some("unused")),
          budget = 1000,
          at = 1,
          origin = thread("2.0"),
          others = Vector(askerAt(origin)),
          posted = Some(askedFor())
        ).nearby
      } ==> Vector(Vector.empty, Vector.empty)
    }

    test("once the thread's first period has closed, its record stands for the asked section") {
      val world = postThread(Vector(Vector(posted), Vector(why)), Vector("A post, asked about."))
      val w = assemble(
        world,
        new Writer(Some("unused")),
        budget = 1000,
        at = 1,
        origin = thread("2.0"),
        others = Vector(askerAt(askedIn)),
        posted = Some(askedFor())
      )
      (w.nearby, ids(world, w)) ==> (Vector.empty, Vector(closingOf(1)))
    }

    test("an asked section keeps its first messages that fit the strand's allowance") {
      val world = postThread(Vector(Vector(posted, why)), Vector.empty)
      val askEntry = world.entries
        .get(EntryId("asker:t0:0"))(using TestTx.fake)
        .toOption
        .flatten
        .getOrElse(sys.error("the ask"))
      val oneLine = Shown
        .asked(askedIn.place, Vector(askEntry), grit.core.store.Speakers.none)
        .fold(sys.error("no section"))(CharEstimate.message)
      assemble(
        world,
        new Writer(Some("unused")),
        budget = 1000,
        at = 1,
        origin = thread("2.0"),
        tuning = Tuning.Default.copy(windowTokens = oneLine),
        others = Vector(askerAt(askedIn)),
        posted = Some(askedFor())
      ).nearby ==> Vector(Nearby.Asked(asker, askedIn.place, seqs(world, "asker:t0:0")))
    }

    test("a strand member is shown in the strand alone, never also as a section from afar") {
      val world = closed(Vector(Vector(ask)), Vector.empty, threads)
      val a = said(world, "1.0", "where did we land on the Engine contract term?" -> 120)
      // An open conversation in the room beside the strand, so the nearby search runs.
      val api = elsewhere(world, "api", close = false, exchange("the term?", "Twelve months."))
      val stitches = follow(world, a)
      val search = new Scripted()
      search.near = Vector((api, "api:t0:1", 1.5))
      val w = assemble(
        world,
        new Writer(Some("engine contract")),
        budget = 1000,
        search,
        at = 0,
        origin = thread("2.0"),
        stitches = Some(stitches)
      )
      (search.nearAsked.flatMap(_.conversations), w.nearby) ==> (
        Vector(api),
        Vector(
          Nearby.Open(api, thread("api").place, seqs(world, "api:t0:0", "api:t0:1")),
          Nearby.Along(a, thread("1.0").place, seqs(world, "1.0:0"))
        )
      )
    }

    test("with nothing open elsewhere, no query is written for a first turn") {
      val writer = new Writer(None)
      val alone = store(ask)
      assemble(alone, writer, budget = 1000, new Scripted(), at = 0).nearby ==> Vector()
      writer.requests ==> Vector()
    }

    test("the weight decides between an own turn and one elsewhere that both match") {
      // Nine fillers, so not every turn fits. Budget for the tail (turns 8 and 9, 22 tokens, and
      // the gap line before them, 12) and one more: turn 0 (22, and its gap line, 12) or api's
      // section (50), not both.
      def pick(weight: Double): (World, Window) = {
        val world = store((buried.take(1) ++ (1 to 9).map(filler) :+ ask)*)
        val api = elsewhere(world, "api", close = false, exchange("Which database?", "grit_agent."))
        val search = new Scripted("t0:1")
        search.scores = Vector(1.0)
        search.near = Vector((api, "api:t0:1", 1.5))
        val w = assemble(
          world,
          new Writer(Some("database")),
          budget = 84,
          search,
          at = 10,
          weight = Weight.of(weight).getOrElse(sys.error("w"))
        )
        (world, w)
      }
      val (one, weighted) = pick(2)
      (turnsOf(one, weighted), weighted.nearby.size) ==> (Vector("t0", "t8", "t9"), 0)
      val (other, even) = pick(1)
      (turnsOf(other, even), even.nearby.map(n => idsAt(other, n.conversation, n.names))) ==>
        (Vector("t8", "t9"), Vector(Vector("api:t0:0", "api:t0:1")))
    }

    test("when every earlier turn fits, the window is linear and no query is written") {
      val entries = store(buried*)
      val writer = new Writer(Some("unused"))
      assemble(entries, writer, budget = 1000) ==> linear(entries, 1000)
      writer.requests ==> Vector.empty
    }

    test(
      "older turns that match the query join the recent tail, in conversation order, noted as recalled"
    ) {
      // The fact, nine fillers and the ask at turn 10: every earlier turn is 121 tokens.
      val entries = store((buried.take(1) ++ (1 to 9).map(filler) :+ ask)*)
      val writer = new Writer(Some("database for probes: grit_agent"))
      // Ranked turn 2 above turn 0: the window still holds them oldest first.
      val search = new Scripted("t2:5", "t0:1")
      // The tail (22), turns 2 (11) and 0 (22), and a gap line before each of the three (36).
      val w = assemble(entries, writer, budget = 91, search, at = 10)
      turnsOf(entries, w) ==> Vector("t0", "t2", "t8", "t9")
      writer.requests ==> Vector(
        ModelRequest(
          QueryWriter.System,
          Vector(Message.User("New message:\nUser: Where should my probe point?"))
        )
      )
      search.asked ==>
        Vector(Asked(c1, TurnSeq.First, TurnSeq(8), "database for probes: grit_agent", Hits))
      w.notes ==> Vector(
        AssemblyNote.Queried(
          "database for probes: grit_agent",
          "writer",
          Usage(Tokens(40), Tokens(6), Tokens.Zero, None),
          writer.requests.headOption.map(CharEstimate.request).getOrElse(Tokens.Zero)
        ),
        AssemblyNote.Recalled(Vector(TurnSeq(0), TurnSeq(2)))
      )
    }

    test("a candidate not admitted is not in the window, and the next that fits takes its place") {
      val entries = store((buried.take(1) ++ (1 to 9).map(filler) :+ ask)*)
      // Turn 2 (11 tokens and its gap line) leaves no room for turn 0 (22 and its gap line).
      def drawn(admit: Results -> Results): Vector[String] = turnsOf(
        entries,
        assemble(
          entries,
          new Writer(Some("database for probes: grit_agent")),
          budget = 80,
          new Scripted("t2:5", "t0:1"),
          at = 10,
          admit = admit
        )
      )
      (
        drawn(r => r),
        drawn(r => Results(r.ranked.filterNot(_._1 == Retrieved.Own(TurnSeq(2)))))
      ) ==>
        (Vector("t2", "t8", "t9"), Vector("t0", "t8", "t9"))
    }

    test(
      "the results of a search are ranked as the window packs them, each with its weighted score"
    ) {
      val entries = store((buried.take(1) ++ (1 to 9).map(filler) :+ ask)*)
      val search = new Scripted("t2:5", "t0:1")
      // Search returns t2 before t0, but scores t0 higher: the window's ranking puts it first.
      search.scores = Vector(1.5, 3.0)
      val w = Weight.value(Weight.Default)
      retrieval(entries, 80, search).search(
        AssemblyRequest(TurnRef(c1, TurnSeq(10))),
        WrittenQuery
      )(using
        new FakeDb
      ) ==> Right(
        Results(Vector(Retrieved.Own(TurnSeq(0)) -> 3.0 * w, Retrieved.Own(TurnSeq(2)) -> 1.5 * w))
      )
    }

    test(
      "judge texts show the turn's message and each found candidate cut to the characters asked, and leave out one gone"
    ) {
      val entries = store((buried.take(1) ++ (1 to 9).map(filler) :+ ask)*)
      retrieval(entries, 80, new Scripted("t2:5", "t0:1")).judgeText(
        AssemblyRequest(TurnRef(c1, TurnSeq(10))),
        Vector(Retrieved.Own(TurnSeq(0)), Retrieved.Own(TurnSeq(42))),
        chars = 24
      )(using new FakeDb) ==> Right(
        JudgeText(
          "Where should my probe po",
          Vector(Retrieved.Own(TurnSeq(0)) -> "Which database do probes")
        )
      )
    }

    test(
      "what was found is read back in one read of its documents and one list per conversation elsewhere"
    ) {
      val world = store(ask)
      val api = elsewhere(
        world,
        "api",
        close = false,
        exchange("lunch?", "later"),
        exchange("flaky?", "TZ")
      )
      val docs = shelf("notes" -> 1.0)
      val a = write(docs, "notes", "a", placeOf("api"), "the probe budget, alpha")
      val b = write(docs, "notes", "b", placeOf("api"), "the probe budget, beta")
      val listing = new Listing(world.entries)
      val found = Vector(
        Retrieved.Doc(b),
        Retrieved.Near(api, TurnSeq(1)),
        Retrieved.Doc(a),
        Retrieved.Near(api, TurnSeq(0))
      )
      val texts = retrieval(
        world,
        1000,
        new Scripted(),
        documents = docs,
        entries = Some(listing)
      ).judgeText(AssemblyRequest(TurnRef(c1, TurnSeq(0))), found, chars = 40)(using new FakeDb)
      (
        texts.map(_.found.map(_._1)),
        docs.reads,
        listing.listed.count(_ == api)
      ) ==> (Right(found), Vector(Vector(b, a)), 1)
    }

    test("after a close, the closings come first and the search stays in the open period") {
      // Period 1 is the buried fact's turn, closed; period 2 is turns 1 to 7, the ask last.
      val w = closed(
        Vector(Vector(buried(0)), (1 to 6).map(filler).toVector :+ ask),
        Vector("Probes use grit_agent.")
      )
      val search = new Scripted("t2:6")
      // The record (28), the tail (22) and turn 2 (11), each turn with a gap line (24); less
      // than the record and every turn of the period (94).
      val got = assemble(w, new Writer(Some("probes database")), budget = 88, search, at = 7)
      ids(w, got).headOption ==> Some(closingOf(1))
      turnsOf(w, got).drop(1) ==> Vector("t2", "t5", "t6")
      search.asked.map(a => (a.from, a.before)) ==> Vector((TurnSeq(1), TurnSeq(5)))
    }

    test("a match brings its turn's messages whole, never a summary") {
      val summarised = exchange("hmm", "ok") :+ Payload.Summary("probes use grit_agent")
      val turns = Vector(summarised) ++ (1 to 5).map(filler) :+ ask
      // t0:2 is the summary.
      val world = store(turns*)
      val w = assemble(world, new Writer(Some("grit_agent")), budget = 60, new Scripted("t0:2"))
      ids(world, w) ==> Vector("t0:0", "t0:1", "t4:9", "t4:10", "t5:11", "t5:12")
    }

    test("each recalled turn is charged a gap line, and the tail one for the turns it leaves out") {
      // The tail (turns 4 and 5) costs 22, and a gap line 12 (Shown.Gap): 34 before turn 0,
      // which costs 11, and 12 more for the gap line it brings. So 57 recalls it; 56 does not.
      val gap = Tokens.value(CharEstimate.message(Shown.Gap))
      gap ==> 12L
      val turns = (0 to 5).map(filler).toVector :+ ask
      def at(budget: Long): Vector[String] = {
        val world = store(turns*)
        turnsOf(world, assemble(world, new Writer(Some("q000")), budget, new Scripted("t0:1")))
      }
      at(57) ==> Vector("t0", "t4", "t5")
      at(56) ==> Vector("t4", "t5")
    }

    test("a width within a budget draws the window in it and searches for its hits") {
      // As above: turn 0 is recalled within 57 and not within 56, whatever it was built for.
      val turns = (0 to 5).map(filler).toVector :+ ask
      def at(built: Long, width: Width): (Vector[String], Vector[Int]) = {
        val world = store(turns*)
        val search = new Scripted("t0:1")
        val w = assemble(world, new Writer(Some("q000")), built, search, width = width)
        (turnsOf(world, w), search.asked.map(_.limit))
      }
      at(57, Width.Within(Tokens(56), 3)) ==> (Vector("t4", "t5"), Vector(3))
      at(56, Width.Within(Tokens(57), 3)) ==> (Vector("t0", "t4", "t5"), Vector(3))
      // Deployed is the window of the budget and hits it was built with.
      at(57, Width.Deployed) ==> at(57, Width.Within(Tokens(57), Hits))
      at(56, Width.Deployed) ==> at(56, Width.Within(Tokens(56), Hits))
    }

    test("a turn elsewhere is costed as its section shows it, its speaker named") {
      // The least budget whose window holds api's section, its question said by `name`.
      def least(name: String): Long = {
        def drawn(budget: Long): Boolean = {
          val world = store(ask)
          val api = elsewhere(world, "api", close = false, exchange("the freeze?", "Friday."))
          grit.assembly.linear.AssemblyFixtures.named(world, "api:t0:0", name)
          val search = new Scripted()
          search.near = Vector((api, "api:t0:1", 2.0))
          assemble(world, new Writer(Some("freeze")), budget, search, at = 0).nearby.nonEmpty
        }
        (1L to 400L).find(drawn).getOrElse(sys.error("never drawn"))
      }
      // Sixteen more characters of name: four more tokens.
      least("Anastasia-Beatrix") - least("A") ==> 4L
    }

    test("a recalled turn's named message is charged its name line") {
      // As above, turn 0 recalled at 57; named "Ana", its question costs 3 tokens more.
      val turns = (0 to 5).map(filler).toVector :+ ask
      def at(budget: Long): Vector[String] = {
        val world = store(turns*)
        grit.assembly.linear.AssemblyFixtures.named(world, "t0:0", "Ana")
        turnsOf(world, assemble(world, new Writer(Some("q000")), budget, new Scripted("t0:1")))
      }
      at(59) ==> Vector("t4", "t5")
      at(60) ==> Vector("t0", "t4", "t5")
    }

    test("a match that does not fit what is left is passed over for one that does") {
      val big = exchange("Which database do probes use? " + ("and why " * 30), "grit_agent.")
      val small = exchange("probes db?", "grit_agent")
      val turns = Vector(big, small) ++ (2 to 5).map(filler) :+ ask
      // The big turn ranks first and cannot fit in the 38 tokens the tail leaves.
      val search = new Scripted("t0:1", "t1:3")
      val world = store(turns*)
      val w = assemble(world, new Writer(Some("probes grit_agent")), budget = 60, search)
      turnsOf(world, w) ==> Vector("t1", "t4", "t5")
    }

    test("a blank query, or a failed writer, falls back to the linear window and says why") {
      val entries = store(buried*)
      val blank = new Writer(Some("  \n "))
      val none = assemble(entries, blank, budget = 60)
      none ==> Window(
        linear(entries, 60).entries,
        Vector(
          AssemblyNote.Queried(
            "",
            "writer",
            Usage(Tokens(40), Tokens(6), Tokens.Zero, None),
            blank.requests.headOption.map(CharEstimate.request).getOrElse(Tokens.Zero)
          ),
          AssemblyNote.FellBack("the query was blank")
        )
      )

      val failed = assemble(entries, new Writer(None), budget = 60)
      failed ==> Window(
        linear(entries, 60).entries,
        Vector(AssemblyNote.FellBack("no query: down"))
      )
    }

    test("with no query pin, no query is written: the linear window, noted so") {
      val entries = store(buried*)
      val writer = new Writer(Some("probes grit_agent"))
      val unasked = assemble(entries, writer, budget = 60, query = None)
      (unasked, writer.requests.size) ==> (
        Window(
          linear(entries, 60).entries,
          Vector(AssemblyNote.FellBack(RetrievalAssembler.NoPin))
        ),
        0
      )
    }

    test("the query is written under the request's pin alone, and notes who served it") {
      val entries = store(buried*)
      val other =
        assemble(entries, new Writer(Some("probes")), budget = 60, query = Some(pin("x/y")))
      other.notes ==> Vector(AssemblyNote.FellBack("no query: asked under x/y"))
      val served = new Writer(Some("probes"), upstream = Some("Fireworks"))
      assemble(entries, served, budget = 60).notes.collect { case q: AssemblyNote.Queried =>
        q.upstream
      } ==> Vector(Some("Fireworks"))
    }

    test("a document is drawn, ranked by its score times its plugin's weight") {
      val docs = shelf("notes" -> 1.0, "rollup" -> 4.0)
      val alpha = write(docs, "notes", "a", placeOf("api"), "the probe budget, alpha")
      val beta = write(docs, "rollup", "b", placeOf("ops"), "the probe budget, beta")
      // Unweighted, alpha ranks first; beta's weight lifts it above: 1 × 4 over 3 × 1.
      docs.scores = Map("a" -> 3.0, "b" -> 1.0)
      val w = assemble(store(ask), new Writer(Some("probe budget")), 1000, at = 0, documents = docs)
      w.documents ==> Vector(beta, alpha)
      docs.asked.map(a => (a.shelves.toSet, a.query, a.limit, a.at)) ==> Vector(
        (
          Set(Shelved(plugin("notes"), placeOf("api")), Shelved(plugin("rollup"), placeOf("ops"))),
          "probe budget",
          Hits,
          Instant.EPOCH
        )
      )
    }

    test("a conversation with only a document to draw writes a query; with none, not") {
      def run(written: Boolean): (Writer, Window) = {
        val docs = shelf("notes" -> 1.0)
        if (written) { val _ = write(docs, "notes", "a", placeOf("api"), "the probe budget") }
        val writer = new Writer(Some("probe budget"))
        (
          writer,
          assemble(
            store(ask),
            writer,
            1000,
            at = 0,
            documents = docs
          )
        )
      }
      val (asked, drawn) = run(written = true)
      (asked.requests.size, drawn.documents.size) ==> (1, 1)
      val (off, none) = run(written = false)
      (off.requests.size, none.documents.size) ==> (0, 0)
    }

    test("a turn elsewhere and a document are both drawn, whatever the conversation is named") {
      // A conversation's id is any text, "document:" too; its turn 1 and version 1 are apart.
      val world = store(ask)
      val named = elsewhere(
        world,
        "document:",
        close = false,
        exchange("lunch?", "later"),
        exchange("the probe budget?", "Ten a day.")
      )
      val docs = shelf("notes" -> 1.0)
      val v = write(docs, "notes", "a", placeOf("api"), "the probe budget")
      val search = new Scripted()
      search.near = Vector((named, "document::t1:3", 2.0))
      val w =
        assemble(world, new Writer(Some("probe budget")), 1000, search, at = 0, documents = docs)
      (DocumentVersionId.value(v), w.nearby.map(_.conversation), w.documents) ==>
        (1L, Vector(named), Vector(v))
    }

    test(
      "a piece ranks beside the turns and documents found, by its score times its plugin's weight"
    ) {
      val docs = shelf("notes" -> 1.0, "files" -> 4.0)
      val alpha = write(docs, "notes", "a", placeOf("api"), "the database for probes")
      val pieces = new InMemoryPieces
      val ids = kept(pieces, "files", "plan", "probes use a database", "nothing here")
      val entries = store((buried.take(1) ++ (1 to 9).map(filler) :+ ask)*)
      val search = new Scripted("t0:1")
      // The own turn scores 3 × the default weight, 2; the piece 2 words × 4; the document 3 × 1.
      search.scores = Vector(3.0)
      val found = retrieval(entries, 80, search, documents = docs, pieces = pieces).search(
        AssemblyRequest(TurnRef(c1, TurnSeq(10))),
        WrittenQuery
      )(using new FakeDb)
      found.map(_.ranked.map(_._1)) ==> Right(
        Vector(
          Retrieved.Piece(ids.head, 0),
          Retrieved.Own(TurnSeq(0)),
          Retrieved.Doc(alpha)
        )
      )
    }

    test("a window draws at most PerFile pieces of one file, saying how many more matched") {
      val docs = shelf("files" -> 1.0)
      val pieces = new InMemoryPieces
      val ids = kept(pieces, "files", "plan", (1 to 11).map(i => s"probe budget part $i")*)
      val w = assemble(
        store(ask),
        new Writer(Some("probe budget")),
        4000,
        at = 0,
        width = Width.Within(Tokens(4000), 30),
        documents = docs,
        pieces = pieces
      )
      w.pieces ==> ids.take(PieceSearch.PerFile).map(Retrieved.Piece(_, 3))
    }

    test("a conversation with only a piece to draw writes a query; with none, not") {
      def run(written: Boolean): (Writer, Window) = {
        val docs = shelf("files" -> 1.0)
        val pieces = new InMemoryPieces
        if (written) { val _ = kept(pieces, "files", "plan", "the probe budget") }
        val writer = new Writer(Some("probe budget"))
        (writer, assemble(store(ask), writer, 1000, at = 0, documents = docs, pieces = pieces))
      }
      val (asked, drawn) = run(written = true)
      (asked.requests.size, drawn.pieces.size) ==> (1, 1)
      val (off, none) = run(written = false)
      (off.requests.size, none.pieces.size) ==> (0, 0)
    }

    test("a piece that does not fit is passed over, costed as the window shows it") {
      val docs = shelf("files" -> 1.0)
      val pieces = new InMemoryPieces
      val big = kept(pieces, "files", "big", "probe budget " + ("word " * 400))
      val small = kept(pieces, "files", "small", "the probe budget")
      val w = assemble(
        store(ask),
        new Writer(Some("probe budget")),
        300,
        at = 0,
        documents = docs,
        pieces = pieces
      )
      (big.size, w.pieces.map(_.id)) ==> (1, small)
    }

    test("the query writer is told its search also finds passages of the files kept") {
      QueryWriter.System.contains("passages of the files and documents kept") ==> true
    }

    test("a plugin not declared is never searched") {
      val docs = shelf("notes" -> 1.0)
      write(docs, "notes", "a", placeOf("api"), "the probe budget")
      write(docs, "undeclared", "b", placeOf("api"), "the probe budget")
      assemble(store(ask), new Writer(Some("probe budget")), 1000, at = 0, documents = docs)
      docs.asked.map(_.shelves) ==> Vector(Vector(Shelved(plugin("notes"), placeOf("api"))))
    }

    test("a document written after the turn's root is not drawn; one written before it is") {
      val docs = shelf("notes" -> 1.0)
      val before = write(docs, "notes", "a", placeOf("api"), "the probe budget")
      write(docs, "notes", "b", placeOf("api"), "the probe budget", Instant.EPOCH.plusSeconds(60))
      val w = assemble(store(ask), new Writer(Some("probe budget")), 1000, at = 0, documents = docs)
      (w.documents, docs.asked.map(_.at)) ==> (Vector(before), Vector(Instant.EPOCH))
    }

    test(
      "a conversation with no entries draws no document and writes no query, though one is shelved"
    ) {
      val docs = shelf("notes" -> 1.0)
      write(docs, "notes", "a", placeOf("api"), "the probe budget")
      val writer = new Writer(Some("probe budget"))
      val w = assemble(store(), writer, 1000, at = 0, documents = docs)
      (w.documents, docs.asked, writer.requests.size) ==> (Vector(), Vector(), 0)
    }

    test(
      "a document that does not fit is passed over for the next, costed as the window shows it"
    ) {
      val docs = shelf("notes" -> 1.0)
      write(docs, "notes", "big", placeOf("api"), "the probe budget " + ("and why " * 30))
      val small = write(docs, "notes", "small", placeOf("api"), "the probe budget")
      docs.scores = Map("big" -> 2.0, "small" -> 1.0)
      // A first turn spends nothing before the pool: the budget is what the documents may cost.
      def at(budget: Long) =
        assemble(
          store(ask),
          new Writer(Some("probe budget")),
          budget,
          at = 0,
          documents = docs
        ).documents
      (at(docCost(docs, small)), at(docCost(docs, small) - 1)) ==> (Vector(small), Vector())
    }

    test(
      "a document tied with a turn of the conversation's own, or one elsewhere, ranks after it"
    ) {
      // Own: the tail (turns 4 and 5) and its gap line cost 34, turn 0 and its gap line 34 more;
      // the budget holds the tail and one of turn 0 and the document, not both.
      val ownDocs = shelf("notes" -> 1.0)
      val ownDoc = write(ownDocs, "notes", "a", placeOf("api"), "probe database")
      ownDocs.scores = Map("a" -> 2.0)
      val buriedWorld = store(buried*)
      val search = new Scripted("t0:1")
      // 1 × the default weight, 2: the document's score.
      search.scores = Vector(1.0)
      val mine = assemble(
        buriedWorld,
        new Writer(Some("probe database")),
        34 + math.max(34, docCost(ownDocs, ownDoc)),
        search,
        documents = ownDocs
      )
      (turnsOf(buriedWorld, mine), mine.documents) ==> (Vector("t0", "t4", "t5"), Vector())

      // Elsewhere: a first turn, the budget holding one of api's section and the document.
      val world = store(ask)
      val api = elsewhere(world, "api", close = false, exchange("probe database?", "grit_agent"))
      val near = new Scripted()
      near.near = Vector((api, "api:t0:1", 2.0))
      val docs = shelf("notes" -> 1.0)
      val doc = write(docs, "notes", "a", placeOf("ops"), "probe database")
      docs.scores = Map("a" -> 2.0)
      val section = world.entries
        .list(api)(using TestTx.fake)
        .toOption
        .flatMap(Shown.nearby(placeOf("api"), _, Speakers.none))
        .fold(sys.error("no section"))(m => Tokens.value(CharEstimate.message(m)))
      val theirs = assemble(
        world,
        new Writer(Some("probe database")),
        math.max(section, docCost(docs, doc)),
        near,
        at = 0,
        documents = docs
      )
      (theirs.nearby.map(_.conversation), theirs.documents) ==> (Vector(api), Vector())
    }

    test("a blank query, or a failed writer, falls back to a window holding no document") {
      def run(writer: Writer): (ScriptedDocs, Window) = {
        val docs = shelf("notes" -> 1.0)
        write(docs, "notes", "a", placeOf("api"), "the probe budget")
        (docs, assemble(store(ask), writer, 1000, at = 0, documents = docs))
      }
      val (failedDocs, failed) = run(new Writer(None))
      (failed, failedDocs.asked) ==> (
        Window(Vector(), Vector(AssemblyNote.FellBack("no query: down"))),
        Vector()
      )
      val (blankDocs, blank) = run(new Writer(Some(" ")))
      (blank.documents, blank.notes.lastOption, blankDocs.asked) ==>
        (Vector(), Some(AssemblyNote.FellBack("the query was blank")), Vector())
    }

    test("the query model is shown the turn's own messages, one line each, and nothing else") {
      def entry(seq: Long, payload: Payload) =
        Entry(
          EntryId(s"t6:$seq"),
          c1,
          TurnSeq(6),
          None,
          EntrySeq(seq),
          payload,
          Instant.EPOCH,
          Integrity.Member
        )
      QueryWriter.request(
        Vector(
          entry(13, Payload.Message(Message.User("Where should my probe point?"))),
          entry(14, Payload.Summary("not shown")),
          entry(15, Payload.Message(Message.User("And the port?")))
        )
      ) ==> ModelRequest(
        QueryWriter.System,
        Vector(
          Message.User("New message:\nUser: Where should my probe point?\nUser: And the port?")
        )
      )
    }

    test("a turn rooted on a heard message: the query model is shown it as overheard") {
      QueryWriter.request(
        Vector(
          Entry(
            EntryId("t7:16"),
            c1,
            TurnSeq(7),
            None,
            EntrySeq(16),
            Payload.Heard("is the freeze still on?"),
            Instant.EPOCH,
            Integrity.Member
          )
        )
      ) ==> ModelRequest(
        QueryWriter.System,
        Vector(Message.User("New message:\nOverheard: is the freeze still on?"))
      )
    }

    test("the query is the reply's text on one line") {
      QueryWriter.text(reply("  postgres\n  sqlite   decision ")) ==> "postgres sqlite decision"
      QueryWriter.text(reply(" \n ")) ==> ""
    }

    test("a control character in the reply, such as NUL, is a space in the query") {
      // Seen live: a query model wrote a NUL, and Postgres refuses one in a text parameter,
      // which failed the turn's assembly.
      QueryWriter.text(reply("utc\u0000date\u0007 picker")) ==> "utc date picker"
    }
  }
}
