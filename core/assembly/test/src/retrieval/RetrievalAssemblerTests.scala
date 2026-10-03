package grit.assembly.retrieval

import java.time.Instant

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
import grit.core.context.{AssemblyNote, AssemblyRequest, Shown, Width, Window}
import grit.core.id.{
  CallSlot,
  CloseRef,
  ConversationId,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  PrincipalId,
  TurnRef,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, LifecycleSettings, Probability, TestClosings}
import grit.core.place.{Locality, Place, Prefix, Scope, Weight}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.stitch.{InMemoryStitchStore, StitchJson, StitchStore, Tuning}
import grit.core.store.{
  ClosingEntry,
  Conversation,
  Entry,
  EntrySearch,
  InMemoryConversationStore,
  InMemoryLifecycleStore,
  Nearby,
  OpenPeriod,
  Origin,
  Payload,
  StoreError,
  Tx
}
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
  private final class Writer(answer: Option[String]) extends Provider {
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      requests = requests :+ request
      answer.map(reply(_, "writer")).toRight(ProviderError.Unavailable("down"))
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

  /** Turn 6's window. With a 25-token tail, the recent turns are 4 and 5. */
  private def assemble(
      world: World,
      writer: Provider^,
      budget: Long,
      search: EntrySearch = new Scripted(),
      at: Long = 6,
      locality: Locality = Locality.Default,
      origin: Origin = Origin.Task("conversation", "c1"),
      stitches: Option[StitchStore] = None,
      tuning: Tuning = Tuning.Default,
      others: Vector[Conversation] = Vector.empty,
      posted: Option[CallSlot] = None,
      width: Width = Width.Deployed
  ): Window = {
    val turn = TurnRef(c1, TurnSeq(at))
    val conversations = new InMemoryConversationStore
    conversations.all = Conversation(c1, origin, PrincipalId.Local, Instant.EPOCH) +: others
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
            locality
          )
          .getOrElse(sys.error("settings"))
      )(using TestTx.fake)
      .getOrElse(sys.error("in-memory store"))
    new RetrievalAssembler(
      world.entries,
      conversations,
      world.periods,
      world.principals,
      lifecycle,
      search,
      stitches.getOrElse(
        new InMemoryStitchStore(
          world.entries,
          c => Origin.Task("conversation", ConversationId.value(c))
        )
      ),
      writer,
      CharEstimate,
      Tokens(budget),
      Tokens(25),
      tuning,
      Hits
    )
      .assemble(AssemblyRequest(turn, width))(using new FakeDb)
      .getOrElse(sys.error("in-memory store"))
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
      .assemble(AssemblyRequest(TurnRef(c1, TurnSeq(6))))(using new FakeDb)
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
        Entry(EntryId(s"$name:$i"), c, next.turnSeq, None, next.seq, Payload.Heard(text), at)
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
        Instant.EPOCH
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
      "a closing elsewhere in scope is a candidate: a first turn writes a query, and a match shows the newest kept record"
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
          Instant.EPOCH
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

    test(
      "under scope room a thread draws on its own channel's threads, open and closed, and not another channel's"
    ) {
      val channel = (c: ConversationId) =>
        ConversationId.value(c) match {
          case "c1" => Origin.Slack("T1", "C1", "1.0")
          case "ops" => Origin.Slack("T1", "C1", "2.0")
          case "api" => Origin.Slack("T1", "C1", "3.0")
          case other => Origin.Slack("T1", "C2", other)
        }
      val world = closed(Vector(Vector(ask)), Vector.empty, channel)
      val ops = elsewhere(world, "ops", close = true, exchange("freeze?", "Friday"))
      val api = elsewhere(world, "api", close = false, exchange("flaky?", "TZ"))
      val far = elsewhere(world, "far", close = true, exchange("freeze?", "Monday"))
      val near = elsewhere(world, "near", close = false, exchange("flaky?", "TZ"))
      val search = new Scripted()
      assemble(
        world,
        new Writer(Some("freeze")),
        budget = 1000,
        search,
        at = 0,
        Locality(Scope.Room, Weight.Default),
        Origin.Slack("T1", "C1", "1.0")
      )
      (search.nearAsked.map(_.conversations), search.closedAsked) ==>
        (Vector(List(api)), Vector(List(ops)))
      val _ = (far, near)
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
      // 45 tokens: the section with all three costs 55, with the opening and "lol" 41.
      shown(45) ==> Vector(Nearby.Along(a, thread("1.0").place, Vector(strand(0), strand(2))))
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
      Conversation(asker, origin, PrincipalId.Local, Instant.EPOCH)

    test(
      "a thread begun by grit's post is shown first the turn that asked for it, out of its room's scope too, and the asker is not also ranked"
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
        locality = Locality(Scope.Everywhere, Weight.Default),
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
        locality = Locality(Scope.Room, Weight.Default),
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
          locality = Locality(Scope.Off, Weight.Default),
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
        locality = Locality(Scope.Off, Weight.Default),
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
        locality = Locality(Scope.Off, Weight.Default),
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

    test("with the scope off, or nothing open elsewhere, no query is written for a first turn") {
      val world = store(ask)
      elsewhere(world, "api", close = false, exchange("flaky", "TZ"))
      val writer = new Writer(None)
      assemble(
        world,
        writer,
        budget = 1000,
        new Scripted(),
        at = 0,
        Locality(Scope.Off, Weight.Default)
      ).nearby ==> Vector()
      val alone = store(ask)
      assemble(alone, writer, budget = 1000, new Scripted(), at = 0).nearby ==> Vector()
      writer.requests ==> Vector()
    }

    test("a place out of scope is never searched") {
      val world = store(ask)
      elsewhere(world, "gone", close = true, exchange("flaky", "TZ"))
      val kept = elsewhere(world, "kept", close = false, exchange("flaky", "TZ"))
      elsewhere(world, "far", close = false, exchange("flaky", "TZ"))
      val search = new Scripted()
      val scope = Scope(Vector(Prefix.At(placeOf("kept"))))
      assemble(world, new Writer(Some("q")), 1000, search, at = 0, Locality(scope, Weight.Default))
      search.nearAsked.map(_.conversations) ==> Vector(List(kept))
    }

    test("the weight decides between an own turn and one elsewhere that both match") {
      // Budget for the tail (turns 4 and 5, 22 tokens, and the gap line before them, 12) and
      // one more turn: turn 0 (22, and its gap line, 12) or api's section (38), not both.
      def pick(weight: Double): (World, Window) = {
        val world = store(buried*)
        val api = elsewhere(world, "api", close = false, exchange("Which database?", "grit_agent."))
        val search = new Scripted("t0:1")
        search.scores = Vector(1.0)
        search.near = Vector((api, "api:t0:1", 1.5))
        val w = assemble(
          world,
          new Writer(Some("database")),
          budget = 72,
          search,
          locality = Locality(Scope.Everywhere, Weight.of(weight).getOrElse(sys.error("w")))
        )
        (world, w)
      }
      val (one, weighted) = pick(2)
      (turnsOf(one, weighted), weighted.nearby.size) ==> (Vector("t0", "t4", "t5"), 0)
      val (other, even) = pick(1)
      (turnsOf(other, even), even.nearby.map(n => idsAt(other, n.conversation, n.names))) ==>
        (Vector("t4", "t5"), Vector(Vector("api:t0:0", "api:t0:1")))
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

    test("the query model is shown the turn's own messages, one line each, and nothing else") {
      def entry(seq: Long, payload: Payload) =
        Entry(EntryId(s"t6:$seq"), c1, TurnSeq(6), None, EntrySeq(seq), payload, Instant.EPOCH)
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
            Instant.EPOCH
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
