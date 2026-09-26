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
import grit.core.context.{AssemblyNote, AssemblyRequest, Window}
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.LifecycleSettings
import grit.core.place.{Locality, Place, Scope, Weight}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{
  Entry,
  EntrySearch,
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
    * it was asked. The ranking is the test's, not a scorer's whose ties would decide it.
    */
  private final class Scripted(ids: String*) extends EntrySearch {
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
      locality: Locality = Locality.Default
  ): Window = {
    val turn = TurnRef(c1, TurnSeq(at))
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
      world.periods,
      lifecycle,
      search,
      writer,
      CharEstimate,
      Tokens(budget),
      Tokens(25),
      Hits
    )
      .assemble(AssemblyRequest(turn))(using new FakeDb)
      .getOrElse(sys.error("in-memory store"))
  }

  private def ids(w: Window): Vector[String] = w.entries.map(EntryId.value)

  private def turnsOf(w: Window): Vector[String] = ids(w).map(_.takeWhile(_ != ':')).distinct

  private def linear(world: World, budget: Long): Window =
    new LinearAssembler(world.entries, world.periods, CharEstimate, Tokens(budget))
      .assemble(AssemblyRequest(TurnRef(c1, TurnSeq(6))))(using new FakeDb)
      .getOrElse(sys.error("in-memory store"))

  private def placeOf(name: String): Place = Origin.Task("conversation", name).place

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
        Nearby(api, placeOf("api"), Vector(EntryId("api:t0:0"), EntryId("api:t0:1")))
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
      val scope = Scope(Vector(placeOf("kept")))
      assemble(world, new Writer(Some("q")), 1000, search, at = 0, Locality(scope, Weight.Default))
      search.nearAsked.map(_.conversations) ==> Vector(List(kept))
    }

    test("the weight decides between an own turn and one elsewhere that both match") {
      // Budget for the tail (turns 4 and 5, 22 tokens) and one more turn: turn 0 (22) or
      // api's section (34), not both.
      def pick(weight: Double): Window = {
        val world = store(buried*)
        val api = elsewhere(world, "api", close = false, exchange("Which database?", "grit_agent."))
        val search = new Scripted("t0:1")
        search.scores = Vector(1.0)
        search.near = Vector((api, "api:t0:1", 1.5))
        assemble(
          world,
          new Writer(Some("database")),
          budget = 56,
          search,
          locality = Locality(Scope.Everywhere, Weight.of(weight).getOrElse(sys.error("w")))
        )
      }
      val weighted = pick(2)
      (turnsOf(weighted), weighted.nearby.size) ==> (Vector("t0", "t4", "t5"), 0)
      val even = pick(1)
      (turnsOf(even), even.nearby.map(_.entries.map(EntryId.value))) ==>
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
      val entries = store(buried*)
      val writer = new Writer(Some("database for probes: grit_agent"))
      // Ranked turn 2 above turn 0: the window still holds them oldest first.
      val search = new Scripted("t2:5", "t0:1")
      val w = assemble(entries, writer, budget = 60, search)
      turnsOf(w) ==> Vector("t0", "t2", "t4", "t5")
      writer.requests ==> Vector(
        ModelRequest(
          QueryWriter.System,
          Vector(Message.User("New message:\nUser: Where should my probe point?"))
        )
      )
      search.asked ==>
        Vector(Asked(c1, TurnSeq.First, TurnSeq(4), "database for probes: grit_agent", Hits))
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
      val got = assemble(w, new Writer(Some("probes database")), budget = 60, search, at = 7)
      ids(got).headOption ==> Some(closingOf(1))
      turnsOf(got).drop(1) ==> Vector("t2", "t5", "t6")
      search.asked.map(a => (a.from, a.before)) ==> Vector((TurnSeq(1), TurnSeq(5)))
    }

    test("a match brings its turn's messages whole, never a summary") {
      val summarised = exchange("hmm", "ok") :+ Payload.Summary("probes use grit_agent")
      val turns = Vector(summarised) ++ (1 to 5).map(filler) :+ ask
      // t0:2 is the summary.
      val w =
        assemble(store(turns*), new Writer(Some("grit_agent")), budget = 60, new Scripted("t0:2"))
      ids(w) ==> Vector("t0:0", "t0:1", "t4:9", "t4:10", "t5:11", "t5:12")
    }

    test("a match that does not fit what is left is passed over for one that does") {
      val big = exchange("Which database do probes use? " + ("and why " * 30), "grit_agent.")
      val small = exchange("probes db?", "grit_agent")
      val turns = Vector(big, small) ++ (2 to 5).map(filler) :+ ask
      // The big turn ranks first and cannot fit in the 38 tokens the tail leaves.
      val search = new Scripted("t0:1", "t1:3")
      val w = assemble(store(turns*), new Writer(Some("probes grit_agent")), budget = 60, search)
      turnsOf(w) ==> Vector("t1", "t4", "t5")
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
        Entry(EntryId(s"t6:$seq"), c1, TurnSeq(6), None, seq, payload, Instant.EPOCH)
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
