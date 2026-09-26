package grit.assembly.retrieval

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.AssemblyFixtures.{FakeDb, World, c1, closed, closingOf, store}
import grit.assembly.linear.LinearAssembler
import grit.core.context.{AssemblyNote, AssemblyRequest, Window}
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, EntrySearch, Payload, StoreError, Tx}

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

  /** One search the assembler made. */
  private final case class Asked(
      conversation: ConversationId,
      from: TurnSeq,
      before: TurnSeq,
      query: String,
      limit: Int
  )

  /** Answers every search with the entries `ids` (`t{turn}:{seq}`), best first in the order
    * given, whatever it is asked; keeps what it was asked. The ranking is the test's, not a
    * scorer's whose ties would decide it.
    */
  private final class Scripted(ids: String*) extends EntrySearch {
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
        EntrySearch.Hit(EntryId(id), TurnSeq(turn), (ids.size - rank).toDouble)
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
      at: Long = 6
  ): Window = {
    val turn = TurnRef(c1, TurnSeq(at))
    new RetrievalAssembler(
      world.entries,
      world.periods,
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

  val tests = Tests {

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
      val none = assemble(entries, new Writer(Some("  \n ")), budget = 60)
      none.entries ==> linear(entries, 60).entries
      none.notes.collect { case AssemblyNote.FellBack(why) => why } ==> Vector(
        "the query was blank"
      )
      assert(none.notes.exists {
        case AssemblyNote.Queried("", "writer", _, _) => true
        case _ => false
      })

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
  }
}
