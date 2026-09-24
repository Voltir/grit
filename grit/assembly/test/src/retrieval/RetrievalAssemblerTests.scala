package grit.assembly.retrieval

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.context.{AssemblyNote, AssemblyRequest, Window}
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{
  Db,
  Entry,
  EntryStore,
  InMemoryEntrySearch,
  InMemoryEntryStore,
  Payload,
  StoreError,
  Tx
}
import grit.dbos.sql.TestTx

import utest.*

object RetrievalAssemblerTests extends TestSuite {

  private val c1 = ConversationId("c1")

  private object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

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

  /** `turns` in a fresh store, turn `t` holding the `t`-th payloads, ids `t{turn}:{seq}`. */
  private def store(turns: Vector[Payload]*): EntryStore = {
    val entries = new InMemoryEntryStore
    given Tx = TestTx.fake
    for (turn <- turns.indices; payload <- turns(turn)) {
      val next = entries.lockNext(c1).getOrElse(sys.error("in-memory store"))
      val _ = entries.insert(
        Entry(
          EntryId(s"t$turn:${next.seq}"),
          c1,
          TurnSeq(turn.toLong),
          None,
          next.seq,
          payload,
          Instant.EPOCH
        )
      )
    }
    entries
  }

  private def exchange(question: String, answer: String): Vector[Payload] =
    Vector(Payload.Message(Message.User(question)), Payload.Message(reply(answer)))

  /** A four-character question and an eight-character answer: 11 estimated tokens. */
  private def filler(n: Int): Vector[Payload] = exchange(f"q$n%03d", f"answer$n%02d")

  /** The fact at turn 0 (22 estimated tokens), five fillers, and the ask at turn 6. */
  private def buried: Vector[Vector[Payload]] =
    Vector(exchange("Which database do probes use?", "grit_agent, never grit.")) ++
      (1 to 5).map(filler) :+
      Vector(Payload.Message(Message.User("Where should my probe point?")))

  private def assemble(
      entries: EntryStore,
      writer: Provider^,
      budget: Long,
      tail: Long = 25
  ): Window = {
    val turn = TurnRef(c1, TurnSeq(6))
    new RetrievalAssembler(
      entries,
      new InMemoryEntrySearch(entries),
      writer,
      CharEstimate,
      Tokens(budget),
      Tokens(tail)
    )
      .assemble(AssemblyRequest(turn))(using FakeDb)
      .getOrElse(sys.error("in-memory store"))
  }

  private def ids(w: Window): Vector[String] = w.entries.map(EntryId.value)

  private def turnsOf(w: Window): Vector[String] = ids(w).map(_.takeWhile(_ != ':')).distinct

  private def linear(entries: EntryStore, budget: Long): Window =
    new LinearAssembler(entries, CharEstimate, Tokens(budget))
      .assemble(AssemblyRequest(TurnRef(c1, TurnSeq(6))))(using FakeDb)
      .getOrElse(sys.error("in-memory store"))

  val tests = Tests {

    test("when every earlier turn fits, the window is linear and no query is written") {
      val entries = store(buried*)
      val writer = new Writer(Some("unused"))
      assemble(entries, writer, budget = 1000) ==> linear(entries, 1000)
      writer.requests ==> Vector.empty
    }

    test(
      "an older turn that matches the query joins the recent tail, in conversation order, noted as recalled"
    ) {
      val entries = store(buried*)
      val writer = new Writer(Some("database for probes: grit_agent"))
      val w = assemble(entries, writer, budget = 60)
      turnsOf(w) ==> Vector("t0", "t4", "t5")
      val asked = writer.requests.headOption
      asked.map(_.system) ==> Some(QueryWriter.System)
      assert(
        asked.exists(
          _.messages.toString.contains("New message:\nUser: Where should my probe point?")
        )
      )
      assert(asked.exists(!_.messages.toString.contains("q005")))
      w.notes ==> Vector(
        AssemblyNote.Queried(
          "database for probes: grit_agent",
          "writer",
          Usage(Tokens(40), Tokens(6), Tokens.Zero, None),
          asked.map(CharEstimate.request).getOrElse(Tokens.Zero)
        ),
        AssemblyNote.Recalled(Vector(TurnSeq(0)))
      )
    }

    test("a match on a reply brings its whole turn") {
      val w = assemble(store(buried*), new Writer(Some("grit_agent")), budget = 60)
      ids(w).take(2) ==> Vector("t0:0", "t0:1")
    }

    test("a match that does not fit what is left is passed over for one that does") {
      val big = exchange("Which database do probes use? " + ("and why " * 30), "grit_agent.")
      val small = exchange("probes db?", "grit_agent")
      val turns = Vector(big, small) ++ (2 to 5).map(filler) :+
        Vector(Payload.Message(Message.User("Where should my probe point?")))
      val entries = store(turns*)
      val w = assemble(entries, new Writer(Some("probes grit_agent")), budget = 60)
      turnsOf(w) ==> Vector("t1", "t4", "t5")
    }

    test("a summary's match brings its turn's messages, never the summary itself") {
      val summarised = exchange("hmm", "ok") :+ Payload.Summary("probes use grit_agent")
      val turns = Vector(summarised) ++ (1 to 5).map(filler) :+
        Vector(Payload.Message(Message.User("Where should my probe point?")))
      val w = assemble(store(turns*), new Writer(Some("grit_agent")), budget = 60)
      ids(w).take(2) ==> Vector("t0:0", "t0:1")
      assert(!ids(w).contains("t0:2"))
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

    test("the query is the reply's text on one line") {
      QueryWriter.text(reply("  postgres\n  sqlite   decision ")) ==> "postgres sqlite decision"
      QueryWriter.text(reply(" \n ")) ==> ""
    }
  }
}
