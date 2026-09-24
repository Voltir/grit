package grit.assembly

import grit.core.*
import grit.dbos.TestTx
import utest.*
import java.time.Instant

object LinearAssemblerTests extends TestSuite {

  private val c1 = ConversationId("c1")

  private object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** A four-character user message and an eight-character reply: 5 + 6 = 11 tokens. */
  private val SmallTurn = 11L

  private def user(text: String): Message = Message.User(text)

  private def reply(text: String): Message =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "test"
    )

  /** A store holding `turns`, each a list of messages, one turn per element from turn 0. */
  private def store(turns: Vector[Message]*): EntryStore = storeOf(turns.toVector)

  private def storeOf(turns: Vector[Vector[Message]]): EntryStore = {
    val entries = new InMemoryEntryStore
    given Tx = TestTx.fake
    for (turn <- turns.indices; message <- turns(turn)) {
      val next = entries.lockNext(c1).getOrElse(sys.error("in-memory store"))
      val _ = entries.insert(
        Entry(
          EntryId(s"t$turn:${next.seq}"),
          c1,
          TurnSeq(turn.toLong),
          None,
          next.seq,
          Payload.Message(message),
          Instant.EPOCH
        )
      )
    }
    entries
  }

  private def small(n: Int): Vector[Message] = Vector(user(f"q$n%03d"), reply(f"answer$n%02d"))

  private def window(entries: EntryStore, turn: Long, budget: Long): Vector[String] =
    new LinearAssembler(entries, Tokens(budget))
      .assemble(AssemblyRequest(TurnRef(c1, TurnSeq(turn))))(using FakeDb)
      .fold(e => sys.error(s"assembly failed: $e"), _.entries.map(EntryId.value))

  val tests = Tests {
    test("a budget the history fits in keeps every earlier turn, oldest first") {
      val entries = store(small(0), small(1), small(2), Vector(user("now!")))
      window(entries, 2, 1000) ==> Vector("t0:0", "t0:1", "t1:2", "t1:3")
    }

    test("the turn's own entries and later turns are never in its window") {
      val entries = store(small(0), small(1), small(2))
      window(entries, 1, 1000) ==> Vector("t0:0", "t0:1")
      window(entries, 0, 1000) ==> Vector()
    }

    test("over budget, the newest whole turns are kept") {
      val entries = store(small(0), small(1), small(2), small(3))
      window(entries, 3, 2 * SmallTurn) ==> Vector("t1:2", "t1:3", "t2:4", "t2:5")
      window(entries, 3, 2 * SmallTurn - 1) ==> Vector("t2:4", "t2:5")
    }

    test("a turn is kept whole, however many messages it holds") {
      // Two messages queued before one reply: 5 + 5 + 6 = 16 tokens.
      val entries = store(small(0), Vector(user("aaaa"), user("bbbb"), reply("answer!!")), small(2))
      window(entries, 2, 16) ==> Vector("t1:2", "t1:3", "t1:4")
      window(entries, 2, 15) ==> Vector()
    }

    test("a turn that does not fit ends the window, though older ones would") {
      val entries = store(small(0), Vector(user("x" * 400)), small(2), small(3))
      window(entries, 3, 50) ==> Vector("t2:3", "t2:4")
    }

    test("a newest turn larger than the budget leaves the window empty") {
      val entries = store(small(0), Vector(user("x" * 400)), small(2))
      window(entries, 2, 50) ==> Vector()
    }

    test("a zero budget leaves the window empty") {
      window(store(small(0), small(1)), 1, 0) ==> Vector()
    }

    test("a store failure is an assembly failure") {
      object Down extends EntryStore {
        private val down = StoreError.DatabaseError("down")
        def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = Left(down)
        def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = Left(down)
        def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = Left(down)
        def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] = Left(down)
      }
      new LinearAssembler(Down, Tokens(1000))
        .assemble(AssemblyRequest(TurnRef(c1, TurnSeq(1))))(using FakeDb) ==>
        Left(AssemblyError.Store(StoreError.DatabaseError("down")))
    }
  }
}
