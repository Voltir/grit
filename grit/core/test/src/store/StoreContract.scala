package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.model.{
  Assignment,
  Catalog,
  Known,
  ModelId,
  ModelRef,
  NameRepair,
  Policy,
  Profile,
  Source,
  StrictSchemas,
  TurnProfile,
  TurnProfileId
}
import grit.core.prompt.{Fragment, FragmentId, Layer, SystemPrompt}
import grit.core.tool.{Retry, ToolName, ToolSet, ToolSetId, ToolSets}

import utest.*

/** The contract every [[EntryStore]], [[UsageLedger]], [[ModelProfileStore]],
  * [[ModelFactStore]], [[ToolSets]] and [[PromptStore]] keeps, run against one
  * implementation of each: the in-memory fakes in core, the SQL stores in grit.dbos. The
  * fakes stand in for the SQL stores in every other module's tests, so whatever those tests
  * rely on belongs here.
  *
  * Tests share the stores' database, so each names its own conversations, entry ids and
  * workflow ids.
  */
abstract class StoreContract extends TestSuite {

  /** The entry store under test. */
  protected def entries: EntryStore

  /** The ledger under test, over the same database as [[entries]]. */
  protected def ledger: UsageLedger

  /** The profile store under test, over the same database as [[entries]]. */
  protected def profiles: ModelProfileStore

  /** The fact store under test, over the same database as [[entries]]. It keeps every fact
    * in one list, so only one test writes to it.
    */
  protected def facts: ModelFactStore

  /** The tool-set store under test, over the same database as [[entries]]. */
  protected def toolSets: ToolSets

  /** The prompt store under test, over the same database as [[entries]]. */
  protected def prompts: PromptStore

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation entries may be written to, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  /** A conversation that does not exist. */
  protected def unknownConversation: ConversationId

  private def entry(
      c: ConversationId,
      id: String,
      seq: Long,
      turn: TurnSeq = TurnSeq.First,
      parent: Option[String] = None
  ): Entry =
    Entry(
      EntryId(id),
      c,
      turn,
      parent.map(EntryId(_)),
      seq,
      Payload.Message(Message.User(id)),
      Instant.EPOCH
    )

  private def ids(listed: Either[StoreError, Vector[Entry]]): Either[StoreError, Vector[String]] =
    listed.map(_.map(e => EntryId.value(e.id)))

  /** A turn profile with every role on `model`, budget `budget`. */
  private def profile(model: String, budget: Int): TurnProfile = {
    val ref = ModelRef(ModelId.of(model).getOrElse(throw new java.lang.AssertionError(model)), None)
    val a = Assignment(ref, budget, None)
    Catalog.of(Policy(a, a, a), Vector.empty).pin
  }

  private val usage = Usage(Tokens(10), Tokens(5), Tokens(2), Some(BigDecimal("0.0000123")))

  val tests = Tests {

    test("an inserted entry is got back by its id, unchanged") {
      val e = entry(conversation("get"), "got", 3, TurnSeq(2), Some("its-parent"))
      transaction(entries.insert(e)) ==> Right(())
      transaction(entries.get(EntryId("got"))) ==> Right(Some(e))
    }

    test("an id no entry has is None") {
      transaction(entries.get(EntryId("never-inserted"))) ==> Right(None)
    }

    test("an id already taken is DuplicateId, whatever the rest of the entry") {
      val c = conversation("duplicate")
      transaction(entries.insert(entry(c, "dup", 0))) ==> Right(())
      transaction(entries.insert(entry(c, "dup", 0))) ==>
        Left(StoreError.DuplicateId(EntryId("dup")))
      transaction(entries.insert(entry(c, "dup", 1))) ==>
        Left(StoreError.DuplicateId(EntryId("dup")))
    }

    test("a DuplicateId leaves the transaction usable") {
      val c = conversation("savepoint")
      val outcome = transaction {
        entries.insert(entry(c, "first", 0))
        (entries.insert(entry(c, "first", 1)), entries.insert(entry(c, "after", 1)))
      }
      outcome ==> (Left(StoreError.DuplicateId(EntryId("first"))), Right(()))
      ids(transaction(entries.list(c))) ==> Right(Vector("first", "after"))
    }

    test("a seq already taken in the conversation is a DatabaseError; another's is free") {
      val c = conversation("seq-taken")
      val other = conversation("seq-free")
      transaction(entries.insert(entry(c, "holder", 0))) ==> Right(())
      val taken = transaction(entries.insert(entry(c, "clash", 0)))
      assert(taken match {
        case Left(StoreError.DatabaseError(_)) => true
        case _ => false
      })
      transaction(entries.insert(entry(other, "elsewhere", 0))) ==> Right(())
      ids(transaction(entries.list(c))) ==> Right(Vector("holder"))
    }

    test("list is one conversation's entries by seq, whatever order they were inserted in") {
      val c = conversation("order")
      transaction {
        entries.insert(entry(c, "order-3", 7))
        entries.insert(entry(c, "order-1", 2))
        entries.insert(entry(conversation("order-other"), "not-mine", 4))
        entries.insert(entry(c, "order-2", 5))
      }
      ids(transaction(entries.list(c))) ==> Right(Vector("order-1", "order-2", "order-3"))
    }

    test("list of an unknown conversation is empty") {
      transaction(entries.list(unknownConversation)) ==> Right(Vector.empty)
    }

    test("lockNext is the start of an empty conversation, and past everything in one") {
      val c = conversation("next")
      transaction(entries.lockNext(c)) ==> Right(EntryStore.Next(TurnSeq.First, 0L))
      transaction {
        entries.insert(entry(c, "late", 4, TurnSeq(2)))
        entries.insert(entry(c, "early", 1, TurnSeq(0)))
        entries.insert(entry(conversation("next-other"), "beyond", 9, TurnSeq(5)))
      }
      transaction(entries.lockNext(c)) ==> Right(EntryStore.Next(TurnSeq(3), 5L))
    }

    test("the ledger gives back what was recorded, the cost exactly and its absence as None") {
      val c = conversation("ledger")
      val free = usage.copy(costUsd = None)
      transaction {
        entries.insert(entry(c, "costly", 0))
        entries.insert(entry(c, "free", 1))
        ledger.record(
          EntryId("costly"),
          TurnRef(c, TurnSeq.First),
          WorkflowId("w-exact"),
          "m",
          usage,
          Tokens(12)
        )
        ledger.record(
          EntryId("free"),
          TurnRef(c, TurnSeq.First),
          WorkflowId("w-exact"),
          "n",
          free,
          Tokens(3)
        )
      }
      transaction(ledger.of(WorkflowId("w-exact"))) ==>
        Right(
          Vector(
            UsageLedger.Row(EntryId("costly"), "m", usage, Tokens(12)),
            UsageLedger.Row(EntryId("free"), "n", free, Tokens(3))
          )
        )
    }

    test("a second record of an entry is DuplicateId, and leaves the transaction usable") {
      val c = conversation("ledger-duplicate")
      val outcome = transaction {
        entries.insert(entry(c, "once", 0))
        entries.insert(entry(c, "next", 1))
        val first = ledger.record(
          EntryId("once"),
          TurnRef(c, TurnSeq.First),
          WorkflowId("w-dup"),
          "m",
          usage,
          Tokens(1)
        )
        val again = ledger.record(
          EntryId("once"),
          TurnRef(c, TurnSeq.First),
          WorkflowId("w-dup"),
          "m",
          usage,
          Tokens(2)
        )
        (
          first,
          again,
          ledger.record(
            EntryId("next"),
            TurnRef(c, TurnSeq.First),
            WorkflowId("w-dup"),
            "m",
            usage,
            Tokens(3)
          )
        )
      }
      outcome ==> (Right(()), Left(StoreError.DuplicateId(EntryId("once"))), Right(()))
      transaction(ledger.of(WorkflowId("w-dup"))).map(_.map(_.estimatedInput)) ==>
        Right(Vector(Tokens(1), Tokens(3)))
    }

    test("of is one workflow's rows in record order, within a transaction and across them") {
      val c = conversation("ledger-order")
      val w = WorkflowId("w-order")
      transaction {
        Vector("z", "a", "m", "other").zipWithIndex.foreach { (id, seq) =>
          entries.insert(entry(c, id, seq.toLong))
        }
        // Recorded in the reverse of their ids' order, so ordering by id cannot pass.
        ledger.record(EntryId("z"), TurnRef(c, TurnSeq.First), w, "m", usage, Tokens(1))
        ledger.record(
          EntryId("other"),
          TurnRef(c, TurnSeq.First),
          WorkflowId("w-order-other"),
          "m",
          usage,
          Tokens(1)
        )
        ledger.record(EntryId("a"), TurnRef(c, TurnSeq.First), w, "m", usage, Tokens(1))
      }
      transaction(ledger.record(EntryId("m"), TurnRef(c, TurnSeq.First), w, "m", usage, Tokens(1)))
      transaction(ledger.of(w)).map(_.map(r => EntryId.value(r.entry))) ==>
        Right(Vector("z", "a", "m"))
    }

    test("of a workflow that recorded nothing is empty") {
      transaction(ledger.of(WorkflowId("w-none"))) ==> Right(Vector.empty)
    }

    test("forget deletes the rows of a conversation's turns in the range, and no other's") {
      val c = conversation("ledger-forget")
      val other = conversation("ledger-forget-other")
      def at(conversation: ConversationId, turn: Long) = TurnRef(conversation, TurnSeq(turn))
      val w = WorkflowId("w-forget")
      transaction {
        Vector("f0", "f1", "f2", "f3").zipWithIndex.foreach { (id, seq) =>
          entries.insert(entry(c, id, seq.toLong))
        }
        entries.insert(entry(other, "f-other", 0))
        ledger.record(EntryId("f0"), at(c, 0), w, "m", usage, Tokens(1))
        ledger.record(EntryId("f1"), at(c, 1), w, "m", usage, Tokens(1))
        ledger.record(EntryId("f2"), at(c, 2), w, "m", usage, Tokens(1))
        ledger.record(EntryId("f3"), at(c, 3), w, "m", usage, Tokens(1))
        ledger.record(EntryId("f-other"), at(other, 1), w, "m", usage, Tokens(1))
      }
      transaction(ledger.forget(c, TurnSeq(1), TurnSeq(2))) ==> Right(())
      transaction(ledger.of(w)).map(_.map(r => EntryId.value(r.entry))) ==>
        Right(Vector("f0", "f3", "f-other"))
    }

    test("forgetting turns' profiles keeps the other turns' and the profiles") {
      val p = profile("a/forget", 100)
      transaction {
        for {
          _ <- profiles.pin(WorkflowId("w-forget-1"), p)
          _ <- profiles.pin(WorkflowId("w-forget-2"), p)
          _ <- profiles.forget(Vector(WorkflowId("w-forget-1"), WorkflowId("w-unknown")))
        } yield ()
      } ==> Right(())
      transaction(profiles.of(WorkflowId("w-forget-1"))) ==> Right(None)
      transaction(profiles.of(WorkflowId("w-forget-2"))) ==> Right(Some(p))
      transaction(profiles.get(p.id)) ==> Right(Some(p))
    }

    test("a pinned turn's profile is got back by the turn and by the profile's id") {
      val p = profile("a/pinned", 100)
      transaction(profiles.pin(WorkflowId("w-pin"), p)) ==> Right(())
      transaction(profiles.of(WorkflowId("w-pin"))) ==> Right(Some(p))
      transaction(profiles.get(p.id)) ==> Right(Some(p))
    }

    test("turns under one profile share it; a turn pinned again keeps its first") {
      val p = profile("a/shared", 100)
      val other = profile("a/shared", 200)
      transaction {
        for {
          _ <- profiles.pin(WorkflowId("w-share-1"), p)
          _ <- profiles.pin(WorkflowId("w-share-2"), p)
          _ <- profiles.pin(WorkflowId("w-share-1"), other)
        } yield ()
      } ==> Right(())
      transaction(profiles.of(WorkflowId("w-share-1"))) ==> Right(Some(p))
      transaction(profiles.of(WorkflowId("w-share-2"))) ==> Right(Some(p))
      transaction(profiles.get(other.id)) ==> Right(Some(other))
    }

    test("a turn never pinned, or an id never kept, is None") {
      transaction(profiles.of(WorkflowId("w-unpinned"))) ==> Right(None)
      transaction(profiles.get(TurnProfileId("0000000000000000"))) ==> Right(None)
    }

    test("facts are kept as approved, and read back oldest first") {
      val store = facts
      val ref =
        ModelRef(ModelId.of("a/facts").getOrElse(throw new java.lang.AssertionError("id")), None)
      val on = java.time.LocalDate.of(2026, 9, 25)
      val first =
        Profile(ref, strict = Known.Of(StrictSchemas.Enforced, Source.Measured("probe", on, 5, 5)))
      val second = Profile(ref, names = Known.Of(NameRepair.AsSent, Source.Declared("nick", on)))
      transaction(store.all()) ==> Right(Vector.empty)
      transaction(store.keep(first, "nick", Instant.parse("2026-09-25T10:00:00Z"))) ==> Right(())
      transaction(store.keep(second, "ana", Instant.parse("2026-09-25T11:00:00Z"))) ==> Right(())
      transaction(store.all()) ==> Right(
        Vector(
          ModelFactStore.Kept(first, "nick", Instant.parse("2026-09-25T10:00:00Z")),
          ModelFactStore.Kept(second, "ana", Instant.parse("2026-09-25T11:00:00Z"))
        )
      )
    }

    test("a recorded prompt is got back by its turn, and by its fragments' ids, in order") {
      val p = StoreContract.prompt("recorded")
      transaction(prompts.record(WorkflowId("w-prompt"), p)) ==> Right(())
      transaction(prompts.of(WorkflowId("w-prompt"))) ==> Right(Some(p))
      transaction(prompts.prompt(p.ids)) ==> Right(p)
    }

    test("a turn recorded again keeps its first prompt; forgetting it keeps the fragments") {
      val first = StoreContract.prompt("first")
      val second = StoreContract.prompt("second")
      transaction {
        for {
          _ <- prompts.record(WorkflowId("w-prompt-again"), first)
          _ <- prompts.record(WorkflowId("w-prompt-again"), second)
        } yield ()
      } ==> Right(())
      transaction(prompts.of(WorkflowId("w-prompt-again"))) ==> Right(Some(first))
      transaction(prompts.forget(Vector(WorkflowId("w-prompt-again")))) ==> Right(())
      transaction(prompts.of(WorkflowId("w-prompt-again"))) ==> Right(None)
      transaction(prompts.prompt(second.ids)) ==> Right(second)
    }

    test("a prompt naming a fragment never kept is Invalid, naming the first such id") {
      val kept = StoreContract.prompt("kept-only")
      val never = Fragment(Layer.Place, "/never/AGENTS.md", "never kept")
      transaction(prompts.keep(kept.fragments)) ==> Right(())
      transaction(prompts.prompt(kept.ids :+ never.id)) ==>
        Left(StoreError.Invalid(s"prompt fragment ${FragmentId.value(never.id)} is not kept"))
    }

    test("a kept tool set is got back by its id, entry for entry") {
      val set = StoreContract.toolSet("kept_tool")
      transaction(toolSets.keep(set)) ==> Right(())
      transaction(toolSets.get(set.id)) ==> Right(set)
    }

    test("keeping a set again changes nothing, and an id never kept is Invalid naming it") {
      val set = StoreContract.toolSet("twice_tool")
      transaction(toolSets.keep(set).flatMap(_ => toolSets.keep(set))) ==> Right(())
      transaction(toolSets.get(set.id)) ==> Right(set)
      val never = StoreContract.toolSet("never_kept").id
      transaction(toolSets.get(never)) ==>
        Left(StoreError.Invalid(s"no tool set ${ToolSetId.value(never)} is kept"))
    }
  }
}

object StoreContract {

  /** A prompt of a base, a reach and five place fragments, each naming `tag`: five, so an
    * order other than theirs (their ids', say) is all but certain to differ from it.
    */
  def prompt(tag: String): SystemPrompt =
    SystemPrompt.of(
      Vector(
        Fragment(Layer.Base, Fragment.Grit, s"Base $tag."),
        Fragment(Layer.Reach, Fragment.Grit, s"Reach $tag.")
      ) ++ (1 to 5).map(depth =>
        Fragment(Layer.Place, s"/$tag${"/d" * depth}/AGENTS.md", s"Depth $depth, $tag.")
      )
    )

  /** A set of one tool named `name`, asking first and interrupted when cut short, beside a
    * free one that reruns: each field of an entry differs from the other's.
    */
  def toolSet(name: String): ToolSet = {
    val named = ToolName.of(name).getOrElse(throw new java.lang.AssertionError(name))
    ToolSet
      .of(
        Vector(
          ToolSet.Entry(
            named,
            s"Does $name.",
            ujson.Obj(
              "type" -> "object",
              "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string"))
            ),
            asks = true,
            Retry.Interrupt
          ),
          ToolSet.Entry(
            ToolName("peek"),
            "Peeks.",
            ujson.Obj("type" -> "object"),
            asks = false,
            Retry.Rerun
          )
        )
      )
      .getOrElse(throw new java.lang.AssertionError("a duplicate name"))
  }
}
