package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.{AgentType, SourceId, ToolCallId, TurnRef}
import grit.core.identity.TestAccounts
import grit.core.inbox.{InMemoryInbox, InboxError, SpawnInbox, TurnEnding}
import grit.core.message.{AssistantBlock, Message, SpendCap, StopReason, Tokens, Usage}
import grit.core.place.Place
import grit.core.store.{Entry, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx

import utest.*

/** A parent turn that hands a task to a run with `delegate` ([[TurnDelegate]]): its child made,
  * started and waited for through its own steps, and what it came to answered as the call's
  * result; over the in-memory inbox, its children run as they are made.
  */
object TurnDelegateTests extends TestSuite {
  import TurnFixtures.*

  private val ana = TestAccounts.account("task:ana-delegating")

  /** A reply calling `delegate` with `args`, then, on every later call, answering `done`. */
  private def delegating(args: ujson.Value): Scripted =
    new Scripted((_, n) =>
      Right(
        if (n == 0) said("", AssistantBlock.ToolCall(ToolCallId("d1"), "delegate", args))
        else said("done")
      )
    )

  private def said(text: String, calls: AssistantBlock.ToolCall*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls,
      if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
      "m"
    )

  private val asked: ujson.Value = ujson.Obj("agent" -> "scholar", "task" -> "Find the paper.")

  /** [[scholars]]' type requiring `requires` of its brief. */
  private def scholarsAt(requires: Integrity): Spawnable =
    scholars.declared.byType.values.headOption
      .flatMap(s =>
        grit.core.agent.Subagent
          .of(s.agent, s.persona, s.pins, s.tools, s.spendCap, requires)
          .flatMap(r => grit.core.agent.Subagents.of(Vector(r)))
          .toOption
      )
      .fold(scholars)(Spawnable.of(_, _ => Vector.empty))

  /** A parent's conversation of `origin` in an in-memory inbox, its first message ana's, run in
    * `durable`: each child it spawns is run at once by `child`, unless `children` is false, and
    * rings its parent; the first read of a child's end dies when `crashing`.
    */
  private final class Parenting(
      durable: InMemoryDurable,
      child: Scripted,
      origin: Origin = Origin.Task("delegating", "parent"),
      children: Boolean = true,
      crashing: Boolean = false,
      subagents: Spawnable = scholarsAt(Integrity.Outside)
  ) {
    val inbox: InMemoryInbox = InMemoryInbox.fresh()
    val parent: TurnRef = inbox
      .ingest(origin, SourceId("p"), Message.User("look it up"), ana, None)
      .fold(e => throw new java.lang.AssertionError(e), identity)
    // The parent runs, so a child's ring reaches it.
    inbox.started = Vector(parent)

    // Written only by the turn's steps, which run on the test's one thread.
    @caps.unsafe.untrackedCaptures
    var made: Vector[TurnRef] = Vector.empty
    @caps.unsafe.untrackedCaptures
    private var dying: Boolean = crashing

    private def originOf(turn: TurnRef): Option[Origin] =
      inbox.conversations.all.find(_.id == turn.conversationId).map(_.origin)

    val spawns: SpawnInbox^{this} = new SpawnInbox {
      def spawned(
          o: Origin.Spawned,
          agent: AgentType,
          brief: String,
          spendCap: SpendCap,
          believed: Integrity,
          classification: Label
      ): Either[InboxError, TurnRef] =
        inbox.spawned(o, agent, brief, spendCap, believed, classification).map { c =>
          if (children && !made.contains(c)) {
            made = made :+ c
            val _ = durable.run(c.workflowId)(body(child, o))
          }
          c
        }
      def ended(c: TurnRef): Either[InboxError, Option[TurnEnding]] =
        if (dying) { dying = false; throw new InMemoryDurable.Crash }
        else inbox.ended(c)
      def reply(c: TurnRef)(using Tx^) = inbox.reply(c)
      def spent(turn: TurnRef)(using Tx^) = inbox.spent(turn)
      def granted(p: TurnRef)(using Tx^): Either[StoreError, Vector[SpendCap]] = inbox.granted(p)
      def returned(c: TurnRef, ending: TurnEnding): Either[InboxError, Unit] =
        inbox.returned(c, ending).map { _ =>
          originOf(c).collect { case Origin.Spawned(p, _, _) => p }.foreach { p =>
            durable.send(p.workflowId, SpawnInbox.topic(c), TurnEnding.word(ending))
          }
        }
    }

    /** A turn's body over the inbox's stores, its model `provider`, its conversation `from`. */
    def body(provider: Scripted, from: Origin)(id: grit.core.id.WorkflowId)(using
        grit.core.durable.Durable^
    ): String =
      spawnedBody(
        inbox.entries,
        provider,
        new Served(
          new grit.core.edge.InMemoryEdges,
          new InMemoryDurable,
          _ => Serve.Now(grit.core.tool.Outcome.Done("read")),
          Place.Everywhere
        ),
        new Files(Map.empty),
        subagents,
        from,
        ledger = inbox.ledger,
        conversations = inbox.conversations,
        spawns = spawns,
        principals = inbox.principals
      )(id)

    /** The parent's turn run with `provider` as its model. */
    def run(provider: Scripted): String = durable.run(parent.workflowId)(body(provider, origin))

    /** What each call the parent's conversation kept came to, as its model was shown it. */
    def results: Vector[String] = {
      given Tx = TestTx.fake
      inbox.entries.list(parent.conversationId).toOption.toVector.flatten.collect {
        case Entry(_, _, _, _, _, Payload.Result(result, _), _, _) => result.content
      }
    }

    /** The parent's steps of its first call, as a watcher is shown them. */
    def steps: Vector[String] =
      Turn.Step
        .named(durable.recordedSteps(parent.workflowId))
        .filter(_.startsWith("call:0:0"))

    /** The children the inbox keeps. */
    def kept: Int =
      inbox.conversations.all.count(_.origin.isInstanceOf[Origin.Spawned])
  }

  val tests = Tests {
    test(
      "a parent's delegate starts its child, waits for its ring and keeps its reply, fenced, as the call's result"
    ) {
      val durable = new InMemoryDurable
      val world = new Parenting(durable, new Scripted((_, _) => Right(said("Found it."))))
      val _ = world.run(delegating(asked))
      (world.results, world.steps, world.kept) ==> (
        Vector("The scholar run replied, spending $0.001:\n<reply>\nFound it.\n</reply>"),
        Vector(
          "call:0:0:narrow",
          "call:0:0:spawn",
          "call:0:0:await",
          "call:0:0:read",
          "call:0:0"
        ),
        1
      )
    }

    test("a parent killed while it waits for its child makes it once, and its rerun waits on") {
      val durable = new InMemoryDurable
      val world = new Parenting(
        durable,
        new Scripted((_, _) => Right(said("x"))),
        children = false,
        crashing = true
      )
      val model = delegating(asked)
      val crashed = scala.util.Try(world.run(model)).isFailure
      val _ = world.run(model)
      (crashed, world.kept, world.results) ==> (
        true,
        1,
        Vector(
          s"The scholar run did not finish within ${grit.act.phase.Spawning.Within.toMinutes} " +
            "minutes; its reply will not reach you here."
        )
      )
    }
  }
}
