package grit.turn

import java.time.Instant

import grit.core.act.Extraction
import grit.core.allot.Allotment
import grit.core.approval.Approval
import grit.core.context.Window
import grit.core.durable.{Flows, InMemoryDurable}
import grit.core.id.{EntryId, ToolCallId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.ToolUse
import grit.core.store.{InMemoryEntryStore, Jot, Tx}
import grit.core.tool.{Bound, ToolSet}
import grit.core.visibility.Integrity

import utest.*
import TurnLoop.{Budget, Pending, Round}

/** [[TurnRound.flow]] over a scripted [[TurnRound.Env]], run on [[InMemoryDurable]]: the steps
  * a turn's rounds record, in order, and what its loop comes to.
  */
object TurnRoundTests extends TestSuite {
  import TurnFixtures.*
  import TurnJournal.given

  private def budget(n: Int): Budget = Budget.of(n) match {
    case Right(b) => b
    case Left(why) => throw new java.lang.AssertionError(why)
  }

  private def call(id: String): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId(id), "peek", ujson.Obj("path" -> id))

  private def reply(
      text: String,
      calls: Vector[String] = Vector.empty,
      stop: StopReason = StopReason.EndTurn
  ): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls.map(call),
      stop,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, None),
      "m"
    )

  private def text(reply: Message.Assistant): String =
    reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString

  /** The scripted replies, the first to the first round. */
  private def replying(
      replies: Either[TurnFailure, Message.Assistant]*
  ): (Round, ToolUse) -> Either[TurnFailure, Message.Assistant] =
    (round, _) => replies.lift(round.index).getOrElse(Left(TurnFailure.Model("no reply scripted")))

  private val offer =
    TurnOffer.Recorded(None, ToolSet.Empty.id, Vector.empty, TurnOffer.Root.Addressed)

  /** A turn's bodies, scripted: its model's reply to each call as `script` gives it, each
    * round's gate at the level `levels` gives it, each reply kept as `kept` says; each call
    * free, unless the loop refused it, and settled as kept, failed when refused. Nothing is
    * hosted or gated.
    */
  private final class Scripted(
      jot: Jot^,
      entries: InMemoryEntryStore,
      script: (Round, ToolUse) -> Either[TurnFailure, Message.Assistant],
      levels: Int -> Integrity,
      kept: Either[TurnFailure, EntryId]
  ) extends TurnRound.Env {
    def gate(at: TurnRound.Rounds): Either[TurnFailure, Gated] =
      Right(Gated(levels(at.round.index), Vector.empty, Vector.empty))
    def call(at: TurnRound.Rounds, use: ToolUse): Either[TurnFailure, Message.Assistant] =
      script(at.round, use)
    def record(at: TurnRound.Rounds, reply: Message.Assistant, calls: Vector[Pending])(using
        Tx^
    ): Either[TurnFailure, TurnRound.Kept] =
      kept.map(
        TurnRound.Kept(
          _,
          calls.map {
            case Pending.Refused(_, _) => TurnTools.Kind.Refused
            case Pending.Run(_) => TurnTools.Kind.Free
          }
        )
      )
    def hosted(
        at: TurnRound.Rounds,
        slot: TurnTools.Slot,
        pending: Pending
    ): Either[TurnFailure, Bound.Hosted] = Left(TurnFailure.Store("nothing is hosted"))
    def shown(call: TurnRound.Call): Either[TurnFailure, String] =
      Left(TurnFailure.Store("nothing asks"))
    def settle(
        call: TurnRound.Call,
        raise: Extraction.Raising^
    ): Either[TurnFailure, TurnTools.Settled] =
      Right(TurnTools.Settled(call.slot.resultId, call.kind == TurnTools.Kind.Refused))
    def decide(call: TurnRound.Call, approval: Approval): Either[TurnFailure, TurnTools.Settled] =
      Left(TurnFailure.Store("nothing is gated"))
    def settling: TurnTools.Settling^{this} = new TurnTools.Settling(jot, entries)
    def now(): Instant = Instant.EPOCH
    def hosting: TurnHosting = TurnFixtures.hosting()
  }

  // The scripted bodies read no pins: the route names the fixture profile's first of each.
  private val routed = Turn.Routed(
    TestCatalog.pin.id,
    Route.Recorded(
      grit.core.visibility.Label.Public,
      0,
      Some(0),
      None,
      grit.core.allot.Headroom.Ample,
      Route.Task.Skipped,
      Allotment.Untouched,
      None
    )
  )

  /** The turn's rounds under a budget of `n` calls, on `durable`: the model's reply to each
    * call as `script` gives it, each round's gate at the level `levels` gives it, each kept
    * reply as `kept` says, and each call bound against `peek`, which answers "text". What the
    * loop came to, and each step it recorded, a `tool:n:j` with whether its result failed.
    */
  private def rounds(
      n: Int,
      script: (Round, ToolUse) -> Either[TurnFailure, Message.Assistant],
      levels: Int -> Integrity = _ => Integrity.Member,
      kept: Either[TurnFailure, EntryId] = Right(EntryId("kept")),
      durable: InMemoryDurable = new InMemoryDurable
  ): (String, Vector[String]) = {
    val entries = new InMemoryEntryStore
    val turn = say(entries, "go")
    val env = new Scripted(new FakeJot, entries, script, levels, kept)
    val looping = TurnRound.Looping(
      turn,
      offer,
      routed,
      Window(Vector.empty),
      None,
      budget(n),
      TurnTools.AnswerWithin
    )
    val out = durable.run(turn.workflowId)(_ =>
      Flows.run(TurnRound.flow, env, looping) match {
        case Right(TurnRound.Looped(answer, at)) =>
          s"answered ${text(answer)} at ${at.round.index} of ${at.replies.size}, " +
            s"believed ${at.level}"
        case Left(TurnRound.Stalled(failure, at)) =>
          s"stalled $failure after ${at.replies.size}, believed ${at.level}"
      }
    )
    val settled = summon[grit.core.durable.Journaled[Either[TurnFailure, TurnTools.Settled]]]
    val steps = durable.history(turn.workflowId).map {
      case InMemoryDurable.Step(name, InMemoryDurable.Outcome.Output(v))
          if Turn.Step.family(name).contains(Turn.Step.Call) =>
        settled.decode(v).fold(_ => s"$name unread", s => s"$name ${s.map(_.failed)}")
      case InMemoryDurable.Step(name, _) => name
    }
    (out, steps)
  }

  val tests = Tests {
    test("a reply that calls no tool answers at once, its round gated before its call") {
      rounds(3, replying(Right(reply("hi")))) ==> (
        "answered hi at 0 of 1, believed member",
        Vector("gate:0", "ask:0")
      )
    }

    test("each reply that calls tools is recorded, then each call settled in order") {
      val first = reply("", Vector("a", "b"))
      val second = reply("more", Vector("c"))
      rounds(4, replying(Right(first), Right(second), Right(reply("answer")))) ==> (
        "answered answer at 2 of 3, believed member",
        Vector(
          "gate:0",
          "ask:0",
          "ask:0:record",
          "call:0:0 Right(false)",
          "call:0:1 Right(false)",
          "gate:1",
          "ask:1",
          "ask:1:record",
          "call:1:0 Right(false)",
          "gate:2",
          "ask:2"
        )
      )
    }

    test("the budget's last call is made with tools off, and its calls are dropped") {
      val script: (Round, ToolUse) -> Either[TurnFailure, Message.Assistant] = (round, use) =>
        Right(if (round.index == 0) reply("", Vector("a")) else reply(s"so $use", Vector("b")))
      rounds(2, script) ==> (
        "answered so Off at 1 of 2, believed member",
        Vector(
          "gate:0",
          "ask:0",
          "ask:0:record",
          "call:0:0 Right(false)",
          "gate:1",
          "ask:1"
        )
      )
    }

    test("a silent reply fails the turn") {
      val (out, steps) = rounds(3, replying(Right(reply("", Vector("a"))), Right(reply(""))))
      (out, steps.lastOption) ==> (
        "stalled Model(the reply to ask:1 said nothing and called no tool) after 2, " +
          "believed member",
        Some("ask:1")
      )
    }

    test("a reply cut off at max tokens has its calls refused, not run") {
      val cut = reply("", Vector("a"), StopReason.MaxTokens)
      rounds(3, replying(Right(cut), Right(reply("ok")))) ==> (
        "answered ok at 1 of 2, believed member",
        Vector(
          "gate:0",
          "ask:0",
          "ask:0:record",
          "call:0:0 Right(true)",
          "gate:1",
          "ask:1"
        )
      )
    }

    test("a failed call ends the loop with no step after it") {
      val down = TurnFailure.Model("HTTP 529")
      val (out, steps) = rounds(3, replying(Right(reply("", Vector("a"))), Left(down)))
      (out, steps.lastOption) ==> (
        "stalled Model(HTTP 529) after 1, believed member",
        Some("ask:1")
      )
    }

    test("a reply that cannot be kept settles nothing") {
      rounds(
        3,
        replying(Right(reply("", Vector("a")))),
        kept = Left(TurnFailure.Store("disk"))
      ) ==> (
        "stalled Store(disk) after 1, believed member",
        Vector("gate:0", "ask:0", "ask:0:record")
      )
    }

    test("a turn's level is the meet of its rounds' gates, lowered and never raised again") {
      val levels: Int -> Integrity =
        Vector(Integrity.Member, Integrity.Outside, Integrity.Member)(_)
      val script =
        replying(Right(reply("", Vector("a"))), Right(reply("", Vector("b"))), Right(reply("so")))
      rounds(4, script, levels)._1 ==> "answered so at 2 of 3, believed outside"
      // A gate's compartments are kept: the level starts at the first gate's, not above it.
      val team = grit.core.visibility.Compartment
        .of("team")
        .getOrElse(throw new java.lang.AssertionError("team is a compartment"))
      val inTeam = Integrity.at(Integrity.Level.Member, team)
      rounds(4, script, _ => inTeam)._1 ==> s"answered so at 2 of 3, believed $inTeam"
    }
  }
}
