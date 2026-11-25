package grit.act.phase

import scala.concurrent.duration.*

import grit.core.act.{Acting, ActsFor, Allowance, Gates}
import grit.core.agent.SubagentFixtures.{ok, scholar, services, subagent}
import grit.core.agent.Subagents
import grit.core.durable.{Flows, InMemoryDurable, StepName}
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{AgentType, CallSlot, SourceId, TurnRef}
import grit.core.identity.{Account, TestAccounts}
import grit.core.inbox.{InMemoryInbox, InboxError, SpawnInbox, SpawnReply, TurnEnding}
import grit.core.message.{Cost, Message, SpendCap}
import grit.core.place.Place
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.{
  Clearance,
  Compartments,
  DeclaredLabel,
  Integrity,
  Label,
  Labelled,
  Level,
  RoomLabels,
  Visibility
}

import utest.*

/** A spawn's steps over the in-memory inbox: refused by its narrowing, which reads its type and
  * its parent's room; its child
  * made and started once; waited for by rungs; its reply read in its parent's transaction.
  */
object SpawningTests extends TestSuite {

  private val ana = TestAccounts.account("task:ana-spawning")
  private val internal = Label.at(Level.Internal)
  private val scholarType = ok(subagent(scholar, services, Integrity.Outside))

  private def key(r: Spawning.Request): String = s"${r.call.round}:${r.call.index}"

  private val steps = SpawnSteps[Spawning.Request](
    StepName.keyed[Spawning.Request]("narrow")(key),
    StepName.keyed[Spawning.Request]("spawn")(key),
    StepName.keyed[Spawning.Request]("await")(key),
    StepName.keyed[Spawning.Request]("ended")(key),
    StepName.keyed[Spawning.Request]("read")(key)
  )

  /** What a child does once it is made. */
  private enum Does {

    /** Nothing: it never ends. */
    case Nothing

    /** Spends `usd`, then ends with `reply` and `outcome` and rings its parent. */
    case Ends(reply: Option[String], outcome: String, usd: BigDecimal)
  }

  private def replied(text: String): Does =
    Does.Ends(Some(text), s"${TurnEnding.Replied}: $text", BigDecimal("0.01"))

  /** A deployment whose room `task:{name}` is labelled `at`, every other public. */
  private def labelling(name: String, at: DeclaredLabel): Visibility = (for {
    room <- Place.read(s"task:$name")
    compartments <- Compartments.of(Vector.empty).left.map(_.toString)
    rooms <- RoomLabels.of(Vector(room -> at), Labelled.Mapped(Label.Public)).left.map(_.toString)
    v <- Visibility.of(compartments, rooms, Vector.empty, Vector.empty).left.map(_.toString)
  } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The in-memory inbox under `visibility`, or `stored` when given, while the parent's
    * transactions run under `visibility`; its children doing as `does` says in the order they are
    * made, each ending child ringing with what `ringing` makes of its outcome (its ending's word,
    * as a child's turn rings), and the first read of a child's end dying when `crashAtEnded`;
    * when `endsLate`, every read of a child's end finds it running, as its workflow's status does
    * between its ring and its return.
    */
  private final class World(
      visibility: Visibility = Visibility.Shipped,
      does: Vector[Does] = Vector.empty,
      crashAtEnded: Boolean = false,
      stored: Option[Visibility] = None,
      ringing: String -> String = outcome => TurnEnding.word(TurnEnding.of(outcome)),
      endsLate: Boolean = false,
      subagents: Subagents = ok(Subagents.of(Vector(scholarType)))
  ) {
    val inbox: InMemoryInbox = InMemoryInbox.fresh(visibility = stored.getOrElse(visibility))
    val durable: InMemoryDurable =
      new InMemoryDurable(resolve = _ => Clearance.of(Label.Public), visibility = visibility)

    // Written only by the spawn's steps, which the test runs on its one thread and waits on.
    @caps.unsafe.untrackedCaptures
    var made: Vector[TurnRef] = Vector.empty
    @caps.unsafe.untrackedCaptures
    var spawnCalls: Int = 0
    @caps.unsafe.untrackedCaptures
    private var crashing: Boolean = crashAtEnded

    val spawns: SpawnInbox^{this} = new SpawnInbox {
      def spawned(
          origin: Origin.Spawned,
          agent: AgentType,
          brief: String,
          spendCap: SpendCap,
          believed: Integrity,
          classification: Label
      ): Either[InboxError, TurnRef] = {
        spawnCalls += 1
        inbox.spawned(origin, agent, brief, spendCap, believed, classification).map { c =>
          if (!made.contains(c)) {
            val next = does.lift(made.size).getOrElse(Does.Nothing)
            made = made :+ c
            next match {
              case Does.Nothing => ()
              case Does.Ends(reply, outcome, usd) =>
                inbox.charge(c, usd)
                inbox.finish(c, reply.map(Message.said(_, "m")), outcome)
                durable.send(origin.parent.workflowId, SpawnInbox.topic(c), ringing(outcome))
            }
          }
          c
        }
      }
      def ended(child: TurnRef): Either[InboxError, Option[TurnEnding]] =
        if (crashing) { crashing = false; throw new InMemoryDurable.Crash }
        else if (endsLate) Right(None)
        else inbox.ended(child)
      def reply(child: TurnRef)(using Tx^): Either[StoreError, SpawnReply] = inbox.reply(child)
      def spent(turn: TurnRef)(using Tx^): Either[StoreError, Cost] = inbox.spent(turn)
      def granted(parent: TurnRef)(using Tx^): Either[StoreError, Vector[SpendCap]] =
        inbox.granted(parent)
      def returned(child: TurnRef, ending: TurnEnding): Either[InboxError, Unit] =
        inbox.returned(child, ending)
    }

    /** A parent turn of `task:{name}`, asked by ana. */
    def parent(name: String): TurnRef = parentAt(Origin.Task(name, "parent"), ana)

    /** A parent turn of `origin`'s conversation, asked by `by`. */
    def parentAt(origin: Origin, by: Account): TurnRef =
      inbox
        .ingest(
          origin,
          SourceId(s"p-${origin.place.written}"),
          Message.User("look it up"),
          by,
          None
        )
        .fold(e => throw new java.lang.AssertionError(e), identity)

    /** The room the child `child` was made in, as its conversation's origin says. */
    def within(child: TurnRef): Option[Place] =
      inbox.conversations.all.find(_.id == child.conversationId).map(_.origin).collect {
        case Origin.Spawned(_, _, within) => within
      }

    /** What `requests` come to, each shown, run in order in their parent's workflow. */
    def spawn(requests: Spawning.Request*): Vector[String] =
      durable.run(workflow(requests)) { _ => body(requests) }.split("\n").toVector

    /** What `requests` come to replayed in a fresh runtime from their parent's history, or why
      * that history did not replay.
      */
    def replayed(requests: Spawning.Request*): Either[String, Vector[String]] = {
      val id = workflow(requests)
      new InMemoryDurable(resolve = _ => Clearance.of(Label.Public), visibility = visibility)
        .replay(id, durable.history(id)) { _ => body(requests) }
        .map(_.split("\n").toVector)
    }

    /** What each of `parent`'s steps recorded, the runtime's own left out. */
    def outputs(parent: TurnRef): Vector[(String, String)] =
      durable.history(parent.workflowId).collect {
        case InMemoryDurable.Step(name, InMemoryDurable.Outcome.Output(o))
            if !name.startsWith("DBOS.") =>
          name -> o
      }

    private def workflow(requests: Seq[Spawning.Request]) =
      requests.headOption.fold(sys.error("a request"))(_.call.turn.workflowId)

    private def body(requests: Seq[Spawning.Request])(using grit.core.durable.Durable^): String =
      requests
        .map(r =>
          shown(Flows.run(spawning, SpawnEnv(spawns, subagents, inbox.conversations), r).merge)
        )
        .mkString("\n")

    /** The steps `parent`'s workflow recorded, the runtime's own left out. */
    def recorded(parent: TurnRef): Vector[String] =
      durable.recordedSteps(parent.workflowId).filterNot(_.startsWith("DBOS."))
  }

  private val spawning = Spawning.spawn(steps, identity)

  private def acting(turn: TurnRef, allowance: Allowance = Allowance.Admitted): Acting =
    Acting(turn, ActsFor.Asker, allowance, Gates.Asker(1.minute), Integrity.Authority)

  private def request(
      turn: TurnRef,
      index: Int = 0,
      parent: Option[Acting] = None,
      bound: Option[Label] = None
  ): Spawning.Request =
    CallSlot
      .of(turn, 0, index)
      .flatMap(call =>
        Spawning.Request.of(
          call,
          parent.getOrElse(acting(turn)),
          scholar,
          "Find the paper.",
          Integrity.Member,
          bound
        )
      )
      .getOrElse(sys.error("a request"))

  /** A spawn's end, its reply's text and belief shown. */
  private def shown(d: Delegated): String = {
    def text(t: Tainted[String]) = {
      val read = Trusted.asIs(t)
      s"${read.extract}@${Integrity.written(read.integrity)}"
    }
    d match {
      case Delegated.Replied(child, reply, spent) =>
        s"Replied(${child.conversationId}, ${text(reply)}, $spent)"
      case Delegated.Limited(child, reply, spent) =>
        s"Limited(${child.conversationId}, ${reply.map(text)}, $spent)"
      case Delegated.Unanswered(child) => s"Unanswered(${child.conversationId})"
      case Delegated.Unread(child) => s"Unread(${child.conversationId})"
      case Delegated.Failed(child, why) => s"Failed(${child.conversationId}, $why)"
      case other => other.toString
    }
  }

  private def usd(s: String): SpendCap = ok(SpendCap.of(BigDecimal(s)))

  val tests = Tests {
    test(
      "a spawn whose parent's own conversation is a direct message's is refused Direct in its narrowing, making nothing"
    ) {
      val w = new World()
      val account = TestAccounts.sourced("slack:T1/U-ana")
      val dm = Origin.Direct(account, "t1")
      val turn = w.parentAt(dm, account)
      val asked = request(turn)
      val got = w.spawn(asked)
      (got, w.outputs(turn), w.spawnCalls, w.replayed(asked)) ==> (
        Vector(s"Refused(Direct(${dm.room}))"),
        Vector("narrow:0:0" -> s"""{"refused":{"direct":"${dm.room.written}"}}"""),
        0,
        Right(got)
      )
    }

    test(
      "a thread's child is made in its parent's own room, the channel its conversation was read in"
    ) {
      val w = new World()
      val thread = Origin.Thread("T1", "C1", "1700000000.000100")
      val turn = w.parentAt(thread, ana)
      val got = w.spawn(request(turn))
      (got, w.made.map(w.within)) ==> (
        Vector("Unanswered(c2)"),
        Vector(Some(thread.room))
      )
    }

    test("a type the deployment no longer declares is Unspawned in its narrowing, making nothing") {
      val w = new World(subagents = Subagents.empty)
      val turn = w.parent("undeclared")
      val got = w.spawn(request(turn))
      (got, w.outputs(turn), w.spawnCalls) ==> (
        Vector("Unspawned(no subagent of the type scholar is declared)"),
        Vector("narrow:0:0" -> """{"store":"no subagent of the type scholar is declared"}"""),
        0
      )
    }

    test("a spawn its narrowing refuses records that step alone, making nothing") {
      val strict = ok(subagent(scholar, services, Integrity.Authority))
      val w = new World(subagents = ok(Subagents.of(Vector(strict))))
      val turn = w.parent("below")
      val asked = request(turn)
      val got = w.spawn(asked)
      (got, w.outputs(turn), w.spawnCalls, w.replayed(asked)) ==> (
        Vector(s"Refused(Below(${Integrity.Authority},${Integrity.Member}))"),
        Vector("narrow:0:0" -> """{"refused":{"below":{"needs":"authority","has":"member"}}}"""),
        0,
        Right(got)
      )
    }

    test("a parent in a public room routed at Internal is refused Above a public room") {
      val w = new World()
      val turn = w.parent("routed")
      val asked = request(turn, bound = Some(internal))
      val got = w.spawn(asked)
      (got, w.outputs(turn), w.replayed(asked)) ==> (
        Vector(s"Refused(Above($internal,${Label.Public}))"),
        Vector(
          "narrow:0:0" -> """{"refused":{"above":{"classification":"internal","room":"public"}}}"""
        ),
        Right(got)
      )
    }

    test(
      "a room its narrowing read at the parent's classification but the inbox keeps lower is refused Above at the spawn step"
    ) {
      val w = new World(labelling("moved", internal), stored = Some(Visibility.Shipped))
      val turn = w.parent("moved")
      val asked = request(turn, bound = Some(internal))
      val got = w.spawn(asked)
      (got, w.outputs(turn), w.made, w.replayed(asked)) ==> (
        Vector(s"Refused(Above($internal,${Label.Public}))"),
        Vector(
          "narrow:0:0" -> """{"go":{"at":"internal","within":"task:moved","cap":"0.05"}}""",
          "spawn:0:0" -> """{"above":{"classification":"internal","room":"public"}}"""
        ),
        Vector.empty,
        Right(got)
      )
    }

    test(
      "a ring sent before the wait wakes the first rung with its child's ending, taken though its end is not yet recorded, and the reply is read once"
    ) {
      val w = new World(does = Vector(replied("Found it.")), endsLate = true)
      val turn = w.parent("rung")
      val got = w.spawn(request(turn))
      val received = w.durable.history(turn.workflowId).collect {
        case InMemoryDurable.Step(name, o) if name.startsWith("DBOS.") => name -> o
      }
      (got, w.recorded(turn), received) ==> (
        Vector("Replied(c2, Found it.@member, Exact(0.01))"),
        Vector("narrow:0:0", "spawn:0:0", "read:0:0"),
        Vector(
          "DBOS.recv" -> InMemoryDurable.Outcome.Output("answered"),
          "DBOS.sleep" -> InMemoryDurable.Outcome.Output("300000")
        )
      )
    }

    test(
      "a ring whose message names no ending has its child's end read, as a timed-out wait does"
    ) {
      val w = new World(does = Vector(replied("Found it.")), ringing = _ => "rang")
      val turn = w.parent("rung-unread")
      (w.spawn(request(turn)), w.recorded(turn)) ==> (
        Vector("Replied(c2, Found it.@member, Exact(0.01))"),
        Vector("narrow:0:0", "spawn:0:0", "ended:0:0", "read:0:0")
      )
    }

    test("a child that never ends is Unanswered after six waits of Every, Within in all") {
      val w = new World()
      val turn = w.parent("silent")
      val got = w.spawn(request(turn))
      val waits = w.durable.history(turn.workflowId).collect {
        case InMemoryDurable.Step(InMemoryDurable.Sleep, InMemoryDurable.Outcome.Output(ms)) => ms
      }
      (got, w.recorded(turn), waits, Spawning.Every * 6) ==> (
        Vector("Unanswered(c2)"),
        Vector("narrow:0:0", "spawn:0:0") ++ Vector.fill(6)("ended:0:0"),
        Vector.fill(6)(Spawning.Every.toMillis.toString),
        Spawning.Within
      )
    }

    test("a child whose room the parent's transaction may not read is Unread, never its text") {
      val w = new World(labelling("internal-child", internal), Vector(replied("Secret.")))
      val turn = w.parent("internal-child")
      val asked = request(turn)
      val got = w.spawn(asked)
      (got, w.outputs(turn).lastOption, w.replayed(asked)) ==> (
        Vector("Unread(c2)"),
        Some("read:0:0" -> """{"unread":true,"spent":{"exact":"0.01"}}"""),
        Right(got)
      )
    }

    test(
      "a child ended at its limit or spent to its cap is Limited, one ended otherwise or replying nothing Failed, a replying one Replied, and each read replays"
    ) {
      val w = new World(does =
        Vector(
          Does.Ends(Some("Half."), s"half${TurnEnding.AtLimitSaid}", BigDecimal("0.01")),
          Does.Ends(None, "workflow error", BigDecimal("0.06")),
          Does.Ends(None, "workflow error", BigDecimal("0.01")),
          Does.Ends(Some("Dear."), s"${TurnEnding.Replied}: Dear.", BigDecimal("0.06")),
          Does.Ends(None, s"${TurnEnding.Replied}: ", BigDecimal("0.01")),
          Does.Ends(None, s"${TurnEnding.Replied}: ", BigDecimal("0.06"))
        )
      )
      val turn = w.parent("limited")
      val other = w.parent("failed")
      val limited = Vector(request(turn), request(turn, index = 1))
      val failed = Vector(request(other))
      val third = w.parent("answered")
      val answered = Vector(request(third), request(third, index = 1))
      val fourth = w.parent("silent-capped")
      val silent = Vector(request(fourth))
      val got = (w.spawn(limited*), w.spawn(failed*), w.spawn(answered*), w.spawn(silent*))
      val reads =
        Vector(turn, other, third, fourth).flatMap(w.outputs).filter(_._1.startsWith("read:"))
      (
        got,
        reads,
        w.replayed(limited*),
        w.replayed(failed*),
        w.replayed(answered*),
        w.replayed(silent*)
      ) ==> (
        (
          Vector(
            "Limited(c5, Some(Half.@member), Exact(0.01))",
            "Limited(c6, None, Exact(0.06))"
          ),
          Vector("Failed(c7, it ended without finishing its task)"),
          Vector("Replied(c8, Dear.@member, Exact(0.06))", "Failed(c9, it ended saying nothing)"),
          Vector("Limited(c10, None, Exact(0.06))")
        ),
        Vector(
          "read:0:0" -> """{"said":{"text":"Half.","integrity":"member"},"spent":{"exact":"0.01"}}""",
          "read:0:1" -> """{"silent":true,"spent":{"exact":"0.06"}}""",
          "read:0:0" -> """{"silent":true,"spent":{"exact":"0.01"}}""",
          "read:0:0" -> """{"said":{"text":"Dear.","integrity":"member"},"spent":{"exact":"0.06"}}""",
          "read:0:1" -> """{"silent":true,"spent":{"exact":"0.01"}}""",
          "read:0:0" -> """{"silent":true,"spent":{"exact":"0.06"}}"""
        ),
        Right(got._1),
        Right(got._2),
        Right(got._3),
        Right(got._4)
      )
    }

    test("a capped parent cannot grant what it granted an earlier child again") {
      val w = new World()
      val turn = w.parent("capped")
      w.inbox.charge(turn, BigDecimal("0.01"))
      val parent = Some(acting(turn, Allowance.Capped(usd("0.10"))))
      val asked = Vector(
        request(turn, parent = parent),
        request(turn, index = 1, parent = parent)
      )
      val got = w.spawn(asked*)
      (got, w.outputs(turn).lastOption, w.replayed(asked*)) ==> (
        Vector(
          "Unanswered(c2)",
          s"Refused(OverCap(${usd("0.05")},${usd("0.10")},${Cost.Exact(BigDecimal("0.06"))}))"
        ),
        Some(
          "narrow:0:1" -> """{"refused":{"overCap":{"asked":"0.05","cap":"0.10","spent":{"exact":"0.06"}}}}"""
        ),
        Right(got)
      )
    }

    test("a turn's spawn past PerTurn is OverLimit") {
      val w = new World()
      val turn = w.parent("many")
      val asked = (0 to Spawning.PerTurn).map(i => request(turn, index = i))
      val got = w.spawn(asked*)
      (got, w.outputs(turn).lastOption, w.replayed(asked*)) ==> (
        Vector("Unanswered(c2)", "Unanswered(c3)", s"OverLimit(${Spawning.PerTurn})"),
        Some(s"narrow:0:${Spawning.PerTurn}" -> """{"overLimit":2}"""),
        Right(got)
      )
    }

    test("a rerun after a crash reads its child back from the spawn's record, making it once") {
      val w = new World(crashAtEnded = true)
      val turn = w.parent("rerun")
      val crashed = scala.util.Try(w.spawn(request(turn))).toEither.left.map(_.getMessage)
      val again = scala.util.Try(w.spawn(request(turn))).toEither.left.map(_.getMessage)
      (crashed, again, w.spawnCalls, w.made.size) ==>
        (Left("simulated crash"), Right(Vector("Unanswered(c2)")), 1, 1)
    }

    test("each step records its pinned form, and a history of them replays to the same end") {
      // A ring that names no ending, so the end is read and its form recorded.
      val w = new World(does = Vector(replied("Found it.")), ringing = _ => "rang")
      val turn = w.parent("journal")
      val first = w.spawn(request(turn))
      (w.outputs(turn), w.replayed(request(turn))) ==> (
        Vector(
          "narrow:0:0" -> """{"go":{"at":"public","within":"task:journal","cap":"0.05"}}""",
          "spawn:0:0" -> """{"child":{"conversation":"c2","turn":0}}""",
          "ended:0:0" -> """{"ok":"answered"}""",
          "read:0:0" -> """{"said":{"text":"Found it.","integrity":"member"},"spent":{"exact":"0.01"}}"""
        ),
        Right(first)
      )
    }
  }
}
