package grit.act.phase

import scala.concurrent.duration.*

import grit.core.act.{Acting, ActsFor, Allowance, Gates}
import grit.core.agent.Subagent
import grit.core.agent.SubagentFixtures.{ok, scholar, services, subagent}
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

/** A spawn's steps over the in-memory inbox: refused before any, or by its narrowing; its child
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
    * made, and the first read of a child's end dying when `crashAtEnded`.
    */
  private final class World(
      visibility: Visibility = Visibility.Shipped,
      does: Vector[Does] = Vector.empty,
      crashAtEnded: Boolean = false,
      stored: Option[Visibility] = None
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
          classification: Label,
          by: Account
      ): Either[InboxError, TurnRef] = {
        spawnCalls += 1
        inbox.spawned(origin, agent, brief, spendCap, believed, classification, by).map { c =>
          if (!made.contains(c)) {
            val next = does.lift(made.size).getOrElse(Does.Nothing)
            made = made :+ c
            next match {
              case Does.Nothing => ()
              case Does.Ends(reply, outcome, usd) =>
                inbox.charge(c, usd)
                inbox.finish(c, reply.map(Message.said(_, "m")), outcome)
                durable.send(origin.parent.workflowId, SpawnInbox.topic(c), "rang")
            }
          }
          c
        }
      }
      def ended(child: TurnRef): Either[InboxError, Option[TurnEnding]] =
        if (crashing) { crashing = false; throw new InMemoryDurable.Crash }
        else inbox.ended(child)
      def reply(child: TurnRef)(using Tx^): Either[StoreError, SpawnReply] = inbox.reply(child)
      def spent(turn: TurnRef)(using Tx^): Either[StoreError, Cost] = inbox.spent(turn)
      def granted(parent: TurnRef)(using Tx^): Either[StoreError, Vector[SpendCap]] =
        inbox.granted(parent)
      def returned(child: TurnRef): Either[InboxError, Unit] = inbox.returned(child)
    }

    /** A parent turn of `task:{name}`, asked by ana, and its conversation's origin. */
    def parent(name: String): (TurnRef, Origin) = {
      val origin = Origin.Task(name, "parent")
      val turn = inbox
        .ingest(origin, SourceId(s"p-$name"), Message.User("look it up"), ana, None)
        .fold(e => throw new java.lang.AssertionError(e), identity)
      (turn, origin)
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
      requests.map(r => shown(Flows.run(spawning, SpawnEnv(spawns), r).merge)).mkString("\n")

    /** The steps `parent`'s workflow recorded, the runtime's own left out. */
    def recorded(parent: TurnRef): Vector[String] =
      durable.recordedSteps(parent.workflowId).filterNot(_.startsWith("DBOS."))
  }

  private val spawning = Spawning.spawn(steps, identity)

  private def acting(turn: TurnRef, allowance: Allowance = Allowance.Admitted): Acting =
    Acting(turn, ActsFor.Asker, allowance, Gates.Asker(1.minute), Integrity.Authority)

  private def request(
      turn: TurnRef,
      origin: Origin,
      index: Int = 0,
      as: Subagent = scholarType,
      parent: Option[Acting] = None,
      bound: Option[Label] = None
  ): Spawning.Request =
    CallSlot
      .of(turn, 0, index)
      .flatMap(call =>
        Spawning.Request.of(
          call,
          parent.getOrElse(acting(turn)),
          origin,
          as,
          "Find the paper.",
          Integrity.Member,
          ana,
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
    test("a spawn within a direct message's room is refused before any step") {
      val w = new World()
      val (turn, _) = w.parent("direct-parent")
      val dm = Origin.Direct(TestAccounts.sourced("slack:T1/U-ana"), "t1")
      (w.spawn(request(turn, dm)), w.recorded(turn), w.spawnCalls) ==> (
        Vector(s"Refused(Direct(${dm.room}))"),
        Vector.empty,
        0
      )
    }

    test("a spawn its narrowing refuses records that step alone, making nothing") {
      val w = new World()
      val (turn, origin) = w.parent("below")
      val strict = ok(subagent(scholar, services, Integrity.Authority))
      val asked = request(turn, origin, as = strict)
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
      val (turn, origin) = w.parent("routed")
      val asked = request(turn, origin, bound = Some(internal))
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
      val (turn, origin) = w.parent("moved")
      val asked = request(turn, origin, bound = Some(internal))
      val got = w.spawn(asked)
      (got, w.outputs(turn), w.made, w.replayed(asked)) ==> (
        Vector(s"Refused(Above($internal,${Label.Public}))"),
        Vector(
          "narrow:0:0" -> """{"go":{"at":"internal"}}""",
          "spawn:0:0" -> """{"above":{"classification":"internal","room":"public"}}"""
        ),
        Vector.empty,
        Right(got)
      )
    }

    test("a ring sent before the wait wakes the first rung, and the reply is read once") {
      val w = new World(does = Vector(replied("Found it.")))
      val (turn, origin) = w.parent("rung")
      val got = w.spawn(request(turn, origin))
      val received = w.durable.history(turn.workflowId).collect {
        case InMemoryDurable.Step(name, o) if name.startsWith("DBOS.") => name -> o
      }
      (got, w.recorded(turn), received) ==> (
        Vector("Replied(c2, Found it.@member, Exact(0.01))"),
        Vector("narrow:0:0", "spawn:0:0", "ended:0:0", "read:0:0"),
        Vector(
          "DBOS.recv" -> InMemoryDurable.Outcome.Output("rang"),
          "DBOS.sleep" -> InMemoryDurable.Outcome.Output("300000")
        )
      )
    }

    test("a child that never ends is Unanswered after six waits of Every, Within in all") {
      val w = new World()
      val (turn, origin) = w.parent("silent")
      val got = w.spawn(request(turn, origin))
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
      val (turn, origin) = w.parent("internal-child")
      val asked = request(turn, origin)
      val got = w.spawn(asked)
      (got, w.outputs(turn).lastOption, w.replayed(asked)) ==> (
        Vector("Unread(c2)"),
        Some("read:0:0" -> """{"unread":true,"spent":{"exact":"0.01"}}"""),
        Right(got)
      )
    }

    test(
      "a child's end and spend decide what it comes to, and its read replays"
    ) {
      val w = new World(does =
        Vector(
          Does.Ends(Some("Half."), s"half${TurnEnding.AtLimitSaid}", BigDecimal("0.01")),
          Does.Ends(None, "workflow error", BigDecimal("0.06")),
          Does.Ends(None, "workflow error", BigDecimal("0.01")),
          Does.Ends(Some("Dear."), s"${TurnEnding.Replied}: Dear.", BigDecimal("0.06")),
          Does.Ends(None, s"${TurnEnding.Replied}: ", BigDecimal("0.01"))
        )
      )
      val (turn, origin) = w.parent("limited")
      val (other, at) = w.parent("failed")
      val limited = Vector(request(turn, origin), request(turn, origin, index = 1))
      val failed = Vector(request(other, at))
      val (third, also) = w.parent("answered")
      val answered = Vector(request(third, also), request(third, also, index = 1))
      val got = (w.spawn(limited*), w.spawn(failed*), w.spawn(answered*))
      val reads = Vector(turn, other, third).flatMap(w.outputs).filter(_._1.startsWith("read:"))
      (got, reads, w.replayed(limited*), w.replayed(failed*), w.replayed(answered*)) ==> (
        (
          Vector(
            "Limited(c4, Some(Half.@member), Exact(0.01))",
            "Limited(c5, None, Exact(0.06))"
          ),
          Vector("Failed(c6, it ended without finishing its task)"),
          Vector("Replied(c7, Dear.@member, Exact(0.06))", "Failed(c8, it ended saying nothing)")
        ),
        Vector(
          "read:0:0" -> """{"said":{"text":"Half.","integrity":"member"},"spent":{"exact":"0.01"}}""",
          "read:0:1" -> """{"silent":true,"spent":{"exact":"0.06"}}""",
          "read:0:0" -> """{"silent":true,"spent":{"exact":"0.01"}}""",
          "read:0:0" -> """{"said":{"text":"Dear.","integrity":"member"},"spent":{"exact":"0.06"}}""",
          "read:0:1" -> """{"silent":true,"spent":{"exact":"0.01"}}"""
        ),
        Right(got._1),
        Right(got._2),
        Right(got._3)
      )
    }

    test("a capped parent cannot grant what it granted an earlier child again") {
      val w = new World()
      val (turn, origin) = w.parent("capped")
      w.inbox.charge(turn, BigDecimal("0.01"))
      val parent = Some(acting(turn, Allowance.Capped(usd("0.10"))))
      val asked = Vector(
        request(turn, origin, parent = parent),
        request(turn, origin, index = 1, parent = parent)
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
      val (turn, origin) = w.parent("many")
      val asked = (0 to Spawning.PerTurn).map(i => request(turn, origin, index = i))
      val got = w.spawn(asked*)
      (got, w.outputs(turn).lastOption, w.replayed(asked*)) ==> (
        Vector("Unanswered(c2)", "Unanswered(c3)", s"OverLimit(${Spawning.PerTurn})"),
        Some(s"narrow:0:${Spawning.PerTurn}" -> """{"overLimit":2}"""),
        Right(got)
      )
    }

    test("a rerun after a crash reads its child back from the spawn's record, making it once") {
      val w = new World(crashAtEnded = true)
      val (turn, origin) = w.parent("rerun")
      val crashed = scala.util.Try(w.spawn(request(turn, origin))).toEither.left.map(_.getMessage)
      val again = scala.util.Try(w.spawn(request(turn, origin))).toEither.left.map(_.getMessage)
      (crashed, again, w.spawnCalls, w.made.size) ==>
        (Left("simulated crash"), Right(Vector("Unanswered(c2)")), 1, 1)
    }

    test("each step records its pinned form, and a history of them replays to the same end") {
      val w = new World(does = Vector(replied("Found it.")))
      val (turn, origin) = w.parent("journal")
      val first = w.spawn(request(turn, origin))
      (w.outputs(turn), w.replayed(request(turn, origin))) ==> (
        Vector(
          "narrow:0:0" -> """{"go":{"at":"public"}}""",
          "spawn:0:0" -> """{"child":{"conversation":"c2","turn":0}}""",
          "ended:0:0" -> """{"ok":"answered"}""",
          "read:0:0" -> """{"said":{"text":"Found it.","integrity":"member"},"spent":{"exact":"0.01"}}"""
        ),
        Right(first)
      )
    }
  }
}
