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

    /** Its start is lost, and the process dies before the spawn's step is recorded. */
    case Unstarted
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

  /** The in-memory inbox under `visibility`, its children doing as `does` says in the order they
    * are made, and the first read of a child's end dying when `crashAtEnded`.
    */
  private final class World(
      visibility: Visibility = Visibility.Shipped,
      does: Vector[Does] = Vector.empty,
      crashAtEnded: Boolean = false
  ) {
    val inbox: InMemoryInbox = InMemoryInbox.fresh(visibility = visibility)
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
              case Does.Unstarted =>
                inbox.started = inbox.started.filterNot(_ == c)
                throw new InMemoryDurable.Crash
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
      durable
        .run(requests.headOption.fold(sys.error("a request"))(_.call.turn.workflowId)) { _ =>
          requests.map(r => shown(Flows.run(spawning, SpawnEnv(spawns), r).merge)).mkString("\n")
        }
        .split("\n")
        .toVector

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
      (w.spawn(request(turn, origin, as = strict)), w.recorded(turn), w.spawnCalls) ==> (
        Vector(s"Refused(Below(${Integrity.Authority},${Integrity.Member}))"),
        Vector("narrow:0:0"),
        0
      )
    }

    test("a parent in a public room routed at Internal is refused Above a public room") {
      val w = new World()
      val (turn, origin) = w.parent("routed")
      w.spawn(request(turn, origin, bound = Some(internal))) ==>
        Vector(s"Refused(Above($internal,${Label.Public}))")
    }

    test("a ring sent before the wait wakes the first rung, and the reply is read once") {
      val w = new World(does = Vector(replied("Found it.")))
      val (turn, origin) = w.parent("rung")
      (w.spawn(request(turn, origin)), w.recorded(turn)) ==> (
        Vector("Replied(c2, Found it.@member, Exact(0.01))"),
        Vector("narrow:0:0", "spawn:0:0", "ended:0:0", "read:0:0")
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
      w.spawn(request(turn, origin)) ==> Vector("Unread(c2)")
    }

    test(
      "a child at its round limit, or past its spend cap, is Limited; one ended otherwise is Failed"
    ) {
      val w = new World(does =
        Vector(
          Does.Ends(Some("Half."), s"half${TurnEnding.AtLimitSaid}", BigDecimal("0.01")),
          Does.Ends(None, "workflow error", BigDecimal("0.06")),
          Does.Ends(None, "workflow error", BigDecimal("0.01"))
        )
      )
      val (turn, origin) = w.parent("limited")
      val (other, at) = w.parent("failed")
      (
        w.spawn(request(turn, origin), request(turn, origin, index = 1)),
        w.spawn(request(other, at))
      ) ==> (
        Vector(
          "Limited(c3, Some(Half.@member), Exact(0.01))",
          "Limited(c4, None, Exact(0.06))"
        ),
        Vector("Failed(c5, it ended without finishing its task)")
      )
    }

    test("a capped parent cannot grant what it granted an earlier child again") {
      val w = new World()
      val (turn, origin) = w.parent("capped")
      w.inbox.charge(turn, BigDecimal("0.01"))
      val parent = Some(acting(turn, Allowance.Capped(usd("0.10"))))
      w.spawn(
        request(turn, origin, parent = parent),
        request(turn, origin, index = 1, parent = parent)
      ) ==> Vector(
        "Unanswered(c2)",
        s"Refused(OverCap(${usd("0.05")},${usd("0.10")},${Cost.Exact(BigDecimal("0.06"))}))"
      )
    }

    test("a turn's spawn past PerTurn is OverLimit") {
      val w = new World()
      val (turn, origin) = w.parent("many")
      w.spawn((0 to Spawning.PerTurn).map(i => request(turn, origin, index = i))*) ==>
        Vector("Unanswered(c2)", "Unanswered(c3)", s"OverLimit(${Spawning.PerTurn})")
    }

    test("a rerun after a crash makes its child once, and one after a lost start starts it") {
      val w = new World(crashAtEnded = true)
      val (turn, origin) = w.parent("rerun")
      val crashed = scala.util.Try(w.spawn(request(turn, origin))).isFailure
      val again = w.spawn(request(turn, origin))
      val v = new World(does = Vector(Does.Unstarted))
      val (other, at) = v.parent("restart")
      val lost = scala.util.Try(v.spawn(request(other, at))).isFailure
      val startedBefore = v.inbox.started.size
      val restarted = v.spawn(request(other, at))
      (
        crashed,
        again,
        w.spawnCalls,
        lost,
        startedBefore,
        restarted,
        v.inbox.started.size,
        v.made.size
      ) ==> (true, Vector("Unanswered(c2)"), 1, true, 0, Vector("Unanswered(c2)"), 1, 1)
    }

    test("each step records its pinned form, and a history of them replays to the same end") {
      val w = new World(does = Vector(replied("Found it.")))
      val (turn, origin) = w.parent("journal")
      val first = w.spawn(request(turn, origin))
      val history = w.durable.history(turn.workflowId)
      val outputs = history.collect {
        case InMemoryDurable.Step(name, InMemoryDurable.Outcome.Output(o))
            if !name.startsWith("DBOS.") =>
          name -> o
      }
      val replayed = new InMemoryDurable().replay(turn.workflowId, history) { _ =>
        shown(Flows.run(spawning, SpawnEnv(w.spawns), request(turn, origin)).merge)
      }
      (outputs, replayed) ==> (
        Vector(
          "narrow:0:0" -> """{"go":{"at":"public"}}""",
          "spawn:0:0" -> """{"child":{"conversation":"c2","turn":0}}""",
          "ended:0:0" -> """{"ok":"answered"}""",
          "read:0:0" -> """{"said":{"text":"Found it.","integrity":"member"},"spent":{"exact":"0.01"}}"""
        ),
        Right(first.mkString)
      )
    }
  }
}
