package grit.act.phase

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.{ActsFor, Gates, Moves}
import grit.core.approval.Approval
import grit.core.durable.{InMemoryDurable, Journaled}
import grit.core.edge.{InMemoryEdges, OutcomeJson, Permit, RequestState, ToolRequest, ToolRequests}
import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  JobName,
  PrincipalId,
  ScheduleId,
  ScheduleKey,
  TestCallSlots,
  ToolCallId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.Principal
import grit.core.job.{Declared, InMemorySchedules, JobRun, PlainJob, ScheduleContract, SlotRule}
import grit.core.message.AssistantBlock
import grit.core.place.{Directory, Place, Service}
import grit.core.store.{Askers, StoreError, Tx}
import grit.core.tool.{
  Args,
  Bound,
  Field,
  Gate,
  Hosted,
  Outcome,
  Repairs,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox
}
import grit.core.visibility.{Clearance, Label, Level, Subject}
import grit.dbos.sql.TestTx

import utest.*

/** A hosted call's phases: its gate, its principal, its request sent, the wait for its edge, a
  * person's approval and the answer read.
  */
object CallingTests extends TestSuite {

  private val turn = TurnRef(ConversationId("c"), TurnSeq.First)
  private val cs: CallSlot = TestCallSlots.at(turn)
  private val id: WorkflowId = turn.workflowId

  private val github: Service =
    Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)

  private def spec(name: ToolName): ToolSpec[String] =
    ToolSpec(name, "A tool.", Args.of((path = Field.text("A path."))).map(_.path))

  /** `fetch` asks no one; `prod` asks a person, shown "Prod {path}?". */
  private val box: Toolbox[{}] =
    Toolbox
      .of(
        new Hosted(spec(ToolName("fetch")), Gate.Free, p => p),
        new Hosted(spec(ToolName("prod")), Gate.Ask(p => s"Prod $p?"), p => p)
      )
      .fold(d => throw new java.lang.AssertionError(d.toString), identity)

  private def bound(tool: String): Bound.Hosted =
    box.requested(
      AssistantBlock.ToolCall(ToolCallId("c1"), tool, ujson.Obj("path" -> "a")),
      Repairs.All,
      None
    ) match {
      case Right(h: Bound.Hosted) => h
      case other => throw new java.lang.AssertionError(s"not hosted: $other")
    }

  private def requestAt(place: Place, slot: CallSlot = cs): ToolRequest =
    Calling.request(slot, bound("fetch"), place, Permit.Free, PrincipalId.Grit)

  /** Askers that resolve every turn to `asker`. */
  private def askers(asker: Option[Principal]): Askers = new Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(asker)
  }

  private final case class NoParams() extends caps.Pure

  private object Nightly extends PlainJob[NoParams] {
    def name: JobName = JobName.of("nightly").fold(sys.error, identity)
    def version: Int = 1
    def write(params: NoParams): ujson.Value = ujson.Obj()
    def read(params: ujson.Value): Either[String, NoParams] = Right(NoParams())
    def run(run: JobRun[NoParams], moves: Moves^): String = "done"
  }

  private val key: ScheduleKey = ScheduleKey.of("nightly").fold(sys.error, identity)

  // A phase's failures, as a test's caller records them.
  private given Faults[String] with {
    def store(reason: String): String = s"store: $reason"
  }

  private given Journaled[Either[String, RequestState]] =
    Journaled.json(
      {
        case Left(why) => ujson.Obj("failed" -> why)
        case Right(RequestState.Expired) => ujson.Obj("state" -> "expired")
        case Right(RequestState.Claimed) => ujson.Obj("state" -> "claimed")
        case Right(RequestState.Answered(o)) => ujson.Obj("answered" -> OutcomeJson.write(o))
      },
      v =>
        v.objOpt.toRight("not an object").flatMap { o =>
          (
            o.get("failed").flatMap(_.strOpt),
            o.get("state").flatMap(_.strOpt),
            o.get("answered")
          ) match {
            case (Some(why), _, _) => Right(Left(why))
            case (_, Some("expired"), _) => Right(Right(RequestState.Expired))
            case (_, Some("claimed"), _) => Right(Right(RequestState.Claimed))
            case (_, _, Some(a)) => OutcomeJson.read(a).map(o => Right(RequestState.Answered(o)))
            case _ => Left("not a state")
          }
        }
    )

  private val steps = WaitSteps("expire:0:0", "abandon:0:0")

  /** What happens to a request while the wait is in a step: nothing, its edge rings, or it is
    * answered `outcome`.
    */
  private enum Meanwhile {
    case Quiet
    case Rings
    case Answered(outcome: Outcome)
  }

  /** `edges`' requests, but the `expire` step's settle and the `abandon` step's abandon are
    * each met by what `atExpire` and `atAbandon` say happens meanwhile: an edge registered as
    * `edge` rings `durable`'s workflow, or answers.
    */
  private final class Racing(
      edges: InMemoryEdges,
      durable: InMemoryDurable,
      edge: grit.core.edge.Registration,
      atExpire: Meanwhile,
      atAbandon: Meanwhile
  ) extends ToolRequests {
    private def happen(m: Meanwhile, slot: CallSlot): Unit = m match {
      case Meanwhile.Quiet => ()
      case Meanwhile.Rings => durable.send(slot.turn.workflowId, slot.key, "rang")
      case Meanwhile.Answered(o) =>
        val _ = edges.answerAs(edge, slot, o)
    }
    def dispatch(requests: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit] =
      edges.dispatch(requests)
    def settle(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = {
      happen(atExpire, slot)
      edges.settle(slot)
    }
    def abandon(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = {
      happen(atAbandon, slot)
      edges.abandon(slot)
    }
    def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]] =
      edges.answered(slot)
  }

  /** A live edge serving `github`, advertising no tools there. */
  private def serving: InMemoryEdges = {
    val edges = new InMemoryEdges
    edges.advertiseAs(edges.register(Set(github.place)), github.place, ToolSet.Empty, Vector.empty)
    edges
  }

  /** An edge serving `github`, with the request for `cs` sent to it, claimed when `claimed`. */
  private def sent(claimed: Boolean): (InMemoryEdges, grit.core.edge.Registration) = {
    val edges = new InMemoryEdges
    val edge = edges.register(Set(github.place))
    val q = requestAt(github.place)
    val _ = edges.dispatch(Vector(q))(using TestTx.fake)
    if (claimed) { val _ = edges.claimAs(edge, q) }
    (edges, edge)
  }

  /** What the wait for `cs` comes to over `requests`, and the steps it recorded. */
  private def waited(
      durable: InMemoryDurable,
      requests: ToolRequests,
      place: Option[Place] = Some(github.place),
      rung: Boolean = false
  ): (String, Vector[String]) = {
    val got = durable.run(id) { _ =>
      // Rung before the wait begins, as an edge that answered at once has.
      if (rung) durable.send(id, cs.key, "rang")
      Calling.await(requests, cs, place, steps, Subject.Turn(turn)).toString
    }
    (got, durable.recordedSteps(id).filterNot(_.startsWith("DBOS.")))
  }

  /** The waits `durable`'s run for `cs` recorded, in milliseconds. */
  private def waits(durable: InMemoryDurable): Vector[String] =
    durable.history(id).collect {
      case InMemoryDurable.Step(InMemoryDurable.Sleep, InMemoryDurable.Outcome.Output(ms)) => ms
    }

  val tests = Tests {
    test("a call whose tool asks no one is free under every gate") {
      (
        Calling.gate(Gates.Asker(1.minute), bound("fetch")),
        Calling.gate(Gates.Closed, bound("fetch"))
      ) ==>
        (Gated.Free, Gated.Free)
    }

    test("a call whose tool asks waits on its asker's approval, shown what its tool asks") {
      Calling.gate(Gates.Asker(1.minute), bound("prod")) ==> Gated.Ask("Prod a?", 1.minute)
    }

    test("under closed gates a call whose tool asks is refused, saying nobody can approve it") {
      Calling.gate(Gates.Closed, bound("prod")) ==> Gated.Refused(
        Outcome.Failed(
          "prod asks a person first, and nobody waits on this call to approve it, so it did not run."
        )
      )
    }

    test("a call for its turn's asker is made for grit as grit, and for a person as their id") {
      given Tx = TestTx.fake
      val schedules = new InMemorySchedules()
      val person = Principal.Person(PrincipalId.Local, Set.empty)
      (
        Calling.principal(ActsFor.Asker, cs, askers(Some(Principal.Grit)), schedules),
        Calling.principal(ActsFor.Asker, cs, askers(Some(person)), schedules)
      ) ==> (Right(Some(PrincipalId.Grit)), Right(Some(PrincipalId.Local)))
    }

    test("a call for a turn with no asker is made for no one") {
      given Tx = TestTx.fake
      Calling.principal(ActsFor.Asker, cs, askers(None), new InMemorySchedules()) ==> Right(None)
    }

    test("a scheduled call is made for its schedule's principal, and for no one once it is gone") {
      given Tx = TestTx.fake
      val schedules = new InMemorySchedules()
      val at = Instant.parse("2026-10-08T12:00:00Z")
      val declared = Declared(key, Nightly, SlotRule.Once(at, ScheduleContract.hour), NoParams())
      val _ = schedules.declare(Vector(Declarer.Deployment -> declared), at)
      val kept = ScheduleId.declared(Declarer.Deployment, key)
      val gone = ScheduleId.declared(
        Declarer.Deployment,
        ScheduleKey.of("gone").fold(sys.error, identity)
      )
      val person = askers(Some(Principal.Person(PrincipalId.Local, Set.empty)))
      (
        Calling.principal(ActsFor.Scheduled(kept), cs, person, schedules),
        Calling.principal(ActsFor.Scheduled(gone), cs, person, schedules)
      ) ==> (Right(Some(PrincipalId.Grit)), Right(None))
    }

    test("requests are sent only to a live edge serving their place; to none, nothing is written") {
      val edges = serving
      val elsewhere = Place.of(Directory.of("/x").fold(sys.error, identity))
      val other = requestAt(elsewhere, TestCallSlots.at(turn, index = 1))
      given Tx = TestTx.fake
      val toGithub =
        Calling.dispatch(edges, edges, github.place, Vector(requestAt(github.place), other))
      val unserved = Calling.dispatch(edges, edges, elsewhere, Vector(other))
      (toGithub, unserved, edges.rows.map(_.request.slot)) ==> (
        Right(true),
        Right(false),
        Vector(cs)
      )
    }

    test("a request its transaction may not send is written answered with its refusal") {
      val edges = serving
      given Tx = TestTx.fake(Clearance.of(Label.at(Level.Internal)))
      val went = Calling.dispatch(edges, edges, github.place, Vector(requestAt(github.place)))
      (went, edges.rows.map(_.state)) ==> (
        Right(true),
        Vector(
          RequestState.Answered(
            Outcome.Failed("Nothing was sent: this conversation may not send to service:github.")
          )
        )
      )
    }

    test("a request whose edge rings within ServeWithin is rung, settling nothing") {
      val durable = new InMemoryDurable()
      val (edges, _) = sent(claimed = true)
      (waited(durable, edges, rung = true), waits(durable)) ==> (
        ("Rung(" + cs + ")", Vector.empty),
        Vector("10000")
      )
    }

    test("a request no edge claimed is expired, and answered that nothing serves its service") {
      val durable = new InMemoryDurable()
      val (edges, _) = sent(claimed = false)
      waited(durable, edges) ==> (
        "Known(Failed(No edge is serving github right now, so this call did not run.))",
        Vector("expire:0:0")
      )
    }

    test("a request answered as its wait ends is known by its answer, at either step") {
      val atExpire = new InMemoryDurable()
      val (early, edge) = sent(claimed = true)
      val answered = Meanwhile.Answered(Outcome.Done("ok"))
      val first = waited(atExpire, new Racing(early, atExpire, edge, answered, Meanwhile.Quiet))
      val atAbandon = new InMemoryDurable()
      val (late, other) = sent(claimed = true)
      val second = waited(atAbandon, new Racing(late, atAbandon, other, Meanwhile.Quiet, answered))
      (first, second) ==> (
        ("Known(Done(ok))", Vector("expire:0:0")),
        ("Known(Done(ok))", Vector("expire:0:0", "abandon:0:0"))
      )
    }

    test("a claimed request is waited on RunWithin more, then rung when its edge rings") {
      val durable = new InMemoryDurable()
      val (edges, edge) = sent(claimed = true)
      val got = waited(durable, new Racing(edges, durable, edge, Meanwhile.Rings, Meanwhile.Quiet))
      (got, waits(durable)) ==> (
        ("Rung(" + cs + ")", Vector("expire:0:0")),
        Vector("10000", "660000")
      )
    }

    test("a claimed request its edge never answers is abandoned, and the call interrupted") {
      val durable = new InMemoryDurable()
      val (edges, _) = sent(claimed = true)
      (waited(durable, edges), edges.rows.map(_.state)) ==> (
        ("Known(Interrupted)", Vector("expire:0:0", "abandon:0:0")),
        Vector(RequestState.Expired)
      )
    }

    test("a request that cannot be read is unread, its failure in its caller's form") {
      val durable = new InMemoryDurable()
      waited(durable, new InMemoryEdges) ==> (
        "Unread(store: Invalid(no tool request " + cs.key + "))",
        Vector("expire:0:0")
      )
    }

    test(
      "a person's approval is read from its call's topic; none in time, or unread, runs nothing"
    ) {
      val call = ToolCallId("c1")
      def answered(message: Option[String]): Approval = {
        val durable = new InMemoryDurable()
        val got = durable.run(id) { _ =>
          message.foreach(durable.send(id, Approval.topic(call), _))
          Approval.encode(Calling.approval(call, 1.minute))
        }
        Approval.decode(got).fold(e => throw new java.lang.AssertionError(e), identity)
      }
      Vector(Some(Approval.encode(Approval.Approved)), None, Some("yes")).map(answered) ==> Vector(
        Approval.Approved,
        Approval.TimedOut,
        Approval.Declined(
          Some("The answer could not be read (not a JSON object), so it did not run.")
        )
      )
    }

    test(
      "a rung request's answer is read; one holding none or gone is Failed, saying so, and an unreadable one is the caller's failure"
    ) {
      given Tx = TestTx.fake
      val (open, _) = sent(claimed = true)
      val (done, edge) = sent(claimed = true)
      val _ = done.answerAs(edge, cs, Outcome.Done("ok"))
      val broken = new ToolRequests {
        def dispatch(requests: Vector[ToolRequest])(using Tx^) = Right(())
        def settle(slot: CallSlot)(using Tx^) = Right(RequestState.Expired)
        def abandon(slot: CallSlot)(using Tx^) = Right(RequestState.Expired)
        def answered(slot: CallSlot)(using Tx^) =
          Left(StoreError.DatabaseError("the database is down"))
      }
      Vector(done, open, new InMemoryEdges, broken).map(Calling.answer[String](_, cs)) ==> Vector(
        Right(Outcome.Done("ok")),
        Right(
          Outcome.Failed(
            "The edge rang, but its request holds no answer, so this call's result is unknown."
          )
        ),
        Right(Outcome.Failed("This call's request is no longer kept, so its result is unknown.")),
        Left("store: DatabaseError(the database is down)")
      )
    }
  }
}
