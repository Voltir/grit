package grit.act.moves

import java.time.{Instant, ZoneOffset}

import grit.core.act.{Acting, ActsFor, Allowance, Gates, Keeping, MoveLimits, Moves}
import grit.core.clock.SetClock
import grit.core.document.{DocLabel, DocWeight, DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.durable.InMemoryDurable
import grit.core.edge.Advert
import grit.core.edge.{
  EdgeDirectory,
  InMemoryEdges,
  Registration,
  RequestState,
  ToolRequest,
  ToolRequests
}
import grit.core.id.{CallSlot, ConversationId, PluginName, TurnRef, TurnSeq}
import grit.core.identity.Principal
import grit.core.job.InMemorySchedules
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.Pinned
import grit.core.place.Place
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, TokenEstimator}
import grit.core.spend.{Budget, DailyCap, Day, Spend, Spending}
import grit.core.store.{Askers, Db, InMemoryToolSets, InMemoryUsageLedger, StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolSet}
import grit.core.visibility.{Clearance, Label, Subject}
import grit.dbos.sql.TestTx

/** [[MovesContract]] against [[DurableMoves]], over [[InMemoryDurable]] and core's in-memory
  * stores, with a live edge at [[MovesContract.Probe]] that answers each request as it is sent.
  */
object DurableMovesTests extends MovesContract {

  def within[A <: caps.Pure](limits: MoveLimits, broken: Boolean)(use: Moves^ -> A): A = {
    val w = new MovesWorld(broken)
    w.run(limits)(use)
  }
}

/** A world for [[DurableMoves]]: a turn whose asker is grit, a model answering
  * [[MovesContract.Answer]], an edge at [[MovesContract.Probe]] advertising
  * [[MovesContract.Tool]] that claims, answers [[MovesContract.Read]] and rings each request as
  * it is written, and the documents of [[MovesWorld.Notes]], whose every transaction is opened
  * at `floor`. When `broken`, the day's spend cannot be read, and the acting's allowance is
  * daily, so every ask's admission fails.
  */
final class MovesWorld(broken: Boolean, floor: Label = Label.Public) extends caps.SharedCapability {
  import MovesContract.*
  import MovesWorld.*

  val durable: InMemoryDurable = new InMemoryDurable(resolve = _ => Clearance.of(floor))
  val documents: InMemoryDocuments = new InMemoryDocuments
  val edges: InMemoryEdges = new InMemoryEdges
  val ledger: InMemoryUsageLedger = new InMemoryUsageLedger
  val toolSets: InMemoryToolSets = new InMemoryToolSets
  val clock: SetClock = new SetClock(Start)
  private val edge: Registration = edges.register(Set(Probe.place))

  {
    val set = ToolSet
      .of(Vector(ToolSet.Entry(Tool, "Reads a path.", Schema, asks = false, Retry.Rerun)))
      .fold(d => throw new java.lang.AssertionError(d.toString), identity)
    val _ = toolSets.keep(set)(using TestTx.fake)
    edges.advertiseAs(edge, Probe.place, set, Vector.empty)
  }

  /** `edges`, but each request is claimed, answered [[MovesContract.Read]] and its workflow
    * rung as it is written.
    */
  private object answering extends ToolRequests, EdgeDirectory {
    def dispatch(requests: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit] = {
      val sent = edges.dispatch(requests)
      requests.foreach { q =>
        if (edges.claimAs(edge, q) && edges.answerAs(edge, q.slot, Outcome.Done(Read)))
          durable.send(q.slot.turn.workflowId, q.slot.key, "rang")
      }
      sent
    }
    def settle(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = edges.settle(slot)
    def abandon(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = edges.abandon(slot)
    def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]] =
      edges.answered(slot)
    def serving(place: Place)(using Tx^): Either[StoreError, Option[Advert]] = edges.serving(place)
  }

  private val spending: Spending = if (broken) Unreadable else ledger

  def env(models: Models^): MovesEnv^ =
    MovesEnv(
      MoveRecords(
        ledger,
        spending,
        answering,
        answering,
        toolSets,
        new InMemorySchedules(),
        GritAsks,
        PerChar,
        documents.savepoints
      ),
      models,
      FakeDb,
      clock
    )

  /** The acting of [[Turn]]: its asker's, closed, admitted, or daily under a cap when
    * `broken`.
    */
  val acting: Acting = Acting(
    Turn,
    ActsFor.Asker,
    if (broken) Allowance.Daily(Budget(ZoneOffset.UTC, DailyCap.of("1").toOption))
    else Allowance.Admitted,
    Gates.Closed
  )

  /** What `use` returns over [[Turn]]'s moves within `limits`, its workflow run once. */
  def run[A <: caps.Pure](limits: MoveLimits)(use: Moves^ -> A): A = {
    val models = new Answering
    var out: Option[A] = None
    durable.run(Turn.workflowId) { _ =>
      out = Some(DurableMoves.plain(acting, limits, env(models))(use))
      "done"
    }
    out.getOrElse(throw new java.lang.AssertionError("the workflow did not run"))
  }

  /** [[Notes]]' documents as it writes them. */
  val keeper: DocumentKeeper = documents.keeper(Notes, NotesTerms)

  /** What `use` returns over [[Turn]]'s moves within `limits`, keeping [[Notes]]' documents
    * through `keeper`, its workflow run once.
    */
  def keeping[A <: caps.Pure](limits: MoveLimits, keeper: DocumentKeeper = keeper)(
      use: Keeping^ -> A
  ): A = {
    val models = new Answering
    var out: Option[A] = None
    durable.run(Turn.workflowId) { _ =>
      out = Some(DurableMoves.keeping(acting, limits, env(models), keeper)(use))
      "done"
    }
    out.getOrElse(throw new java.lang.AssertionError("the workflow did not run"))
  }
}

object MovesWorld {

  val Start: Instant = Instant.parse("2026-10-08T12:00:00Z")

  val Turn: TurnRef = TurnRef(ConversationId("c"), TurnSeq.First)

  /** The plugin whose documents a keep writes. */
  val Notes: PluginName =
    PluginName.of("notes").fold(e => throw new java.lang.AssertionError(e), identity)

  val NotesTerms: DocumentTerms =
    DocLabel
      .of("notes")
      .flatMap(
        DocumentTerms.of(_, DocWeight.Unscaled, scala.concurrent.duration.Duration(1, "day"), 10)
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  val Schema: ujson.Value =
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string"))
    )

  /** Models whose every provider answers [[MovesContract.Answer]], counting its calls. */
  final class Answering extends Models {
    @caps.unsafe.untrackedCaptures
    var calls = 0
    def catalog(): Either[String, grit.core.model.Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ = new Provider {
      def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] = {
        calls += 1
        Right(
          Message.Assistant(
            Vector(AssistantBlock.Text(MovesContract.Answer)),
            StopReason.EndTurn,
            Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
            "test/summary"
          )
        )
      }
    }
  }

  val TestCatalog: grit.core.model.Catalog = {
    import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
    def role(name: String) =
      Assignment(
        ModelRef(
          ModelId.of(s"test/$name").getOrElse(throw new java.lang.AssertionError(name)),
          None
        ),
        1024,
        None
      )
    grit.core.model.Catalog.of(
      Policy(role("turn"), role("summary"), role("query"), role("summary")),
      Vector.empty
    )
  }

  object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Every turn's asker is grit. */
  object GritAsks extends Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] =
      Right(Some(Principal.Grit))
  }

  object PerChar extends TokenEstimator {
    def message(message: Message): Tokens = Tokens(message.toString.length.toLong)
    def system(prompt: String): Tokens = Tokens(prompt.length.toLong)
  }

  object Unreadable extends Spending {
    def on(day: Day)(using Tx^): Either[StoreError, Spend] = Left(StoreError.DatabaseError("down"))
    def conversation(id: ConversationId)(using Tx^): Either[StoreError, Spend] =
      Left(StoreError.DatabaseError("down"))
  }
}
