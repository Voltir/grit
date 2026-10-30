package grit.eval.harness.reply

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.context.{
  AssemblyError,
  AssemblyRequest,
  CarriedWindow,
  ContextAssembler,
  Packing,
  Prepared,
  Width,
  Window
}
import grit.core.durable.StepRecord
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.Pinned
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.route.Routed
import grit.core.stitch.Tuning
import grit.core.store.{Db, Nearby, Payload, StoreError}
import grit.core.visibility.Subject
import grit.dbos.internal.Reader
import grit.models.Seed
import grit.turn.{Turn, TurnRecord}

/** How the shipped [[RetrievalAssembler]] is built to rebuild a window: the deployment's
  * `window` and `tail` budgets, its `hits` per search and the stitch `tuning`. A deployment
  * sets its window and tail from its environment, which no database records.
  */
final case class Assembled(window: Tokens, tail: Tokens, hits: Int, tuning: Tuning)

object Assembled {

  /** grit's own defaults: [[LinearAssembler.DefaultBudget]], [[RetrievalAssembler.DefaultTail]],
    * [[RetrievalAssembler.DefaultHits]] and [[Tuning.Default]].
    */
  val Shipped: Assembled = Assembled(
    LinearAssembler.DefaultBudget,
    RetrievalAssembler.DefaultTail,
    RetrievalAssembler.DefaultHits,
    Tuning.Default
  )
}

/** A recorded turn's window rebuilt: the one it recorded (`None` when it recorded none), the
  * rebuilt one, and the moment it was rebuilt as of: when the turn's `assemble` step started.
  * `lost` when the recorded window names an entry or a document the database no longer holds.
  */
final case class RebuiltWindow(
    at: Instant,
    recorded: Option[Window],
    rebuilt: Window,
    lost: Boolean
) {

  /** How the rebuilt window stands to the recorded one; `None` when none was recorded. */
  def drift: Option[Drift] = recorded.map(Drift.of(_, rebuilt, lost))
}

/** Windows rebuilt by the shipped assembler over a restored database, as of a moment
  * ([[AsOf]]), so a window can be drawn again at another width.
  */
object Rebuild {

  /** `turn`'s window, `width` wide, drawn by the shipped [[RetrievalAssembler]] built as
    * `assembled` says over `view`, its search query written by `writer` under `query` when it
    * writes one; `None` writes none. `Left` naming what failed, never a message's text.
    */
  def window(
      view: AsOf,
      turn: TurnRef,
      assembled: Assembled,
      writer: Provider^,
      width: Width,
      query: Option[Pinned]
  )(using Db^): Either[String, Window] =
    ContextAssembler
      .draw(shipped(view, assembled), AssemblyRequest(turn, width, CarriedWindow.Empty))(
        asked => query.fold(Left(Prepared.NoPin))(_ => writer.complete(asked).left.map(_.cause)),
        found => found
      )
      .map(_.window)
      .left
      .map(unbuilt(turn))

  /** The shipped [[RetrievalAssembler]], built as `assembled` says over `view`. */
  private[reply] def shipped(view: AsOf, assembled: Assembled): RetrievalAssembler =
    new RetrievalAssembler(
      view.entries,
      view.conversations,
      view.periods,
      view.principals,
      view.lifecycle,
      view.search,
      view.documents,
      view.pieces,
      view.stitches,
      CharEstimate,
      assembled.window,
      assembled.tail,
      assembled.tuning,
      assembled.hits
    )

  private def unbuilt(turn: TurnRef)(e: AssemblyError): String = e match {
    case AssemblyError.Store(e) =>
      s"the window of ${WorkflowId.value(turn.workflowId)} was not rebuilt: ${e.kind}"
  }

  /** What a read for `turn` through `db` was routed for (ADR 0031): whom a query written from
    * its window may be sent to ([[Routed.cleared]]), and the bound the window's reads are held
    * to ([[Routed.bound]]). `Left` naming what failed.
    */
  def routed(db: Db^, turn: TurnRef): Either[String, Routed] =
    db.read(Subject.Turn(turn))(Right(Routed.of))
      .left
      .map(e => s"the routing of ${WorkflowId.value(turn.workflowId)} was not read: ${e.kind}")

  /** The window of the turn `workflow` recorded, rebuilt by the shipped assembler over
    * `reader`'s database as it stood when the turn's `assemble` step started. A turn that
    * recorded its search ([[TurnRecord.retrieval]]) packs what `admission` admits of the
    * results it recorded by the verdicts it recorded, under the query it recorded: no search
    * is run, no query written, no classifier asked. One that recorded none is drawn again
    * ([[window]]), packing all its search finds, its query the one the turn recorded: asked
    * for one, the writer answers with it, so nothing is spent; a turn that recorded none is
    * rebuilt with a writer that fails, as live's did or was never asked. `Left` when the turn
    * recorded no `assemble` step or no start for it, or what it needs cannot be read.
    */
  def recorded(
      reader: Reader^,
      workflow: WorkflowId,
      assembled: Assembled,
      admission: Admission,
      width: Width
  ): Either[String, RebuiltWindow] = {
    val name = WorkflowId.value(workflow)
    def read[A](what: String)(body: (grit.core.store.Tx^) ?=> Either[StoreError, A]) =
      reader.all.read(body).left.map(e => s"$what of $name unread: ${e.kind}")
    for {
      begun <- this.begun(reader, workflow)
      Begun(turn, steps, at, query) = begun
      recorded <- read("window")(reader.entries.get(Turn.windowId(turn))).map(
        _.flatMap(e =>
          e.payload match {
            case w: Payload.Window => Some(Window.of(w))
            case _ => None
          }
        )
      )
      lost <- recorded.fold[Either[String, Boolean]](Right(false))(w =>
        read("recorded entries")(
          for {
            own <- reader.entries.at(turn.conversationId, w.entries)
            near <- Nearby.read(w.nearby, reader.entries)
            kept <- reader.documents.read(w.documents.distinct)
          } yield own.size < w.entries.distinct.size || near.size < w.nearby
            .map(_.names.distinct.size)
            .sum || kept.size < w.documents.distinct.size
        )
      )
      searched <- TurnRecord
        .retrieval(steps)
        .left
        .map(why => s"the search of $name does not read: $why")
      rebuilt <- searched match {
        case Some(r) =>
          shipped(AsOf(reader, at), assembled)
            .assemble(
              AssemblyRequest(turn, width, CarriedWindow.Empty),
              Packing.Searched(r.written, admission.admitted(r))
            )(using reader.db)
            .map(_.window)
            .left
            .map(unbuilt(turn))
        case None => replayed(AsOf(reader, at), begun, assembled, width)(using reader.db)
      }
    } yield RebuiltWindow(at, recorded, rebuilt, lost)
  }

  /** The window of the turn `workflow` recorded, drawn again by the shipped assembler
    * ([[window]]) over `reader`'s database as it stood when the turn's `assemble` step started,
    * but for its documents, read as they stood at `documents` ([[AsOf.documentsAsOf]]), so a
    * document written since can be found: its search run again under the query the turn
    * recorded, whatever it recorded of its search, packing all it finds, no classifier asked
    * and nothing spent. `Left` as [[recorded]]'s.
    */
  def redrawn(
      reader: Reader^,
      workflow: WorkflowId,
      assembled: Assembled,
      width: Width,
      documents: Instant
  ): Either[String, Window] =
    begun(reader, workflow).flatMap(b =>
      replayed(AsOf(reader, b.at).documentsAsOf(documents), b, assembled, width)(using reader.db)
    )

  /** What a turn recorded of its start: its `steps`, when its `assemble` step started (`at`),
    * and the query it recorded.
    */
  private final case class Begun(
      turn: TurnRef,
      steps: Vector[StepRecord],
      at: Instant,
      query: Option[String]
  )

  private def begun(reader: Reader^, workflow: WorkflowId): Either[String, Begun] = {
    val name = WorkflowId.value(workflow)
    for {
      turn <- TurnRef.fromWorkflowId(workflow).toRight(s"$name is not a turn")
      steps <- reader.steps(workflow).left.map(e => s"steps of $name unread: ${e.kind}")
      at <- steps
        .find(_.name == Turn.Step.Assemble)
        .toRight(s"$name recorded no assemble step")
        .flatMap(_.started.toRight(s"$name's assemble step recorded no start"))
      query <- reader.all
        .read(reader.entries.get(Turn.queryId(turn, 0)))
        .left
        .map(e => s"query of $name unread: ${e.kind}")
        .map(
          _.flatMap(e =>
            e.payload match {
              case Payload.Query(q) => Some(q)
              case _ => None
            }
          )
        )
    } yield Begun(turn, steps, at, query)
  }

  /** `begun`'s window drawn over `view`, the writer replaying the query the turn recorded, so
    * nothing is sent.
    */
  private def replayed(view: AsOf, begun: Begun, assembled: Assembled, width: Width)(using
      Db^
  ): Either[String, Window] =
    Seed.catalog.flatMap(seed =>
      window(
        view,
        begun.turn,
        assembled,
        new Replayed(begun.query),
        width,
        Some(seed.pin.query.first)
      )
    )

  /** `rebuilt`, each turn's window rebuilt or why not, as lines to print: how many were
    * rebuilt, each [[Drift]]'s count and how many recorded no window, then each turn that
    * drifted or was not rebuilt, by workflow id. Ids and counts alone.
    */
  def report(rebuilt: Vector[(WorkflowId, Either[String, RebuiltWindow])]): Vector[String] = {
    val drifts = rebuilt.flatMap((w, r) => r.toOption.map(x => w -> x.drift))
    def n(d: Drift) = drifts.count(_._2.contains(d))
    Vector(
      s"turns: ${rebuilt.size}; rebuilt ${drifts.size}, not rebuilt ${rebuilt.count(_._2.isLeft)}",
      s"  drift: same ${n(Drift.Same)}, elsewhere changed ${n(Drift.Elsewhere)}, own changed " +
        s"${n(Drift.Own)}, both ${n(Drift.Both)}, gone ${n(Drift.Gone)}, no window recorded " +
        s"${drifts.count(_._2.isEmpty)}"
    ) ++ drifts.collect {
      case (w, Some(d)) if d != Drift.Same => s"  ${WorkflowId.value(w)}: ${d.toString.toLowerCase}"
    } ++ rebuilt.collect { case (w, Left(why)) => s"  ${WorkflowId.value(w)}: $why" }
  }

  /** A query writer that answers every request with `query`, the one a turn recorded; or,
    * with none, fails every request.
    */
  private[reply] final class Replayed(query: Option[String]) extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      query match {
        case None => Left(ProviderError.Unavailable("the turn recorded no query"))
        case Some(q) =>
          Right(
            Message.Assistant(
              Vector(AssistantBlock.Text(q)),
              StopReason.EndTurn,
              Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
              "replayed"
            )
          )
      }
  }
}
