package grit.lifecycle.post

import grit.core.clock.Clock
import grit.core.document.{DocumentKeeper, DocumentTerms}
import grit.core.durable.{Durable, Flow, Flows, Journaled, Step, StepName}
import grit.core.id.{PluginName, WorkflowId}
import grit.core.period.CloseOrdinal
import grit.core.plugin.{CacheDocs, Plugin, PluginCursors, PostRef}
import grit.core.retention.Target
import grit.core.store.{ClosedPeriod, Jot, PeriodStore, StoreError, Tombstones}
import grit.core.visibility.Subject

/** What posting works with besides its `Durable`: the closed periods, each plugin's cursor,
  * `cache`, which gives one plugin the documents of the one closed period it is posting,
  * `documents`, which gives one plugin its documents under the terms it declares,
  * `tombstones`, where a run marks the runs from a cursor it has moved past, `jot`, whose
  * transaction holds one post and its cursor's move together, and `clock`, which says when a
  * cursor starts again.
  */
final case class PostEnv(
    periods: PeriodStore,
    cursors: PluginCursors,
    cache: (PluginName, ClosedPeriod) -> CacheDocs,
    documents: (PluginName, DocumentTerms) -> DocumentKeeper,
    tombstones: Tombstones,
    jot: Jot^,
    clock: Clock^
)

/** Posting: one workflow per run of a plugin from its cursor ([[PostRef]]), on a queue of
  * its own partitioned by plugin. Each step, `post:n`, posts the next closed period after the
  * plugin's cursor, in close order, and moves the cursor past it, in one transaction: both
  * commit, or neither, so each closed period is posted once however often a run is
  * repeated. A run posts at most [[MaxPerRun]]; a `Left` from the plugin ends it, the cursor
  * left before that period for the next sweep to try again.
  */
object Posting {

  /** The most closed periods one run posts. */
  val MaxPerRun = 100

  /** The run of `ref`, posting for `plugin`, at its `n`-th period. */
  private final case class Run(ref: PostRef, plugin: Plugin, n: Int)

  private val posted: Step[PostEnv, Run, Either[String, Option[CloseOrdinal]]] =
    Step[PostEnv, Run, Either[String, Option[CloseOrdinal]]](
      StepName.keyed[Run]("post")(_.n.toString)
    )(env => run => next(run.ref, run.plugin, env).left.map(describe))

  /** One period posted, or the run's end: at [[MaxPerRun]], with nothing left, or stopped. */
  private val round: Flow[PostEnv, Run, String, Either[Run, String]] =
    Flow
      .fromEither((run: Run) =>
        if (run.n == MaxPerRun) Left(s"posted ${run.n}; more to come") else Right(run)
      )
      .andThen(Flow.step(posted).keeping)
      .map {
        case (run, Right(Some(_))) => Left(run.copy(n = run.n + 1))
        case (run, Right(None)) => Right(s"posted ${run.n}")
        case (run, Left(why)) => Right(s"posted ${run.n}; stopped: $why")
      }

  /** The posting workflow among the enabled `plugins`, from the run's workflow id: what it
    * did, for logs, at whichever end. A plugin with nothing to post ([[Plugin.posts]]) takes no
    * step.
    */
  def flow(plugins: Vector[Plugin]): Flow[PostEnv, WorkflowId, String, String] =
    Flow
      .fromEither((id: WorkflowId) =>
        WorkflowId.as("post", PostRef.fromWorkflowId)(id).flatMap { ref =>
          plugins.find(p => p.name == ref.plugin && p.version == ref.version) match {
            case None =>
              Left(s"no plugin ${PluginName.value(ref.plugin)} at version ${ref.version}")
            case Some(plugin) if !plugin.posts =>
              Left(s"plugin ${PluginName.value(ref.plugin)} posts nothing")
            case Some(plugin) => Right(Run(ref, plugin, 0))
          }
        }
      )
      .andThen(Flow.loop(round))

  /** The posting workflow for the run whose workflow id is `workflowId`: [[flow]] run. */
  def body(plugins: Vector[Plugin], env: PostEnv^)(
      workflowId: WorkflowId
  )(using d: Durable^): String =
    Flows.run(flow(plugins), env, workflowId).merge

  /** The next closed period after `plugin`'s cursor posted (its cache, then its documents)
    * and the cursor moved past it, and the runs from `ref`'s cursor marked for deletion; its
    * close ordinal, or `None` when there is none. The run making the mark is one of those runs:
    * the collector waits for it.
    *
    * Two transactions. The first, public, starts the cursor and finds which period closed next
    * and in which conversation, whatever its label. The second, opened for that conversation,
    * so what it keeps is kept at the conversation's label, posts it, refusing when the period
    * after the cursor is no longer that one.
    */
  private def next(
      ref: PostRef,
      plugin: Plugin,
      env: PostEnv^
  ): Either[StoreError, Option[CloseOrdinal]] =
    env.jot
      .write(Subject.Public) {
        for {
          cursor <- env.cursors.start(plugin.name, plugin.version, env.clock.now())
          found <- env.periods.nextClosed(cursor)
        } yield found
      }
      .flatMap {
        case None => Right(None)
        case Some((ordinal, conversation)) =>
          env.jot.write(Subject.Conversation(conversation))(post(ref, plugin, env, ordinal))
      }

  /** Posts the closed period after `plugin`'s cursor, which must be `ordinal`'s. */
  private def post(ref: PostRef, plugin: Plugin, env: PostEnv^, ordinal: CloseOrdinal)(using
      grit.core.store.Tx^
  ): Either[StoreError, Option[CloseOrdinal]] =
    for {
      cursor <- env.cursors.start(plugin.name, plugin.version, env.clock.now())
      after <- env.periods.closedAfter(cursor, 1)
      closed <- after.headOption
        .filter(_.order == ordinal)
        .toRight(
          StoreError.Invalid(
            s"the period after the cursor is no longer the one closed at ${CloseOrdinal.value(ordinal)}"
          )
        )
      _ <- plugin.cache.fold(Right(()))(_.post(closed, env.cache(plugin.name, closed)))
      _ <- plugin.documents.fold(Right(()))(d =>
        d.posting.fold(Right(()))(_.post(closed, env.documents(plugin.name, d.terms)))
      )
      _ <- env.cursors.advance(plugin.name, plugin.version, closed.order)
      _ <- env.tombstones.write(
        Target.PostRuns(ref.plugin, ref.version, ref.cursor),
        env.clock.now()
      )
    } yield Some(closed.order)

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${grit.core.id.EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
    case StoreError.Full(bound) =>
      s"its plugin already holds the $bound documents it may, so nothing was kept"
  }

  /** A step's output: `{"ok": ordinal}` for a period posted, `{"ok": null}` when there was
    * none, `{"failed": why}`. Recorded, so change it only with the workflow's epoch.
    */
  private given Journaled[Either[String, Option[CloseOrdinal]]] =
    Journaled.json[Either[String, Option[CloseOrdinal]]](
      {
        case Right(Some(o)) => ujson.Obj("ok" -> CloseOrdinal.value(o).toDouble)
        case Right(None) => ujson.Obj("ok" -> ujson.Null)
        case Left(why) => ujson.Obj("failed" -> why)
      },
      v =>
        v.objOpt.map(o => (o.get("ok"), o.get("failed"))) match {
          case Some((Some(ujson.Null), None)) => Right(Right(None))
          case Some((Some(ujson.Num(n)), None)) if n.isWhole =>
            CloseOrdinal.of(n.toLong).map(o => Right(Some(o))).toRight(s"post: bad ordinal $n")
          case Some((None, Some(ujson.Str(why)))) => Right(Left(why))
          case _ => Left("post: expected {ok} or {failed}")
        }
    )
}
