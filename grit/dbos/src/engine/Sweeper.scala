package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.jdk.CollectionConverters.*

import grit.core.id.{CloseRef, PluginName, SettleRef, WorkflowId}
import grit.core.plugin.{PluginCursors, PostRef}
import grit.core.retention.Target
import grit.core.store.{
  ConversationStore,
  EntryStore,
  LifecycleStore,
  ModelProfileStore,
  PeriodStore,
  StoreError,
  Tombstones,
  Tx,
  UsageLedger
}
import grit.dbos.workflow.{Closes, Posts, Settles}

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.workflow.ListWorkflowsInput

/** One sweep of the lifecycle. Every decision it makes is recomputed from the database, and
  * every action it takes is a workflow under a deterministic id, so a missed or doubled sweep,
  * a crash, or two engines sweeping at once change nothing.
  */
private[engine] final class Sweeper(
    dataSource: DataSource,
    client: DBOSClient,
    conversations: ConversationStore,
    entries: EntryStore,
    periods: PeriodStore,
    ledger: UsageLedger,
    profiles: ModelProfileStore,
    lifecycle: LifecycleStore,
    tombstones: Tombstones,
    cursors: PluginCursors,
    plugins: () -> Vector[(PluginName, Int)]
) {

  private val collector = new Collector(
    dataSource,
    client,
    conversations,
    entries,
    periods,
    ledger,
    profiles,
    cursors,
    tombstones
  )

  private def attempted[A](body: => A): Either[StoreError, A] = Transact.attempted(body)

  private def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    Transact.write(dataSource)(body)

  private def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    Transact.read(dataSource)(body)

  /** Closes and asks, then posts, then collects: see [[Engine.sweep]]. */
  def once(now: Instant): Either[StoreError, Swept] =
    for {
      settings <- read(lifecycle.current())
      open <- read(periods.open())
      (due, quiet) = open.partition(a => !a.due(settings).at.isAfter(now))
      closed <- due.foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, a) =>
        acc.flatMap(done => close(a.attempt(settings)).map(done + _))
      }
      asked <- quiet
        .filter(_.asks(settings).exists(!_.isAfter(now)))
        .foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, a) =>
          acc.flatMap(done => ask(a.question).map(done + _))
        }
      disabled <- enabling(now)
      posted <- plugins().foldLeft[Either[StoreError, Swept]](Right(disabled)) { (acc, p) =>
        acc.flatMap(done => post(p._1, p._2, now).map(done + _))
      }
      collected <- collector.once(settings, now)
    } yield closed + asked + posted + collected

  /** Every enabled plugin's disabled tombstone spared, and every other plugin with a cursor
    * marked for deletion at `now` ([[Target.Disabled]]); those newly marked are `disabled`.
    */
  private def enabling(now: Instant): Either[StoreError, Swept] =
    write {
      val on = plugins().map(_._1).toSet
      for {
        stored <- cursors.stored()
        _ <- on.foldLeft[Either[StoreError, Unit]](Right(())) { (acc, p) =>
          acc.flatMap(_ => tombstones.spare(Target.Disabled(p), now))
        }
        marked <- stored.map(_._1).filterNot(on).foldLeft[Either[StoreError, Vector[PluginName]]](
          Right(Vector.empty)
        ) { (acc, p) =>
          acc.flatMap(done => tombstones.write(Target.Disabled(p), now).map(if (_) done :+ p else done))
        }
      } yield Swept(disabled = marked)
    }

  /** The next run posting to `plugin` at `version` from its cursor, when the cursor is behind
    * the newest closed period and no run from it is still going: enqueued as [[close]]
    * enqueues an attempt. A finished run that left the cursor where it was failed, and after
    * [[PostRef.Attempts]] of them the cursor is `stuck`. Starting the cursor at `now` leaves
    * the plugin's documents unread, and marked for deletion, when its version changed.
    */
  private def post(plugin: PluginName, version: Int, now: Instant): Either[StoreError, Swept] =
    write {
      for {
        cursor <- cursors.start(plugin, version, now)
        after <- periods.closedAfter(cursor, 1)
      } yield Option.when(after.nonEmpty)(cursor)
    }.flatMap {
      case None => Right(Swept.nothing)
      case Some(cursor) =>
        attempted {
          val runs = client
            .listWorkflows(
              new ListWorkflowsInput().withWorkflowIdPrefix(PostRef.prefix(plugin, version, cursor))
            )
            .asScala
            .toVector
          if (runs.exists(r => Option(r.status()).forall(_.isActive()))) Swept.nothing
          else if (runs.size >= PostRef.Attempts)
            Swept(stuck = Vector(PostRef(plugin, version, cursor, runs.size - 1).workflowId))
          else {
            val run = PostRef(plugin, version, cursor, runs.size)
            val _ = client.enqueueWorkflow[String, Exception](
              Posts.enqueueOptions(run),
              // As `enqueue`'s: the array is empty and DBOS only reads it.
              caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
            )
            Swept(posted = Vector(run))
          }
        }
    }

  /** `attempt` enqueued if DBOS has no workflow under its id. One it has, running or
    * finished, is left alone: an attempt is made once. One that finished with its period
    * still open on the same deadline did not close it, and is `stuck` until the deadline moves.
    */
  private def close(attempt: CloseRef): Either[StoreError, Swept] =
    attempted {
      val id = attempt.workflowId
      Option(client.retrieveWorkflow[String, Exception](WorkflowId.value(id)).getStatus())
        .map(_.status()) match {
        case Some(state) if state.isActive() => Swept.nothing
        case Some(_) => Swept(stuck = Vector(id))
        case None =>
          enqueue(attempt)
          Swept(enqueued = Vector(attempt))
      }
    }

  /** `question` enqueued if DBOS has no workflow under its id; one it has, running or
    * finished, is left alone: a question is asked once. One that finished and left the
    * period to be asked still (a verdict it could not keep) is `stuck` until new activity.
    */
  private def ask(question: SettleRef): Either[StoreError, Swept] =
    attempted {
      val id = question.workflowId
      Option(client.retrieveWorkflow[String, Exception](WorkflowId.value(id)).getStatus())
        .map(_.status()) match {
        case Some(state) if state.isActive() => Swept.nothing
        case Some(_) => Swept(stuck = Vector(id))
        case None =>
          val _ = client.enqueueWorkflow[String, Exception](
            Settles.enqueueOptions(question),
            // As `enqueue`'s: the array is empty and DBOS only reads it.
            caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
          )
          Swept(asked = Vector(question))
      }
    }

  // A repeated enqueue of the same id is a no-op. The array is empty and DBOS only reads
  // it; separation checking treats arrays as mutable.
  private def enqueue(attempt: CloseRef): Unit = {
    val _ = client.enqueueWorkflow[String, Exception](
      Closes.enqueueOptions(attempt),
      caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
    )
  }

}

/** What a sweep did: the close attempts it `enqueued`, the posting runs it enqueued
  * (`posted`), the questions whether anyone is waiting on a quiet period it enqueued
  * (`asked`), the targets whose tombstones it `collected`, `spared` (found alive) or
  * `deferred` (a workflow of theirs still queued or running, or waiting on another target),
  * and the workflows it found `stuck`: a close attempt that finished without closing its
  * period, whose deadline has not moved since, or a plugin's last run from a cursor it failed
  * to move [[PostRef.Attempts]] times, and the plugins with a cursor but not enabled whose
  * documents it newly marked for deletion (`disabled`). A stuck workflow is not run again; a
  * close is attempted anew once its deadline moves.
  */
final case class Swept(
    enqueued: Vector[CloseRef] = Vector.empty,
    collected: Vector[Target] = Vector.empty,
    posted: Vector[PostRef] = Vector.empty,
    stuck: Vector[WorkflowId] = Vector.empty,
    asked: Vector[SettleRef] = Vector.empty,
    spared: Vector[Target] = Vector.empty,
    deferred: Vector[Target] = Vector.empty,
    disabled: Vector[PluginName] = Vector.empty
) {
  def +(other: Swept): Swept =
    Swept(
      enqueued ++ other.enqueued,
      collected ++ other.collected,
      posted ++ other.posted,
      stuck ++ other.stuck,
      asked ++ other.asked,
      spared ++ other.spared,
      deferred ++ other.deferred,
      disabled ++ other.disabled
    )
}

object Swept {
  val nothing: Swept = Swept()
}
