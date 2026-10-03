package grit.dbos.engine

import java.time.{Instant, ZoneOffset}
import javax.sql.DataSource

import scala.jdk.CollectionConverters.*

import grit.core.id.{CloseRef, PluginName, SettleRef, ShadowRef, WorkflowId}
import grit.core.plugin.{PluginCursors, PostRef}
import grit.core.retention.Target
import grit.core.speech.SpeechStore
import grit.core.spend.Day
import grit.core.store.{
  ConversationStore,
  EntryStore,
  LifecycleStore,
  ModelProfileStore,
  PeriodStore,
  PromptStore,
  StoreError,
  Tombstones,
  Tx,
  UsageLedger
}
import grit.core.triage.{Shadowing, TriageShadows}
import grit.dbos.workflow.{Closes, Posts, Settles, Shadows}

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.workflow.{ListWorkflowsInput, WorkflowState}

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
    speech: SpeechStore,
    profiles: ModelProfileStore,
    prompts: PromptStore,
    lifecycle: LifecycleStore,
    tombstones: Tombstones,
    cursors: PluginCursors,
    shadows: TriageShadows,
    plugins: () -> Vector[(PluginName, Int)],
    declared: () -> Vector[Shadowing]
) {

  private val collector = new Collector(
    dataSource,
    client,
    conversations,
    entries,
    periods,
    ledger,
    speech,
    profiles,
    prompts,
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
      shadowed <- declared().foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, s) =>
        acc.flatMap(done => shadow(s, now).map(done + _))
      }
      collected <- collector.once(settings, now)
    } yield closed + asked + posted + shadowed + collected

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
        marked <- stored
          .map(_._1)
          .filterNot(on)
          .foldLeft[Either[StoreError, Vector[PluginName]]](
            Right(Vector.empty)
          ) { (acc, p) =>
            acc.flatMap(done =>
              tombstones.write(Target.Disabled(p), now).map(if (_) done :+ p else done)
            )
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

  /** `variant`'s oldest unshadowed messages enqueued, when none of its shadows is queued or
    * running, so nothing in flight is counted twice: as many as the rest of its cap for the
    * UTC day of `now` covers ([[Shadowing.batch]]). A message whose shadow DBOS already has,
    * finished without keeping a row, is `stuck`: it is not run again, and the batch passes
    * over it to the messages after it, so stuck shadows never stall the variant; each sweep
    * whose cap covers a call reports again every stuck one among the messages it read.
    */
  private def shadow(variant: Shadowing, now: Instant): Either[StoreError, Swept] =
    attempted {
      client
        .listWorkflows(
          new ListWorkflowsInput()
            .withWorkflowName(Shadows.WorkflowName)
            .withStatus(WorkflowState.PENDING, WorkflowState.ENQUEUED, WorkflowState.DELAYED)
        )
        .asScala
        .exists(w =>
          ShadowRef.fromWorkflowId(WorkflowId(w.workflowId())).exists(_.name == variant.name)
        )
    }.flatMap {
      case true => Right(Swept.nothing)
      case false =>
        read {
          for {
            spent <- shadows.spent(variant.name, Day.at(now, ZoneOffset.UTC).from)
            recent <- shadows.recent(variant.name, Shadowing.Recent)
          } yield variant.batch(spent, recent)
        }.flatMap(n => due(variant, n, n))
          .flatMap { found =>
            attempted {
              found.fresh.foreach { shadow =>
                val _ = client.enqueueWorkflow[String, Exception](
                  Shadows.enqueueOptions(shadow),
                  // As `enqueue`'s: the array is empty and DBOS only reads it.
                  caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
                )
              }
              Swept(shadowed = found.fresh, stuck = found.stuck.map(_.workflowId))
            }
          }
    }

  /** Up to `n` of `variant`'s oldest unshadowed messages DBOS has no shadow of, and those
    * read beside them that DBOS has one of (finished, as none is running). A read of `limit`
    * that found fewer than `n` reads again, `limit` widened past every one it passed over,
    * until a read comes back short or holds `n`.
    */
  private def due(
      variant: Shadowing,
      n: Int,
      limit: Int
  ): Either[StoreError, Due] =
    if (n <= 0) Right(Due(Vector.empty, Vector.empty))
    else {
      read(shadows.unshadowed(variant.name, variant.since, limit)).flatMap { offered =>
        attempted {
          offered.map { t =>
            val shadow = ShadowRef(t, variant.name)
            val known = Option(
              client
                .retrieveWorkflow[String, Exception](WorkflowId.value(shadow.workflowId))
                .getStatus()
            ).nonEmpty
            (shadow, known)
          }
        }.flatMap { checked =>
          val fresh = checked.collect { case (s, false) => s }
          val stuck = checked.collect { case (s, true) => s }
          if (fresh.size >= n || offered.size < limit) Right(Due(fresh.take(n), stuck))
          else due(variant, n, n + stuck.size)
        }
      }
    }

  /** A variant's shadows to enqueue, and those passed over as stuck. */
  private final case class Due(fresh: Vector[ShadowRef], stuck: Vector[ShadowRef])

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
  * (`asked`), the shadows it enqueued (`shadowed`), the targets whose tombstones it
  * `collected`, `spared` (found alive) or `deferred` (a workflow of theirs still queued or
  * running, or waiting on another target), and the workflows it found `stuck`: a close
  * attempt that finished without closing its period, whose deadline has not moved since, a
  * plugin's last run from a cursor it failed to move [[PostRef.Attempts]] times, or a shadow
  * that ended keeping nothing; and the plugins with a cursor but not enabled whose documents
  * it newly marked for deletion (`disabled`). A stuck workflow is not run again; a close is
  * attempted anew once its deadline moves.
  */
final case class Swept(
    enqueued: Vector[CloseRef] = Vector.empty,
    collected: Vector[Target] = Vector.empty,
    posted: Vector[PostRef] = Vector.empty,
    stuck: Vector[WorkflowId] = Vector.empty,
    asked: Vector[SettleRef] = Vector.empty,
    spared: Vector[Target] = Vector.empty,
    deferred: Vector[Target] = Vector.empty,
    disabled: Vector[PluginName] = Vector.empty,
    shadowed: Vector[ShadowRef] = Vector.empty
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
      disabled ++ other.disabled,
      shadowed ++ other.shadowed
    )
}

object Swept {
  val nothing: Swept = Swept()
}
