package grit.job.run

import java.time.Instant

import grit.act.moves.{DurableMoves, MovesEnv}
import grit.core.act.{Acting, ActsFor, Allowance, Gates}
import grit.core.document.DocumentKeeper
import grit.core.durable.Durable
import grit.core.id.{EntryId, JobName, TurnRef, WorkflowId}
import grit.core.inbox.InboundId
import grit.core.job.{Job, JobRun, Jobs, KeepingJob, Owned, PlainJob, Report, Slot}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.store.{Db, Entry, Jot, Origin, Payload, StoreError, Tx}
import grit.core.visibility.Subject

/** A job's run (ADR 0029): the workflow `run`, one per turn of a slot's conversation, which the
  * inbox started with the slot's opening ([[grit.core.inbox.Inbox.startSlot]]).
  */
object Run {

  /** The run's steps, as DBOS records their names, in the order they run. */
  object Step {
    val ReadSlot = "read-slot"
    val Reply = "reply"
  }

  /** The workflow of a job's run, the turn whose workflow id is `id`:
    *   1. `read-slot` — which slot it runs, its schedule, and the version it was started at;
    *   1. under its job's current version in `jobs`, the job's run: the moves it makes, each as
    *      the steps `grit.act.moves.MoveSteps` names, for its schedule's principal, each ask
    *      admitted by `env.budget` against the day's spend, a call whose tool asks a person
    *      first never sent ([[grit.core.act.Gates.Closed]]), within the job's limits, and a
    *      plugin's keeping job's keeps over its plugin's documents, as `env.keepers` gives them;
    *   1. `reply` — the job's reply (or the line saying its parameters could not be read) as
    *      the turn's reply, awaited where its schedule reports, and the slot marked replied, in
    *      one transaction. A reply already kept is returned first, whatever the version. Under
    *      another version, or with its job gone, the job does not run and this writes nothing:
    *      superseded.
    * What it returns says which. A store that fails fails the run (the clock edge then ends the
    * slot `failed`). A run resumed under another version before any move records
    * `superseded`; one resumed after making moves ends in error at `reply`, whose position its
    * history holds a move, and the clock edge supersedes it.
    */
  def body(env: RunEnv^, jobs: Jobs)(id: WorkflowId)(using d: Durable^): String =
    TurnRef.fromWorkflowId(id) match {
      case None => s"not a run: ${WorkflowId.value(id)}"
      case Some(turn) =>
        import RunJournal.given
        val (records, db, jot, clock) = (env.records, env.db, env.jot, env.clock)
        d.step(Step.ReadSlot)(() => readSlot(records, db, turn)) match {
          case SlotRead.Unreadable(why) => s"unreadable: $why"
          case read: SlotRead.Read =>
            val acting = Acting(
              turn,
              ActsFor.Scheduled(read.slot.schedule),
              Allowance.Daily(env.budget),
              Gates.Closed
            )
            val planned: Planned = jobs.named(read.job) match {
              case None => Planned.Jobless
              case Some(owned) if owned.job.version != read.version =>
                Planned.Superseded(owned.job.version)
              case Some(Owned.Deployments(job)) => Planned.Says(plain(job, read, acting, env.moves))
              case Some(Owned.Plugins(_, job)) => Planned.Says(plain(job, read, acting, env.moves))
              case Some(Owned.Keeps(plugin, terms, job)) =>
                Planned.Says(keeping(job, read, acting, env.moves, env.keepers(plugin, terms)))
            }
            d.step(Step.Reply)(() => reply(records, jot, turn, read, planned, clock.now())) match {
              case RunEnd.Replied(_) => s"replied: ${EntryId.value(turn.replyId)}"
              case RunEnd.Superseded(current) =>
                s"superseded: started at v${read.version}, its job at v$current"
              case RunEnd.Jobless => s"jobless: no job ${JobName.value(read.job)}"
              case RunEnd.Failed(why) => s"failed: $why"
            }
        }
    }

  /** The line a run replies when its job cannot read its schedule's parameters, `why`. */
  def unreadParams(job: JobName, why: String): String =
    s"This run of ${JobName.value(job)} could not read its parameters: $why"

  private def readSlot(records: RunRecords, db: Db^, turn: TurnRef): SlotRead =
    db.read(Subject.Turn(turn)) {
      for {
        conversation <- records.conversations.get(turn.conversationId)
        entries <- records.entries.ofTurn(turn)
        found =
          for {
            c <- conversation.toRight("its conversation is gone")
            slot <- Slot.of(c.origin).toRight("its conversation is no slot's")
            job <- c.origin match {
              case Origin.Task(name, _) => JobName.of(name)
              case Origin.Tui(_, _) | Origin.Slack(_, _, _) | Origin.Direct(_, _) =>
                Left("its conversation is no task's")
            }
            opening <- entries.headOption.toRight("its turn has no opening")
            version <- InboundId
              .source(opening.id)
              .flatMap((_, source) => Slot.version(source))
              .toRight(s"its opening ${EntryId.value(opening.id)} names no version")
          } yield (slot, job, version, opening.createdAt)
        read <- found match {
          case Left(why) => Right(SlotRead.Unreadable(why))
          case Right((slot, job, version, started)) =>
            records.schedules.read(slot.schedule).map {
              case None => SlotRead.Unreadable("its schedule is gone")
              case Some(s) => SlotRead.Read(slot, job, version, started, s.params, s.report)
            }
        }
      } yield read
    }.fold(e => SlotRead.Unreadable(s"the store failed: ${describe(e)}"), identity)

  private def reply(
      records: RunRecords,
      jot: Jot^,
      turn: TurnRef,
      read: SlotRead.Read,
      planned: Planned,
      now: Instant
  ): RunEnd =
    jot
      .write(Subject.Turn(turn)) {
        records.entries.get(turn.replyId).flatMap {
          case Some(kept) => Right(RunEnd.Replied(textOf(kept)))
          case None =>
            planned match {
              case Planned.Jobless => Right(RunEnd.Jobless)
              case Planned.Superseded(current) => Right(RunEnd.Superseded(current))
              case Planned.Says(text) =>
                for {
                  next <- records.entries.lockNext(turn.conversationId)
                  _ <- records.entries.insert(
                    Entry(
                      turn.replyId,
                      turn.conversationId,
                      turn.turnSeq,
                      None,
                      next.seq,
                      Payload.Message(saying(text, read)),
                      now
                    )
                  )
                  _ <- awaited(records, turn, read.report)
                  _ <- records.schedules.replied(read.slot, read.version, now)
                } yield RunEnd.Replied(text)
            }
        }
      }
      .fold(e => RunEnd.Failed(describe(e)), identity)

  /** What a run's `reply` step does unless its reply is kept already: write the job's reply, or
    * nothing under another version or with the job gone. Decided before the step, from what
    * `read-slot` recorded and the deployment's jobs.
    */
  private enum Planned {
    case Says(text: String)
    case Superseded(current: Int)
    case Jobless
  }

  /** A job's reply, as its moves' body returns it. */
  private final case class Said(text: String) extends caps.Pure

  /** What `run` replies to the run `read` of `job`, given its parameters, or the line saying
    * they could not be read.
    */
  private def said[P <: caps.Pure](job: Job[P], read: SlotRead.Read)(
      run: JobRun[P] => String
  ): String =
    job
      .read(read.params)
      .fold(
        why => unreadParams(read.job, why),
        params => run(JobRun(params, read.slot.nominal, read.started))
      )

  /** `job`'s reply to the run `read`, after the moves it makes under `acting` through `env`. */
  private def plain[P <: caps.Pure](
      job: PlainJob[P],
      read: SlotRead.Read,
      acting: Acting,
      env: MovesEnv^
  )(using d: Durable^): String =
    said(job, read)(run =>
      DurableMoves.plain(acting, job.limits, env)(moves => Said(job.run(run, moves))).text
    )

  /** As [[plain]], its moves also keeping `keeper`'s documents. */
  private def keeping[P <: caps.Pure](
      job: KeepingJob[P],
      read: SlotRead.Read,
      acting: Acting,
      env: MovesEnv^,
      keeper: DocumentKeeper
  )(using d: Durable^): String =
    said(job, read)(run =>
      DurableMoves
        .keeping(acting, job.limits, env, keeper)(moves => Said(job.run(run, moves)))
        .text
    )

  /** `text` as the run's reply, written by its job at the run's version, at no cost. */
  private def saying(text: String, read: SlotRead.Read): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage.Zero,
      s"job:${JobName.value(read.job)}@v${read.version}"
    )

  private def awaited(records: RunRecords, turn: TurnRef, report: Report)(using
      Tx^
  ): Either[StoreError, Unit] =
    report match {
      case Report.Kept => Right(())
      case Report.Posted(to) => records.deliveries.await(turn, to.address)
    }

  private def textOf(entry: Entry): String = entry.payload match {
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      blocks.collect { case AssistantBlock.Text(t) => t }.mkString
    case _ => ""
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
