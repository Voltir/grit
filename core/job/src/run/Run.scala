package grit.job.run

import java.time.Instant

import grit.act.moves.{DurableMoves, MovesEnv}
import grit.core.act.{Acting, ActsFor, Allowance, Gates}
import grit.core.document.{DocumentKeeper, DocumentTerms}
import grit.core.durable.{Durable, Flow, Flows, Step as DurableStep, StepName, Steps}
import grit.core.flow.Trusted
import grit.core.id.{EntryId, JobName, PluginName, TurnRef, WorkflowId}
import grit.core.inbox.InboundId
import grit.core.job.{Job, JobRun, Jobs, KeepingJob, Owned, PlainJob, Report, Slot}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.spend.Budget
import grit.core.store.{Db, Entry, Jot, Origin, Payload, StoreError, Tx}
import grit.core.visibility.Subject

/** A job's run (ADR 0029): the workflow `run`, one per turn of a slot's conversation, which the
  * inbox started with the slot's opening ([[grit.core.inbox.Inbox.startSlot]]).
  */
object Run {

  private val ReadSlotName: StepName[Any] = StepName("read-slot")
  private val ReplyName: StepName[Any] = StepName("reply")

  /** The run's steps, as DBOS records their names, in the order they run. */
  object Step {
    val ReadSlot: String = ReadSlotName.family
    val Reply: String = ReplyName.family
  }

  private val readSlot: DurableStep[RunEnv, TurnRef, SlotRead] = {
    import RunJournal.given
    DurableStep[RunEnv, TurnRef, SlotRead](ReadSlotName)(env =>
      turn => slotOf(env.records, env.db, turn)
    )
  }

  private val replied: DurableStep[RunEnv, Replying, RunEnd] = {
    import RunJournal.given
    DurableStep[RunEnv, Replying, RunEnd](ReplyName)(env =>
      r => reply(env.records, env.jot, r.turn, r.read, r.planned, env.clock.now())
    )
  }

  /** The workflow of a job's run, the turn whose workflow id it starts from:
    *   1. `read-slot` — which slot it runs, its schedule, and the version it was started at;
    *   1. under its job's current version in `jobs`, the job's run: the moves it makes, each as
    *      `ask:{name}`, `call:{name}` or `keep:{name}` and its phases
    *      ([[grit.act.moves.ActName]]), for its schedule's principal, each ask admitted by `budget` against the day's spend, a call whose tool
    *      asks a person first never sent ([[grit.core.act.Gates.Closed]]), within the job's
    *      limits, and a plugin's keeping job's keeps over its plugin's documents, as `keepers`
    *      gives them;
    *   1. `reply` — the job's reply (or the line saying its parameters could not be read) as
    *      the turn's reply, awaited where its schedule reports, and the slot marked replied, in
    *      one transaction. A reply already kept is returned first, whatever the version. Under
    *      another version, or with its job gone, the job does not run and this writes nothing:
    *      superseded.
    * What it returns says which, at either end. A store that fails fails the run (the clock edge
    * then ends the slot `failed`). A run resumed under another version before any move records
    * `superseded`; one resumed after making moves ends in error at `reply`, whose position its
    * history holds a move, and the clock edge supersedes it.
    */
  def flow(
      jobs: Jobs,
      budget: Budget,
      keepers: (PluginName, DocumentTerms) -> DocumentKeeper
  ): Flow[RunEnv, WorkflowId, String, String] =
    Flow
      .fromEither(WorkflowId.as("run", TurnRef.fromWorkflowId))
      .andThen(Flow.decide(readSlot) {
        case (_, SlotRead.Unreadable(why)) => Left(s"unreadable: $why")
        case (turn, read: SlotRead.Read) => Right((turn, read))
      })
      .andThen(
        Flow.branch((r: (TurnRef, SlotRead.Read)) => planned(jobs, budget, r._1, r._2))(
          moves(keepers).over[RunEnv](env => env.moves).keeping.map { (p, text) =>
            Replying(p.acting.turn, p.read, Planned.Says(text))
          },
          Flow.pass[Replying]
        )
      )
      .andThen(Flow.step(replied).keeping.map {
        case (r, RunEnd.Replied(_)) => s"replied: ${EntryId.value(r.turn.replyId)}"
        case (r, RunEnd.Superseded(current)) =>
          s"superseded: started at v${r.read.version}, its job at v$current"
        case (r, RunEnd.Jobless) => s"jobless: no job ${JobName.value(r.read.job)}"
        case (_, RunEnd.Failed(why)) => s"failed: $why"
      })

  /** The run of a job whose turn's workflow id is `id`: [[flow]] run over `env`, with the jobs
    * of `jobs` and `env`'s budget and keepers.
    */
  def body(env: RunEnv^, jobs: Jobs)(id: WorkflowId)(using d: Durable^): String =
    Flows.run(flow(jobs, env.budget, env.keepers), env, id).merge

  /** What a read slot comes to under `jobs`: its job's moves to make under the acting of its
    * schedule, admitted by `budget`, or a reply that makes none.
    */
  private def planned(
      jobs: Jobs,
      budget: Budget,
      turn: TurnRef,
      read: SlotRead.Read
  ): Either[Planning, Replying] = {
    val acting = Acting(
      turn,
      ActsFor.Scheduled(read.slot.schedule),
      Allowance.Daily(budget),
      Gates.Closed,
      read.integrity
    )
    jobs.named(read.job) match {
      case None => Right(Replying(turn, read, Planned.Jobless))
      case Some(owned) if owned.job.version != read.version =>
        Right(Replying(turn, read, Planned.Superseded(owned.job.version)))
      case Some(owned) => Left(Planning(owned, read, acting))
    }
  }

  /** The job `owned` to run for the slot `read`, its moves made under `acting`. */
  private final case class Planning(owned: Owned, read: SlotRead.Read, acting: Acting)

  /** The `reply` step's input: the run `read` of `turn`, and what it replies. */
  private final case class Replying(turn: TurnRef, read: SlotRead.Read, planned: Planned)

  /** A job's run: the moves it makes, through the steps of a program, and its reply. */
  private def moves(
      keepers: (PluginName, DocumentTerms) -> DocumentKeeper
  ): Flow[MovesEnv, Planning, Nothing, String] =
    Flow.program[MovesEnv, Planning, String]("moves") { steps => p =>
      p.owned match {
        case Owned.Deployments(job) => plain(job, p.read, p.acting)(using steps)
        case Owned.Plugins(_, job) => plain(job, p.read, p.acting)(using steps)
        case Owned.Keeps(plugin, terms, job) =>
          keeping(job, p.read, p.acting, keepers(plugin, terms))(using steps)
      }
    }

  /** The line a run replies when its job cannot read its schedule's parameters, `why`. */
  def unreadParams(job: JobName, why: String): String =
    s"This run of ${JobName.value(job)} could not read its parameters: $why"

  private def slotOf(records: RunRecords, db: Db^, turn: TurnRef): SlotRead =
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
              case Some(s) =>
                SlotRead.Read(slot, job, version, started, s.params, s.report, s.integrity)
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
                  level <- records.entries.believed(turn)
                  _ <- records.entries.insert(
                    Entry(
                      turn.replyId,
                      turn.conversationId,
                      turn.turnSeq,
                      None,
                      next.seq,
                      Payload.Message(saying(text, read)),
                      now,
                      level
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
    * `read-slot` recorded, the deployment's jobs and the job's moves.
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
  private def said[P <: caps.Pure](job: Job[P], read: SlotRead.Read, acting: Acting)(
      run: Trusted[JobRun[P]] => String
  ): String =
    job
      .read(read.params)
      .fold(
        why => unreadParams(read.job, why),
        params => run(DurableMoves.started(JobRun(params, read.slot.nominal, read.started), acting))
      )

  /** `job`'s reply to the run `read`, after the moves it makes under `acting` through `steps`. */
  private def plain[P <: caps.Pure](job: PlainJob[P], read: SlotRead.Read, acting: Acting)(using
      steps: Steps[MovesEnv]^
  ): String =
    said(job, read, acting)(run =>
      DurableMoves.plain(acting, job.limits)(moves => Said(job.run(run, moves))).text
    )

  /** As [[plain]], its moves also keeping `keeper`'s documents. */
  private def keeping[P <: caps.Pure](
      job: KeepingJob[P],
      read: SlotRead.Read,
      acting: Acting,
      keeper: DocumentKeeper
  )(using steps: Steps[MovesEnv]^): String =
    said(job, read, acting)(run =>
      DurableMoves.keeping(acting, job.limits, keeper)(moves => Said(job.run(run, moves))).text
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
