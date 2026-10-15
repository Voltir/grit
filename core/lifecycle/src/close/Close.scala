package grit.lifecycle.close

import java.time.Instant

import grit.act.moves.Judging
import grit.core.act.MoveName
import grit.core.durable.{Durable, Flow, Flows as DurableFlows, Step, StepName}
import grit.core.id.{CloseRef, EntryId, PeriodRef, PeriodSeq, TurnRef, TurnSeq, WorkflowId}
import grit.core.job.Slot
import grit.core.message.{AssistantBlock, Message, StopReason}
import grit.core.period.{Balance, CloseReason, Closing, Edit, Flows}
import grit.core.retention.Target
import grit.core.route.Routed
import grit.core.store.{
  Entry,
  EntryTopics,
  Payload,
  Reads,
  Sealed,
  Speakers,
  StoreError,
  Tombstones,
  Tx
}
import grit.core.triage.Earning
import grit.core.visibility.Subject
import grit.lifecycle.transcript.PeriodTranscript

/** The close: one workflow per attempt to close a period ([[CloseRef]]), run on the turns'
  * queue under its conversation, so never beside one of its turns. Each step's output is
  * recorded, so a close resumed after a crash never calls a model twice.
  *
  *   1. `check` — the period, under its conversation's lock: already closed; abandoned when
  *      its deadline ([[grit.core.period.Deadline]], under the settings in force) is no longer
  *      the attempt's, because a turn came in, a turn's entries were written or the settings
  *      changed, so the sweep makes a new attempt on the new deadline; otherwise due, for its
  *      reason, with the balance it opened with and the cap its closing's balance is held
  *      to. The reason is `Ran`, whatever the deadline's, when the period's conversation is a
  *      job's run ([[grit.core.job.Slot.of]] its origin); otherwise `Unearned`, whatever the
  *      deadline's, when the period did not earn a written closing
  *      ([[grit.core.triage.Earning]], over what triage made of its heard messages). The
  *      attempt is taken to be enqueued once its deadline had come.
  *   1. `ask:gate` — what of the closing is new beside the balance the period opened with and
  *      what its windows showed from other conversations ([[CloseGate]]); every part when
  *      the classifier does not answer; none, with no classifier call, when unearned or ran.
  *   1. `ask:summary` — the closing: its flows written by the catalog's summary pin
  *      ([[ClosingSummary]]), shown what the period drew from elsewhere as known, and the
  *      balance it opened with after the writer's edits and the topics'
  *      ([[grit.core.store.EntryTopics.edits]]), held to the cap
  *      ([[grit.core.period.Balance]]); when the model fails, writes nothing readable or
  *      is cut off at its token limit, or when the gate found nothing new, the day's spend
  *      reached its cap or no pin of its role is cleared for what its period reads now
  *      ([[grit.core.route.Routed]]; and then no model is called), the period's per-turn
  *      summaries joined as the prose, and the balance carried with the topics' edits alone.
  *      A period grit
  *      only heard ([[grit.lifecycle.transcript.PeriodTranscript.overheard]]) that earned
  *      its closing is written by the catalog's heard pin, as reported speech, asking only its
  *      prose and outcome; one that did not is closed with no model call, its prose
  *      `Heard 4 messages; nothing kept.` and its balance carried. A run's period is closed
  *      with no model call: its prose the run's opening, its outcome the run's reply verbatim
  *      (`No reply.` without one), and its balance carried unchanged. A close never fails for
  *      a model. Its model is admitted by the day's spend as the gate's classifier is
  *      ([[Judging]]), and every reply is charged, however it reads, in `ask:summary:record`;
  *      a store that cannot record it ends the close there, sealing nothing.
  *   1. `seal` — under the lock again: the closing entry, the period
  *      closed, and the tombstones on its raw entries, on the closing it replaces and on its
  *      conversation going quiet ([[grit.core.retention.Target]]), together; abandoned,
  *      writing nothing, when a turn came in meanwhile.
  */
object Close {

  /** What `check` found. */
  enum Checked {

    /** The period closes now for `reason`; its turns start at `first`; it opened with
      * `known`, and its balance is held to `cap` bytes.
      */
    case Due(first: TurnSeq, reason: CloseReason, known: Balance, cap: Int)

    /** The period is closed already. */
    case Closed

    /** The attempt is over without closing, for `why`. */
    case Abandoned(why: String)
  }

  /** What `ask:summary` wrote: the `closing` (`None` only when the period had no text at all
    * and no turns), and why no model wrote it.
    */
  final case class Summarised(closing: Option[Closing], note: Option[String])

  import CloseJournal.given

  /** The attempt `ref`, due: its period closes for `reason`, its turns from `first`, opened
    * with `known` and held to `cap` bytes.
    */
  private final case class Attempt(
      ref: CloseRef,
      first: TurnSeq,
      reason: CloseReason,
      known: Balance,
      cap: Int
  )

  /** The attempt `at`, with the parts of its closing the gate found new, and why no
    * classifier did.
    */
  private final case class Gated(at: Attempt, asked: Asked, note: Option[String])

  /** The gated attempt `gated`, and what its summary wrote. */
  private final case class Written(gated: Gated, summarised: Summarised) {

    /** Why no classifier or model did, for logs. */
    def noted: String = (gated.note ++ summarised.note).map(n => s"; $n").mkString
  }

  private def subject(ref: CloseRef): Subject = Subject.Conversation(ref.period.conversationId)

  private val checked: Step[CloseEnv, CloseRef, Either[String, Checked]] =
    Step.transact[CloseEnv, CloseRef, Either[String, Checked]](StepName("check"), subject)(env =>
      ref => check(env.records, ref)
    )

  /** What the attempt `at`'s judgments cost is charged to: its last turn, under its workflow. */
  private def charged(at: Attempt): Judging.Charged =
    Judging.Charged(
      TurnRef(at.ref.period.conversationId, at.ref.last),
      at.ref.workflowId,
      subject(at.ref)
    )

  private val gated: Flow[CloseEnv, Attempt, Nothing, Judging.Judged[(Asked, Option[String])]] =
    Judging.flow[CloseEnv, Attempt, (Asked, Option[String])](MoveName("gate"), charged)(
      _.moves,
      _.budget
    )(env => at => gate(env, at))

  /** An attempt's gate: none, asking nothing, when it is unearned or ran; else [[gated]]. */
  private val gating: Flow[CloseEnv, Attempt, String, Gated] =
    Flow.mayEnd(
      Flow.branch[CloseEnv, CloseEnv, Attempt, Attempt, Gated, Nothing, Either[String, Gated]] {
        at =>
          at.reason match {
            case CloseReason.Unearned => Right(Gated(at, Asked.NoPart, Some("unearned")))
            case CloseReason.Ran => Right(Gated(at, Asked.NoPart, Some("ran")))
            case _ => Left(at)
          }
      }(
        gated.keeping.map {
          case (_, Judging.Judged(_, Left(why))) => Left(s"failed: its cost unrecorded: $why")
          case (at, Judging.Judged((asked, note), Right(()))) => Right(Gated(at, asked, note))
        },
        Flow.pass[Gated].map(Right(_))
      )
    )

  private val summary: Flow[CloseEnv, Gated, Nothing, Judging.Judged[Summarised]] =
    Judging.flow[CloseEnv, Gated, Summarised](MoveName("summary"), g => charged(g.at))(
      _.moves,
      _.budget
    )(env => g => summarise(env, g.at.ref, g.at.first, g.asked, g.at.known, g.at.cap, g.at.reason))

  /** A gated attempt's summary; the close ends when its cost cannot be recorded. */
  private val summarised: Flow[CloseEnv, Gated, String, Written] =
    Flow.mayEnd(summary.keeping.map {
      case (_, Judging.Judged(_, Left(why))) => Left(s"failed: its cost unrecorded: $why")
      case (g, Judging.Judged(s, Right(()))) => Right(Written(g, s))
    })

  private val sealedUp: Step[CloseEnv, Written, Either[String, Sealed]] =
    Step.transact[CloseEnv, Written, Either[String, Sealed]](
      StepName("seal"),
      w => subject(w.gated.at.ref)
    )(env =>
      w => seal(env.records, w.gated.at.ref, w.gated.at.reason, w.summarised, env.clock.now())
    )

  /** The close workflow, from the attempt's workflow id: what the close did, for logs, at
    * whichever end; the closing entry is in the store.
    */
  val flow: Flow[CloseEnv, WorkflowId, String, String] =
    Flow
      .fromEither(WorkflowId.as("close", CloseRef.fromWorkflowId))
      .andThen(Flow.decide(checked) {
        case (_, Left(why)) => Left(s"failed: $why")
        case (_, Right(Checked.Closed)) => Left("already closed")
        case (_, Right(Checked.Abandoned(why))) => Left(s"abandoned: $why")
        case (ref, Right(Checked.Due(first, reason, known, cap))) =>
          Right(Attempt(ref, first, reason, known, cap))
      })
      .andThen(gating)
      .andThen(summarised)
      .andThen(Flow.step(sealedUp).keeping)
      .map {
        case (w, Left(why)) => s"failed: $why${w.noted}"
        case (w, Right(Sealed.Closed(entry))) => s"closed: ${EntryId.value(entry)}${w.noted}"
        case (w, Right(Sealed.Abandoned)) =>
          s"abandoned: a turn came in while it was summarised${w.noted}"
      }

  /** The close workflow for the attempt whose workflow id is `workflowId`: [[flow]] run. */
  def body(env: CloseEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    DurableFlows.run(flow, env, workflowId).merge

  /** The `ask:gate` step: what of `at`'s closing is new, asked of the classifier only when its
    * service is cleared for what was read.
    */
  private def gate(env: CloseEnv^, at: Attempt): Judging.Pose[(Asked, Option[String])]^{env} =
    Routed
      .reading(reads(env, at.ref)) { (routed, bounded) =>
        val entries = own(bounded, env, at.ref, at.first)
        CloseGate.asked(
          env.moves.classifier,
          routed,
          CloseGate.Transcript(
            at.known,
            elsewhere(bounded, env, entries),
            transcript(bounded, env, entries)
          )
        )
      }
      .fold(e => Judging.Pose.Made((Asked.Every, Some(s"gate unread: ${describe(e)}"))), identity)

  /** The `check` step. */
  private def check(records: CloseRecords, attempt: CloseRef)(using
      Tx^
  ): Either[String, Checked] =
    (for {
      _ <- records.entries.lockNext(attempt.period.conversationId)
      period <- records.periods.get(attempt.period)
      activity <- records.periods.activity(attempt.period)
      settings <- records.lifecycle.current()
      opening <- period.fold[Either[StoreError, Option[Balance]]](Right(None))(p =>
        records.periods
          .opening(TurnRef(attempt.period.conversationId, p.first))
          .map(o => Some(o.balance))
      )
      // Its own entries and what triage made of them, read under the lock, so a message
      // heard after this check is in a later attempt's.
      own <- period.fold[Either[StoreError, Vector[Entry]]](Right(Vector.empty))(p =>
        records.entries
          .list(attempt.period.conversationId)
          .map(_.filter { e =>
            val t = TurnSeq.value(e.turnSeq)
            t >= TurnSeq.value(p.first) && t <= TurnSeq.value(attempt.last)
          })
      )
      tags <- records.triage.of(own.collect {
        case e @ Entry(_, _, _, _, _, Payload.Heard(_), _, _) =>
          e.id
      })
      ran <- records.conversations
        .get(attempt.period.conversationId)
        .map(_.flatMap(c => Slot.of(c.origin)).isDefined)
    } yield (period, activity) match {
      case (Some(p), Some(a)) =>
        val current = a.attempt(settings)
        if (current == attempt)
          Checked.Due(
            p.first,
            if (ran) CloseReason.Ran
            else if (Earning.earns(own, tags)) a.due(settings).reason
            else CloseReason.Unearned,
            opening.getOrElse(Balance.empty),
            settings.balance
          )
        else if (a.last != attempt.last) Checked.Abandoned(s"turn ${TurnSeq.value(a.last)} came in")
        else Checked.Abandoned(s"its deadline moved to ${current.due}")
      case (Some(_), None) => Checked.Closed
      case (None, _) => Checked.Abandoned("no such period")
    }).left.map(describe)

  /** The entries of the attempt's period, from its turn `first` to its last. */
  private def own(
      reads: Reads^,
      env: CloseEnv^,
      attempt: CloseRef,
      first: TurnSeq
  ): Either[String, Vector[Entry]] =
    PeriodTranscript
      .entries(reads, env.records.entries, attempt.period, first, attempt.last)
      .left
      .map(describe)

  /** What the period's windows showed from other conversations
    * ([[PeriodTranscript.elsewhere]]); none when unread.
    */
  private def elsewhere(
      reads: Reads^,
      env: CloseEnv^,
      entries: Either[String, Vector[Entry]]
  ): Vector[String] =
    entries
      .flatMap(PeriodTranscript.elsewhere(reads, env.records.entries, _).left.map(describe))
      .getOrElse(Vector.empty)

  /** The period as one transcript ([[PeriodTranscript.of]]); empty when unread. */
  private def transcript(
      reads: Reads^,
      env: CloseEnv^,
      entries: Either[String, Vector[Entry]]
  ): String = {
    val own = entries.getOrElse(Vector.empty)
    PeriodTranscript.of(own, names(reads, env, own))
  }

  /** Who wrote `entries`; nobody named when that cannot be read, so each line is then its
    * role's.
    */
  private def names(reads: Reads^, env: CloseEnv^, entries: Vector[Entry]): Speakers =
    PeriodTranscript
      .speakers(reads, env.records.principals, entries)
      .getOrElse(Speakers.none)

  /** The store as the attempt's period's conversation reads it: work nobody asked for, kept at
    * that conversation's label.
    */
  private def reads(env: CloseEnv^, attempt: CloseRef): Reads^ =
    env.db.as(Subject.Conversation(attempt.period.conversationId))

  /** The `ask:summary` step, for a period closing for `reason`: the closing of the period's
    * turns `first` to the attempt's last, from the flows and edits the summary model writes,
    * asking for `asked`, applied to `known` and held to `cap`; or, when it fails, the fallback
    * prose ([[fallback]]) with `known` carried unedited. A run's ([[ran]]) and an unearned one
    * are written without a model.
    */
  private def summarise(
      env: CloseEnv^,
      attempt: CloseRef,
      first: TurnSeq,
      asked: Asked,
      known: Balance,
      cap: Int,
      reason: CloseReason
  ): Judging.Pose[Summarised]^{env} =
    // What the writer is sent is read under one routing of the period's conversation, which
    // chooses the pin it may go to: a direct message's floor may have risen since the gate.
    Routed
      .reading(reads(env, attempt)) { (routed, read) =>
        written(env, attempt, first, asked, known, cap, reason, Right(routed), read)
      } match {
      case Right(pose) => pose
      // Unrouted, the period is unread (only the names of no entries are read), and no model
      // is asked.
      case Left(e) =>
        written(
          env,
          attempt,
          first,
          asked,
          known,
          cap,
          reason,
          Left(describe(e)),
          reads(env, attempt)
        )
    }

  /** [[summarise]], its period read through `read` under `routed`; when the routing read
    * failed (`Left`), the period closed as unread.
    */
  private def written(
      env: CloseEnv^,
      attempt: CloseRef,
      first: TurnSeq,
      asked: Asked,
      known: Balance,
      cap: Int,
      reason: CloseReason,
      routed: Either[String, Routed],
      read: Reads^
  ): Judging.Pose[Summarised]^{env} = {
    val entries = routed.flatMap(_ => own(read, env, attempt, first))
    // What the writer reads and cites into: one value, cut once (ClosingSummary.visible).
    val labelled = {
      val own = entries.getOrElse(Vector.empty)
      PeriodTranscript.labelled(own, names(read, env, own))
    }
    val period = attempt.period.seq
    // The topics' edits are the fold's, free and exact: made whether or not a model writes.
    val topics =
      EntryTopics.edits(
        known,
        entries.getOrElse(Vector.empty).flatMap(e => EntryTopics.events(e.payload))
      )
    def closed(prose: String, outcome: Option[String], edits: Vector[Edit]): Option[Closing] = {
      val edited = known.edit(edits ++ topics, period)
      val fitted = edited.balance.fit(cap, period)
      Flows
        .of(prose, outcome, edited.changes ++ fitted.changes)
        .map(Closing(_, fitted.balance))
    }
    // A period grit only heard is always written, under the heard pin, asking for its prose
    // and outcome alone: nothing said to each other stands as the conversation's own.
    val overheard = PeriodTranscript.overheard(entries.getOrElse(Vector.empty))
    val asking = if (overheard) asked.heard else asked
    val request =
      ClosingSummary.request(labelled, known, elsewhere(read, env, entries), asking, overheard)
    def carried(note: String) = Summarised(
      fallback(entries.getOrElse(Vector.empty), attempt, first)
        .flatMap(closed(_, None, Vector.empty)),
      Some(note)
    )
    // The summary pin's reply, or why there is none, made into the closing.
    def replied(reply: Either[String, Message.Assistant]): Summarised =
      reply.fold(
        why => carried(s"no summary: $why"),
        m =>
          // A reply cut off at its token limit may have lost any part, a Resolved one
          // included, and its last line may be half written: none of it is read.
          if (m.stop == StopReason.MaxTokens) carried("no summary: cut off at its token limit")
          else
            ClosingSummary
              .read(m, known, asking, labelled)
              .flatMap(read => closed(read.prose, read.outcome, read.edits))
              .fold(carried(s"no summary: the summary had no prose (stop: ${m.stop})"))(c =>
                Summarised(Some(c), None)
              )
      )
    if (reason == CloseReason.Ran)
      Judging.Pose.Made(
        entries.fold(
          why =>
            Summarised(
              fallback(Vector.empty, attempt, first).flatMap(closed(_, None, Vector.empty)),
              Some(s"unread: $why")
            ),
          own => Summarised(ran(own, attempt, first, known), None)
        )
      )
    else if (reason == CloseReason.Unearned)
      Judging.Pose.Made(
        Summarised(
          fallback(entries.getOrElse(Vector.empty), attempt, first)
            .flatMap(closed(_, None, Vector.empty)),
          None
        )
      )
    else if (asked.nothingNew && !overheard) Judging.Pose.Made(carried("nothing new: carried"))
    else {
      val models = env.moves.models
      (for {
        r <- routed
        catalog <- models.catalog()
      } yield Judging.Pose.writing(
        models,
        r,
        if (overheard) catalog.heard else catalog.pin.summary,
        request
      )(replied))
        .fold(why => Judging.Pose.Made(carried(s"no summary: $why")), identity)
    }
  }

  /** A run's closing, written without a model: its prose the run's opening (the first message
    * said to grit; without one, the [[fallback]]), its outcome the run's reply, its last
    * assistant message's text, verbatim ("No reply." without one), and `known` carried
    * unchanged.
    */
  private def ran(
      entries: Vector[Entry],
      attempt: CloseRef,
      first: TurnSeq,
      known: Balance
  ): Option[Closing] = {
    val opening = entries.collectFirst {
      case Entry(_, _, _, _, _, Payload.Message(Message.User(text)), _, _) => text
    }
    val reply = entries.reverse.collectFirst {
      case Entry(_, _, _, _, _, Payload.Message(Message.Assistant(blocks, _, _, _, _)), _, _) =>
        blocks.collect { case AssistantBlock.Text(t) => t }.mkString
    }
    opening
      .orElse(fallback(entries, attempt, first))
      .flatMap(prose => Flows.of(prose, Some(reply.getOrElse("No reply.")), Vector.empty))
      .map(Closing(_, known))
  }

  /** A closing's prose written without a model: for a period grit only heard, how many
    * messages it heard ("Heard 4 messages; nothing kept."); otherwise the period's per-turn
    * summaries joined; without any, its user messages; without those, how many turns it had.
    */
  private def fallback(
      entries: Vector[Entry],
      attempt: CloseRef,
      first: TurnSeq
  ): Option[String] =
    if (PeriodTranscript.overheard(entries)) {
      val heard = entries.count(_.payload match {
        case Payload.Heard(_) => true
        case _ => false
      })
      Some(s"Heard $heard message${if (heard == 1) "" else "s"}; nothing kept.")
    } else {
      val summaries =
        entries.collect { case Entry(_, _, _, _, _, Payload.Summary(text), _, _) => text }
      val asked = entries.collect {
        case Entry(_, _, _, _, _, Payload.Message(Message.User(text)), _, _) => text
      }
      val turns = TurnSeq.value(attempt.last) - TurnSeq.value(first) + 1
      Vector(
        summaries.mkString(" "),
        asked.mkString(" / "),
        s"A period of $turns turns, with nothing kept."
      )
        .find(_.trim.nonEmpty)
    }

  /** The `seal` step, at `now`: the closing entry, and the period closed. */
  private def seal(
      records: CloseRecords,
      attempt: CloseRef,
      reason: CloseReason,
      summarised: Summarised,
      now: Instant
  )(using Tx^): Either[String, Sealed] =
    summarised.closing match {
      case None => Left("the period left nothing to close with")
      case Some(closing) =>
        (for {
          outcome <- records.periods.seal(attempt, reason, closing, now)
          _ <- outcome match {
            case Sealed.Closed(_) => tombstones(records.tombstones, attempt.period, now)
            case Sealed.Abandoned => Right(())
          }
        } yield outcome).left.map(describe)
    }

  /** What a seal of `period` at `now` marks for deletion: its raw entries, the closing of the
    * period before it, which it replaces, and its conversation, quiet from now on unless
    * another period opens.
    */
  private def tombstones(tombstones: Tombstones, period: PeriodRef, now: Instant)(using
      Tx^
  ): Either[StoreError, Unit] =
    for {
      _ <- tombstones.write(Target.Raw(period), now)
      _ <- PeriodSeq.of(PeriodSeq.value(period.seq) - 1) match {
        case Some(before) =>
          tombstones.write(Target.Superseded(PeriodRef(period.conversationId, before)), now)
        case None => Right(())
      }
      _ <- tombstones.write(Target.Quiet(period), now)
    } yield ()

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
