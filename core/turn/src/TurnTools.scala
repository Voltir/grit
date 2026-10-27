package grit.turn

import java.time.Instant

import grit.core.approval.Approval
import grit.core.flow.Mint
import grit.core.id.{CallSlot, EntryId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.place.{Place, Service}
import grit.core.store.{Entry, EntryStore, Jot, Payload, Reads, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, ToolName, Toolbox}
import grit.core.visibility.{Accounted, Integrity, Subject}

import TurnLoop.{Pending, Round}

/** How a turn settles one tool call, inside the call's own step. A step cut short by a crash
  * runs again, so the call's result is kept in the store under an id fixed by its place in
  * the loop ([[Slot.resultId]]), never in the journal, and a rerun that finds it returns it
  * without running anything. A tool that only reads runs again when a crash came between
  * its run and its result. One a person approves runs at most once ([[Settling.decide]]).
  */
object TurnTools {

  /** Where a call's settling left it: the entry holding its result, and whether that result
    * is an error.
    */
  final case class Settled(result: EntryId, failed: Boolean)

  /** A tool call's place in its turn's loop. Every id and step name its settling uses comes
    * from it.
    */
  final case class Slot(call: CallSlot) {

    def turn: TurnRef = call.turn

    def round: Round = Round.at(call.round)

    def index: Int = call.index

    /** The id of the entry keeping the call's result. */
    def resultId: EntryId = EntryId(s"result:$key")

    /** The id of the entry keeping why the person declined the call, a gated one. */
    def declinedId: EntryId = EntryId(s"declined:$key")

    /** The id of the entry recording that the call, a gated one, began. */
    def attemptId: EntryId = EntryId(s"attempt:$key")

    /** The id of the entry asking a person about the call, a gated one. */
    def askId: EntryId = EntryId(s"ask:$key")

    private def key: String = s"${WorkflowId.value(turn.workflowId)}:${call.round}:$index"
  }

  object Slot {

    /** The call at `index` (from 0) of the reply to `round` in `turn`; `Left` when `index` is
      * negative.
      */
    def of(turn: TurnRef, round: Round, index: Int): Either[TurnFailure, Slot] =
      CallSlot
        .of(turn, round.index, index)
        .map(Slot(_))
        .toRight(TurnFailure.Store(s"no call at round ${round.index}, index $index"))
  }

  /** What a call read by [[read]] is, as data that crosses a step: which steps settle it. Its
    * runner is found again, inside each step, by reading the call again.
    */
  enum Kind {

    /** Answered unrun, with the outcome [[read]] gave. */
    case Refused

    /** Run in its own step. */
    case Free

    /** Run in its own step once a person approves it. */
    case Gated

    /** Sent to the edge serving `place`, the place of its tool `tool`, a person asked first when
      * it `asks`.
      */
    case Hosted(tool: ToolName, asks: Boolean, place: Option[Place])
  }

  object Kind {

    /** The kind of `read`, what [[read]] gave, a hosted tool's place as `placeOf` says. */
    def of(
        read: Either[Outcome, (Bound.Runs | Bound.Hosted)^],
        placeOf: ToolName -> Option[Place]
    ): Kind = read match {
      case Left(_) => Refused
      case Right(_: Bound.Free) => Free
      case Right(_: Bound.Gated) => Gated
      case Right(h: Bound.Hosted) => Hosted(h.tool, h.ask.nonEmpty, placeOf(h.tool))
    }
  }

  /** The id of the entry keeping `turn`'s reply to `round`, which called tools. */
  def callId(turn: TurnRef, round: Round): EntryId =
    EntryId(s"call:${WorkflowId.value(turn.workflowId)}:${round.index}")

  /** The `ask:n:j` step: an [[Payload.Ask]] entry at [[Slot.askId]], dated `at`, after
    * everything in the conversation, asking a person about `call`, the gated call at `slot`,
    * by showing them `shown`, waiting for their answer until `until`.
    */
  def ask(
      entries: EntryStore,
      slot: Slot,
      call: ToolCallId,
      shown: String,
      at: Instant,
      until: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val turn = slot.turn
    (for {
      next <- entries.lockNext(turn.conversationId)
      level <- entries.believed(turn)
      _ <- entries.insert(
        Entry(
          slot.askId,
          turn.conversationId,
          turn.turnSeq,
          None,
          next.seq,
          Payload.Ask(call, shown, Some(until)),
          at,
          level
        )
      )
    } yield slot.askId).left.map(storeFailure)
  }

  /** `pending`, stamped at `at`, read against `tools` with `repairs`, ready to run, a call of
    * a tool that reads the store run through `reads` ([[Bound.Reading]]); or the outcome it is
    * answered with, unrun: a refused call's, or the [[grit.core.tool.CallError]] of one that
    * does not bind, one stamped below its tool's requirement among them.
    */
  def read[C^](
      tools: Toolbox[C],
      pending: Pending,
      repairs: Repairs,
      reads: Reads^,
      at: Integrity
  ): Either[Outcome, (Bound.Runs | Bound.Hosted)^{C, reads}] =
    pending match {
      case Pending.Refused(_, refused) => Left(refused)
      case Pending.Run(call) =>
        tools.bind(Mint.tainted(call, at), repairs) match {
          case Left(error) => Left(error.outcome)
          case Right(reading: Bound.Reading) => Right(reading.through(reads))
          case Right(runs: Bound.Runs) => Right(runs)
          case Right(hosted: Bound.Hosted) => Right(hosted)
        }
    }

  /** Where a turn keeps what its tool calls came to: in `entries`, written through `jot` from
    * inside each call's step. Each method settles the call at a [[Slot]], whose id is `call`,
    * and keeps its result as a [[Payload.Result]] entry at [[Slot.resultId]], dated `at`,
    * after everything in the conversation, with the call as it is shown ([[Bound.shown]]).
    * One run here is believed at what its outcome says ([[Outcome.Done.integrity]]), else at
    * its tool's ([[Bound.Runs.output]], read at the turn's `level`: [[grit.core.tool.Believed.at]]); one
    * answered, at `from`: what the service it went to declares of its outcome's items
    * ([[grit.core.visibility.Visibility.believed]]), else the integrity given. A
    * result already kept is returned as it is, and nothing runs. Each fails only when `jot`
    * cannot read or write the store.
    */
  final class Settling(jot: Jot^, val entries: EntryStore) {

    /** The call answered with `outcome`, unrun, shown as `named`, the tool's name as the
      * call sent it.
      */
    def answer(
        slot: Slot,
        call: ToolCallId,
        named: String,
        outcome: Outcome,
        from: Either[Integrity, Service],
        at: Instant
    ): Either[TurnFailure, Settled] =
      settle(slot, call, named, Belief.From(from), at)(() => Right(outcome))

    /** The call answered, unrun, with what `read` finds in the store, in a transaction of its
      * own, shown as `named`; `read` runs only when no result is kept, and when it fails, so
      * does the settling, keeping nothing.
      */
    def answerFrom(
        slot: Slot,
        call: ToolCallId,
        named: String,
        from: Either[Integrity, Service],
        at: Instant
    )(
        read: (Tx^) ?=> Either[StoreError, Outcome]
    ): Either[TurnFailure, Settled] =
      settle(slot, call, named, Belief.From(from), at)(() =>
        jot.write(Subject.Turn(slot.turn))(read).left.map(storeFailure)
      )

    /** `free` run. */
    def run(
        slot: Slot,
        call: ToolCallId,
        free: Bound.Free^,
        level: Integrity,
        at: Instant
    ): Either[TurnFailure, Settled] =
      settle(slot, call, free.shown, Belief.Ran(free.output.at(level)), at)(() =>
        Right(free(slot.call))
      )

    /** `gated` answered by `approval`. Run when approved, at most once: its attempt is
      * recorded at [[Slot.attemptId]] before it starts, and a rerun that finds the attempt
      * and no result answers [[Outcome.Interrupted]] without running it. Denied otherwise.
      */
    def decide(
        slot: Slot,
        call: ToolCallId,
        gated: Bound.Gated^,
        approval: Approval,
        level: Integrity,
        at: Instant
    ): Either[TurnFailure, Settled] =
      settle(slot, call, gated.shown, Belief.Ran(gated.output.at(level)), at)(() =>
        approval match {
          case Approval.Approved =>
            began(slot, Payload.Attempt(call), at).map {
              case true => Outcome.Interrupted
              case false => gated(Approval.Approved, slot.call)
            }
          case other => Right(gated(other, slot.call))
        }
      )

    private def settle(
        slot: Slot,
        call: ToolCallId,
        shown: String,
        believed: Belief,
        at: Instant
    )(
        outcome: () => Either[TurnFailure, Outcome]
    ): Either[TurnFailure, Settled] =
      jot
        .write(Subject.Turn(slot.turn))(entries.get(slot.resultId))
        .left
        .map(storeFailure)
        .flatMap {
          case Some(kept) => settledBy(kept)
          case None =>
            outcome().flatMap { o =>
              val result = Payload.Result(o.result(call), shown)
              val reason = o match {
                case Outcome.Declined(Some(said)) =>
                  Some(slot.declinedId -> Payload.Declined(call, said))
                case _ => None
              }
              keep(slot.turn, slot.resultId, result, believed.of(o), at, reason)
                .flatMap(settledBy)
            }
        }

    /** Whether `slot`'s attempt was already recorded; if not, it is, as `marker`, before
      * this returns.
      */
    private def began(slot: Slot, marker: Payload, at: Instant): Either[TurnFailure, Boolean] = {
      val (turn, id) = (slot.turn, slot.attemptId)
      def record(using Tx^): Either[StoreError, Boolean] =
        entries.get(id).flatMap {
          case Some(_) => Right(true)
          case None =>
            for {
              next <- entries.lockNext(turn.conversationId)
              level <- entries.believed(turn)
              _ <- entries.insert(
                Entry(id, turn.conversationId, turn.turnSeq, None, next.seq, marker, at, level)
              )
            } yield false
        }
      jot.write(Subject.Turn(turn))(record) match {
        case Left(StoreError.DuplicateId(_)) => Right(true)
        case other => other.left.map(storeFailure)
      }
    }

    /** `payload` kept as `turn`'s entry `id`, dated `at`, after everything in the
      * conversation, believed at `believed`'s integrity, and in the same write `person`'s
      * payload, a person's words, under its id right after it, believed at the turn's
      * integrity; the entry already there when one has that id.
      */
    private def keep(
        turn: TurnRef,
        id: EntryId,
        payload: Payload,
        believed: Either[Integrity, (Service, Accounted)],
        at: Instant,
        person: Option[(EntryId, Payload)]
    ): Either[TurnFailure, Entry] = {
      def write(using tx: Tx^): Either[StoreError, Entry] =
        for {
          next <- entries.lockNext(turn.conversationId)
          output = believed.fold(i => i, Tx.visibility(tx).believed(_, _))
          entry = Entry(id, turn.conversationId, turn.turnSeq, None, next.seq, payload, at, output)
          _ <- entries.insert(entry)
          _ <- person.fold[Either[StoreError, Unit]](Right(())) { (said, words) =>
            for {
              after <- entries.lockNext(turn.conversationId)
              level <- entries.believed(turn)
              _ <- entries.insert(
                Entry(said, turn.conversationId, turn.turnSeq, None, after.seq, words, at, level)
              )
            } yield ()
          }
        } yield entry
      jot.write(Subject.Turn(turn))(write) match {
        case Left(StoreError.DuplicateId(_)) =>
          jot
            .write(Subject.Turn(turn))(entries.get(id))
            .left
            .map(storeFailure)
            .flatMap(_.toRight(TurnFailure.Store(s"entry ${EntryId.value(id)} vanished")))
        case other => other.left.map(storeFailure)
      }
    }
  }

  /** How a call's result is believed: as answered (`From`, what the service it went to declares
    * of its items, or the integrity given), or as one run here says (`Ran`, `output` when its
    * outcome does not).
    */
  private enum Belief {
    case From(from: Either[Integrity, Service])
    case Ran(output: Integrity)

    /** The integrity `o` is believed at, or the service it came from with what its extension
      * read of its items.
      */
    def of(o: Outcome): Either[Integrity, (Service, Accounted)] = this match {
      case From(from) => from.map((_, o.accounted))
      case Ran(output) =>
        o match {
          case Outcome.Done(_, Some(integrity), _, _, _) => Left(integrity)
          case _ => Left(output)
        }
    }
  }

  private def settledBy(kept: Entry): Either[TurnFailure, Settled] = kept.payload match {
    case Payload.Result(Message.ToolResult(_, _, failed), _) => Right(Settled(kept.id, failed))
    case _ => Left(TurnFailure.Store(s"entry ${EntryId.value(kept.id)} is not a tool result"))
  }

  private def storeFailure(error: StoreError): TurnFailure = error match {
    case StoreError.DuplicateId(id) =>
      TurnFailure.Store(s"entry ${EntryId.value(id)} already exists")
    case StoreError.DatabaseError(cause) => TurnFailure.Store(cause)
    case StoreError.Invalid(cause) => TurnFailure.Store(cause)
  }
}
