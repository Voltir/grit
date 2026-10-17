package grit.eval.harness.loss

import java.time.Instant

import grit.core.classify.{Classifier, ClassifierError}
import grit.core.id.{EntrySeq, TurnRef, WorkflowId}
import grit.core.period.Probability
import grit.core.store.{StoreError, Tx}
import grit.dbos.internal.Reader
import grit.eval.harness.capture.{Capture, Fields}

/** The turns a database recorded in a window, read for the lost-tool measure through
  * `grit.dbos`'s `Reader` and the turn's own reading of its records, never through SQL of its
  * own. Nothing here writes.
  */
object LossWindow {

  /** Every turn workflow `reader`'s database created at or after `since` and before `until`,
    * oldest first, each with its gates ([[TurnGates.of]]). Reads every turn workflow's record (its
    * status and creation, no output), then the recorded steps of those in the window alone:
    * one window of step outputs. `Left` naming what could not be read.
    */
  def gates(
      reader: Reader^,
      since: Instant,
      until: Instant
  ): Either[String, Vector[(TurnRef, TurnGates)]] =
    for {
      workflows <- reader.turns(until).left.map(e => s"turn workflows unread: ${Capture.kind(e)}")
      window = workflows.collect {
        case (w, recorded) if !recorded.created.isBefore(since) => TurnRef.fromWorkflowId(w)
      }.flatten
      gated <- Fields.each(window) { turn =>
        val id = WorkflowId.value(turn.workflowId)
        reader
          .steps(turn.workflowId)
          .left
          .map(e => s"steps of $id unread: ${Capture.kind(e)}")
          .flatMap(TurnGates.of(_).left.map(why => s"$id: $why"))
          .map(turn -> _)
      }
    } yield gated

  /** The words of each of `turns`' root, its first entry ([[grit.core.store.Payload.said]]):
    * `Left` for a turn whose first entry holds no words a person said, or that has none. Reads
    * each of their conversations' entries once, whatever their labels. `Left` naming the
    * conversation whose entries could not be read.
    */
  def roots(
      reader: Reader^,
      turns: Vector[TurnRef]
  ): Either[String, Map[TurnRef, Either[String, String]]] =
    Fields
      .each(turns.map(_.conversationId).distinct) { conversation =>
        reader.all
          .read((tx: Tx^) ?=> reader.entries.list(conversation))
          .left
          .map((e: StoreError) => s"entries unread: ${Capture.kind(e)}")
          .map(conversation -> _)
      }
      .map { listed =>
        val entries = listed.toMap
        turns.map { turn =>
          val first = entries
            .getOrElse(turn.conversationId, Vector.empty)
            .filter(_.turnSeq == turn.turnSeq)
            .minByOption(e => EntrySeq.value(e.seq))
          turn -> first
            .toRight("no root entry")
            .flatMap(_.payload.said.toRight("its root holds no words a person said"))
        }.toMap
      }

  /** Each of `roots` asked of `jev` ([[WriteOut.question]]), one request a root, in order: the
    * probability it asks for a write out, or the kind of error Jev gave (`unavailable`,
    * `refused`, `unreadable`), and what Jev reported every answered request cost, in USD.
    */
  def ask(
      jev: Classifier^,
      roots: Vector[(TurnRef, String)]
  ): (Map[TurnRef, Either[String, Probability]], BigDecimal) = {
    import WriteOut.given
    val answered =
      roots.map((turn, said) => turn -> jev.ask(WriteOut.Said(said), WriteOut.question))
    val asked = answered.map((turn, a) =>
      turn -> a.map(_.value).left.map {
        case ClassifierError.Unavailable(_) => "unavailable"
        case ClassifierError.Refused(_) => "refused"
        case ClassifierError.Unreadable(_) => "unreadable"
      }
    )
    val spent = answered.flatMap(_._2.toOption).flatMap(_.usage.costUsd).sum
    (asked.toMap, spent)
  }
}
