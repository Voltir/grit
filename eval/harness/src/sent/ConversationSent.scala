package grit.eval.harness.sent

import java.time.Instant

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.store.{Entry, Tx}
import grit.dbos.internal.Reader
import grit.eval.harness.capture.Unread

/** A conversation's turns as their models were sent them: `turns`, each [[TurnSent]], oldest
  * first, and `entries`, every entry the conversation keeps, in order.
  */
final case class ConversationSent(turns: Vector[TurnSent], entries: Vector[Entry])

object ConversationSent {

  /** Conversation `conversation`'s turns whose workflows DBOS recorded created before `until`,
    * read through `reader`. `Left` when DBOS's or the store's tables cannot be read, the
    * conversation has no turn, or a turn cannot be read ([[TurnSent.read]], its reason given).
    */
  def read(
      reader: Reader^,
      conversation: ConversationId,
      until: Instant
  ): Either[String, ConversationSent] =
    for {
      all <- reader
        .turns(until)
        .left
        .map(e => s"the turns could not be read: ${e.described}")
      mine = all
        .flatMap((id, _) => TurnRef.fromWorkflowId(id).filter(_.conversationId == conversation))
        .sortBy(t => TurnSeq.value(t.turnSeq))
      _ <- Either.cond(
        mine.nonEmpty,
        (),
        s"no turn of ${ConversationId.value(conversation)} is recorded"
      )
      turns <- mine.foldLeft[Either[String, Vector[TurnSent]]](Right(Vector.empty))((acc, t) =>
        acc.flatMap(done => TurnSent.read(reader, t.workflowId).map(done :+ _))
      )
      entries <- reader.all
        .read((tx: Tx^) ?=> reader.entries.list(conversation)(using tx))
        .left
        .map(Unread.said(s"the entries of ${ConversationId.value(conversation)}"))
    } yield ConversationSent(turns, entries)
}
