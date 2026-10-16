package grit.core.tool

import grit.core.id.ToolCallId
import grit.core.message.Message
import grit.core.store.Tx
import grit.core.visibility.{Accounted, Integrity, Label, Part}

/** What settling a tool call came to. Every case reaches the model as the call's result
  * ([[result]]), and the loop carries on: none of them fails the turn. A command's non-zero
  * exit is `Done`, the exit code in its text.
  */
enum Outcome {

  /** It ran; `text` is what the model reads, believed at `integrity` when the tool knows (the
    * meet of the rows it read), else at its tool's ([[Believed]]). Only a tool grit runs is
    * believed when it says: an edge's `integrity` is not read. An edge's result is believed at
    * what its service declares of `items`, the provenances its extension read
    * ([[grit.core.visibility.Visibility.believed]]); a tool grit runs leaves them unread.
    * `parts`, its items as its service's extension cut them (`None`: not cut), are never shown
    * as they are: [[readable]] shows a reader the ones it may read.
    */
  case Done(
      text: String,
      integrity: Option[Integrity] = None,
      items: Accounted = Accounted.Unread,
      parts: Option[Vector[Part]] = None
  )

  /** It could not run, or could not finish: an unknown tool, arguments that do not read, a
    * bad path, an `oldText` not found, a timeout (`why` holding the output so far).
    */
  case Failed(why: String)

  /** The person declined it, with their `reason` if they gave one. */
  case Declined(reason: Option[String])

  /** Nobody answered in time whether it may run, so it did not. */
  case Unanswered

  /** A crash cut a gated call short. It may have partly run, and is not run again. */
  case Interrupted

  /** What its service's extension read of its items: `Done`'s `items`; any other case's,
    * [[Accounted.Unread]].
    */
  def accounted: Accounted = this match {
    case Done(_, _, items, _) => items
    case _ => Accounted.Unread
  }

  /** As this transaction reads it: a cut `Done`'s parts it may not read ([[Tx.readsItem]]) left
    * out, leaving no trace; the others appended to its text in order, each on lines of its own,
    * their provenances added to its items (unaccountable stays so); [[Outcome.NoneShown]]
    * appended when it kept none; no parts left. Every other outcome as it is, read at
    * [[Label.Public]].
    */
  def readable(using tx: Tx^): Outcome.Read = this match {
    case Done(text, integrity, items, Some(parts)) =>
      val kept = parts.filter(p => Tx.readsItem(p.item.place, p.readers))
      val texts = if (kept.isEmpty) Vector(Outcome.NoneShown) else kept.map(_.text)
      val shown = (Vector(text).filter(_.nonEmpty) ++ texts).mkString("\n")
      val read = kept.map(_.item)
      val accounted = items match {
        case Accounted.Unaccountable => Accounted.Unaccountable
        case Accounted.Unread => if (read.isEmpty) Accounted.Unread else Accounted.of(read)
        case Accounted.Items(had) => Accounted.of(had ++ read)
      }
      val label = read.map(p => Tx.roomLabel(p.place)).foldLeft[Label](Label.Public)(_.join(_))
      Outcome.Read(Done(shown, integrity, accounted, None), label)
    case other => Outcome.Read(other, Label.Public)
  }

  /** The result the model reads for the call `call`: `Done` is not an error, every other
    * case is. `Declined` and `Unanswered` each begin with their name ("Declined: …",
    * "Unanswered: …"), a declined one ending with the person's reason when they gave one;
    * `Interrupted` says the call may have partly run and should be checked before it is
    * tried again.
    */
  def result(call: ToolCallId): Message.ToolResult = this match {
    case Done(text, _, _, _) => Message.ToolResult(call, text, isError = false)
    case Failed(why) => Message.ToolResult(call, why, isError = true)
    case Declined(reason) =>
      val said = reason.fold("")(r => s" Their reason: $r")
      Message.ToolResult(
        call,
        s"Declined: the person declined this call; it did not run.$said",
        isError = true
      )
    case Unanswered =>
      Message.ToolResult(
        call,
        "Unanswered: nobody answered in time; the call did not run.",
        isError = true
      )
    case Interrupted =>
      Message.ToolResult(
        call,
        "This call was cut short by a crash and may have partly run; it was not run again. " +
          "Check what it changed before trying it again.",
        isError = true
      )
  }
}

object Outcome {

  /** What [[Outcome.readable]] read: the outcome, and the join of the labels in force at its kept
    * parts' places ([[Tx.roomLabel]]), [[Label.Public]] when it kept none.
    */
  final case class Read(outcome: Outcome, label: Label)

  /** What a cut result the reader keeps no part of says, the same whether it had none or none
    * was read.
    */
  val NoneShown: String = "Nothing was found."
}
