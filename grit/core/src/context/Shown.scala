package grit.core.context

import grit.core.message.Message
import grit.core.store.{Entry, Payload}

/** What the model is shown of an entry a window names: the one definition, which an
  * assembler costs and the turn sends.
  */
object Shown {

  /** A message as it is; a closing entry as one user message, [[grit.core.period.Closing.shown]]
    * dated by the entry; `None` for any other entry.
    */
  def of(entry: Entry): Option[Message] = entry.payload match {
    case Payload.Message(m) => Some(m)
    case Payload.Closed(_, reason, closing) =>
      Some(Message.User(closing.shown(entry.createdAt, reason)))
    case _ => None
  }
}
