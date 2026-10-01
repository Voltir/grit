package grit.core.inbox

import grit.core.message.Message

/** Where a turn has got to, as an edge reads it through the [[Inbox]]. */
enum Progress {

  /** Not yet run to its end: never started, queued, or running. */
  case Open

  /** Run to its end: its reply (the entry [[grit.core.id.TurnRef.replyId]] names), or `None`
    * when it ended with none, and the turn's outcome as its workflow returned it, or why it
    * returned none.
    */
  case Done(reply: Option[Message.Assistant], outcome: String)
}
