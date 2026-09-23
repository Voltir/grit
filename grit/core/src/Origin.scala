package grit.core

/** Where a conversation's work comes from. Each origin names exactly one
  * conversation: the same origin always finds the same one.
  */
enum Origin {

  /** A TUI session. */
  case Tui(session: String)

  /** A Slack thread, named by its root message's timestamp. */
  case Slack(team: String, channel: String, threadTs: String)

  /** One run of a triggered task, such as one schedule slot. */
  case Task(name: String, run: String)
}
