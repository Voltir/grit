package grit.core.id

/** A served edge's name, as the log and a refusal state it (`slack`). */
opaque type EdgeName = String

object EdgeName {
  def apply(value: String): EdgeName = value
  def value(name: EdgeName): String = name

  /** The terminal's: a TUI session's messages arrive through it. */
  val Tui: EdgeName = "tui"

  /** Slack's: a Slack thread's messages arrive through it, and it posts their replies. */
  val Slack: EdgeName = "slack"

  /** grit's own, for a task's run, which grit starts itself and no person writes to. */
  val Task: EdgeName = "task"
}
