package grit.core.store

/** Acceptance commands and failed checks for tests. */
object TestWentOn {

  /** `text` as a command; a test's own command is never blank or over the bound. */
  def command(text: String): WentOn.Command =
    WentOn.Command.of(text).getOrElse(throw new java.lang.AssertionError(s"$text is a command"))

  /** `text` failed with `exit`, its output `output`, as [[WentOn.failed]] keeps it. */
  def failed(text: String, exit: Option[Int], output: String): WentOn.CheckFailed =
    WentOn.failed(command(text), exit, output)
}
