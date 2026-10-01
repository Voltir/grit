package grit.tui.wire.term

/** The process's own standard output, for the one case that is not a terminal.
  *
  * When there is no tty there is nothing to take modes on, but there is still a frame to
  * emit -- `./app` piped into a file, and what `./gate` leans on. That is still the
  * process touching a device, so it lives at the seam with everything else that does:
  * the quarantine rule says the terminal is touched in exactly one package, and the
  * answer to a violation is to put the code where it belongs rather than to widen the
  * rule.
  */
object Stdout {

  /** Write `s` and flush. No modes, no state, no restoring -- there is nothing to give
    * back.
    */
  def emit(s: String): Unit = {
    System.out.print(s)
    System.out.flush()
  }
}
