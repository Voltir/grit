package grit.tui.runtime.app

/** Where a running app's messages can be delivered from outside the loop. */
trait Mailbox[-Msg] {

  /** Queue `msg` for the app, to be handled on the loop's thread. Safe from any thread. */
  def offer(msg: Msg): Unit
}

/** What an embedding program does with an app's [[Effect.ToHost]] messages: the one place
  * the app's requests meet the capabilities they need.
  *
  * `receive` runs on the loop's thread, so it must return quickly: slow work belongs on a
  * thread of the host's own, answering through `mailbox` when it is done.
  */
trait Host[Msg] {
  def receive(msg: Msg, mailbox: Mailbox[Msg]): Unit

  /** A throwable the loop caught in the app ([[Fault]]), for the host to log where it
    * logs. Runs on the loop's thread, like `receive`. Ignored by default.
    */
  def fault(@scala.annotation.unused f: Fault): Unit = ()
}

object Host {

  /** A host that ignores every message: the default for an app that sends none. */
  def none[Msg]: Host[Msg] = (_, _) => ()
}
