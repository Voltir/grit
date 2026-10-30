package grit.chat.run

import java.nio.file.Path

import grit.chat.host.ChatHost
import grit.chat.look.Theme
import grit.chat.screen.ChatScreen
import grit.chat.settings.Prefs
import grit.core.clock.{Clock, Fresh}
import grit.core.edge.Console
import grit.core.message.Tokens
import grit.core.provider.TokenEstimator
import grit.core.store.Origin
import grit.tui.runtime.app.{Host, Mailbox}
import grit.tui.runtime.loop.Runtime

/** The chat on this process's terminal. */
object Chat {

  /** Runs the chat on this process's terminal until the person quits: the conversation
    * `origin` names, on the engine `opener` opens behind the first frame; titled `model`, beside
    * `session` and who runs the engine; first in `theme`; windows measured against `budget`, and
    * each turn's window and prompt estimated with `estimator`. A theme chosen with `/theme` is
    * kept in `prefs`, when there is one; a write that fails says so in the status line. A
    * throwable the screen survived is appended to `log`, when there is one. Returns once the
    * engine, however far it opened, is closed. The JVM needs `--enable-native-access=ALL-UNNAMED`
    * (grit.tui's terminal).
    */
  def run(
      opener: Console.Opener^,
      origin: Origin,
      model: String,
      session: String,
      theme: Theme,
      budget: Tokens,
      estimator: TokenEstimator,
      prefs: Option[Path],
      log: Option[Path],
      clock: Clock^,
      fresh: Fresh^
  ): Unit = {
    val host = new ChatHost(origin, opener, estimator, clock, fresh, log)
    // Closing the host stops following and closes the engine, however far it got.
    try
      Runtime.run(
        new ChatScreen.App(model, theme, budget, heading(session, opener.runner)),
        keeping(host, prefs)
      )
    finally host.close()
  }

  /** The status line's `session`, and who runs the engine `runner` found. */
  private[run] def heading(session: String, runner: Console.Runner): String = {
    val mode = runner match {
      case Console.Runner.Here => "engine"
      case Console.Runner.Elsewhere(holder) =>
        holder.fold("attached")(h => s"attached · engine on ${h.machine} pid ${h.pid}")
    }
    s"$session · $mode"
  }

  /** `host`, and the screen's [[ChatScreen.Msg.KeepTheme]] written to the preferences at
    * `file` (nowhere without one), off the screen's thread; a write that fails says so in
    * the status line.
    */
  private def keeping(
      host: Host[ChatScreen.Msg]^,
      file: Option[Path]
  ): Host[ChatScreen.Msg]^{host} =
    new Host[ChatScreen.Msg] {
      def receive(msg: ChatScreen.Msg, mailbox: Mailbox[ChatScreen.Msg]): Unit = msg match {
        case ChatScreen.Msg.KeepTheme(key) =>
          file.foreach { f =>
            val _ = Thread.ofVirtual().name("grit-prefs").start { () =>
              Prefs.save(f, Prefs.load(f).copy(theme = Some(key))).left.foreach { why =>
                mailbox.offer(ChatScreen.Msg.Noted(s"theme $key, not kept: $why"))
              }
            }
          }
        case other => host.receive(other, mailbox)
      }
    }
}
