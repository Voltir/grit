package grit.app

import grit.core.{Message, Origin, SourceId}
import grit.dbos.Engine
import grit.tui.runtime.{Host, Mailbox}
import java.util.UUID
import scala.util.control.NonFatal

/** The chat screen's engine side, for the conversation `origin` names. The screen reaches
  * the engine only through its inbox and store, as any edge does (ADR 0002).
  *
  *   - `Load` starts following the conversation: its entries are polled and every new one
  *     is shown, whichever turn wrote it, so replies to turns recovered after a restart
  *     appear too ([[Follow]]).
  *   - `Send` ingests a message and starts its turn; the reply arrives by following.
  *
  * Both run on virtual threads and answer through the mailbox, so the screen never waits
  * on the database or the model. [[close]] stops following.
  */
final class ChatHost(engine: Engine^, origin: Origin)
    extends Host[ChatScreen.Msg],
      AutoCloseable,
      caps.SharedCapability {

  /** How often the conversation is read. A `LISTEN` on the store's notifications
    * replaces this when replies stream.
    */
  private val PollMs = 400L

  @volatile @caps.unsafe.untrackedCaptures
  private var open = true

  def receive(msg: ChatScreen.Msg, mailbox: Mailbox[ChatScreen.Msg]): Unit =
    msg match {
      case ChatScreen.Msg.Load => background(() => follow(mailbox))
      case ChatScreen.Msg.Send(text) => background(() => send(text, mailbox))
      case _ => ()
    }

  def close(): Unit = open = false

  private def follow(mailbox: Mailbox[ChatScreen.Msg]): Unit =
    engine.conversation(origin) match {
      case Left(e) => mailbox.offer(ChatScreen.Msg.Failed(s"could not open the conversation: $e"))
      case Right(conversation) =>
        var state = Follow.start
        while (open) {
          engine.db.read(engine.entries.list(conversation)) match {
            case Right(entries) =>
              val (next, msgs) = Follow.step(state, entries, turn => engine.status(turn))
              state = next
              msgs.foreach(mailbox.offer)
            // The engine closes before this notices it should stop; nothing to report.
            case Left(_) => ()
          }
          Thread.sleep(PollMs)
        }
    }

  private def send(text: String, mailbox: Mailbox[ChatScreen.Msg]): Unit = {
    // The TUI never redelivers, so each message is its own source.
    val started = for {
      turn <- engine.inbox.ingest(origin, SourceId(UUID.randomUUID().toString), Message.User(text))
      _ <- engine.inbox.startTurn(turn)
    } yield turn
    started.left.foreach(e => mailbox.offer(ChatScreen.Msg.Failed(s"not sent: $e")))
  }

  private def background(work: () => Unit): Unit = {
    val _ = Thread
      .ofVirtual()
      .name("grit-chat")
      .start { () =>
        try work()
        catch { case _: InterruptedException => (); case NonFatal(_) => () }
      }
  }
}
