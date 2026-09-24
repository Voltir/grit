package grit.app

import grit.core.{Message, Origin, Payload, SourceId, WorkflowId}
import grit.dbos.Engine
import grit.tui.runtime.{Host, Mailbox}
import grit.tui.runtime.std.Std
import java.util.UUID
import scala.util.control.NonFatal

/** The chat screen's engine side: what [[ChatScreen]]'s requests mean against `engine`,
  * for the conversation `origin` names. The screen reaches the engine only through its
  * inbox and store, as any edge does (ADR 0002). Every request runs on a virtual thread of
  * its own and answers through the mailbox, so the screen never waits on the database or
  * the model.
  */
final class ChatHost(engine: Engine^, origin: Origin)
    extends Host[Std | ChatScreen.Msg],
      caps.SharedCapability {

  def receive(msg: Std | ChatScreen.Msg, mailbox: Mailbox[Std | ChatScreen.Msg]): Unit =
    msg match {
      case ChatScreen.Msg.Load => background(() => mailbox.offer(load()))
      case ChatScreen.Msg.Send(text) => background(() => send(text, mailbox))
      case _ => ()
    }

  private def load(): ChatScreen.Msg =
    engine.history(origin) match {
      case Left(e) => ChatScreen.Msg.Failed(s"could not load the conversation: $e")
      case Right(entries) =>
        ChatScreen.Msg.Loaded(entries.flatMap { e =>
          val user = e.payload match {
            case Payload.Message(Message.User(_)) => true
            case _ => false
          }
          Replies.text(e).map(ChatScreen.Said(user, _))
        })
    }

  private def send(text: String, mailbox: Mailbox[Std | ChatScreen.Msg]): Unit = {
    // The TUI never redelivers, so each message is its own source.
    val started = for {
      turn <- engine.inbox.ingest(origin, SourceId(UUID.randomUUID().toString), Message.User(text))
      _ <- engine.inbox.startTurn(turn)
    } yield turn
    started match {
      case Left(e) => mailbox.offer(ChatScreen.Msg.Failed(s"not sent: $e"))
      case Right(turn) =>
        mailbox.offer(ChatScreen.Msg.Started(WorkflowId.value(turn.workflowId)))
        val outcome =
          try engine.awaitTurn(turn)
          catch { case NonFatal(e) => s"threw: ${e.getMessage}" }
        // The reply is read from the store, never from the workflow's output.
        mailbox.offer(Replies.of(engine, turn) match {
          case Right(Some(reply)) => ChatScreen.Msg.Replied(reply)
          case Right(None) => ChatScreen.Msg.Failed(outcome)
          case Left(why) => ChatScreen.Msg.Failed(why)
        })
    }
  }

  private def background(work: () => Unit): Unit = {
    val _ = Thread.ofVirtual().name("grit-chat").start(() => work())
  }
}
