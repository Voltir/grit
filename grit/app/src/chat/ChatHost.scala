package grit.app.chat

import java.util.UUID
import java.util.concurrent.CountDownLatch

import scala.util.control.NonFatal

import grit.core.id.SourceId
import grit.core.message.Message
import grit.core.store.Origin
import grit.dbos.engine.Engine
import grit.tui.runtime.app.{Host, Mailbox}

/** The chat screen's engine side, for the conversation `origin` names. The screen reaches
  * the engine only through its inbox and store, as any edge does (ADR 0002).
  *
  *   - `Load` opens the engine with `opener`, says `Opened`, then follows the
  *     conversation: its entries are polled and every new one is shown, whichever turn
  *     wrote it, so replies to turns recovered after a restart appear too ([[Follow]]).
  *   - `Send` ingests a message and starts its turn, once the engine is open; the reply
  *     arrives by following.
  *
  * All of it runs on virtual threads and answers through the mailbox, so the screen paints
  * at once and never waits on the database or the model. [[close]] stops following and
  * closes the engine, including one that finishes opening after it.
  */
final class ChatHost(origin: Origin, opener: ChatHost.Opener^)
    extends Host[ChatScreen.Msg],
      AutoCloseable,
      caps.SharedCapability {

  /** How often the conversation is read. A `LISTEN` on the store's notifications
    * replaces this when replies stream.
    */
  private val PollMs = 400L

  @volatile @caps.unsafe.untrackedCaptures
  private var open = true

  /** The open engine; `None` until it opens, and again once closed. Guarded by `lock`. */
  @caps.unsafe.untrackedCaptures
  private var engine: Option[Engine^] = None

  private val lock = new Object

  /** Released once the engine is open, or will never be. */
  private val settled = new CountDownLatch(1)

  def receive(msg: ChatScreen.Msg, mailbox: Mailbox[ChatScreen.Msg]): Unit =
    msg match {
      case ChatScreen.Msg.Load =>
        background { () =>
          opened(mailbox) match {
            case Some(e) =>
              mailbox.offer(ChatScreen.Msg.Opened)
              follow(e, mailbox)
            case None => ()
          }
        }
      case ChatScreen.Msg.Send(text) =>
        background { () =>
          settled.await()
          current match {
            case Some(e) => send(e, text, mailbox)
            case None => mailbox.offer(ChatScreen.Msg.Failed("not sent: the engine is not open"))
          }
        }
      case _ => ()
    }

  def close(): Unit = {
    lock.synchronized {
      open = false
      engine match {
        case Some(e) => e.close()
        case None => ()
      }
      engine = None
    }
    settled.countDown()
  }

  private def current: Option[Engine^] = lock.synchronized(engine)

  /** The engine, opened; `None` when it failed (said on the mailbox) or the host closed
    * first (the engine is closed at once: DBOS's threads would keep the JVM alive).
    */
  private def opened(mailbox: Mailbox[ChatScreen.Msg]): Option[Engine^] = {
    val result =
      try Right(opener.open())
      catch { case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString)) }
    val kept = result match {
      case Left(why) =>
        mailbox.offer(ChatScreen.Msg.Failed(s"could not open the engine: $why"))
        None
      case Right(e) =>
        lock.synchronized {
          if (open) { engine = Some(e); Some(e) }
          else { e.close(); None }
        }
    }
    settled.countDown()
    kept
  }

  private def follow(engine: Engine^, mailbox: Mailbox[ChatScreen.Msg]): Unit =
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

  private def send(engine: Engine^, text: String, mailbox: Mailbox[ChatScreen.Msg]): Unit = {
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

object ChatHost {

  /** Opens the engine the screen talks to, its turn launched. Called once, off the screen's
    * thread; a throw is reported on screen.
    */
  trait Opener extends caps.SharedCapability {
    def open(): Engine^
  }
}
