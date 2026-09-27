package grit.app.chat

import java.util.UUID
import java.util.concurrent.CountDownLatch

import scala.util.control.NonFatal

import grit.app.config.Lifecycle
import grit.core.id.{ConversationId, PrincipalId, SourceId, TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.provider.TokenEstimator
import grit.core.store.{Entry, Origin, Payload, StoreError, UsageLedger}
import grit.dbos.engine.{Engine, TurnStatus}
import grit.tui.runtime.app.{Fault, Host, Mailbox}
import grit.turn.TurnStream

/** The chat screen's engine side, for the conversation `origin` names. The screen reaches
  * the engine only through its inbox and store, as any edge does (ADR 0002).
  *
  *   - `Load` opens the engine with `opener`, says `Opened`, then follows the
  *     conversation: its entries are polled and every new one is shown, whichever turn
  *     wrote it, so replies to turns recovered after a restart appear too ([[Follow]]);
  *     and its latest turn (or the one `Show` pinned) is described for the turn panel
  *     whenever that changes ([[TurnView]], its window and the system prompt it recorded
  *     estimated with `estimator`); and the conversation as a whole is described for the
  *     panel's session tab whenever an entry is added ([[SessionView]]), and its topics
  *     for the topics tab, with the shown turn's placing, whenever those change
  *     ([[TopicsView]]).
  *   - `Send` ingests a message and starts its turn, once the engine is open; the reply
  *     arrives by following.
  *   - `Show` pins the panel to a turn, or back to the latest.
  *   - `Answer` answers a turn's call that asks first, through the inbox; a failure says so.
  *   - `Settings` reads the lifecycle's settings or changes one; the status line says what
  *     came of it.
  *
  * All of it runs on virtual threads and answers through the mailbox, so the screen paints
  * at once and never waits on the database or the model. [[close]] stops following and
  * closes the engine, including one that finishes opening after it. A throwable the
  * screen's loop survived is appended to `log`, when there is one.
  */
final class ChatHost(
    origin: Origin,
    opener: ChatHost.Opener^,
    estimator: TokenEstimator,
    log: Option[java.nio.file.Path] = None
) extends Host[ChatScreen.Msg],
      AutoCloseable,
      caps.SharedCapability {

  /** How often the conversation is read. A `LISTEN` on the store's notifications
    * replaces this when replies stream.
    */
  private val PollMs = 400L

  @volatile @caps.unsafe.untrackedCaptures
  private var open = true

  /** The turn the panel is pinned to; `None` follows the latest. */
  @volatile @caps.unsafe.untrackedCaptures
  private var pinned: Option[TurnSeq] = None

  /** The open engine; `None` until it opens, and again once closed. Guarded by `lock`. */
  @caps.unsafe.untrackedCaptures
  private var engine: Option[Engine^] = None

  private val lock = new Object

  /** Released once the engine is open, or will never be. */
  private val settled = new CountDownLatch(1)

  override def fault(f: Fault): Unit =
    log.foreach { path =>
      val entry = (s"[grit-tui] ${f.stage} failed: ${f.error}" +: f.trace.map("    at " + _))
        .mkString("", "\n", "\n")
      try {
        val _ = java.nio.file.Files.writeString(
          path,
          entry,
          java.nio.file.StandardOpenOption.CREATE,
          java.nio.file.StandardOpenOption.APPEND
        )
      } catch { case NonFatal(_) => () } // a log that cannot be written is not the screen's to fix
    }

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
      case ChatScreen.Msg.Show(turn) => pinned = turn
      case ChatScreen.Msg.Answer(workflow, call, approval) =>
        background { () =>
          settled.await()
          current match {
            case Some(e) =>
              e.inbox.answer(workflow, call, approval).left.foreach { error =>
                mailbox.offer(ChatScreen.Msg.Failed(s"not answered: $error"))
              }
            case None =>
              mailbox.offer(ChatScreen.Msg.Failed("not answered: the engine is not open"))
          }
        }
      case ChatScreen.Msg.Settings(change) =>
        whenOpen(mailbox, "settings not read") { e =>
          val result = change match {
            case None => e.db.read(e.lifecycle.current()).left.map(_.toString)
            case Some(c) =>
              // One transaction: read, change and write, so two changes at once both land.
              e.jot
                .write(e.lifecycle.current().flatMap { now =>
                  c.applied(now) match {
                    case Right(next) => e.lifecycle.set(next).map(_ => Right(next))
                    case Left(why) => Right(Left(why))
                  }
                })
                .left
                .map(_.toString)
                .flatten
          }
          mailbox.offer(
            ChatScreen.Msg.Noted(result.fold(why => s"not set: $why", Lifecycle.describe))
          )
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

  /** Runs `work` off the screen's thread once the engine is open; says `failed` when it will
    * not open.
    */
  private def whenOpen(mailbox: Mailbox[ChatScreen.Msg], failed: String)(
      work: Engine^ => Unit
  ): Unit =
    background { () =>
      settled.await()
      current match {
        case Some(e) => work(e)
        case None => mailbox.offer(ChatScreen.Msg.Failed(s"$failed: the engine is not open"))
      }
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
    engine.conversation(origin, PrincipalId.Local) match {
      case Left(e) => mailbox.offer(ChatScreen.Msg.Failed(s"could not open the conversation: $e"))
      case Right(conversation) =>
        var state = Follow.start
        var shown: Option[TurnView] = None
        var topics: Option[TopicsView] = None
        var listened = Set.empty[TurnSeq]
        var costs = Map.empty[TurnSeq, Vector[UsageLedger.Row]]
        var settled = Set.empty[TurnSeq]
        var sessionAt = Long.MinValue
        while (open) {
          engine.db.read(engine.entries.list(conversation)) match {
            case Right(entries) =>
              val (next, msgs) = Follow.step(state, entries, turn => engine.status(turn))
              state = next
              msgs.foreach(mailbox.offer)
              // The ledger is written with the entries it prices, so it changes only when
              // they do.
              val last = entries.lastOption.fold(-1L)(_.seq)
              if (last != sessionAt) {
                val (unread, final1) = SessionView.unread(entries, settled)
                ledgers(engine, conversation, unread).foreach { read =>
                  costs = costs ++ read
                  settled = final1
                  sessionAt = last
                  val view = SessionView.of(entries, costs.values.toVector.flatten)
                  mailbox.offer(ChatScreen.Msg.Session(view))
                }
              }
              // A running turn's reply is followed as it streams, once per turn.
              TurnView.latest(entries).filter(_ => state.thinking).foreach { turn =>
                if (!listened.contains(turn.turnSeq)) {
                  listened = listened + turn.turnSeq
                  listen(engine, turn, mailbox)
                }
              }
              val target = pinned
                .filter(t => entries.exists(_.turnSeq == t))
                .map(TurnRef(conversation, _))
                .orElse(TurnView.latest(entries))
              val placed = TopicsView.of(entries, target.map(_.turnSeq))
              if (!topics.contains(placed)) {
                topics = Some(placed)
                mailbox.offer(ChatScreen.Msg.Topics(placed))
              }
              target.foreach { turn =>
                // A settled turn changes no more: its view is not read again.
                if (!shown.exists(v => v.turn == turn.turnSeq && v.settled)) {
                  val view = described(engine, turn, entries)
                  if (!shown.contains(view)) {
                    shown = Some(view)
                    mailbox.offer(ChatScreen.Msg.Turn(view))
                  }
                }
              }
            // The engine closes before this notices it should stop; nothing to report.
            case Left(_) => ()
          }
          Thread.sleep(PollMs)
        }
    }

  /** The ledger rows of each of `turns`, read in one transaction; `None` when the store
    * could not be read.
    */
  private def ledgers(
      engine: Engine^,
      conversation: ConversationId,
      turns: Vector[TurnSeq]
  ): Option[Map[TurnSeq, Vector[UsageLedger.Row]]] =
    engine.db.read {
      turns.foldLeft[Either[StoreError, Map[TurnSeq, Vector[UsageLedger.Row]]]](Right(Map.empty)) {
        (acc, t) =>
          acc.flatMap(m =>
            engine.ledger.of(TurnRef(conversation, t).workflowId).map(m.updated(t, _))
          )
      }
    }.toOption

  /** `turn` as the panel shows it, from `entries` and what DBOS and the ledger hold. A
    * turn not yet enqueued counts as running: its first step is next.
    */
  private def described(engine: Engine^, turn: TurnRef, entries: Vector[Entry]): TurnView = {
    val (running, steps) = engine.status(turn) match {
      case TurnStatus.Running(recorded) => (true, recorded)
      case TurnStatus.Unknown => (true, Vector.empty)
      case TurnStatus.Finished(_) => (false, engine.steps(turn))
    }
    val costs = engine.db.read(engine.ledger.of(turn.workflowId)).getOrElse(Vector.empty)
    val profile = engine.db.read(engine.profiles.of(turn.workflowId)).toOption.flatten
    // The entries of other conversations the turn's recorded window showed, as it showed
    // them; one purged since is simply not counted.
    val shown = entries
      .filter(_.turnSeq == turn.turnSeq)
      .flatMap(_.payload match {
        case Payload.Window(_, _, nearby) => nearby.flatMap(_.entries)
        case _ => Vector.empty
      })
    val nearby = engine.db
      .read(
        shown.foldLeft[Either[StoreError, Vector[Entry]]](Right(Vector.empty)) { (acc, id) =>
          acc.flatMap(found => engine.entries.get(id).map(found ++ _))
        }
      )
      .getOrElse(Vector.empty)
    val prompt = engine.db.read(engine.prompts.of(turn.workflowId)).toOption.flatten
    TurnView.of(turn, entries, steps, running, costs, prompt, estimator, profile, nearby)
  }

  /** Follows `turn`'s reply stream on a thread of its own, telling the screen what it has
    * heard so far after each piece, the latest attempt's ([[TurnStream.Heard]]). Ends with
    * the turn's workflow, or with the host.
    */
  private def listen(engine: Engine^, turn: TurnRef, mailbox: Mailbox[ChatScreen.Msg]): Unit =
    background { () =>
      val pieces = engine.stream(turn, TurnStream.Key)
      var heard = TurnStream.Heard.nothing
      while (open && pieces.hasNext) {
        TurnStream.decode(pieces.next()).foreach { piece =>
          heard = heard + piece
          mailbox.offer(
            ChatScreen.Msg.Heard(
              ChatScreen.Hearing(turn.turnSeq, heard.reasoning, heard.text, heard.calling)
            )
          )
        }
      }
    }

  private def send(engine: Engine^, text: String, mailbox: Mailbox[ChatScreen.Msg]): Unit = {
    // The TUI never redelivers, so each message is its own source.
    val started = for {
      turn <- engine.inbox.ingest(
        origin,
        SourceId(UUID.randomUUID().toString),
        Message.User(text),
        PrincipalId.Local
      )
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
