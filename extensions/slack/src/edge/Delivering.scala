package grit.slack.edge

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import grit.core.approval.Approval
import grit.core.clock.{Clock, Utc}
import grit.core.edge.{Acknowledgement, EdgeStores, Part, Pending}
import grit.core.id.{ToolCallId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.inbox.{InboxError, Progress}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Subject
import grit.prose.form.{Block, Doc, Text}
import grit.prose.markdown.Markdown
import grit.slack.client.{Card, Respond, Self, Slack, SlackError, Tag}
import grit.slack.event.{Clicked, Events, TeamId, Ts, UserId}
import grit.slack.text.{Post, RichText}

/** What an edge posts in a turn's thread ([[SlackEdge.acknowledge]], [[SlackEdge.deliver]],
  * [[SlackEdge.click]]): the mark on a message while its turn runs, the turn's reply, and its asks
  * as cards. `requiet` is told when a delivered turn's asks may have quieted a room.
  */
private[slack] final class Delivering(
    slack: Slack,
    self: Self,
    stores: EdgeStores^,
    note: ReplyNote,
    clock: Clock^,
    said: String => Unit,
    names: Names^{slack, said},
    requiet: () => Unit
) {
  import SlackEdge.*
  import Delivering.posts

  // A flag, written only through its own atomic operations: a pass racing the first either
  // sets it or sees it set, and the restart it guards is a no-op for a turn already started.
  @caps.unsafe.untrackedCaptures
  private val started = new AtomicBoolean(false)

  // Each ask this edge has shown its card for, by its turn's workflow and its call: added once
  // Slack has the card, and a turn's taken out as its reply is delivered, each atomically. A
  // pass racing another may show a card twice, which the asker sees as two cards of one ask,
  // either of whose clicks answers it; a card is never left unshown, as asks are read from the
  // store each pass, never from it.
  @caps.unsafe.untrackedCaptures
  private val carded = ConcurrentHashMap.newKeySet[(WorkflowId, ToolCallId)]()

  /** Posts `posts` in `at`'s thread under `tag`, unless a message there already carries it. */
  def once(at: Address, tag: Tag, posts: Vector[Post]): Either[String, Unit] =
    slack
      .tagged(at.channel, at.thread, tag)
      .flatMap { there =>
        if (there.nonEmpty) Right(())
        else
          posts.foldLeft[Either[SlackError, Unit]](Right(()))((done, post) =>
            done.flatMap(_ => slack.post(at.channel, at.thread, post, tag).map(_ => ()))
          )
      }
      .left
      .map(_.toString)

  /** One pass over the marks standing, as [[SlackEdge.acknowledge]] says. */
  def acknowledge(): Either[StoreError, Int] =
    stores.jot.write(Subject.Public)(stores.acknowledgements.standing()).flatMap { standing =>
      val read = standing.map { a =>
        if (!a.shown)
          stores.inbox
            .startTurn(a.turn)
            .left
            .foreach(e => said(s"slack: ${WorkflowId.value(a.turn.workflowId)} not started: $e"))
        a -> stores.inbox.progress(a.turn)
      }
      // Read after each turn's progress: a turn seen ended has kept whether its reply is awaited.
      val awaited: Either[StoreError, Set[TurnRef]] =
        if (
          read.exists(_._2 match {
            case Right(Progress.Done(_, _)) => true
            case _ => false
          })
        )
          stores.jot.write(Subject.Public)(stores.deliveries.pending()).map(_.map(_.turn).toSet)
        else Right(Set.empty)
      awaited.map(posting => read.count((a, progress) => acknowledged(a, progress, posting)))
    }

  /** Puts `a`'s mark up, takes it down, or clears it, as [[acknowledge]] says, given its turn's
    * `progress` and the turns whose replies are still `posting`; whether it changed.
    */
  private def acknowledged(
      a: Acknowledgement,
      progress: Either[InboxError, Progress],
      posting: Set[TurnRef]
  ): Boolean = {
    val turn = WorkflowId.value(a.turn.workflowId)
    def record(write: Instant => (Tx^) ?=> Either[StoreError, Unit]): Boolean = {
      val at = clock.now()
      stores.jot.write(Subject.Turn(a.turn))(write(at)) match {
        case Right(()) => true
        case Left(e) =>
          said(s"slack: the mark of $turn not recorded: $e")
          false
      }
    }
    def clear: Boolean = record(at => stores.acknowledgements.cleared(a.turn, at))
    (Address.read(a.to), progress) match {
      case (None, _) =>
        said(s"slack: $turn acknowledged at an address grit did not write, cleared: ${a.to}")
        clear
      case (_, Left(e)) =>
        said(s"slack: $turn unreadable: $e")
        false
      case (Some(to), Right(Progress.Open)) =>
        if (a.shown) false
        else
          slack.react(to.channel, to.answered, Working) match {
            case Right(()) => record(at => stores.acknowledgements.shown(a.turn, at))
            case Left(SlackError.MessageGone) => clear
            case Left(e) =>
              said(s"slack: $turn not marked: $e")
              false
          }
      case (Some(to), Right(Progress.Done(_, _))) =>
        if (posting.contains(a.turn)) false
        else if (!a.shown) clear
        else
          slack.unreact(to.channel, to.answered, Working) match {
            case Right(()) => clear
            case Left(e) =>
              said(s"slack: $turn not unmarked: $e")
              false
          }
    }
  }

  /** One pass over the replies awaited, as [[SlackEdge.deliver]] says. */
  def deliver(): Either[StoreError, Int] =
    stores.jot.write(Subject.Public)(stores.deliveries.pending()).map { pending =>
      if (!started.getAndSet(true))
        pending.foreach(p =>
          stores.inbox.startTurn(p.turn) match {
            case Right(()) | Left(InboxError.SlotRun(_)) => ()
            case Left(e) => said(s"slack: not restarted: $e")
          }
        )
      pending.count { p =>
        stores.inbox.progress(p.turn) match {
          case Right(Progress.Done(reply, outcome)) =>
            // An ask approved may have quieted its room: label_room and quiet_room ask first.
            if (carded.removeIf(_._1 == p.turn.workflowId)) requiet()
            post(p, reply, outcome)
          case Right(Progress.Open) =>
            ask(p)
            false
          case Left(e) =>
            said(s"slack: ${WorkflowId.value(p.turn.workflowId)} unreadable: $e")
            false
        }
      }
    }

  /** `parts` with `p`'s note after them when its reply is noted ([[ReplyNote.of]]); as they are
    * when it has no reply, or its turn's spend cannot be read, which is said.
    */
  private def noted(
      p: Pending,
      reply: Option[Message.Assistant],
      parts: Vector[Post]
  ): Vector[Post] =
    if (reply.isEmpty || note == ReplyNote.Off) parts
    else
      stores.jot.write(Subject.Turn(p.turn))(stores.spending.turn(p.turn)) match {
        case Right(spend) => note.of(spend).fold(parts)(RichText.noted(parts, _))
        case Left(e) =>
          said(s"slack: ${WorkflowId.value(p.turn.workflowId)} posted without its note: $e")
          parts
      }

  /** Shows each of `p`'s turn's open asks this edge has not shown to its asker, as a card in the
    * turn's thread ([[Slack.ask]]), read in a transaction opened for the turn
    * ([[Subject.Turn]]): its words, and when it expires in the zone Slack places the asker in,
    * else in UTC. Before the card, a reply everyone in the thread sees says, naming the asker,
    * that the turn waits for them ([[Tag.Asking]]); it is posted once per ask, ever, though a
    * restarted edge shows the card again, and it stays. An ask put to an account that is no
    * user of this team is said once, and neither noted nor shown. A note Slack will not post is
    * said and the card shown all the same; a card Slack will not show, and asks the store could
    * not read, are said, and tried again next pass, the note with the card when still missing.
    */
  private def ask(p: Pending): Unit =
    Address.read(p.to).foreach { to =>
      val workflow = p.turn.workflowId
      stores.jot.write(Subject.Turn(p.turn))(stores.deliveries.asks(p.turn)) match {
        case Left(e) => said(s"slack: the asks of ${WorkflowId.value(workflow)} unread: $e")
        case Right(open) =>
          open.filterNot(a => carded.contains((workflow, a.call))).foreach { a =>
            SlackAccounts.user(a.to, self.team) match {
              case None =>
                val _ = carded.add((workflow, a.call))
                said(
                  s"slack: ${ToolCallId.value(a.call)} of ${WorkflowId.value(workflow)} asks " +
                    s"${Account.written(a.to)}, no user of ${TeamId.value(self.team)}; not shown"
                )
              case Some(user) =>
                val until = names.namedOf(user).flatMap(_.zone) match {
                  case Some(zone) => Utc.toDayMinute(a.until, zone)
                  case None => Utc.toDayMinute(a.until)
                }
                val note = RichText.mentioning(
                  "Waiting for ",
                  user,
                  " to approve or cancel this; the buttons are shown only to them, in this thread."
                )
                once(
                  to,
                  Tag.Asking(WorkflowId.value(workflow), ToolCallId.value(a.call)),
                  Vector(note)
                ).left
                  .foreach(e =>
                    said(
                      s"slack: the note for ${ToolCallId.value(a.call)} of ${WorkflowId.value(workflow)} not posted: $e"
                    )
                  )
                slack.ask(
                  to.channel,
                  to.thread,
                  user,
                  Card(workflow, a.call, a.shown, until)
                ) match {
                  case Right(()) => val _ = carded.add((workflow, a.call))
                  case Left(e) =>
                    said(
                      s"slack: the card for ${ToolCallId.value(a.call)} of ${WorkflowId.value(workflow)} not shown: $e"
                    )
                }
            }
          }
      }
    }

  /** One click on an ask's card, as [[SlackEdge.click]] says. */
  def click(payload: String): Unit =
    Events.clicked(payload) match {
      case Left(why) => said(s"slack: a click grit cannot read: $why")
      case Right(c) =>
        val call = s"${ToolCallId.value(c.call)} of ${WorkflowId.value(c.workflow)}"
        SlackAccounts.account(c.team, c.user) match {
          case Left(why) =>
            said(s"slack: a click on $call not answered: its clicker cannot be named: $why")
          case Right(by) =>
            val approval = c.button match {
              case Clicked.Button.Apply => Approval.Approved
              case Clicked.Button.Cancel => Approval.Declined(None)
            }
            val shown: Option[(String, Respond)] =
              stores.inbox.answer(c.workflow, c.call, approval, by) match {
                case Right(()) =>
                  Some(
                    (if (approval == Approval.Approved) Applying else Cancelled, Respond.Replace)
                  )
                case Left(InboxError.Closed(_, _)) => Some((Expired, Respond.Replace))
                case Left(InboxError.NotAsker(_)) => Some((onlyAsker(askerOf(c)), Respond.Reply))
                case Left(e) =>
                  said(s"slack: a click on $call not answered: $e")
                  None
              }
            shown.foreach((text, mode) =>
              slack
                .respond(c.respondAt, text, mode)
                .left
                .foreach(e => said(s"slack: the card for $call not rewritten: $e"))
            )
        }
    }

  /** The Slack name of the person `c`'s card asks, when its ask is still open and they are a
    * user of this team grit knows by name.
    */
  private def askerOf(c: Clicked): Option[String] =
    TurnRef.fromWorkflowId(c.workflow).flatMap { turn =>
      stores.jot
        .write(Subject.Turn(turn))(stores.deliveries.asks(turn))
        .toOption
        .flatMap(_.find(_.call == c.call))
        .flatMap(a => SlackAccounts.user(a.to, self.team))
        .flatMap(names.namedOf)
        .map(_.name)
    }

  /** Posts `p`'s reply, part by part; whether all of it is posted and the turn delivered. */
  private def post(p: Pending, reply: Option[Message.Assistant], outcome: String): Boolean =
    Address.read(p.to) match {
      case None =>
        said(
          s"slack: ${WorkflowId.value(p.turn.workflowId)} awaited at an address grit did not write: ${p.to}"
        )
        val at = clock.now()
        stores.jot.write(Subject.Turn(p.turn))(
          stores.deliveries
            .delivered(p.turn)
            .flatMap(_ => stores.acknowledgements.cleared(p.turn, at))
        ) match {
          case Right(()) => true
          case Left(e) =>
            said(
              s"slack: ${WorkflowId.value(p.turn.workflowId)} given up but not marked delivered: $e"
            )
            false
        }
      case Some(to) =>
        val parts = noted(p, reply, posts(reply, outcome))
        val all = parts.zipWithIndex.forall { (part, i) =>
          val tag = Tag.Reply(WorkflowId.value(p.turn.workflowId), i)
          val there: Either[SlackError, Boolean] = p.parts.get(i) match {
            case Some(Part.Posted(_)) => Right(true)
            case Some(Part.Posting) => slack.tagged(to.channel, to.thread, tag).map(_.nonEmpty)
            case None => Right(false)
          }
          val done = there.flatMap { yes =>
            if (yes) Right(())
            else
              for {
                _ <- stores.jot
                  .write(Subject.Turn(p.turn))(stores.deliveries.posting(p.turn, i))
                  .left
                  .map(e => SlackError.Unreachable(e.toString))
                ts <- slack.post(to.channel, to.thread, part, tag)
                _ <- stores.jot
                  .write(Subject.Turn(p.turn))(stores.deliveries.posted(p.turn, i, Ts.value(ts)))
                  .left
                  .map(e => SlackError.Unreachable(e.toString))
              } yield ()
          }
          done.left.foreach(e =>
            said(s"slack: part $i of ${WorkflowId.value(p.turn.workflowId)} not posted: $e")
          )
          done.isRight
        }
        all && {
          slack
            .unreact(to.channel, to.answered, Working)
            .left
            .foreach(e => said(s"slack: not unmarked: $e"))
          val at = clock.now()
          stores.jot.write(Subject.Turn(p.turn))(
            stores.deliveries
              .delivered(p.turn)
              .flatMap(_ => stores.acknowledgements.cleared(p.turn, at))
          ) match {
            case Right(()) => true
            case Left(e) =>
              said(
                s"slack: ${WorkflowId.value(p.turn.workflowId)} posted but not marked delivered: $e"
              )
              false
          }
        }
    }
}

private[slack] object Delivering {

  /** The Slack messages of a reply: its text as [[RichText]] renders it, or, when it has no
    * text, one line saying grit could not answer, and `outcome`.
    */
  private def posts(reply: Option[Message.Assistant], outcome: String): Vector[Post] = {
    val text =
      reply.map(_.blocks.collect { case AssistantBlock.Text(t) => t }.mkString).getOrElse("")
    val rendered = RichText.render(Markdown.parse(text))
    if (rendered.nonEmpty) rendered
    else
      RichText.render(Doc(Vector(Block.Paragraph(Text.plain(s"grit could not answer: $outcome")))))
  }
}
