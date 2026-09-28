package grit.slack.edge

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import grit.core.edge.{Deliveries, Part, Pending}
import grit.core.id.{PrincipalId, SourceId, WorkflowId}
import grit.core.inbox.{Inbox, Progress}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Jot, Origin, Principals}
import grit.prose.form.{Block, Doc, Text}
import grit.prose.markdown.Markdown
import grit.slack.client.{Self, Slack, SlackError, Tag}
import grit.slack.event.{ChannelId, Event, Events, TeamId, Ts, UserId}
import grit.slack.text.{Incoming, Post, RichText}

/** What the Slack edge records through: the inbox it hands messages to and reads turns from,
  * the people it enrolls, the replies it awaits, and `jot`, the short transactions it writes
  * those two in.
  */
final case class EdgeStores(inbox: Inbox, principals: Principals, deliveries: Deliveries, jot: Jot)

/** The Slack edge (ADR 0002, 0019): Slack's messages in as turns, grit's replies out, through
  * the stores alone; grit being `self` in the workspace. `said` is told what it did that a
  * person running it may want to read.
  */
final class SlackEdge(slack: Slack, self: Self, stores: EdgeStores^, said: String => Unit) {
  import SlackEdge.*

  // Caches and a flag, each written only through its own atomic operations. What they hold
  // is looked up again from Slack or the store when missing, so a reader that races a writer
  // sees either value, and both are right.
  @caps.unsafe.untrackedCaptures
  private val names = new ConcurrentHashMap[UserId, String]()

  @caps.unsafe.untrackedCaptures
  private val publics = new ConcurrentHashMap[ChannelId, java.lang.Boolean]()

  @caps.unsafe.untrackedCaptures
  private val started = new AtomicBoolean(false)

  /** Enrolls grit's name in Slack (its bot user's) as the assistant's in this workspace
    * ([[Origin.assistant]]), so turns from now on are told it; why not, when Slack or the
    * store could not be asked, or the bot has no name.
    */
  def introduce(): Either[String, Unit] =
    for {
      named <- slack.name(self.bot).left.map(_.toString)
      name <- named.toRight(s"grit's bot ${UserId.value(self.bot)} has no name in Slack")
      _ <- stores.jot
        .write(stores.principals.enrollAssistant(assistant, name))
        .left
        .map(_.toString)
    } yield ()

  private val assistant: PrincipalId = Origin.slackAssistant(TeamId.value(self.team))

  /** One Events API payload. A person's message in a public channel is recorded as a turn of
    * its thread's conversation when it mentions grit, or when it is in a thread grit started
    * (one whose root message mentioned grit); everything else is ignored. Recorded, it is in
    * the person's words ([[Incoming]]), written by them as enrolled under their Slack name
    * (`slack:{team}/{user}`), its turn started, its reply awaited, and the message marked
    * `:eyes:` until the reply is posted. `true` once that is done or needs no doing (a
    * redelivery included), so the payload may be acknowledged; `false` when Slack or the
    * database could not be asked, so Slack sends it again.
    */
  def receive(payload: String): Boolean =
    Events.read(payload, self.bot) match {
      case Left(why) =>
        said(s"slack: an event grit cannot read, acknowledged: $why")
        true
      case Right(Event.Ignored(_)) => true
      case Right(m: Event.Said) =>
        val origin =
          Origin.Slack(TeamId.value(m.team), ChannelId.value(m.channel), Ts.value(m.thread))
        val result = for {
          wanted <-
            if (m.mentions) Right(true)
            else if (m.thread == m.ts) Right(false)
            else
              stores.inbox
                .ingested(origin, SourceId(Ts.value(m.thread)))
                .map(_.nonEmpty)
                .left
                .map(_.toString)
          done <- if (!wanted) Right(()) else record(m, origin)
        } yield done
        result match {
          case Right(()) => true
          case Left(why) =>
            said(
              s"slack: message ${Ts.value(m.ts)} not recorded, left for Slack to send again: $why"
            )
            false
        }
    }

  private def record(m: Event.Said, origin: Origin): Either[String, Unit] =
    public(m.channel).flatMap { open =>
      if (!open) Right(())
      else {
        val mentioned = Mentioned.findAllMatchIn(m.text).map(x => UserId(x.group(1))).toVector
        val known = (m.user +: mentioned).distinct.flatMap(u => nameOf(u).map(u -> _)).toMap
        val author = PrincipalId(s"slack:${TeamId.value(m.team)}/${UserId.value(m.user)}")
        val message: Message.User = Message.User(Incoming.text(m.text, self.bot, known.get))
        for {
          _ <- stores.jot
            .write(stores.principals.enroll(author, known.getOrElse(m.user, UserId.value(m.user))))
            .left
            .map(_.toString)
          turn <- stores.inbox
            .ingest(origin, SourceId(Ts.value(m.ts)), message, author)
            .left
            .map(_.toString)
          _ <- stores.jot
            .write(stores.deliveries.await(turn, Address(m.channel, m.thread, m.ts).written))
            .left
            .map(_.toString)
          _ <- stores.inbox.startTurn(turn).left.map(_.toString)
        } yield slack
          .react(m.channel, m.ts, Working)
          .left
          .foreach(e => said(s"slack: not marked: $e"))
      }
    }

  /** Whether `channel` is public, asked of Slack once per channel. */
  private def public(channel: ChannelId): Either[String, Boolean] =
    Option(publics.get(channel)) match {
      case Some(known) => Right(known.booleanValue)
      case None =>
        slack.public(channel).left.map(_.toString).map { open =>
          val _ = publics.put(channel, java.lang.Boolean.valueOf(open))
          open
        }
    }

  /** `user`'s Slack name, asked of Slack once per user; `None` when they have none, or Slack
    * could not say (the id stands in, and they are asked again next time).
    */
  private def nameOf(user: UserId): Option[String] =
    Option(names.get(user)).orElse {
      slack.name(user) match {
        case Right(Some(n)) =>
          val _ = names.put(user, n)
          Some(n)
        case Right(None) => None
        case Left(e) =>
          said(s"slack: no name for ${UserId.value(user)}: $e")
          None
      }
    }

  /** One pass over the replies awaited. On the edge's first pass, each is started again (a
    * no-op for one already started), since a start can be lost between ingest and start. Each
    * finished turn has its reply posted to its thread, in as many messages as [[RichText]]
    * makes of it (one line saying grit could not answer, and why, when it ended with none),
    * and `:eyes:` removed from the message it answers. A part left posting by a crash is looked
    * for by its tag first, and posted only when Slack does not have it. A part Slack refuses
    * stays posting, for the next pass. How many turns it delivered; why not, when the store
    * could not be read.
    */
  def deliver(): Either[String, Int] =
    stores.jot.write(stores.deliveries.pending()).left.map(_.toString).map { pending =>
      if (!started.getAndSet(true))
        pending.foreach(p =>
          stores.inbox.startTurn(p.turn).left.foreach(e => said(s"slack: not restarted: $e"))
        )
      pending.count { p =>
        stores.inbox.progress(p.turn) match {
          case Right(Progress.Done(reply, outcome)) => post(p, reply, outcome)
          case Right(Progress.Open) => false
          case Left(e) =>
            said(s"slack: ${WorkflowId.value(p.turn.workflowId)} unreadable: $e")
            false
        }
      }
    }

  /** Posts `p`'s reply, part by part; whether all of it is posted and the turn delivered. */
  private def post(p: Pending, reply: Option[Message.Assistant], outcome: String): Boolean =
    Address.read(p.to) match {
      case None =>
        said(
          s"slack: ${WorkflowId.value(p.turn.workflowId)} awaited at an address grit did not write: ${p.to}"
        )
        false
      case Some(to) =>
        val parts = posts(reply, outcome)
        val all = parts.zipWithIndex.forall { (part, i) =>
          val tag = Tag(WorkflowId.value(p.turn.workflowId), i)
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
                  .write(stores.deliveries.posting(p.turn, i))
                  .left
                  .map(e => SlackError.Unreachable(e.toString))
                ts <- slack.post(to.channel, to.thread, part, tag)
                _ <- stores.jot
                  .write(stores.deliveries.posted(p.turn, i, Ts.value(ts)))
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
          stores.jot.write(stores.deliveries.delivered(p.turn)) match {
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

object SlackEdge {

  /** The reaction a message wears while grit works on it. */
  val Working = "eyes"

  /** `<@U…>` in a message's text. */
  private val Mentioned = "<@([A-Z0-9]+)(?:\\|[^>]*)?>".r

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

  /** Where a reply goes: the channel, the thread to post in, and the message it answers (whose
    * `:eyes:` it removes). Written as `{channel}/{thread}/{answered}` in [[Deliveries]].
    */
  private final case class Address(channel: ChannelId, thread: Ts, answered: Ts) {
    def written: String = s"${ChannelId.value(channel)}/${Ts.value(thread)}/${Ts.value(answered)}"
  }

  private object Address {
    def read(s: String): Option[Address] = s.split('/') match {
      case Array(c, t, a) => Some(Address(ChannelId(c), Ts(t), Ts(a)))
      case _ => None
    }
  }
}
