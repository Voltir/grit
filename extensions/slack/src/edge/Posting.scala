package grit.slack.edge

import java.time.Instant
import java.util.concurrent.locks.ReentrantLock

import grit.core.clock.Clock
import grit.core.edge.{Route, ToolRequest}
import grit.core.speech.Rate
import grit.core.tool.{
  Args,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox
}
import grit.edge.{Run, Tools}
import grit.prose.markdown.Markdown
import grit.slack.client.{Slack, SlackError, Tag}
import grit.slack.event.{ChannelId, MessageLink}
import grit.slack.text.RichText

/** `slack_post`, run by the Slack edge wherever a request to it was routed: a post in one of
  * `channels` (each a name Slack gave, and its id), at its top level or in the thread a
  * message link names, through `slack`, at most `rate` across them all, the posts counted by
  * `clock`'s time. Built by [[Posting.of]], which needs at least one channel.
  */
private[slack] final class Posting private (
    slack: Slack,
    clock: Clock,
    rate: Rate,
    first: (String, ChannelId),
    rest: Vector[(String, ChannelId)]
) extends Tools {
  import Posting.*

  private val channels: Vector[(String, ChannelId)] = first +: rest

  /** The tool as the edge advertises it and reads a call of it. */
  val hosted: Hosted[PostArgs] =
    new Hosted(
      ToolSpec(
        Name,
        "Post a message in a Slack channel, as grit: at the channel's top level, or as a " +
          "reply in a thread when `thread` is a link to a message in that channel. It posts " +
          s"only in ${channels.map((n, _) => s"#$n").mkString(", ")}, and at most " +
          s"${rate.count} posts per ${rate.per} across them. Use it only when the person " +
          "asks for something to be posted there; your reply to them is posted where they " +
          "wrote, without it. The text is posted as written: nothing in it becomes a mention. " +
          "A post cut short may already be in Slack.",
        Args.of(
          (
            channel = Field.oneOf("The channel to post in, by name.", first._1, rest.map(_._1)*),
            text = Field.text(
              s"What to post, in markdown: one Slack message, at most ${RichText.MaxChars} " +
                "characters."
            ),
            thread = Field
              .text(
                "A link to a message in that channel, to post as a reply in its thread; " +
                  "left out, the post is at the channel's top level."
              )
              .optional
          )
        ),
        Retry.Interrupt
      ),
      Gate.Free,
      a => s"#${a.channel}"
    )

  /** What the edge advertises: [[hosted]] alone. */
  def offered: ToolSet = ToolSet.of(Vector(hosted.entry)).getOrElse(ToolSet.Empty)

  // The instants of the posts made, the latest last; read and written only under `lock`,
  // held across a post so two calls never both take the last place in the rate.
  @caps.unsafe.untrackedCaptures
  private var made = Vector.empty[Instant]

  // Its only effect is ordering the calls that hold it, around `made` alone.
  @caps.unsafe.untrackedCaptures
  private val lock = new ReentrantLock()

  def run(route: Route, request: ToolRequest): Outcome =
    Toolbox.of(hosted.over(a => post(request.slot.key, a))) match {
      case Right(box) => Run.request(request, box)
      // One tool cannot repeat its own name.
      case Left(_) => Outcome.Failed(s"${ToolName.value(Name)} is offered twice; nothing ran.")
    }

  /** `a` posted for the request keyed `request`, as the tool's text says. */
  private def post(request: String, a: PostArgs): Outcome =
    channels.find(_._1 == a.channel) match {
      // The arguments' enum holds only these names.
      case None => Outcome.Failed(s"grit does not post in #${a.channel}. $Unposted")
      case Some((name, id)) =>
        RichText.render(Markdown.parse(a.text)) match {
          case Vector() => Outcome.Failed(s"The text is empty. $Unposted")
          case Vector(one) =>
            a.thread.map(link => (link, MessageLink.read(link))) match {
              case Some((link, None)) =>
                Outcome.Failed(s"`$link` is not a link to a Slack message. $Unposted")
              case Some((_, Some(l))) if l.channel != id =>
                Outcome.Failed(s"That link is to a message outside #$name. $Unposted")
              case thread =>
                val in = thread.flatMap(_._2).map(_.thread)
                within { () =>
                  in.fold(slack.postTopLevel(id, one, Tag.Sent(request)))(t =>
                    slack.post(id, t, one, Tag.Sent(request))
                  )
                } match {
                  case Left(refusal) => Outcome.Failed(refusal)
                  case Right(Right(_)) =>
                    Outcome.Done(
                      if (in.isEmpty) s"Posted in #$name." else s"Posted in the thread in #$name."
                    )
                  case Right(Left(e)) => Outcome.Failed(failed(e, name))
                }
            }
          case _ =>
            Outcome.Failed(
              s"The text is too long for one Slack message (at most ${RichText.MaxChars} " +
                s"characters and ${RichText.MaxBlocks} blocks). $Unposted"
            )
        }
    }

  /** `body` run when the rate allows a post now, the post counted unless Slack surely did not
    * take it; why not, when it does not allow one.
    */
  private def within[A](
      body: () => Either[SlackError, A]
  ): Either[String, Either[SlackError, A]] = {
    lock.lock()
    try {
      val now = clock.now()
      val recent = made.filter(_.isAfter(now.minusNanos(rate.per.toNanos)))
      if (recent.size >= rate.count)
        Left(
          s"grit has made ${rate.count} posts in the last ${rate.per}, as many as it may. $Unposted"
        )
      else {
        val result = body()
        val taken = result match {
          case Right(_) | Left(SlackError.Unreachable(_)) => true
          case Left(_) => false
        }
        made = if (taken) recent :+ now else recent
        Right(result)
      }
    } finally lock.unlock()
  }
}

private[slack] object Posting {

  /** What a call of `slack_post` reads into. */
  type PostArgs = (channel: String, text: String, thread: Option[String])

  val Name: ToolName = ToolName("slack_post")

  /** Said after every refusal. */
  private val Unposted = "Nothing was posted."

  /** `slack_post` over `named`, each channel's name and id; `None` when `named` is empty. */
  def of(
      slack: Slack,
      clock: Clock,
      rate: Rate,
      named: Vector[(String, ChannelId)]
  ): Option[Posting^{slack, clock}] =
    named match {
      case first +: rest => Some(new Posting(slack, clock, rate, first, rest))
      case _ => None
    }

  /** Why Slack's `e` left a post to #`name` unmade, or possibly made. */
  private def failed(e: SlackError, name: String): String = e match {
    case SlackError.Refused("not_in_channel") =>
      s"grit's bot is not in #$name, so it cannot post there. $Unposted"
    case SlackError.Refused(code) => s"Slack refused the post ($code). $Unposted"
    case SlackError.Limited(after) =>
      s"Slack is limiting grit's posts; it may post again after ${after.toSeconds} s. $Unposted"
    case SlackError.Unreachable(cause) =>
      s"Slack could not be reached ($cause): the post may or may not have been made."
  }
}
