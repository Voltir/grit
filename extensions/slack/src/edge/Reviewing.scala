package grit.slack.edge

import java.util.concurrent.atomic.AtomicBoolean

import grit.core.clock.Clock
import grit.core.edge.EdgeStores
import grit.core.id.ConversationId
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.Subject
import grit.slack.client.{Self, Slack, SlackError, Tag}
import grit.slack.event.{ChannelId, Event, Events, TeamId, Ts}
import grit.slack.text.RichText

/** The review an edge answers ([[SlackEdge.prompt]]): its prompts posted to the review's place,
  * and the rater's reactions to them kept as verdicts.
  */
private[slack] final class Reviewing(
    slack: Slack,
    self: Self,
    stores: EdgeStores^,
    review: Option[SlackReview],
    clock: Clock^,
    said: String => Unit,
    names: Names^{slack, said}
) {
  import Reviewing.PromptAt

  // Set once, atomically, when the review's place is first found unwritable, so that is said
  // once; a racing pass either sets it or sees it set.
  @caps.unsafe.untrackedCaptures
  private val unwritableSaid = new AtomicBoolean(false)

  /** The reviews this edge prompts for, triage's first, each with the subject its transactions
    * are opened for: triage's public, as its prompts hold no text, and each other for the
    * review's place ([[Subject.Room]]).
    */
  private val promptings: Vector[(Subject, Prompting[?, ?])] =
    review.fold(Vector.empty[(Subject, Prompting[?, ?])])(r =>
      (Subject.Public -> SlackEdge.triage(stores.reviews, self.team)) +:
        r.prompting.map(made => Subject.Room(r.place) -> made(stores.documentReviews))
    )

  /** Keeps `r` as a verdict on the review prompt it is on, added or withdrawn, when it is the
    * review's rater's: by the first of [[promptings]] whose verdicts hold its emoji and whose
    * reviews find a prompt at its message, each asked in a transaction for its own subject;
    * nothing for anyone else's, another emoji, or a message no prompt was posted at. Whether it
    * may be acknowledged.
    */
  def reacted(r: Event.Reacted): Boolean = {
    val emoji = Prompting.named(r.emoji)
    val keeping = promptings.filter((_, p) => p.verdicts.contains(emoji))
    review.filter(v => r.team == self.team && r.user == v.rater && keeping.nonEmpty) match {
      case None => true
      case Some(_) =>
        val at = PromptAt(r.channel, r.ts).written
        SlackAccounts.account(r.team, r.user).left.map(StoreError.Invalid(_)).flatMap { rater =>
          keeping.foldLeft[Either[StoreError, Boolean]](Right(false)) { (done, kept) =>
            done.flatMap(found =>
              if (found) Right(true)
              else stores.jot.write(kept._1)(verdictOn(kept._2, emoji, at, rater, r))
            )
          }
        } match {
          case Right(_) => true
          case Left(e) =>
            said(
              s"slack: a reaction to ${Ts.value(r.ts)} not kept, left for Slack to send again: $e"
            )
            false
        }
    }
  }

  /** Keeps or withdraws, as `r` was added or removed, the verdict `emoji` gives under `p` on the
    * prompt posted at `at`, by `rater`: whether `p` found one there to keep it on.
    */
  private def verdictOn[S <: caps.Pure, V <: caps.Pure](
      p: Prompting[S, V],
      emoji: String,
      at: String,
      rater: Account,
      r: Event.Reacted
  )(using Tx^): Either[StoreError, Boolean] =
    p.verdicts.get(emoji) match {
      case None => Right(false)
      case Some(v) =>
        if (r.added) p.reviews.reacted(at, rater, v, r.at) else p.reviews.unreacted(at, rater, v)
    }

  /** One pass over the review's prompts, as [[SlackEdge.prompt]] says. */
  def prompt(): Either[StoreError, Int] = review match {
    case None => Right(0)
    case Some(r) =>
      if (!unwritableSaid.get())
        stores.jot.write(Subject.Public)(Right(Tx.writable(r.place))) match {
          case Right(None) if unwritableSaid.compareAndSet(false, true) =>
            said(
              s"slack: the review's place, ${r.place.written}, is not labelled for grit to write to, so no prompt is posted there"
            )
          case _ => ()
        }
      promptings.foldLeft[Either[StoreError, Int]](Right(0)) { (done, next) =>
        done.flatMap(n => pass(r, next._1, next._2).map(n + _))
      }
  }

  /** One pass of [[prompt]] over `p`'s prompts, in transactions for `subject`: how many it
    * posted and kept.
    */
  private def pass[S <: caps.Pure, V <: caps.Pure](
      r: SlackReview,
      subject: Subject,
      p: Prompting[S, V]
  ): Either[StoreError, Int] =
    stores.jot
      .write(subject)(p.reviews.unposted(r.place))
      .map(_.count { s =>
        p.message(s) match {
          case Prompting.Of.Elsewhere(why) =>
            why.foreach(said)
            false
          case Prompting.Of.NoMessage => posting(r, subject, p, s, Right(None))
          case Prompting.Of.Message(channel, ts) => prompted(r, subject, p, s, channel, ts)
        }
      })

  /** Posts `s`, of message `ts` in `channel`, to `r`'s place, as [[prompt]] says; whether it
    * was posted and kept.
    */
  private def prompted[S <: caps.Pure, V <: caps.Pure](
      r: SlackReview,
      subject: Subject,
      p: Prompting[S, V],
      s: S,
      channel: ChannelId,
      ts: Ts
  ): Boolean = {
    val shown = names.channelNameOf(channel).fold(ChannelId.value(channel))(n => s"#$n")
    val of = s"slack: the review prompt of ${p.name(s)}"
    slack.permalink(channel, ts) match {
      case Left(SlackError.MessageGone) =>
        stores.jot.write(subject)(p.reviews.dropped(s, clock.now())) match {
          case Right(true) => said(s"$of: its message is gone from Slack, so it is dropped")
          case Right(false) => ()
          case Left(e) => said(s"$of: its message is gone from Slack, but its drop is not kept: $e")
        }
        false
      case linked => posting(r, subject, p, s, linked.map(link => Some((shown, link))))
    }
  }

  /** Posts `s` to `r`'s place, linking its message, shown and linked as `linked` says, as
    * [[prompt]] says; whether it was posted and kept.
    */
  private def posting[S <: caps.Pure, V <: caps.Pure](
      r: SlackReview,
      subject: Subject,
      p: Prompting[S, V],
      s: S,
      linked: Either[SlackError, Option[(String, String)]]
  ): Boolean = {
    val of = s"slack: the review prompt of ${p.name(s)}"
    // Some(whether it was kept as posted), or None once the place no longer receives it.
    val done: Either[String, Option[Boolean]] = for {
      message <- linked.left.map(e => s"its message is not linked: $e")
      post <- RichText.render(p.doc(s, message, threadLink(r.place))) match {
        case Vector(one) => Right(one)
        case more => Left(s"it renders as ${more.size} messages")
      }
      before <-
        if (p.label(s).isEmpty) Right(true)
        else stores.jot.write(subject)(Right(receives(r, p, s))).left.map(e => s"not posted: $e")
      kept <-
        if (!before) Right(None)
        else
          for {
            at <- slack
              .postTopLevel(r.channel, post, Tag.Prompt(p.name(s)))
              .left
              .map(e => s"not posted: $e")
            _ = p.verdicts.keys.foreach(emoji =>
              slack
                .react(r.channel, at, emoji)
                .left
                .foreach(e => said(s"slack: a review prompt left without :$emoji:: $e"))
            )
            when <- Events
              .time(Ts.value(at))
              .toRight(s"posted at ${Ts.value(at)}, which names no time")
            kept <- stores.jot
              .write(subject)(
                if (receives(r, p, s))
                  p.reviews.posted(s, PromptAt(r.channel, at).written, when).map(Some(_))
                else Right(None)
              )
              .left
              .map(e => s"posted at ${Ts.value(at)} but not kept, so it is posted again: $e")
          } yield kept
    } yield kept
    done match {
      case Right(Some(true)) => true
      case Right(Some(false)) =>
        said(s"slack: a review prompt posted at a message another prompt is kept at")
        false
      case Right(None) =>
        val why = "the review's place is no longer written at a label dominating it"
        stores.jot.write(subject)(p.reviews.dropped(s, clock.now())) match {
          case Right(_) => said(s"$of: $why, so it is dropped")
          case Left(e) => said(s"$of: $why, but its drop is not kept: $e")
        }
        false
      case Left(why) =>
        said(s"$of: $why")
        false
    }
  }

  /** Whether `r`'s place is written at a label dominating `s`'s, when `p` labels it; `true` for
    * a subject it does not.
    */
  private def receives[S <: caps.Pure, V <: caps.Pure](r: SlackReview, p: Prompting[S, V], s: S)(
      using Tx^
  ): Boolean =
    p.label(s).forall(l => Tx.writable(r.place).exists(_.dominates(l)))

  /** The permalink of the thread `conversation` is, read for `place` ([[Subject.Room]]), when it
    * is a Slack thread of this workspace and Slack links it; `None` otherwise.
    */
  private def threadLink(place: Place)(conversation: ConversationId): Option[String] =
    stores.jot.write(Subject.Room(place))(stores.origins.of(conversation)) match {
      case Right(Some(Origin.Slack(team, channel, thread))) if team == TeamId.value(self.team) =>
        slack.permalink(ChannelId(channel), Ts(thread)).toOption
      case _ => None
    }
}

private[slack] object Reviewing {

  /** Where a review's prompt was posted: its channel and its ts. Written as `{channel}/{ts}` in
    * [[grit.core.review.Reviews]].
    */
  final case class PromptAt(channel: ChannelId, ts: Ts) {
    def written: String = s"${ChannelId.value(channel)}/${Ts.value(ts)}"
  }
}
