package grit.slack.edge

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.immutable.VectorMap

import grit.core.clock.Clock
import grit.core.edge.{
  Asked,
  Attesting,
  CatchUp,
  EdgeRefusal,
  EdgeStores,
  Membership,
  RealmSource,
  ServedEdge
}
import grit.core.id.{CallSlot, EntryId, Href, SourceId}
import grit.core.identity.{Account, Realm, Standing}
import grit.core.inbox.{InboundId, InboxError}
import grit.core.message.Message
import grit.core.place.Service
import grit.core.review.{Prompt, Reviews, Verdict}
import grit.core.speech.{Rate, Reach}
import grit.core.spend.Budget
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.Subject
import grit.prose.form.{Block, Doc, Mark, Span, Text}
import grit.slack.client.{AppToken, BotToken, Root, Self, Slack, SocketSlack, Tag}
import grit.slack.event.{ChannelId, Event, Events, MessageLink, TeamId, Ts, UserId}
import grit.slack.text.{Incoming, RichText}

/** The Slack edge (ADR 0002, 0019): Slack's messages in as turns, grit's replies out, through
  * the stores alone; grit being `self` in the workspace, answering `review` when one is
  * given, and noting under each reply it delivers what `note` says ([[ReplyNote]]); `clock`
  * dates the marks it puts up and takes down. `said` is told what it did that a person running
  * it may want to read.
  */
final class SlackEdge(
    slack: Slack,
    self: Self,
    stores: EdgeStores^,
    review: Option[SlackReview],
    note: ReplyNote,
    clock: Clock^,
    said: String => Unit
) {
  import SlackEdge.*
  import Names.Named

  // Set as a join is recorded pending, and taken by joinsWaiting, each atomically. A set racing
  // a take is either seen now or by the next take; the joins it stands for are read from the
  // store, never from it, so a lost set leaves a join pending until the next join or open.
  @caps.unsafe.untrackedCaptures
  private val waiting = new AtomicBoolean(true)

  /** The channels grit's bot is a member of: what it hears. */
  private val members: Members^{stores} = new Members(stores.joins, self.team)

  /** The names of the users and channels it shows. */
  private val names = new Names(slack, self.team, said)

  /** The name grit's bot user goes by in Slack, for the log; why not, when Slack could not be
    * asked or the bot has no name there.
    */
  def displayName(): Either[String, String] =
    for {
      named <- slack.member(self.team, self.bot).left.map(_.toString)
      name <- named.name.toRight(s"grit's bot ${UserId.value(self.bot)} has no name in Slack")
    } yield name

  /** Records which channels grit's bot is a member of, as Slack lists them now
    * ([[Slack.channels]]): each listed a member (the review's channel left out: it is never
    * heard), each recorded member Slack no longer lists left, and rooms left at least
    * [[grit.core.edge.Joins.KeptLeft]] ago forgotten ([[Members.reconcile]]); and says which,
    * as a person reads them. Why not, when Slack or the database could not be asked.
    */
  def reconcile(): Either[String, Unit] =
    slack.channels().left.map(e => s"Slack would not list grit's channels: $e").flatMap { listed =>
      val (reviewed, heard) = listed.partition((c, _) => review.exists(_.channel == c))
      reviewed
        .foreach((c, _) => said(s"slack: the review's channel, ${names.shown(c)}, is never heard"))
      val access = heard.flatMap((c, k) => Members.access(k).map(c -> _))
      members.reconcile(access, clock.now()).left.map(_.toString).map { forgotten =>
        said(members.all match {
          case Vector() => "slack: a member of no channel"
          case all => s"slack: a member of ${all.map(names.shown).mkString(", ")}"
        })
        if (forgotten > 0) said(s"slack: forgot $forgotten rooms left over a day ago")
      }
    }

  /** Every channel grit's bot is a member of, by id, as last recorded. */
  def channels: Vector[ChannelId] = members.all

  // Set when a room is made quiet or not through a command, a turn this edge showed an ask's
  // card for is delivered, or a re-offer failed, and taken by
  // reoffer, each atomically. A set racing a take is seen by this take or the next; which rooms
  // are quiet is read from the store, never from it.
  @caps.unsafe.untrackedCaptures
  private val requiet = new AtomicBoolean(false)

  /** When the channels `slack_post` may post in may have changed since this last ran (the
    * first time; a join or leave recorded since; a room made quiet, or not, through
    * [[command]]; a turn delivered whose ask this edge showed as a card, which may have changed
    * it through `quiet_room`), runs `offer` over them: each channel grit's bot is a member of that is not
    * quiet ([[Tx.quiet]]) and that Slack gives a name, by name and id, in the order of their
    * ids; one Slack gives no name is left out. A room made quiet elsewhere is seen at the next
    * such change. When the database cannot be read, or `offer` fails, that is said, and it
    * runs at the next call.
    */
  def reoffer[C^](offer: Vector[(String, ChannelId)] ->{C} Either[String, Unit]): Unit =
    // Both taken, whichever is set.
    if (members.changed() | requiet.getAndSet(false)) {
      val all = members.all
      val offered = for {
        loud <- stores.jot
          .write(Subject.Public)(Right(all.filterNot(c => Tx.quiet(members.room(c)))))
          .left
          .map(e => s"which rooms are quiet is unread: $e")
        _ <- offer(loud.flatMap(c => names.channelNameOf(c).map(_ -> c).toVector))
      } yield ()
      offered.left.foreach { why =>
        requiet.set(true)
        said(s"slack: slack_post not offered again, tried at the next delivery: $why")
      }
    }

  /** One Events API payload. A person's message in a channel grit's bot is a member of (never
    * the review's channel) is addressed to grit when it mentions grit, or is in a thread whose
    * root did. Addressed, it is recorded as a turn of its thread's conversation, in the person's
    * words ([[Incoming]]), written through their account in their own team
    * ([[SlackAccounts.account]], [[Event.Said.author]]), named as Slack names them; its turn is
    * started, its reply awaited, and the message marked `:eyes:` until the reply is posted. A new
    * message the inbox refuses over the day's cap is not recorded, and is answered once, in its
    * thread, with [[Budget.Refusal]]. Not addressed, it is heard ([[grit.core.inbox.Inbox.hear]])
    * in the same words under the same name, unmarked, its thread kept as where a reply would go
    * and whom it names besides grit ([[grit.core.speech.Reach]]): grit replies there only when
    * it drafts a reply and the draft is posted (ADR 0022). A direct message to grit
    * ([[Event.Told]]) is recorded as an addressed one is, in its author's direct room
    * ([[grit.core.store.Origin.Direct]]); one the inbox refuses as sealed is answered once in its
    * thread with [[InboxError.SealedReply]], and one over the cap as a channel's is. A group
    * direct message is ignored. A message from a channel not recorded a member of, as when it
    * races its channel's join, asks Slack once whether grit's bot is in it ([[Slack.kind]]): if
    * so, the join is recorded first, as of the message, and the message is taken; if not, it is
    * ignored. The bot joining a channel ([[Event.Joined]]) is recorded with the channel's access
    * as Slack reports it, and its inviter's account, checked first ([[Attesting.before]]), so a
    * join whose inviter is no full member skips its backfill, which is said; leaving or being
    * removed ([[Event.Left]]) is recorded, and nothing said there after is heard. Each is ordered
    * by when it happened ([[grit.core.edge.Joins]]); the review's channel is never recorded a
    * member, and a join Slack says the bot is no longer in is not recorded. A message recorded
    * or heard that begins its conversation as a reply in a thread whose root the bot posted with
    * `slack_post` ([[Tag.Sent]]) first records that post, as Slack gives its text, as the
    * conversation's opening made by the call its tag names ([[grit.core.inbox.Inbox.posted]]);
    * when Slack cannot be asked for the root, the message is recorded without it, which is said.
    * A reaction the review's rater adds to a prompt grit posted keeps the verdict it gives
    * (its prompting's [[Prompting.verdicts]], a skin tone ignored) as the prompt's, replacing the
    * one standing; removing the reaction that stands withdraws it. Everything else is ignored,
    * any other reaction included.
    * Before a message is recorded or heard, its author's account is checked
    * ([[Attesting.before]]): Slack is asked who they are ([[Slack.member]]) unless it answered
    * for them within [[Attesting.Fresh]], so the turn the message starts opens under that
    * answer; one Slack cannot answer for and never has is not recorded. A change Slack reports
    * to a user ([[Event.UserChanged]]) asks Slack again ([[Attesting.changed]]). `true` once
    * that is done or needs no doing (a redelivery included), so the payload may be acknowledged;
    * `false` when Slack or the database could not be asked, so Slack sends it again (a message
    * racing its channel's join included, so the join wins on a later try).
    */
  def receive(payload: String): Boolean =
    Events.read(payload, self.bot) match {
      case Left(why) =>
        said(s"slack: an event grit cannot read, acknowledged: $why")
        true
      case Right(Event.Ignored(_)) => true
      case Right(j: Event.Joined) => joined(j)
      case Right(l: Event.Left) => left(l)
      case Right(r: Event.Reacted) => reviewing.reacted(r)
      case Right(c: Event.UserChanged) => changed(c)
      case Right(t: Event.Told) =>
        told(t) match {
          case Right(()) => true
          case Left(why) =>
            said(
              s"slack: direct message ${Ts.value(t.ts)} not recorded, left for Slack to send again: $why"
            )
            false
        }
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
          done <-
            if (wanted) record(m, origin)
            else hear(m, origin, live = true)
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

  /** One slash command's payload ([[Events.command]]): when it is `registered`, run and
    * answered to its asker alone at its response url ([[Slack.respond]]); any other command is
    * ignored, and said. Its words are read by [[Command.read]], a mention (`<@U…>`) naming
    * that user's account in the command's team, a link as Slack wraps one read as the link,
    * and a place named as `registered`'s places name it ([[SlackCommand.places]]). Its asker's account is checked first
    * ([[Attesting.before]]), then the command is run as theirs ([[Administration.run]]), now,
    * in its room: its channel's (`slack:{team}/{channel}`), whether grit's bot is a member
    * there or not, or, asked in any direct message, the asker's own direct room. Words read
    * wrong are answered with why and the commands; an asker Slack cannot answer for and never
    * has, or a database that failed, with a line saying nothing was run. An answer Slack will
    * not take, and a payload grit cannot read, are said.
    */
  def command(registered: SlackCommand)(payload: String): Unit =
    commanding.command(registered)(payload)

  /** What Slack says of the accounts of grit's own team, `slack:{self.team}/…`, and nothing of
    * any other's: each user as [[Slack.member]] reads them, an account of another team outside
    * without asking, and the team's listing as [[Slack.members]] reads it, any other realm's
    * empty. A failure to ask is [[Asked.Unreached]]. The name an answer gives is kept as the
    * user's name and zone ([[Names.namedOf]]), so a new author costs one `users.info`.
    */
  private val source: RealmSource^{slack, names} = new RealmSource {
    def ask(account: Account): Asked =
      SlackAccounts.user(account, self.team).fold(Asked.Said(Standing.Outside)) { user =>
        slack.member(self.team, user) match {
          case Right(m) =>
            m.name.foreach(n => names.remember(user, Named(n, m.zone)))
            Asked.Said(m.standing)
          case Left(e) => Asked.Unreached(e.toString)
        }
      }

    def all(realm: Realm): Either[Asked.Unreached, Map[Account, Standing]] =
      if (!SlackAccounts.realm(self.team).toOption.exists(ours => ours == realm)) Right(Map.empty)
      else
        slack
          .members(self.team)
          .left
          .map(e => Asked.Unreached(e.toString): Asked.Unreached)
          .map(
            _.flatMap((user, m) =>
              SlackAccounts.account(self.team, user).toOption.map(_ -> m.standing)
            )
          )
  }

  private val reviewing = new Reviewing(slack, self, stores, review, clock, said, names)

  private val delivering =
    new Delivering(slack, self, stores, note, clock, said, names, () => requiet.set(true))

  private val commanding =
    new Commanding(slack, stores, clock, said, source, () => requiet.set(true))

  /** Asks Slack again about the user `c` names, however recently it was asked
    * ([[Attesting.changed]]); whether the event may be acknowledged: not when the store failed.
    */
  private def changed(c: Event.UserChanged): Boolean =
    SlackAccounts.account(c.team, c.user) match {
      case Left(why) =>
        said(s"slack: a change to a user grit cannot name, acknowledged: $why")
        true
      case Right(account) =>
        stores.attesting.changed(source, account) match {
          case Right(()) => true
          case Left(e) =>
            said(
              s"slack: a change to ${Account.written(account)} not recorded, left for Slack to send again: $e"
            )
            false
        }
    }

  /** One look at the accounts of grit's team that are due to be asked about again
    * ([[Attesting.round]]), listing the team once when any is; how many it recorded.
    */
  def attest(): Either[StoreError, Int] = stores.attesting.round(source)

  /** What [[backfill]] would hear of `channel`: each message said there from `since` on,
    * read as [[receive]] reads a live one ([[Events.listed]]), that the inbox has not
    * recorded, oldest first; none unless grit's bot is a member of `channel`. A listing grit cannot read
    * is left out, and said. Why not, when Slack or the inbox could not be asked.
    */
  def unheard(channel: ChannelId, since: Instant): Either[String, Vector[Event.Said]] =
    served(channel, clock.now()).flatMap { open =>
      if (!open) Right(Vector.empty)
      else listed(channel, since).flatMap(unrecorded(channel, _))
    }

  /** Each message said in `channel` from `since` on, read as [[receive]] reads a live one
    * ([[Events.listed]]), oldest first; a listing grit cannot read is left out, and said.
    */
  private def listed(channel: ChannelId, since: Instant): Either[String, Vector[Event.Said]] =
    slack.history(channel, since).left.map(_.toString).map { listed =>
      listed.flatMap(l =>
        Events.listed(l, self.team, channel, self.bot) match {
          case Right(m: Event.Said) => Some(m)
          case Right(
                Event.Ignored(_) | Event.Reacted(_, _, _, _, _, _, _) | Event.UserChanged(_, _) |
                Event.Told(_, _, _, _, _, _, _, _) | Event.Joined(_, _, _, _) | Event.Left(_, _, _)
              ) =>
            None
          case Left(why) =>
            said(s"slack: a listed message grit cannot read, left out: $why")
            None
        }
      )
    }

  /** Those of `spoken`, said in `channel`, the inbox has not recorded, in their order. */
  private def unrecorded(
      channel: ChannelId,
      spoken: Vector[Event.Said]
  ): Either[String, Vector[Event.Said]] =
    spoken
      .groupBy(_.thread)
      .toVector
      .foldLeft[Either[String, Set[Ts]]](Right(Set.empty)) { case (acc, (thread, ms)) =>
        acc.flatMap(known =>
          stores.inbox
            .recorded(originOf(channel, thread), ms.map(m => SourceId(Ts.value(m.ts))).toSet)
            .left
            .map(_.toString)
            .map(r => known ++ r.map(s => Ts(SourceId.value(s))))
        )
      }
      .map(known => spoken.filterNot(m => known.contains(m.ts)))

  /** Whether a join has been recorded with its backfill pending since this was last asked
    * ([[backfillJoins]] has work); `true` when first asked, so an open resumes what a crash or
    * a close left pending.
    */
  private[slack] def joinsWaiting(): Boolean = waiting.getAndSet(false)

  /** Hears what was said before grit's bot joined each channel whose join's backfill is
    * pending ([[Membership.Member]]), one join at a time, within `bounds`: of what was said in
    * its `days` before the join, the newest `messages` at most, less what the inbox has
    * recorded, each heard as [[backfill]] hears one, at its own time and never answered; then
    * the join is marked done ([[grit.core.edge.Joins.backfilled]]) and how many were heard is
    * said. A join is instead marked done unheard, and why said, while the day's spend is over
    * the inbox's cap ([[grit.core.inbox.Inbox.overCap]]), or when `bounds.joinsPerDay` joins
    * had their backfill done or skipped in the day before it. `stopping` is asked before each
    * message: once it says so, nothing more is heard and the join stays pending, to be heard
    * from where it stopped. How many joins it marked done; why not, naming the channel, when
    * Slack, the inbox or the database could not be asked or an author was never attested and
    * Slack could not be asked about them, the join then left pending.
    */
  def backfillJoins(bounds: Backfill, stopping: () => Boolean): Either[String, Int] =
    members.pending().left.map(_.toString).flatMap {
      _.foldLeft[Either[String, Int]](Right(0)) { case (done, (channel, join)) =>
        done.flatMap(n =>
          caughtUp(channel, join, bounds, stopping).left
            .map(why => s"${names.shown(channel)} not backfilled: $why")
            .map(finished => if (finished) n + 1 else n)
        )
      }
    }

  /** One join's backfill, as [[backfillJoins]] says; whether it was marked done. */
  private def caughtUp(
      channel: ChannelId,
      join: Instant,
      bounds: Backfill,
      stopping: () => Boolean
  ): Either[String, Boolean] = {
    def skipped(why: String): Either[String, Boolean] =
      members.backfilled(channel, join).left.map(_.toString).map { _ =>
        said(
          s"slack: what was said in ${names.shown(channel)} before grit's bot joined is not heard: $why"
        )
        true
      }
    stores.inbox.overCap().left.map(_.toString).flatMap {
      case Some(over) =>
        skipped(
          s"the day's spend, ${over.spent.cost.written}, has reached its cap, $$${over.cap.usd}"
        )
      case None =>
        members
          .backfilledSince(join.minus(java.time.Duration.ofDays(1)))
          .left
          .map(_.toString)
          .flatMap { n =>
            if (n >= bounds.joinsPerDay)
              skipped(s"$n joins were backfilled in the day before it, the most a day allows")
            else
              for {
                all <- listed(channel, join.minus(java.time.Duration.ofDays(bounds.days.toLong)))
                before = all.filterNot(_.at.isAfter(join)).takeRight(bounds.messages)
                fresh <- unrecorded(channel, before)
                finished <- heard(fresh, stopping)
                _ <-
                  if (!finished) Right(())
                  else
                    members.backfilled(channel, join).left.map(_.toString).map { _ =>
                      said(
                        s"slack: heard ${fresh.size} messages said in ${names.shown(channel)} before grit's bot joined"
                      )
                    }
              } yield finished
          }
    }
  }

  /** The conversation of the thread rooted at `thread` in `channel`. */
  private def originOf(channel: ChannelId, thread: Ts): Origin =
    Origin.Slack(TeamId.value(self.team), ChannelId.value(channel), Ts.value(thread))

  /** Hears each of `messages`, in order, in its thread's conversation, dated when it was said
    * ([[grit.core.inbox.Inbox.hear]]), in the person's words under their Slack name, a mention of grit
    * included: a past message is heard, never answered; a thread under a post of grit's
    * begins with that post, as in [[receive]], and each author is checked first, as there.
    * Nothing but in a channel grit's bot is a member of. Why not, naming the
    * first message not heard, when Slack or the inbox could not be asked, or its author was never
    * attested and Slack could not be asked about them; those before it stay heard.
    */
  def backfill(messages: Vector[Event.Said]): Either[String, Unit] =
    heard(messages, () => false).map(_ => ())

  /** Hears each of `messages`, in order, as [[backfill]] says, asking `stopping` before each;
    * whether all were heard: `false` once it said to stop.
    */
  private def heard(
      messages: Vector[Event.Said],
      stopping: () => Boolean
  ): Either[String, Boolean] =
    messages.foldLeft[Either[String, Boolean]](Right(true)) { (going, m) =>
      going.flatMap { on =>
        if (!on || stopping()) Right(false)
        else {
          val origin =
            Origin.Slack(TeamId.value(m.team), ChannelId.value(m.channel), Ts.value(m.thread))
          hear(m, origin, live = false).left
            .map(why => s"message ${Ts.value(m.ts)} not heard: $why")
            .map(_ => true)
        }
      }
    }

  /** Records `m`, heard, in its thread's conversation, with whom it names besides grit, its
    * link ([[linkTo]]) and, when it is heard `live`, its thread as where a reply to it would
    * go; nothing in a conversation grit does not serve.
    */
  private def hear(m: Event.Said, origin: Origin, live: Boolean): Either[String, Unit] =
    spoken(m).flatMap {
      case None => Right(())
      case Some((author, text)) =>
        val to = Option.when(live)(Address(m.channel, m.thread, m.ts).written)
        for {
          asked <- Mentioned
            .findAllMatchIn(m.text)
            .map(x => UserId(x.group(1)))
            .filterNot(_ == self.bot)
            .foldLeft[Either[String, Set[Account]]](Right(Set.empty))((read, u) =>
              read.flatMap(as => SlackAccounts.account(m.team, u).map(as + _))
            )
          _ <- opening(m, origin, author)
          _ <- stores.inbox
            .hear(
              origin,
              SourceId(Ts.value(m.ts)),
              text,
              author,
              m.at,
              Reach(to, asked),
              linkTo(m.channel, m.thread, m.ts)
            )
            .left
            .map(_.toString)
        } yield ()
    }

  /** Who wrote `m`, their account named as Slack names them, and its text in their words; `None`
    * in a conversation grit does not serve.
    */
  private def spoken(m: Event.Said): Either[String, Option[(Account, String)]] =
    served(m.channel, m.at).flatMap { open =>
      if (!open) Right(None) else voiced(m.author, m.user, m.text).map(Some(_))
    }

  /** The account of `user` of `team`, checked first ([[Attesting.before]]), named as Slack
    * names them and placed in the zone Slack places them in, when it names one; and `text`,
    * which they wrote, in their words.
    */
  private def voiced(
      team: TeamId,
      user: UserId,
      text: String
  ): Either[String, (Account.Sourced, String)] =
    for {
      author <- SlackAccounts.account(team, user)
      _ <- stores.attesting.before(source, author).left.map {
        case Attesting.Unchecked.Unattested(_, why) =>
          s"${Account.written(author)} was never attested and Slack could not be asked: $why"
        case Attesting.Unchecked.Store(e) => e.toString
      }
      mentioned = Mentioned.findAllMatchIn(text).map(x => UserId(x.group(1))).toVector
      known = (user +: mentioned).distinct.flatMap(u => names.namedOf(u).map(u -> _)).toMap
      linked = Incoming
        .channels(text)
        .collect { case (c, None) => c }
        .distinct
        .flatMap(c => names.channelNameOf(c).map(c -> _))
        .toMap
      theirs = known.get(user)
      _ <- stores.jot
        .write(Subject.Public)(
          stores.principals
            .name(author, theirs.fold(UserId.value(user))(_.name))
            .flatMap(_ =>
              theirs.flatMap(_.zone).fold(Right(()))(z => stores.principals.zoned(author, z))
            )
        )
        .left
        .map(_.toString)
    } yield (author, Incoming.text(text, self.bot, known.get(_).map(_.name), linked.get))

  /** Records `m` as a turn of its thread's conversation in its author's direct room, as
    * [[receive]] says.
    */
  private def told(m: Event.Told): Either[String, Unit] =
    voiced(m.author, m.user, m.text).flatMap { (author, text) =>
      ingest(
        Origin.Direct(author, Ts.value(m.thread)),
        Address(m.channel, m.thread, m.ts),
        Message.User(text),
        author
      )
    }

  private def record(m: Event.Said, origin: Origin): Either[String, Unit] =
    spoken(m).flatMap {
      case None => Right(())
      case Some((author, text)) =>
        val message: Message.User = Message.User(text)
        opening(m, origin, author).flatMap(_ =>
          ingest(origin, Address(m.channel, m.thread, m.ts), message, author)
        )
    }

  /** Records `message`, the one at `at`, written through `author`, with its link
    * ([[linkTo]]), as a turn of `origin`'s conversation, its reply awaited at `at`, its turn
    * started and the message marked; a refusal over the cap, or of a sealed thread, answered
    * once in its thread instead.
    */
  private def ingest(
      origin: Origin,
      at: Address,
      message: Message.User,
      author: Account
  ): Either[String, Unit] =
    stores.inbox
      .ingest(
        origin,
        SourceId(Ts.value(at.answered)),
        message,
        author,
        linkTo(at.channel, at.thread, at.answered)
      ) match {
      case Left(over @ InboxError.OverCap(_, _, _)) =>
        said(
          s"slack: message ${Ts.value(at.answered)} refused: ${over.spent.cost.written} spent on ${over.day.date}, the cap is $$${over.cap.usd}"
        )
        refuse(at, Budget.Refusal)
      case Left(InboxError.Sealed(_)) =>
        said(
          s"slack: message ${Ts.value(at.answered)} refused: its thread began when its author was cleared for more"
        )
        refuse(at, InboxError.SealedReply)
      case Left(other) => Left(other.toString)
      case Right(turn) =>
        for {
          _ <- stores.jot
            .write(Subject.Turn(turn))(stores.deliveries.await(turn, at.written))
            .left
            .map(_.toString)
          _ <- stores.inbox.startTurn(turn).left.map(_.toString)
        } yield slack
          .react(at.channel, at.answered, Working)
          .left
          .foreach(e => said(s"slack: not marked: $e"))
    }

  /** Records grit's post that `m`'s thread begins with as its conversation's opening
    * ([[grit.core.inbox.Inbox.posted]]), written by `by`, when `m` is a reply that begins its
    * conversation and grit's bot posted the thread's root with `slack_post` ([[Tag.Sent]]):
    * its text as Slack gives it, made by the call its tag names. Nothing for any other
    * message. When Slack cannot be asked for the root, or the post is not recorded, that is
    * said and `m` goes on without it; why not, when the inbox could not be asked.
    */
  private def opening(m: Event.Said, origin: Origin, by: Account): Either[String, Unit] =
    if (m.thread == m.ts) Right(())
    else
      stores.inbox.begun(origin).left.map(_.toString).flatMap { begun =>
        if (begun) Right(())
        else
          slack.root(m.channel, m.thread) match {
            case Left(e) =>
              said(
                s"slack: the root of thread ${Ts.value(m.thread)} not read, so a post there is not recorded: $e"
              )
              Right(())
            case Right(Some(Root(Some(user), Some(Tag.Sent(key)), text))) if user == self.bot =>
              (CallSlot.read(key), Events.time(Ts.value(m.thread))) match {
                case (Some(request), Some(at)) =>
                  val shown = Incoming.text(text, self.bot, _ => None, _ => None)
                  stores.inbox
                    .posted(origin, SourceId(Ts.value(m.thread)), shown, at, request, by)
                    .left
                    .map(_.toString)
                    .map { recorded =>
                      if (!recorded)
                        said(
                          s"slack: grit's post ${Ts.value(m.thread)} not recorded: its thread has begun"
                        )
                    }
                case _ =>
                  said(
                    s"slack: grit's post ${Ts.value(m.thread)} names no call grit can read: $key"
                  )
                  Right(())
              }
            case Right(_) => Right(())
          }
      }

  /** Tells the person who wrote the message at `at` that it was not taken, in `line`, in its
    * thread, once: a refusal already there under its tag is not posted again.
    */
  private def refuse(at: Address, line: String): Either[String, Unit] =
    delivering.once(
      at,
      Tag.Refused(at.answered),
      RichText.render(Doc(Vector(Block.Paragraph(Text.plain(line)))))
    )

  /** Whether grit serves `channel`: addresses, hears and backfills what is said there. A
    * channel grit's bot is recorded a member of, yes, but never the review's; any other, only
    * when Slack says the bot is in it, its join then recorded as of `at` first, with no
    * inviter named; nothing else. Why not, when Slack or the database could not be asked.
    */
  private def served(channel: ChannelId, at: Instant): Either[String, Boolean] =
    if (review.exists(_.channel == channel)) Right(false)
    else if (members.contains(channel)) Right(true)
    else
      slack.kind(channel).left.map(_.toString).flatMap { kind =>
        Members.access(kind) match {
          case None => Right(false)
          case Some(access) =>
            members.joined(channel, access, None, at).left.map(_.toString).map {
              case Membership.Member(_, backfill) =>
                if (backfill.nonEmpty) waiting.set(true)
                said(s"slack: grit's bot is in ${names.shown(channel)}, found by a message from it")
                true
              case Membership.Gone => false
            }
        }
      }

  /** Records grit's bot joining `j`'s channel, as [[receive]] says; whether it may be
    * acknowledged.
    */
  private def joined(j: Event.Joined): Boolean =
    if (review.exists(_.channel == j.channel)) {
      said(s"slack: grit's bot joined the review's channel, ${names.shown(j.channel)}: never heard")
      true
    } else
      slack.kind(j.channel) match {
        case Left(e) =>
          said(
            s"slack: a join of ${ChannelId.value(j.channel)} not recorded, left for Slack to send again: $e"
          )
          false
        case Right(kind) =>
          Members.access(kind) match {
            case None =>
              said(
                s"slack: a join of ${ChannelId.value(j.channel)} not recorded: Slack says grit's bot is not in it"
              )
              true
            case Some(access) =>
              val inviter = j.inviter.flatMap { user =>
                SlackAccounts.account(j.team, user).toOption.map { account =>
                  // An inviter Slack cannot answer for is no full member: the join skips its
                  // backfill, the safe side.
                  stores.attesting
                    .before(source, account)
                    .left
                    .foreach(e => said(s"slack: inviter ${Account.written(account)} unchecked: $e"))
                  account
                }
              }
              members.joined(j.channel, access, inviter, j.at) match {
                case Left(e) =>
                  said(
                    s"slack: a join of ${ChannelId.value(j.channel)} not recorded, left for Slack to send again: $e"
                  )
                  false
                case Right(Membership.Member(at, backfill)) =>
                  if (backfill.nonEmpty) waiting.set(true)
                  if (at == j.at)
                    said(
                      s"slack: grit's bot joined ${names.shown(j.channel)}" + (backfill match {
                        case None =>
                          s"; what was said before is not heard: ${inviter.fold("")(Account.written)} " +
                            "invited it, whom no trusted realm vouches a full member"
                        case Some(_) => ""
                      })
                    )
                  true
                case Right(Membership.Gone) =>
                  said(
                    s"slack: a join of ${names.shown(j.channel)} made before its recorded leave, ignored"
                  )
                  true
              }
          }
      }

  /** Records grit's bot leaving `l`'s channel, as [[receive]] says; whether it may be
    * acknowledged.
    */
  private def left(l: Event.Left): Boolean =
    members.left(l.channel, l.at) match {
      case Left(e) =>
        said(
          s"slack: a leave of ${ChannelId.value(l.channel)} not recorded, left for Slack to send again: $e"
        )
        false
      case Right(Membership.Gone) =>
        said(s"slack: grit's bot left ${names.shown(l.channel)}: nothing there is heard")
        true
      case Right(Membership.Member(_, _)) =>
        said(s"slack: a leave of ${names.shown(l.channel)} made before its recorded join, ignored")
        true
    }

  /** One pass over the review's prompts not yet posted that its place may receive
    * ([[grit.core.review.Reviews.unposted]]), when this edge answers a review: for each of
    * [[Reviewing.promptings]] in order, each prompt of a message heard in this workspace's Slack, or of
    * none, is posted at the top of the review's place, linking its message by its permalink
    * and each conversation it names by its thread's ([[Prompting.doc]]): never a message's text
    * or a draft's, nor why it was picked or what either gate decided; then given its
    * prompting's reactions, which [[receive]] keeps as its verdict; then kept as posted. A
    * prompt of a message Slack no longer has is dropped ([[grit.core.review.Reviews.dropped]]),
    * and that said, once; one Slack will not link or post waits for a later pass; a reaction
    * Slack will not add is said, and the prompt kept as posted without it. A prompt of a
    * labelled subject ([[Prompting.label]]) is posted, and kept as posted, only while the
    * review's place is written at a label dominating it, rechecked before it is posted and in
    * the transaction keeping it, and dropped, which is said, once it is not. A prompt posted but
    * not kept, as after a crash between the two, is posted again by a later pass, and the
    * reactions to the first post count for nothing. A review's place grit may not write to is
    * said once. How many prompts it posted and kept; none without a review; why not, when the
    * store could not be read.
    */
  def prompt(): Either[StoreError, Int] = reviewing.prompt()

  /** The link to message `ts` in the thread rooted at `thread` of `channel`, built from the
    * workspace's address ([[MessageLink.to]]), never asked of Slack; `None` when Slack gave
    * grit no address.
    */
  private def linkTo(channel: ChannelId, thread: Ts, ts: Ts): Option[Href] =
    self.workspace.flatMap(MessageLink.to(_, channel, thread, ts))

  /** One pass over the acknowledgements standing: a heard message a turn will answer, put to
    * grit by name ([[grit.core.speech.Decision.Answering]]), wears [[Working]] while the turn
    * runs, recorded as shown once Slack has it. Once the turn has ended its mark is taken down,
    * unless its reply is still to be posted, which [[deliver]] takes it down after; one never
    * shown is cleared without being put up, and one on a message Slack no longer has is
    * cleared. A mark Slack will not put up or take down is said, and tried again next pass; one
    * put up or taken down twice, as after a crash, is put up or taken down once. Each turn not
    * yet shown is started again (a no-op for one started), since a start can be lost. How many
    * marks it put up, took down or cleared; why not, when the store could not be read.
    */
  def acknowledge(): Either[StoreError, Int] = delivering.acknowledge()

  /** One pass over the replies awaited. On the edge's first pass, each turn is started again
    * (a no-op for one already started), since a start can be lost between ingest and start; a
    * scheduled run's is not, its start being the clock's ([[InboxError.SlotRun]]). Each turn
    * still running has its open asks shown to its asker as cards ([[Delivering.ask]]). Each
    * finished turn has its reply posted to its thread, in as many messages as [[RichText]]
    * makes of it (one line saying grit could not answer, and why, when it ended with none),
    * its note after the last ([[ReplyNote.of]]; none, and said, when its spend cannot be read),
    * and `:eyes:` removed from the message it answers, which clears its acknowledgement. A part
    * left posting by a crash is looked for by its tag first, and posted only when Slack does
    * not have it. A part Slack refuses stays posting, for the next pass. A reply awaited at an
    * address grit did not write is given up, said once, and settled as delivered. How many turns
    * it delivered; why not, when the store could not be read.
    */
  def deliver(): Either[StoreError, Int] = delivering.deliver()

  /** One click on an ask's card ([[Events.clicked]]), sent to the inbox as its clicker's
    * answer ([[grit.core.inbox.Inbox.answer]]): Apply lets the call run, Cancel declines it.
    * The card is then rewritten in place ([[Respond.Replace]]): [[Applying]] or [[Cancelled]]
    * once the answer is taken, which a second click is told too, whatever the first was;
    * [[Expired]] once the ask is closed, settled or past its time. A clicker who is not the
    * turn's asker is told, beside the card, who alone can answer it ([[onlyAsker]]), and the card
    * is left for the asker. A payload grit cannot read, an answer the inbox cannot take for any
    * other reason, and a rewrite Slack will not show are said, and the card left as it was.
    */
  def click(payload: String): Unit = delivering.click(payload)
}

object SlackEdge {

  /** The Slack edge, served over Socket Mode with `SLACK_BOT_TOKEN` and `SLACK_APP_TOKEN`:
    * every message in a channel its bot is a member of heard (ADR 0020, 0033), one addressed to
    * grit answered in its thread, `command` answered to its asker alone
    * ([[SlackEdge.command]]), and grit's bot's Slack name logged at open. At open, which
    * channels the bot is in is read from Slack and recorded ([[SlackEdge.reconcile]]); each
    * join and leave after is recorded as it happens, and a channel left is not heard from
    * then. What was said before each join is heard within `backfill`
    * ([[SlackEdge.backfillJoins]]), on a thread of its own, one join at a time: those pending
    * at open (found then, or left by a crash or a close) as it opens, and each join after from
    * the next delivery; closing waits for one under way at most [[Backfill.StopWithin]], and
    * one cut short is heard from where it stopped at the next open. Refused at open when Slack
    * will not list its channels. It shows each open ask to its asker alone as a card in its
    * turn's thread, and sends their click as the answer ([[ServedEdge.Asks.Answered]],
    * [[SlackEdge.deliver]], [[SlackEdge.click]]). It is the
    * attester [[SlackAccounts.Attester]]: for the realm of its own workspace, when a
    * deployment trusts it there, it says who each account is as Slack states it
    * ([[SlackEdge.receive]]), and a look ([[grit.core.edge.ServedEdge.Open.attest]]) lists
    * the workspace's users once when any account is due. Each reply it delivers is noted as
    * `note` says ([[ReplyNote]]).
    */
  def serving(command: SlackCommand, backfill: Backfill, note: ReplyNote): ServedEdge =
    serving(command, backfill, None, None, note)

  /** As [[serving]], posting when `posting` is given, and answering `review` when given.
    * With `posting`, it serves `slack_post` at [[PostsAt]]'s place, in every channel its bot is
    * a member of that is not quiet ([[grit.core.store.Tx.quiet]]) and that Slack gives a name,
    * at most `posting` posts across them, offered again at the next delivery after a join, a
    * leave, a room made quiet or not through its slash command, or a turn delivered whose ask
    * it showed as a card (one made quiet elsewhere is seen at the next such change). Each channel is offered under its name, with and without
    * `#`, at the place `slack:{team}/{id}`; a turn is offered only those its room may write to.
    * A post is a top-level message, or a reply in a thread its message link names, rendered as
    * a reply is (nothing in it becomes a mention), and one Slack message at most; it never asks
    * first and is never run again after a crash. With `review`, each delivery posts the
    * review's prompts not yet posted in its place, and a reaction its rater gives a prompt
    * there is kept as the prompt's verdict ([[SlackEdge.prompt]], [[SlackEdge.receive]]);
    * nothing said in the review's channel is heard, and it does not open when `review`'s team
    * is not the bot's.
    */
  def serving(
      command: SlackCommand,
      backfill: Backfill,
      posting: Option[Rate],
      review: Option[SlackReview],
      note: ReplyNote
  ): ServedEdge =
    Served.serving(command, backfill, posting, review, note, Socket, Backfilling.Virtual)

  /** Triage's review, over `reviews`, as an edge of `team` prompts for it: each prompt of the
    * heard message it names in `team`'s Slack, the bold question "Would a reply here have been
    * welcome?" and that message linked under the channel's name, then the reactions' legend;
    * one of another workspace's, or of none, never posted. Its reactions: `+1` welcome, `-1`
    * an interruption, `bust_in_silhouette` meant for someone in particular.
    */
  private[edge] def triage(
      reviews: Reviews[Prompt, Verdict],
      team: TeamId
  ): Prompting[Prompt, Verdict] =
    Prompting(
      reviews,
      p => EntryId.value(p.entry),
      p =>
        p.origin match {
          case Origin.Slack(t, channel, _) if t == TeamId.value(team) =>
            InboundId.source(p.entry) match {
              case Some((_, source)) =>
                Prompting.Of.Message(ChannelId(channel), Ts(SourceId.value(source)))
              case None =>
                Prompting.Of.Elsewhere(
                  Some(s"slack: the review's ${EntryId.value(p.entry)} names no Slack message")
                )
            }
          case _ => Prompting.Of.Elsewhere(None)
        },
      _ => None,
      // Every prompt of triage's is of a message, so linked is always given.
      (_, linked, _) =>
        linked.fold(Doc.empty)((channel, link) =>
          Doc(
            Vector(
              Block.Paragraph(
                Text(
                  Vector(
                    Span("Would a reply here have been welcome?", Set(Mark.Strong)),
                    Span.plain(" "),
                    Span(s"A message in $channel", Set(Mark.Link(link)))
                  )
                )
              ),
              Block.Paragraph(
                Text.plain(
                  "👍 welcome · 👎 an interruption · 👤 meant for someone in particular (even if an answer would have helped)"
                )
              )
            )
          )
        ),
      VectorMap(
        "+1" -> Verdict.Welcome,
        "-1" -> Verdict.Interruption,
        "bust_in_silhouette" -> Verdict.CutIn
      )
    )

  /** The service place `slack_post` is served at: `service:slack`. A deployment links
    * conversations to it with [[grit.core.place.Reaches]].
    */
  val PostsAt: Service =
    // "slack" is a service's name by Service.of's rule, so the Left is never taken.
    Service.of("slack").fold(why => throw new IllegalStateException(why), identity)

  /** What a person can no longer have done when the posting tool is withheld. */
  val PostsAction: String = "post to channels"

  /** What every channel grit's bot is a member of said over the `days` before the catch-up
    * opens that the database has not recorded, one [[grit.core.edge.Unheard]] per channel, in
    * the channel's room; heard at the times it was said, a past mention of grit never
    * answered, each author checked first as [[serving]]'s edge checks one. Which channels the
    * bot is in is read from Slack and recorded as [[serving]]'s open records it. Refused when
    * the bot is in none, or Slack will not list them.
    */
  def backfill(days: Int): CatchUp =
    Served.backfill(days, Socket)

  /** The team grit's bot token, `SLACK_BOT_TOKEN`, is installed in (`auth.test`): the
    * workspace [[serving]]'s edge attests, which a deployment trusts by default for it. Refused,
    * naming the variable, when either token is unset or malformed, or when Slack refuses the
    * token or cannot be reached, as the edge's open would be.
    */
  def installedIn(env: Map[String, String]): Either[EdgeRefusal, TeamId] =
    Served.installedIn(env, Socket)

  private object Socket extends Served.Connect {
    def apply(bot: BotToken, app: AppToken): Slack^ = new SocketSlack(bot, app)
  }

  /** The reaction a message wears while grit works on it: one said to grit, from when it is
    * recorded; one heard that a turn put to grit by name answers, from that turn's
    * acknowledgement ([[SlackEdge.acknowledge]]); each until its reply is posted.
    */
  val Working = "eyes"

  /** The answer to a command whose asker Slack cannot say who they are, and never has. */
  val NotAttested: String =
    "Nothing was run: Slack could not say who you are just now. Try again in a minute."

  /** What a card says once its asker's Apply is taken. */
  val Applying: String = "Confirmed by you; applying…"

  /** What a card says once its asker's Cancel is taken. */
  val Cancelled: String = "Cancelled"

  /** What a card says when clicked once its ask is closed. */
  val Expired: String = "Expired"

  /** What a clicker who is not the asker is told: the asker's Slack name, when grit knows it. */
  def onlyAsker(name: Option[String]): String =
    s"Only ${name.getOrElse("the person it asks")} can answer this"

  /** The answer to a command the database could not run. */
  val NotRun: String =
    "Nothing was run: grit could not reach its database just now. Try again in a minute."

  /** `<@U…>` in a message's text. */
  private[edge] val Mentioned = "<@([A-Z0-9]+)(?:\\|[^>]*)?>".r

  /** A link as Slack escapes one in a message's text, `<https://…>` or `<https://…|label>`. */
  private[edge] val Linked = "<(https?://[^|>]+)(?:\\|[^>]*)?>".r

  /** Where a reply goes: the channel, the thread to post in, and the message it answers (whose
    * `:eyes:` it removes). Written as `{channel}/{thread}/{answered}` in [[grit.core.edge.Deliveries]].
    */
  private[edge] final case class Address(channel: ChannelId, thread: Ts, answered: Ts) {
    def written: String = s"${ChannelId.value(channel)}/${Ts.value(thread)}/${Ts.value(answered)}"
  }

  private[edge] object Address {
    def read(s: String): Option[Address] = s.split('/') match {
      case Array(c, t, a) => Some(Address(ChannelId(c), Ts(t), Ts(a)))
      case _ => None
    }
  }
}
