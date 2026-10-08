package grit.slack.edge

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import grit.core.admin.Command
import grit.core.clock.Clock
import grit.core.edge.{
  Acknowledgement,
  Asked,
  Attesting,
  CatchUp,
  EdgeRefusal,
  EdgeStores,
  Membership,
  Part,
  Pending,
  RealmSource,
  ServedEdge
}
import grit.core.id.{CallSlot, EntryId, SourceId, TurnRef, WorkflowId}
import grit.core.identity.{Account, Realm, Standing}
import grit.core.inbox.{InboundId, InboxError, Progress}
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.Service
import grit.core.review.Prompt
import grit.core.speech.Reach
import grit.core.spend.Budget
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.Subject
import grit.prose.form.{Block, Doc, Text}
import grit.prose.markdown.Markdown
import grit.slack.client.{AppToken, BotToken, Root, Self, Slack, SlackError, SocketSlack, Tag}
import grit.slack.event.{ChannelId, Event, Events, TeamId, Ts, UserId}
import grit.slack.text.{Incoming, Post, RichText}

/** The Slack edge (ADR 0002, 0019): Slack's messages in as turns, grit's replies out, through
  * the stores alone; grit being `self` in the workspace, answering `review` when one is
  * given; `clock` dates the marks it puts up and takes down. `said` is told what it did that a
  * person running it may want to read.
  */
final class SlackEdge(
    slack: Slack,
    self: Self,
    stores: EdgeStores^,
    review: Option[SlackReview],
    clock: Clock^,
    said: String => Unit
) {
  import SlackEdge.*

  // Caches and a flag, each written only through its own atomic operations. What they hold
  // is looked up again from Slack or the store when missing, so a reader that races a writer
  // sees either value, and both are right.
  @caps.unsafe.untrackedCaptures
  private val names = new ConcurrentHashMap[UserId, String]()

  @caps.unsafe.untrackedCaptures
  private val channelNames = new ConcurrentHashMap[ChannelId, String]()

  @caps.unsafe.untrackedCaptures
  private val started = new AtomicBoolean(false)

  /** The channels grit's bot is a member of: what it hears. */
  private val members: Members^{stores} = new Members(stores.joins, self.team)

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
      reviewed.foreach((c, _) => said(s"slack: the review's channel, ${shown(c)}, is never heard"))
      val access = heard.flatMap((c, k) => Members.access(k).map(c -> _))
      members.reconcile(access, clock.now()).left.map(_.toString).map { forgotten =>
        said(members.all match {
          case Vector() => "slack: a member of no channel"
          case all => s"slack: a member of ${all.map(shown).mkString(", ")}"
        })
        if (forgotten > 0) said(s"slack: forgot $forgotten rooms left over a day ago")
      }
    }

  /** Every channel grit's bot is a member of, by id, as last recorded. */
  def channels: Vector[ChannelId] = members.all

  /** `channel` as a person reads it: `#{name} ({id})`, its id alone when Slack gives no name. */
  private def shown(channel: ChannelId): String = {
    val id = ChannelId.value(channel)
    channelNameOf(channel).fold(id)(name => s"#$name ($id)")
  }

  /** One Events API payload. A person's message in a channel grit's bot is a member of (never
    * the review's channel) is addressed to grit when it mentions grit, or is in a thread whose root did; addressed, it is recorded as a turn of
    * its thread's conversation, in the person's words ([[Incoming]]), written through their
    * account in their own team ([[SlackAccounts.account]], [[Event.Said.author]]), named as Slack names them, its turn started, its reply
    * awaited, and the message marked `:eyes:` until the reply is posted. A new message the
    * inbox refuses over the day's cap is not recorded: it is answered, once, in its thread,
    * with [[Budget.Refusal]]. A message not addressed is heard
    * ([[grit.core.inbox.Inbox.hear]]) in the same words under the same name, with no mark, its
    * thread kept as where a reply would go and whom it names besides grit
    * ([[grit.core.speech.Reach]]): grit replies there only when it drafts a reply and the
    * draft is posted (ADR 0022).
    * A person's direct message to grit ([[Event.Told]]) is recorded as an addressed message is,
    * as a turn of its thread's conversation in its author's direct room
    * ([[grit.core.store.Origin.Direct]]); one the inbox refuses as
    * sealed is answered once in its thread with [[InboxError.SealedReply]], and one refused
    * over the cap as a channel's is. A group direct message is ignored.
    * A message from a channel not recorded a member of, as when it races its channel's join,
    * asks Slack once whether grit's bot is in it ([[Slack.kind]]): when it is, the join is
    * recorded first, as of the message, then the message is taken; when not, it is ignored.
    * grit's bot joining a channel ([[Event.Joined]]) is recorded with the channel's access as
    * Slack reports it, and its inviter's account, checked first ([[Attesting.before]]), so a
    * join whose inviter is no full member skips its backfill, which is said; leaving or being
    * removed from one ([[Event.Left]]) is recorded, and nothing said there after is heard. Each
    * is ordered by when it happened ([[grit.core.edge.Joins]]); the review's channel is never
    * recorded a member, and a join Slack says the bot is no longer in is not recorded.
    * A message recorded or heard that begins its conversation as a reply in a thread whose root
    * grit's bot posted with `slack_post` ([[Tag.Sent]]) first records that post as the
    * conversation's opening ([[grit.core.inbox.Inbox.posted]]), in its text as Slack gives it,
    * made by the call its tag names; when Slack cannot be asked for the root, the message is
    * recorded without it, and that is said.
    * A reaction the review's rater adds to a prompt grit posted keeps it as the prompt's
    * verdict, replacing the one standing, and removing the reaction that stands withdraws it:
    * `:+1:` welcome, `:-1:` an interruption, `:bust_in_silhouette:` meant for someone in
    * particular, a skin tone ignored. Everything else is ignored, any other reaction included.
    * Before a message is recorded or heard, its author's account is checked
    * ([[Attesting.before]]): Slack is asked who they are ([[Slack.member]]) unless it answered
    * for them within [[Attesting.Fresh]], so the turn the message starts opens under that
    * answer; one Slack cannot answer for and never has is not recorded. A change Slack reports
    * to a user ([[Event.UserChanged]]) asks Slack again ([[Attesting.changed]]).
    * `true` once that is done or needs no doing (a redelivery included), so the payload may be
    * acknowledged; `false` when Slack or the database could not be asked, so Slack sends it
    * again (a message racing its channel's join included, so the join wins on a later try).
    */
  def receive(payload: String): Boolean =
    Events.read(payload, self.bot) match {
      case Left(why) =>
        said(s"slack: an event grit cannot read, acknowledged: $why")
        true
      case Right(Event.Ignored(_)) => true
      case Right(j: Event.Joined) => joined(j)
      case Right(l: Event.Left) => left(l)
      case Right(r: Event.Reacted) => reacted(r)
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
    * that user's account in the command's team. Its asker's account is checked first
    * ([[Attesting.before]]), then the command is run as theirs ([[Administration.run]]), now,
    * in its room: its channel's (`slack:{team}/{channel}`), whether grit's bot is a member
    * there or not, or, asked in any direct message, the asker's own direct room. Words read
    * wrong are answered with why and the commands; an asker Slack cannot answer for and never
    * has, or a database that failed, with a line saying nothing was run. An answer Slack will
    * not take, and a payload grit cannot read, are said.
    */
  def command(registered: SlackCommand)(payload: String): Unit =
    Events.command(payload) match {
      case Left(why) => said(s"slack: a slash command grit cannot read: $why")
      case Right(c) if c.command != registered.name =>
        said(s"slack: a slash command not grit's, ${c.command}, ignored")
      case Right(c) =>
        val answer = SlackAccounts.account(c.team, c.user) match {
          case Left(why) =>
            said(s"slack: ${c.command} not run: its asker cannot be named: $why")
            NotAttested
          case Right(asker) =>
            stores.attesting.before(source, asker) match {
              case Left(e) =>
                said(s"slack: ${c.command} not run: ${Account.written(asker)} unchecked: $e")
                NotAttested
              case Right(_) =>
                val room =
                  if (c.direct) Origin.Direct(asker, "").room
                  else Origin.channel(TeamId.value(c.team), ChannelId.value(c.channel))
                // A mention as Slack escapes it, `<@U…|name>`, is read as the user alone.
                val words = Mentioned.replaceAllIn(c.text, m => s"<@${m.group(1)}>")
                def person(word: String): Option[Account] = word match {
                  case Mentioned(user) => SlackAccounts.account(c.team, UserId(user)).toOption
                  case _ => None
                }
                Command.read(words, person) match {
                  case Left(why) => why
                  case Right(command) =>
                    stores.administration.run(asker, room, command, clock.now()) match {
                      case Right(answered) => answered.text
                      case Left(e) =>
                        said(s"slack: ${c.command} not run: $e")
                        NotRun
                    }
                }
            }
        }
        slack
          .respond(c.answerAt, answer)
          .left
          .foreach(e => said(s"slack: the answer to ${c.command} not shown: $e"))
    }

  /** What Slack says of the accounts of grit's own team, `slack:{self.team}/…`, and nothing of
    * any other's: each user as [[Slack.member]] reads them, an account of another team outside
    * without asking, and the team's listing as [[Slack.members]] reads it, any other realm's
    * empty. A failure to ask is [[Asked.Unreached]]. The name an answer gives is kept as the
    * user's name ([[nameOf]]), so a new author costs one `users.info`.
    */
  private val source: RealmSource^{slack} = new RealmSource {
    def ask(account: Account): Asked =
      SlackAccounts.user(account, self.team).fold(Asked.Said(Standing.Outside)) { user =>
        slack.member(self.team, user) match {
          case Right(m) =>
            m.name.foreach(n => names.put(user, n))
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

  /** Keeps `r` as a verdict on the review prompt it is on ([[ReviewPrompt.verdict]]), added or
    * withdrawn, when it is the review's rater's; nothing for anyone else's, another emoji, or
    * a message no prompt was posted at. Whether it may be acknowledged.
    */
  private def reacted(r: Event.Reacted): Boolean =
    review
      .filter(v => r.team == self.team && r.user == v.rater)
      .zip(ReviewPrompt.verdict(r.emoji)) match {
      case None => true
      case Some((_, verdict)) =>
        val at = PromptAt(r.channel, r.ts).written
        SlackAccounts.account(r.team, r.user).left.map(StoreError.Invalid(_)).flatMap { rater =>
          stores.jot.write(Subject.Public)(
            if (r.added) stores.reviews.reacted(at, rater, verdict, r.at)
            else stores.reviews.unreacted(at, rater, verdict)
          )
        } match {
          case Right(_) => true
          case Left(e) =>
            said(
              s"slack: a reaction to ${Ts.value(r.ts)} not kept, left for Slack to send again: $e"
            )
            false
        }
    }

  /** What [[backfill]] would hear of `channel`: each message said there from `since` on,
    * read as [[receive]] reads a live one ([[Events.listed]]), that the inbox has not
    * recorded, oldest first; none unless grit's bot is a member of `channel`. A listing grit cannot read
    * is left out, and said. Why not, when Slack or the inbox could not be asked.
    */
  def unheard(channel: ChannelId, since: Instant): Either[String, Vector[Event.Said]] =
    served(channel, clock.now()).flatMap { open =>
      if (!open) Right(Vector.empty)
      else
        slack.history(channel, since).left.map(_.toString).flatMap { listed =>
          val spoken = listed.flatMap(l =>
            Events.listed(l, self.team, channel, self.bot) match {
              case Right(m: Event.Said) => Some(m)
              case Right(
                    Event.Ignored(_) | Event.Reacted(_, _, _, _, _, _, _) |
                    Event.UserChanged(_, _) | Event.Told(_, _, _, _, _, _, _, _) |
                    Event.Joined(_, _, _, _) | Event.Left(_, _, _)
                  ) =>
                None
              case Left(why) =>
                said(s"slack: a listed message grit cannot read, left out: $why")
                None
            }
          )
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
    messages.foldLeft[Either[String, Unit]](Right(())) { (done, m) =>
      done.flatMap { _ =>
        val origin =
          Origin.Slack(TeamId.value(m.team), ChannelId.value(m.channel), Ts.value(m.thread))
        hear(m, origin, live = false).left.map(why => s"message ${Ts.value(m.ts)} not heard: $why")
      }
    }

  /** Records `m`, heard, in its thread's conversation, with whom it names besides grit and,
    * when it is heard `live`, its thread as where a reply to it would go; nothing in a
    * conversation grit does not serve.
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
            .hear(origin, SourceId(Ts.value(m.ts)), text, author, m.at, Reach(to, asked))
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

  /** The account of `user` of `team`, checked first ([[Attesting.before]]) and named as Slack
    * names them, and `text`, which they wrote, in their words.
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
      known = (user +: mentioned).distinct.flatMap(u => nameOf(u).map(u -> _)).toMap
      linked = Incoming
        .channels(text)
        .collect { case (c, None) => c }
        .distinct
        .flatMap(c => channelNameOf(c).map(c -> _))
        .toMap
      _ <- stores.jot
        .write(Subject.Public)(
          stores.principals.name(author, known.getOrElse(user, UserId.value(user)))
        )
        .left
        .map(_.toString)
    } yield (author, Incoming.text(text, self.bot, known.get, linked.get))

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

  /** Records `message`, the one at `at`, written through `author`, as a turn of `origin`'s
    * conversation, its reply awaited at `at`, its turn started and the message marked; a
    * refusal over the cap, or of a sealed thread, answered once in its thread instead.
    */
  private def ingest(
      origin: Origin,
      at: Address,
      message: Message.User,
      author: Account
  ): Either[String, Unit] =
    stores.inbox.ingest(origin, SourceId(Ts.value(at.answered)), message, author) match {
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
  private def refuse(at: Address, line: String): Either[String, Unit] = {
    val tag = Tag.Refused(at.answered)
    slack
      .tagged(at.channel, at.thread, tag)
      .flatMap { there =>
        if (there.nonEmpty) Right(())
        else
          RichText
            .render(Doc(Vector(Block.Paragraph(Text.plain(line)))))
            .foldLeft[Either[SlackError, Unit]](Right(()))((done, post) =>
              done.flatMap(_ => slack.post(at.channel, at.thread, post, tag).map(_ => ()))
            )
      }
      .left
      .map(_.toString)
  }

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
              case Membership.Member(_, _) =>
                said(s"slack: grit's bot is in ${shown(channel)}, found by a message from it")
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
      said(s"slack: grit's bot joined the review's channel, ${shown(j.channel)}: never heard")
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
                  if (at == j.at)
                    said(
                      s"slack: grit's bot joined ${shown(j.channel)}" + (backfill match {
                        case None =>
                          s"; what was said before is not heard: ${inviter.fold("")(Account.written)} " +
                            "invited it, whom no trusted realm vouches a full member"
                        case Some(_) => ""
                      })
                    )
                  true
                case Right(Membership.Gone) =>
                  said(
                    s"slack: a join of ${shown(j.channel)} made before its recorded leave, ignored"
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
        said(s"slack: grit's bot left ${shown(l.channel)}: nothing there is heard")
        true
      case Right(Membership.Member(_, _)) =>
        said(s"slack: a leave of ${shown(l.channel)} made before its recorded join, ignored")
        true
    }

  /** `user`'s Slack name, asked of Slack once per user; `None` when they have none, or Slack
    * could not say (the id stands in, and they are asked again next time).
    */
  private def nameOf(user: UserId): Option[String] =
    Option(names.get(user)).orElse {
      slack.member(self.team, user).map(_.name) match {
        case Right(Some(n)) =>
          val _ = names.put(user, n)
          Some(n)
        case Right(None) => None
        case Left(e) =>
          said(s"slack: no name for ${UserId.value(user)}: $e")
          None
      }
    }

  /** `channel`'s Slack name, asked of Slack once per channel; `None` when it has none grit may
    * see, or Slack could not say (the id stands in, and it is asked again next time).
    */
  private def channelNameOf(channel: ChannelId): Option[String] =
    Option(channelNames.get(channel)).orElse {
      slack.channelName(channel) match {
        case Right(Some(n)) =>
          val _ = channelNames.put(channel, n)
          Some(n)
        case Right(None) => None
        case Left(e) =>
          said(s"slack: no name for channel ${ChannelId.value(channel)}: $e")
          None
      }
    }

  /** One pass over the review's prompts not yet posted that its place may receive
    * ([[grit.core.review.Reviews.unposted]]), when this edge answers a review: each
    * whose message was heard in this workspace's Slack is posted at the top of the review's
    * place, linking the message by its permalink ([[ReviewPrompt.doc]]): never a message's text
    * or a draft's, nor why it was picked or what either gate decided; then given the three
    * reactions [[receive]] keeps as its verdict; then kept as posted. A
    * prompt Slack will not link or post waits for a later pass; a reaction Slack will not add
    * is said, and the prompt kept as posted without it. A prompt posted but not kept, as after
    * a crash between the two, is posted again by a later pass, and the reactions to the first
    * post count for nothing. How many prompts it posted and kept; none without a review; why
    * not, when the store could not be read.
    */
  def prompt(): Either[StoreError, Int] = review match {
    case None => Right(0)
    case Some(r) =>
      stores.jot
        .write(Subject.Public)(stores.reviews.unposted(r.place))
        .map(_.count { p =>
          p.origin match {
            case Origin.Slack(team, channel, _) if team == TeamId.value(self.team) =>
              InboundId.source(p.entry) match {
                case Some((_, source)) =>
                  prompted(r, p, ChannelId(channel), Ts(SourceId.value(source)))
                case None =>
                  said(s"slack: the review's ${EntryId.value(p.entry)} names no Slack message")
                  false
              }
            case _ => false
          }
        })
  }

  /** Posts `p`, of message `ts` in `channel`, to `r`'s place, as [[prompt]] says; whether it
    * was posted and kept.
    */
  private def prompted(r: SlackReview, p: Prompt, channel: ChannelId, ts: Ts): Boolean = {
    val shown = channelNameOf(channel).fold(ChannelId.value(channel))(n => s"#$n")
    val done = for {
      link <- slack.permalink(channel, ts).left.map(e => s"its message is not linked: $e")
      post <- RichText.render(ReviewPrompt.doc(shown, link)) match {
        case Vector(one) => Right(one)
        case more => Left(s"it renders as ${more.size} messages")
      }
      at <- slack
        .postTopLevel(r.channel, post, Tag.Prompt(EntryId.value(p.entry)))
        .left
        .map(e => s"not posted: $e")
      _ = ReviewPrompt.Reactions.keys.foreach(emoji =>
        slack
          .react(r.channel, at, emoji)
          .left
          .foreach(e => said(s"slack: a review prompt left without :$emoji:: $e"))
      )
      when <- Events.time(Ts.value(at)).toRight(s"posted at ${Ts.value(at)}, which names no time")
      kept <- stores.jot
        .write(Subject.Public)(
          stores.reviews.posted(p.entry, PromptAt(r.channel, at).written, when)
        )
        .left
        .map(e => s"posted at ${Ts.value(at)} but not kept, so it is posted again: $e")
    } yield kept
    done match {
      case Right(true) => true
      case Right(false) =>
        said(s"slack: a review prompt posted at a message another prompt is kept at")
        false
      case Left(why) =>
        said(s"slack: the review prompt of ${EntryId.value(p.entry)}: $why")
        false
    }
  }

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
            case Left(SlackError.Refused("message_not_found")) => clear
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

  /** One pass over the replies awaited. On the edge's first pass, each turn is started again
    * (a no-op for one already started), since a start can be lost between ingest and start; a
    * scheduled run's is not, its start being the clock's ([[InboxError.SlotRun]]). Each
    * finished turn has its reply posted to its thread, in as many messages as [[RichText]]
    * makes of it (one line saying grit could not answer, and why, when it ended with none),
    * and `:eyes:` removed from the message it answers, which clears its acknowledgement. A part
    * left posting by a crash is looked for by its tag first, and posted only when Slack does
    * not have it. A part Slack refuses stays posting, for the next pass. How many turns it
    * delivered; why not, when the store could not be read.
    */
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

object SlackEdge {

  /** The Slack edge, served over Socket Mode with `SLACK_BOT_TOKEN` and `SLACK_APP_TOKEN`:
    * every message in a channel its bot is a member of heard (ADR 0020, 0033), one addressed to
    * grit answered in its thread, `command` answered to its asker alone
    * ([[SlackEdge.command]]), and grit's bot's Slack name logged at open. At open, which
    * channels the bot is in is read from Slack and recorded ([[SlackEdge.reconcile]]); each
    * join and leave after is recorded as it happens, and a channel left is not heard from
    * then. Refused at open when Slack will not list its channels. It cannot answer a tool call
    * that asks first. It is the attester [[SlackAccounts.Attester]]: for the realm of its own
    * workspace, when a deployment trusts it there, it says who each account is as Slack states
    * it ([[SlackEdge.receive]]), and a look ([[grit.core.edge.ServedEdge.Open.attest]]) lists
    * the workspace's users once when any account is due.
    */
  def serving(command: SlackCommand): ServedEdge =
    Served.serving(command, None, None, Socket)

  /** As [[serving]], and posting as `posts` allows: it serves `slack_post` at [[PostsAt]]'s
    * place, offering the channels of `posts` whose names Slack gives at open; one without is
    * left out and logged, and with none left nothing is served there. Each channel is offered
    * under its name, with and without `#`, at the place `slack:{team}/{id}`; a turn is offered
    * only those its room may write to. A post is a top-level
    * message, or a reply in a thread its message link names, rendered as a reply is (nothing
    * in it becomes a mention), and one Slack message at most; it never asks first and is never
    * run again after a crash.
    */
  def serving(command: SlackCommand, posts: Posts): ServedEdge =
    Served.serving(command, Some(posts), None, Socket)

  /** As [[serving]], posting as `posts` allows when given, and answering `review`: each
    * delivery posts the review's prompts not yet posted in its place, and a reaction its rater
    * gives a prompt there is kept as the prompt's verdict ([[SlackEdge.prompt]],
    * [[SlackEdge.receive]]); nothing said in the review's channel is heard. It does not open
    * when `review`'s team is not the bot's.
    */
  def serving(command: SlackCommand, posts: Option[Posts], review: SlackReview): ServedEdge =
    Served.serving(command, posts, Some(review), Socket)

  /** The service place `slack_post` is served at: `service:slack`. A deployment links
    * conversations to it with [[grit.core.place.Reaches]].
    */
  val PostsAt: Service =
    // "slack" is a service's name by Service.of's rule, so the Left is never taken.
    Service.of("slack").fold(why => throw new IllegalStateException(why), identity)

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

  /** The answer to a command the database could not run. */
  val NotRun: String =
    "Nothing was run: grit could not reach its database just now. Try again in a minute."

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
    * `:eyes:` it removes). Written as `{channel}/{thread}/{answered}` in [[grit.core.edge.Deliveries]].
    */
  private final case class Address(channel: ChannelId, thread: Ts, answered: Ts) {
    def written: String = s"${ChannelId.value(channel)}/${Ts.value(thread)}/${Ts.value(answered)}"
  }

  /** Where a review's prompt was posted: its channel and its ts. Written as `{channel}/{ts}` in
    * [[grit.core.review.Reviews]].
    */
  private final case class PromptAt(channel: ChannelId, ts: Ts) {
    def written: String = s"${ChannelId.value(channel)}/${Ts.value(ts)}"
  }

  private object Address {
    def read(s: String): Option[Address] = s.split('/') match {
      case Array(c, t, a) => Some(Address(ChannelId(c), Ts(t), Ts(a)))
      case _ => None
    }
  }
}
