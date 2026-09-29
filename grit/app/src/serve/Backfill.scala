package grit.app.serve

import java.time.{Duration, Instant, LocalDate}

import grit.core.host.ProcessIdentity
import grit.core.message.Cost
import grit.core.spend.Budget
import grit.dbos.engine.{Engine, Link, Swept}
import grit.dbos.sql.DbConfig
import grit.slack.client.SocketSlack
import grit.slack.edge.{EdgeStores, SlackEdge}
import grit.slack.event.{ChannelId, Event}

/** `grit backfill`: a listened channel's last days heard, as if grit had been listening, and
  * closed as they would have closed, before `grit serve` starts.
  */
object Backfill {

  /** The variable saying how many days back to read: a whole number above zero. */
  val DaysVar = "GRIT_BACKFILL_DAYS"

  /** The days read when [[DaysVar]] is unset. */
  val DefaultDays: Int = 2

  /** The days `env` says to read ([[DaysVar]]); why not, naming the variable, when it is not a
    * whole number above zero.
    */
  def days(env: Map[String, String]): Either[String, Int] =
    env.get(DaysVar) match {
      case None => Right(DefaultDays)
      case Some(raw) =>
        raw.trim.toIntOption
          .filter(_ > 0)
          .toRight(s"$DaysVar is a whole number of days above zero, not '$raw'")
    }

  /** What a person reads of `e`, the unheard messages of `channel` (as [[SlackEdge.listened]]
    * names it) since the start of `since`.
    */
  def line(channel: String, e: Estimate, since: LocalDate): String =
    s"backfill $channel: ${e.messages} messages in ${e.threads} threads since $since; " +
      s"up to ${usd(e.total)} (triage ${usd(e.triage)}, closings up to ${usd(e.closings)})"

  /** `amount` in dollars, rounded up to a hundredth of a cent. */
  def usd(amount: BigDecimal): String =
    "$" + amount.bigDecimal.setScale(4, java.math.RoundingMode.UP).stripTrailingZeros.toPlainString

  /** How long [[run]] waits between looks at the workflows it is waiting out: 2 s. */
  val Pause: Duration = Duration.ofSeconds(2)

  /** Hears what each channel in `GRIT_SLACK_LISTEN` said over the last [[days]] and the inbox
    * has not recorded ([[SlackEdge.unheard]]), after `confirm` agrees to the [[Estimate]]
    * `say` shows for each channel, with the tokens [[Serve]] takes. It opens the engine of
    * `config`'s database, launched by `launch` with no sweep of its own, and hears each
    * message at the time it was said ([[SlackEdge.backfill]]), a past mention of grit never
    * answered; then sweeps until nothing is left to close or ask ([[drain]]), says what the
    * day's recorded spend rose by, and closes the engine. A thread still inside idle is left
    * open for `grit serve`. Nothing caps what it spends: over what `budget`'s cap leaves of
    * today, it warns, then asks as before. Why it could not run: a setting missing or
    * malformed (a token never quoted), no channel listened in, the database's engine held
    * by another grit (named), Slack refusing the tokens or a channel's history, or the
    * database failing.
    */
  def run(
      env: Map[String, String],
      config: DbConfig,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget,
      launch: Engine^ => Unit,
      confirm: String => Boolean,
      say: String => Unit
  ): Option[String] = {
    val start = for {
      (bot, app, listen) <- Serve.slackOf(env)
      back <- days(env)
      _ <- Either.cond(
        listen.nonEmpty,
        (),
        s"${Serve.ListenVar} names no channel: there is nothing to backfill"
      )
    } yield (bot, app, listen, back)
    start match {
      case Left(why) => Some(why)
      case Right((bot, app, listen, back)) =>
        Engine.open(config, epoch, identity, budget) match {
          case Left(refused) => Some(refused.message(Instant.now()))
          case Right(engine) =>
            val slack = new SocketSlack(bot, app)
            try {
              launch(engine)
              val link: Link^{engine} = engine
              slack.self() match {
                case Left(e) => Some(s"Slack refused the bot token: $e")
                case Right(self) =>
                  val edge = new SlackEdge(
                    slack,
                    self,
                    EdgeStores(link.inbox, link.principals, link.deliveries, link.jot),
                    listen,
                    say
                  )
                  val now = Instant.now()
                  val since = now.minus(Duration.ofDays(back.toLong))
                  val from = LocalDate.ofInstant(since, budget.zone)
                  val channels = listen.toVector.sortBy(ChannelId.value)
                  val read = channels.foldLeft[Either[String, Vector[Vector[Event.Said]]]](
                    Right(Vector.empty)
                  ) { (acc, channel) =>
                    acc.flatMap(done =>
                      edge
                        .unheard(channel, since)
                        .left
                        .map(why => s"${ChannelId.value(channel)} not read: $why")
                        .map(done :+ _)
                    )
                  }
                  read match {
                    case Left(why) => Some(why)
                    case Right(unheard) =>
                      val each = unheard.map(Estimate.of)
                      channels.zip(each).foreach { (channel, e) =>
                        val id = ChannelId.value(channel)
                        val name = slack.channelName(channel).toOption.flatten
                        say(line(name.fold(id)(n => s"#$n ($id)"), e, from))
                      }
                      val all = each.foldLeft(Estimate.Zero)(_ + _)
                      if (all.messages == 0) {
                        say("backfill: nothing unheard")
                        None
                      } else
                        spentToday(link, budget, now) match {
                          case Left(why) => Some(why)
                          case Right(before) =>
                            budget.cap.foreach { cap =>
                              val left = cap.usd - before
                              if (all.total > left)
                                say(
                                  s"backfill: up to ${usd(all.total)} is more than the ${usd(left.max(0))} today's cap leaves; " +
                                    "nothing caps what backfill spends, and grit serve takes no new message once the cap is reached"
                                )
                            }
                            if (!confirm(s"hear ${all.messages} messages, up to ${usd(all.total)}?"))
                              {
                                say("backfill: nothing heard")
                                None
                              }
                            else
                              edge.backfill(unheard.flatten) match {
                                case Left(why) => Some(why)
                                case Right(()) =>
                                  say(s"backfill: heard ${all.messages} messages; closing what is due")
                                  drain(
                                    () => engine.sweep(Instant.now()).left.map(_.toString),
                                    () => engine.unfinished().left.map(_.toString),
                                    () => Thread.sleep(Pause.toMillis)
                                  ) match {
                                    case Left(why) => Some(s"the sweep failed: $why")
                                    case Right(_) =>
                                      spentToday(link, budget, Instant.now()) match {
                                        case Left(why) => Some(why)
                                        case Right(after) =>
                                          say(
                                            s"backfill: spent ${usd(after - before)} (estimated up to ${usd(all.total)})"
                                          )
                                          None
                                      }
                                  }
                              }
                        }
                  }
              }
            } finally {
              slack.close()
              engine.close()
            }
        }
    }
  }

  /** The dollars `link`'s ledger recorded on the day `now` falls on in `budget`'s zone, a call
    * no provider priced counted as nothing.
    */
  private def spentToday(link: Link^, budget: Budget, now: Instant): Either[String, BigDecimal] =
    link.db
      .read(link.spending.on(budget.today(now)))
      .left
      .map(e => s"today's spend could not be read: $e")
      .map(_.cost match {
        case Cost.Exact(usd) => usd
        case Cost.AtLeast(usd) => usd
      })

  /** Sweeps with `sweep` until a sweep made once `unfinished` counts no workflow queued or
    * running enqueues nothing: no close, question or posting. `pause` runs between any two
    * looks. How many sweeps it made; why not, the first time a sweep or the count fails.
    */
  def drain(
      sweep: () => Either[String, Swept],
      unfinished: () => Either[String, Int],
      pause: () => Unit
  ): Either[String, Int] = {
    @scala.annotation.tailrec
    def settled(): Either[String, Unit] =
      unfinished() match {
        case Left(why) => Left(why)
        case Right(0) => Right(())
        case Right(_) =>
          pause()
          settled()
      }
    @scala.annotation.tailrec
    def from(swept: Int): Either[String, Int] =
      settled().flatMap(_ => sweep()) match {
        case Left(why) => Left(why)
        case Right(s) if s.enqueued.isEmpty && s.asked.isEmpty && s.posted.isEmpty =>
          Right(swept + 1)
        case Right(_) =>
          pause()
          from(swept + 1)
      }
    from(0)
  }
}
