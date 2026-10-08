package grit.kit.run

import java.time.{Duration, Instant, LocalDate}

import grit.core.clock.Clock
import grit.core.edge.{CatchUp, EdgeStores}
import grit.core.spend.Budget
import grit.core.visibility.{Label, Visibility}
import grit.dbos.engine.Swept

/** A catch-up's flow over an open engine: what it would hear shown, estimated and agreed to,
  * heard, and swept until nothing is left to close.
  */
private[run] object CatchingUp {

  /** How long [[drain]] waits between looks at the workflows it is waiting out: 2 s. */
  val Pause: Duration = Duration.ofSeconds(2)

  /** Said once before a backfill asks to hear: what it hears keeps the label shown. */
  val Relabel: String =
    "backfill: each source is heard at the label shown beside it; a label declared later does " +
      "not relabel what is heard now"

  /** What a person reads of `e`, the unheard messages of `source`, heard at `label`, since the
    * start of `since`.
    */
  def line(source: String, label: Label, e: Estimate, since: LocalDate): String =
    s"backfill $source, heard at ${Label.written(label)}: ${e.messages} messages in ${e.threads} threads since $since; " +
      s"up to ${usd(e.total)} (triage ${usd(e.triage)}, closings up to ${usd(e.closings)})"

  /** `amount` in dollars, rounded up to a hundredth of a cent. */
  def usd(amount: BigDecimal): String =
    "$" + amount.bigDecimal.setScale(4, java.math.RoundingMode.UP).stripTrailingZeros.toPlainString

  /** [[Kit.catchUp]]'s flow, over `stores`, each source shown at the label `visibility` gives
    * its room and opened at `clock`'s time, reading today's spend with `spent`, sweeping with
    * `sweep` and counting what is left with `unfinished`.
    */
  def run(
      catchUp: CatchUp,
      stores: EdgeStores^,
      env: Map[String, String],
      budget: Budget,
      visibility: Visibility,
      clock: Clock^,
      spent: () => Either[String, BigDecimal],
      sweep: () => Either[String, Swept],
      unfinished: () => Either[String, Int],
      agree: String => Boolean,
      say: String => Unit
  ): Either[KitFailure, Unit] = {
    val name = catchUp.name
    catchUp.open(stores, env, clock.now(), say) match {
      case Left(refusal) => Left(KitFailure.Edge(name, refusal))
      case Right(open) =>
        try {
          val from = Kit.dayOf(open.since, budget)
          val each = open.unheard.map(u => (u, Estimate.of(u)))
          each.foreach((u, e) => say(line(u.source, visibility.roomLabel(u.place), e, from)))
          val all = each.map(_._2).foldLeft(Estimate.Zero)(_ + _)
          if (all.messages == 0) {
            say("backfill: nothing unheard")
            Right(())
          } else
            spent().left.map(KitFailure.Store(_)).flatMap { before =>
              say(Relabel)
              budget.cap.foreach { cap =>
                val left = cap.usd - before
                if (all.total > left)
                  say(
                    s"backfill: up to ${usd(all.total)} is more than the ${usd(left.max(0))} today's cap leaves; " +
                      "nothing caps what backfill spends, and grit serve takes no new message once the cap is reached"
                  )
              }
              if (!agree(s"hear ${all.messages} messages, up to ${usd(all.total)}?")) {
                say("backfill: nothing heard")
                Right(())
              } else
                for {
                  _ <- open.hear().left.map(KitFailure.Edge(name, _))
                  _ = say(s"backfill: heard ${all.messages} messages; closing what is due")
                  _ <- drain(sweep, unfinished, () => Thread.sleep(Pause.toMillis)).left
                    .map(why => KitFailure.Store(s"the sweep failed: $why"))
                  after <- spent().left.map(KitFailure.Store(_))
                } yield say(
                  s"backfill: spent ${usd(after - before)} (estimated up to ${usd(all.total)})"
                )
            }
        } finally open.close()
    }
  }

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
