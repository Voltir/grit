package grit.kit.run

import grit.core.id.{ConversationId, TurnSeq}
import grit.core.message.{Cost, Tokens, Usage}
import grit.turn.{TurnBasis, TurnCache, TurnRecord}

/** How a conversation's turns used the provider's prompt cache, for a person to read
  * ([[Kit.cacheReport]]).
  */
object CacheReport {

  /** The share of a carried turn's input its calls should read from the cache: 0.8. */
  val Target: Double = 0.8

  /** The report of `turns` of `conversation`, oldest first: per turn its carry or why it drew
    * afresh, its own calls' cache-read share, and the start-to-start gap from the previous
    * turn's last own call to its first; per call what it was for, whether it was the turn's
    * first own call, its model, input, cache-read and cache-write tokens and cost; then how
    * many carried turns reach [[Target]].
    */
  def lines(conversation: ConversationId, turns: Vector[TurnCache]): Vector[String] = {
    val previous = None +: turns.map(Some(_))
    val body = turns.zip(previous).flatMap { (t, before) =>
      val gap = for {
        b <- before
        from <- b.own.lastOption.flatMap(_.started)
        to <- t.own.headOption.flatMap(_.started)
      } yield s"${duration(java.time.Duration.between(from, to))} after turn ${TurnSeq
          .value(b.turn.turnSeq)}'s last call"
      val share = t.share.fold("no own calls")(s =>
        s"cache reads ${percent(s)} of ${t.own.map(o => Tokens.value(o.usage.input)).sum} input"
      )
      val head =
        (Vector(s"turn ${TurnSeq.value(t.turn.turnSeq)}", basis(t.carried), share) ++ gap)
          .mkString(" · ")
      val own = t.own.zipWithIndex.map { (o, i) =>
        s"  round ${o.round} · ${if (i == 0) "first" else "later"} · ${o.model} · ${spent(o.usage)}"
      }
      val aside = t.aside.map(a => s"  ${named(a.role)} · ${a.model} · ${spent(a.usage)}")
      head +: (own ++ aside)
    }
    val carried = turns.filter(_.carried.exists(_.head.nonEmpty))
    val reached = carried.count(_.share.exists(_ >= Target))
    (s"conversation ${ConversationId.value(conversation)}" +: body) :+
      s"carried turns: ${carried.size} · at ${percent(Target)} or more: $reached"
  }

  private def basis(carried: Option[TurnBasis]): String = carried match {
    case Some(TurnBasis.Against(_)) => "carried"
    case Some(TurnBasis.Afresh(why)) => s"afresh: ${why.word}"
    case None => "carry not recorded"
  }

  private def spent(u: Usage): String =
    s"in ${Tokens.value(u.input)} · read ${Tokens.value(u.cacheRead)} · write ${Tokens
        .value(u.cacheWrite)} · ${u.costUsd.fold("cost unknown")(Cost.cents)}"

  private def percent(share: Double): String = s"${math.round(share * 100)}%"

  private def named(role: TurnRecord.Role): String = role match {
    case TurnRecord.Role.Relevance(batch) => s"relevance ${batch}"
    case TurnRecord.Role.Round(index) => s"round ${index}"
    case other => other.toString.toLowerCase
  }

  private def duration(d: java.time.Duration): String = {
    val s = math.max(0L, d.getSeconds)
    Vector(s / 3600 -> "h", s % 3600 / 60 -> "m", s % 60 -> "s")
      .dropWhile((n, _) => n == 0)
      .map((n, unit) => s"$n$unit")
      .mkString(" ") match {
      case "" => "0s"
      case written => written
    }
  }
}
