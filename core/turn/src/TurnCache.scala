package grit.turn

import java.time.Instant

import grit.core.durable.StepRecord
import grit.core.id.TurnRef
import grit.core.message.Usage
import grit.core.store.UsageLedger

/** A turn's model calls as a provider's prompt cache sees them, read back from what it recorded. */
final case class TurnCache(
    turn: TurnRef,
    carried: Option[TurnBasis],
    own: Vector[TurnCache.Own],
    aside: Vector[TurnCache.Aside]
) extends caps.Pure {

  /** Of `own`'s input tokens, the share read from the cache; `None` when they had none. */
  def share: Option[Double] = Usage.total(own.map(_.usage)).cacheShare
}

object TurnCache {

  /** The turn's own call of loop round `round` (its reply's included), started when its
    * `ask:n` step did (`None`: not recorded).
    */
  final case class Own(round: Int, model: String, usage: Usage, started: Option[Instant])

  /** Any other call the turn made (a classifier's, its summary's), by what it was for. */
  final case class Aside(role: TurnRecord.Role, model: String, usage: Usage)

  /** `turn` from its `steps` and its workflow's rows of the ledger, `rows`; a row of no role of
    * `turn`'s is left out. `Left` naming the step whose output does not read.
    */
  def of(
      turn: TurnRef,
      steps: Vector[StepRecord],
      rows: Vector[UsageLedger.Row]
  ): Either[String, TurnCache] =
    TurnRecord.carried(steps).map { carried =>
      val asked: Map[Int, Option[Instant]] = steps.flatMap { s =>
        TurnRound.Names.Step.of(s.name).collect { case TurnRound.Names.Step.Ask(n) =>
          n -> s.started
        }
      }.toMap
      val roled = rows.flatMap(r => TurnRecord.role(turn, r.entry).map(_ -> r))
      val rounds = roled.count((role, _) => role.isInstanceOf[TurnRecord.Role.Round])
      // The reply answers the last round asked; with no ask recorded, the one after the rounds.
      val replied = asked.keys.maxOption.getOrElse(rounds)
      val own = roled.collect {
        case (TurnRecord.Role.Round(n), r) => Own(n, r.model, r.usage, asked.get(n).flatten)
        case (TurnRecord.Role.Reply, r) =>
          Own(replied, r.model, r.usage, asked.get(replied).flatten)
      }
      val aside = roled.collect {
        case (role, r)
            if !(role.isInstanceOf[TurnRecord.Role.Round] || role == TurnRecord.Role.Reply) =>
          Aside(role, r.model, r.usage)
      }
      TurnCache(turn, carried, own.sortBy(_.round), aside)
    }
}
