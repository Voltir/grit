package grit.kit.rehearse

import grit.core.classify.ClearedClassifier
import grit.core.clock.Clock
import grit.core.id.{ShadowName, SourceId}
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.dbos.engine.{Rehearsing, Starts}
import grit.dbos.sql.DbConfig
import grit.kit.deployment.Deployment
import grit.kit.run.Launch
import grit.lifecycle.shadow.ShadowAsking

/** Rehearsing a situation through a deployment's shipped workflow bodies. */
object Rehearse {

  /** `situation` run through `d`'s shipped workflow bodies over the database `config` names, as
    * it stands now, every model and classifier call answered by `answers` and none sent: each
    * workflow the situation starts runs in the order the engine starts it, until the turn's
    * own model call ([[grit.turn.Turn.Step.answering]]), which is shown and never answered.
    * The message and every workflow are dated by `clock`. Nothing written outlives it
    * ([[Rehearsing]]). `Left` when the seed catalog cannot be read, the database cannot be
    * opened, `d`'s lifecycle settings or schedules cannot be written there, the inbox refuses
    * the message (one at a place `d` does not serve among them), or the rehearsal outlasts
    * [[Rehearsing.Within]].
    */
  def dry(
      config: DbConfig,
      d: Deployment,
      situation: Situation,
      answers: Answers,
      clock: Clock^
  ): Either[String, Rehearsed] =
    Taps.seed(d.policy).flatMap { seed =>
      val tape = new Tape
      val runner = new RehearsalRunner(arrives(situation), tape)
      Rehearsing(config, d.budget, d.visibility, clock, runner) { r =>
        val classifier = Taps.classifier(d.topics, answers, tape)
        // Launch.asking throws only for what its deployment cannot write or offer.
        try {
          Launch.asking(
            r,
            d,
            shadowing(d, classifier),
            Taps.models(seed, r.db, r.modelSettings, answers, tape),
            classifier,
            classifier,
            _ => ()
          )
          None
        } catch { case e: IllegalStateException => Some(e.getMessage) }
      } match {
        case Left(Rehearsing.Unrehearsed.Unopened(why)) => Left(s"the database did not open: $why")
        case Left(Rehearsing.Unrehearsed.Overran(within)) =>
          Left(s"the rehearsal outlasted $within and was ended")
        case Right(Some(why)) => Left(why)
        case Right(None) =>
          runner.arrived match {
            case Some(Left(refused)) => Left(s"the message was refused: $refused")
            case Some(Right(_)) => Right(Rehearsed.of(tape.entries, runner.ran))
            // Launch.asking launches unless it throws.
            case None => Left("nothing was launched")
          }
      }
    }

  /** Each of `d`'s shadows by name, asking its question through `classifier` of the model the
    * live engine's would ask of ([[Launch.shadowModel]]).
    */
  private[rehearse] def shadowing(
      d: Deployment,
      classifier: ClearedClassifier^
  ): Map[ShadowName, ShadowAsking^{classifier}] =
    d.shadows
      .map(v => v.name -> ShadowAsking(v.questions, Launch.shadowModel(d.topics, v), classifier))
      .toMap

  /** `situation`'s message recorded through the session, as its edge would hand it over now. */
  private def arrives(situation: Situation): (r: Rehearsing^) -> Either[InboxError, Starts] =
    r => {
      val now = r.clock.now()
      // Unique within its origin, so it is never taken for a redelivery of a message held.
      val source = SourceId(s"rehearsal:${now.toEpochMilli}")
      situation match {
        case Situation.Heard(origin, by, text, reach) =>
          r.hear(origin, source, text, by, now, reach)
        case Situation.Said(origin, by, text) =>
          r.ingest(origin, source, Message.User(text), by)
      }
    }
}
