package grit.kit.rehearse

import java.nio.file.Path

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

  /** `docker-compose.yml`'s database, the operator's own: [[dry]] refuses one of its name. */
  val Own: String = DbConfig.DefaultDatabase

  /** Why [[dry]] refuses a database named [[Own]]. */
  val Mine: String =
    "a rehearsal never runs on the operator's own database: name another in GRIT_DATABASE_URL"

  /** Why [[dry]] refuses a database URL that names no database. */
  val Unnamed: String = "a rehearsal runs only on a database its URL names"

  /** `situation` run through `d`'s shipped workflow bodies over the database `config` names, as
    * it stands now, every model and classifier call answered by `answers` and none sent: each
    * workflow the situation starts runs in the order the engine starts it, until the turn's
    * own model call ([[grit.turn.Turn.Step.answering]]), shown and never answered, unless
    * `answers` supplies its step: then the turn goes on with that reply, a heard message's
    * draft judged, and stops before [[grit.turn.Turn.Step.RecordSpeech]] would post it.
    * The message and every workflow are dated by `clock`. Nothing written outlives it
    * ([[Rehearsing]]). `Left`, opening nothing, when `config`'s URL names no database
    * ([[Unnamed]]) or names [[Own]] ([[Mine]]); `Left`
    * when the seed catalog cannot be read, the database cannot be
    * opened, `d`'s lifecycle settings or schedules cannot be written there, the inbox refuses
    * the message (one at a place `d` does not serve among them), or the rehearsal outlasts
    * [[Rehearsing.Within]].
    */
  def dry(
      config: DbConfig,
      d: Deployment,
      situation: Situation,
      answers: StandIns,
      clock: Clock^
  ): Either[String, Rehearsed] =
    rehearsable(config).flatMap(_ => Taps.seed(d.policy)).flatMap { seed =>
      val tape = new Tape
      val runner = new RehearsalRunner(arrives(situation), tape)
      Rehearsing(config, d.budget, d.visibility, clock, runner) { r =>
        val classifier = Taps.classifier(d.placer, answers, tape)
        // Launch.asking throws only for what its deployment cannot write or offer.
        try {
          Launch.asking(
            r,
            d,
            shadowing(d, classifier),
            Taps.models(seed, r.db, r.modelSettings, answers, tape),
            classifier,
            classifier,
            Launch.area(
              r,
              d,
              Launch.filesDir(None, config, Path.of(System.getProperty("java.io.tmpdir")))
            ),
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

  /** `config`, unless its URL names no database ([[Unnamed]]) or names [[Own]], whatever its
    * host ([[Mine]]).
    */
  private def rehearsable(config: DbConfig): Either[String, DbConfig] =
    config.endpoint.database match {
      case None => Left(Unnamed)
      case Some(Own) => Left(Mine)
      case Some(_) => Right(config)
    }

  /** Each of `d`'s shadows by name, asking its question through `classifier` of the model the
    * live engine's would ask of ([[Launch.shadowModel]]).
    */
  private[rehearse] def shadowing(
      d: Deployment,
      classifier: ClearedClassifier^
  ): Map[ShadowName, ShadowAsking^{classifier}] =
    d.shadows
      .map(v => v.name -> ShadowAsking(v.questions, Launch.shadowModel(d.placer, v), classifier))
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
