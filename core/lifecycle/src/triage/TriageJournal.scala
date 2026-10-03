package grit.lifecycle.triage

import grit.core.classify.AnswersJson
import grit.core.durable.Journaled
import grit.core.id.{EntryId, WorkflowId}
import grit.core.period.Probability
import grit.core.speech.{Decision, SpeechJson}
import grit.core.stitch.{Placed, StitchJson}
import grit.core.store.PayloadJson
import grit.core.triage.{Kind, Tags}

/** How the triage's step outputs are recorded: `{"ok": value}` or `{"failed": reason}`. A
  * triage in flight must read back what an earlier build wrote, so change these only with
  * the workflow's epoch (ADR 0004).
  */
private[triage] object TriageJournal {

  /** The probabilities a build before named answers recorded, under these keys. */
  private val Weights: Vector[String] = Vector("kindP", "waiting", "durable", "helps")

  /** `tags` as recorded: `{"answers": [...], "model", "usage"}`, the answers as
    * [[AnswersJson.writeNamed]] writes them; or `{"unanswered": why}`.
    */
  def writeTags(tags: Tags): ujson.Value = tags match {
    case Tags.Weighed(answers, model, usage) =>
      ujson.Obj(
        "answers" -> AnswersJson.writeNamed(answers),
        "model" -> model,
        "usage" -> PayloadJson.writeUsage(usage)
      )
    case Tags.Unanswered(why) => ujson.Obj("unanswered" -> why)
  }

  /** What [[writeTags]] wrote; or what a build before named answers wrote, `{"kind",
    * "kindP", "waiting", "durable", "helps", "model", "usage"}`, read as v1's answers
    * ([[Tags.V1.answers]]).
    */
  def readTags(v: ujson.Value): Either[String, Tags] =
    v.objOpt.toRight("tags: expected an object").flatMap { o =>
      (o.get("unanswered"), o.get("answers")) match {
        case (Some(ujson.Str(why)), _) => Right(Tags.Unanswered(why))
        case (Some(_), _) => Left("tags: unanswered is not a string")
        case (None, answered) =>
          for {
            answers <- answered.fold(v1(o))(AnswersJson.readNamed)
            model <- o.get("model").flatMap(_.strOpt).toRight("tags: missing model")
            usage <- o.get("usage").toRight("tags: missing usage").flatMap(PayloadJson.readUsage)
          } yield Tags.Weighed(answers, model, usage)
      }
    }

  /** The answers a build before named answers recorded in `o`, as v1's. */
  private def v1(o: collection.Map[String, ujson.Value]) = {
    val ps = Weights.flatMap(k => o.get(k).flatMap(_.numOpt).flatMap(Probability.of))
    for {
      kind <- o.get("kind").flatMap(_.strOpt).flatMap(Kind.read).toRight("tags: bad kind")
      answers <- ps match {
        case Vector(kindP, waiting, durable, helps) =>
          Right(Tags.V1.answers(kind, kindP, waiting, durable, helps))
        case _ => Left("tags: expected four probabilities")
      }
    } yield answers
  }

  given asked: Journaled[Either[String, (EntryId, Tags)]] =
    outcome(
      (entry, tags) => ujson.Obj("entry" -> EntryId.value(entry), "tags" -> writeTags(tags)),
      v =>
        for {
          o <- v.objOpt.toRight("asked: expected an object")
          entry <- o.get("entry").flatMap(_.strOpt).toRight("asked: missing entry")
          tags <- o.get("tags").toRight("asked: missing tags").flatMap(readTags)
        } yield (EntryId(entry), tags)
    )

  /** A `stitch` step's output: the heard message and where it was placed
    * ([[StitchJson.write]]), or `null` when nothing was asked.
    */
  given stitched: Journaled[Either[String, Option[(EntryId, Placed)]]] =
    outcome(StitchJson.writeKept, StitchJson.readKept)

  /** A `stitched` step's output: what the placement it waited for did, or `null` when it
    * waited for none.
    */
  given waited: Journaled[Either[String, Option[String]]] =
    outcome(
      _.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
      {
        case ujson.Null => Right(None)
        case ujson.Str(what) => Right(Some(what))
        case _ => Left("placed: expected a string or null")
      }
    )

  /** A `consider` step's output: the decision ([[SpeechJson.writeDecision]]). */
  given considered: Journaled[Either[String, Decision]] =
    outcome(SpeechJson.writeDecision, SpeechJson.readDecision)

  /** A `start` step's output: the workflow id of the turn it queued. */
  given started: Journaled[Either[String, WorkflowId]] =
    outcome(
      w => ujson.Str(WorkflowId.value(w)),
      v => v.strOpt.map(WorkflowId(_)).toRight("started: expected a workflow id")
    )

  given recorded: Journaled[Either[String, Boolean]] =
    outcome(ujson.Bool(_), _.boolOpt.toRight("recorded: expected a boolean"))

  private def outcome[A](
      write: A -> ujson.Value,
      read: ujson.Value -> Either[String, A]
  ): Journaled[Either[String, A]] =
    Journaled.json[Either[String, A]](
      {
        case Right(a) => ujson.Obj("ok" -> write(a))
        case Left(reason) => ujson.Obj("failed" -> reason)
      },
      v =>
        v.objOpt match {
          case None => Left("expected an object")
          case Some(o) =>
            (o.get("ok"), o.get("failed").flatMap(_.strOpt)) match {
              case (Some(value), None) => read(value).map(Right(_))
              case (None, Some(reason)) => Right(Left(reason))
              case _ => Left("expected {ok} or {failed}")
            }
        }
    )
}
