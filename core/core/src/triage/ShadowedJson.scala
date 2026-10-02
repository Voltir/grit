package grit.core.triage

import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError}
import grit.core.store.PayloadJson

/** How a [[Shadowed]] and its answers are stored: a shadow workflow's journal and its row
  * read back what an earlier build wrote, so change these only with the epoch (ADR 0004).
  */
object ShadowedJson {

  /** `answers` in order: a choice as `{"choice", "weights": [{"key", "p"}]}`, a yes/no as
    * `{"yes"}`; a choice's confidence is not kept, it is computed from its weights.
    */
  def writeAnswers(answers: Vector[Answer]): ujson.Value =
    ujson.Arr.from(answers.map {
      case Answer.Choice(choice, weights, _) =>
        ujson.Obj(
          "choice" -> choice,
          "weights" -> ujson.Arr.from(
            weights.map(w => ujson.Obj("key" -> w.key, "p" -> w.probability))
          )
        )
      case Answer.YesNo(yes) => ujson.Obj("yes" -> yes)
    })

  def readAnswers(v: ujson.Value): Either[String, Vector[Answer]] =
    v.arrOpt.toRight("answers: expected an array").flatMap { arr =>
      arr.toVector.foldLeft[Either[String, Vector[Answer]]](Right(Vector.empty)) { (acc, a) =>
        acc.flatMap(done => answer(a).map(done :+ _))
      }
    }

  private def answer(v: ujson.Value): Either[String, Answer] =
    v.objOpt.toRight("answer: expected an object").flatMap { o =>
      (o.get("yes").flatMap(_.numOpt), o.get("choice").flatMap(_.strOpt)) match {
        case (Some(yes), None) => Right(Answer.YesNo(yes))
        case (None, Some(choice)) =>
          o.get("weights")
            .flatMap(_.arrOpt)
            .toRight("answer: missing weights")
            .flatMap(
              _.toVector.foldLeft[Either[String, Vector[Answer.Weight]]](Right(Vector.empty)) {
                (acc, w) =>
                  acc.flatMap(done =>
                    (for {
                      wo <- w.objOpt
                      key <- wo.get("key").flatMap(_.strOpt)
                      p <- wo.get("p").flatMap(_.numOpt)
                    } yield done :+ Answer.Weight(key, p))
                      .toRight("answer: a weight is not {key, p}")
                  )
              }
            )
            .map(ws => Answer.Choice(choice, ws, Answer.confidence(ws.map(_.probability))))
        case _ => Left("answer: expected {yes} or {choice, weights}")
      }
    }

  /** `row` as a journal records it: `{"answered": {...}}` or `{"failed": {...}}`. */
  def write(row: Shadowed): ujson.Value = row match {
    case Shadowed.Answered(request, answers, usage, requested, answered, latency) =>
      ujson.Obj(
        "answered" -> ujson.Obj(
          "request" -> request,
          "answers" -> writeAnswers(answers),
          "usage" -> PayloadJson.writeUsage(usage),
          "requested" -> requested,
          "model" -> answered,
          "ms" -> latency.toMillis.toDouble
        )
      )
    case Shadowed.Failed(request, failure, latency) =>
      ujson.Obj(
        "failed" -> ujson.Obj(
          "request" -> request,
          "kind" -> kindWritten(failure),
          "ms" -> latency.toMillis.toDouble
        )
      )
  }

  def read(v: ujson.Value): Either[String, Shadowed] =
    v.objOpt.toRight("shadowed: expected an object").flatMap { o =>
      def str(f: ujson.Obj, key: String) =
        f.value.get(key).flatMap(_.strOpt).toRight(s"shadowed: missing $key")
      def ms(f: ujson.Obj) =
        f.value.get("ms").flatMap(_.numOpt).map(_.toLong.millis).toRight("shadowed: missing ms")
      (o.get("answered").flatMap(_.objOpt), o.get("failed").flatMap(_.objOpt)) match {
        case (Some(a), None) =>
          val f = ujson.Obj.from(a)
          for {
            request <- str(f, "request")
            answers <- f.value
              .get("answers")
              .toRight("shadowed: missing answers")
              .flatMap(readAnswers)
            usage <- f.value
              .get("usage")
              .toRight("shadowed: missing usage")
              .flatMap(PayloadJson.readUsage)
            requested <- str(f, "requested")
            model <- str(f, "model")
            latency <- ms(f)
          } yield Shadowed.Answered(request, answers, usage, requested, model, latency)
        case (None, Some(a)) =>
          val f = ujson.Obj.from(a)
          for {
            request <- str(f, "request")
            kind <- str(f, "kind").flatMap(kindRead)
            latency <- ms(f)
          } yield Shadowed.Failed(request, kind, latency)
        case _ => Left("shadowed: expected {answered} or {failed}")
      }
    }

  /** `kind` as stored: `unavailable` or `unreadable`. */
  def kindWritten(kind: ClassifierError.Kind): String = kind match {
    case ClassifierError.Kind.Unavailable => "unavailable"
    case ClassifierError.Kind.Unreadable => "unreadable"
  }

  def kindRead(s: String): Either[String, ClassifierError.Kind] = s match {
    case "unavailable" => Right(ClassifierError.Kind.Unavailable)
    case "unreadable" => Right(ClassifierError.Kind.Unreadable)
    case other => Left(s"shadowed: unknown failure kind $other")
  }
}
