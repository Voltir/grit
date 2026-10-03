package grit.core.triage

import scala.concurrent.duration.*

import grit.core.classify.{Answer, AnswersJson, ClassifierError}
import grit.core.store.PayloadJson

/** How a [[Shadowed]] and its answers are stored: a shadow workflow's journal and its row
  * read back what an earlier build wrote (ADR 0004), so a form once written is read by every
  * later build of the epoch. A new form is added only beside the old ones, told apart by its
  * own shape: an answer with a name is a question set's, without one a wording's.
  */
object ShadowedJson {

  /** `answers` in order, as [[AnswersJson]] stores them: a `Worded` answer without a name,
    * a `Named` one with its question's.
    */
  def writeAnswers(answers: ShadowAnswers): ujson.Value = answers match {
    case ShadowAnswers.Worded(as) => ujson.Arr.from(as.map(AnswersJson.write))
    case ShadowAnswers.Named(as) => AnswersJson.writeNamed(as)
  }

  /** What [[writeAnswers]] wrote: `Worded` when no answer has a name, `Named` when every one
    * has; `Left` when some have and some not, when a name repeats or does not read
    * ([[grit.core.id.QuestionName.read]]), or when an answer does not read.
    */
  def readAnswers(v: ujson.Value): Either[String, ShadowAnswers] =
    v.arrOpt.toRight("answers: expected an array").flatMap { arr =>
      val named = arr.count(_.objOpt.exists(_.contains("name")))
      if (named == 0)
        arr.toVector
          .foldLeft[Either[String, Vector[Answer]]](Right(Vector.empty))((acc, a) =>
            acc.flatMap(done => AnswersJson.read(a).map(done :+ _))
          )
          .map(ShadowAnswers.Worded(_))
      else if (named < arr.size) Left("answers: some named, some not")
      else AnswersJson.readNamed(v).map(ShadowAnswers.Named(_))
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
