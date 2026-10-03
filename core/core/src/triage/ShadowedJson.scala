package grit.core.triage

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError}
import grit.core.id.QuestionName
import grit.core.store.PayloadJson

/** How a [[Shadowed]] and its answers are stored: a shadow workflow's journal and its row
  * read back what an earlier build wrote (ADR 0004), so a form once written is read by every
  * later build of the epoch. A new form is added only beside the old ones, told apart by its
  * own shape: an answer with a name is a question set's, without one a wording's.
  */
object ShadowedJson {

  /** `answers` in order: a choice as `{"choice", "weights": [{"key", "p"}]}`, a yes/no as
    * `{"yes"}`, a choice's confidence not kept (it is computed from its weights); a
    * `Named` answer as the same object with its question's `"name"` first.
    */
  def writeAnswers(answers: ShadowAnswers): ujson.Value = answers match {
    case ShadowAnswers.Worded(as) => ujson.Arr.from(as.map(written))
    case ShadowAnswers.Named(as) =>
      ujson.Arr.from(as.toVector.map { (name, a) =>
        ujson.Obj.from(("name" -> ujson.Str(QuestionName.value(name))) +: written(a).value.toSeq)
      })
  }

  private def written(answer: Answer): ujson.Obj = answer match {
    case Answer.Choice(choice, weights, _) =>
      ujson.Obj(
        "choice" -> choice,
        "weights" -> ujson.Arr.from(
          weights.map(w => ujson.Obj("key" -> w.key, "p" -> w.probability))
        )
      )
    case Answer.YesNo(yes) => ujson.Obj("yes" -> yes)
  }

  /** What [[writeAnswers]] wrote: `Worded` when no answer has a name, `Named` when every one
    * has; `Left` when some have and some not, when a name repeats or does not read
    * ([[QuestionName.read]]), or when an answer does not read.
    */
  def readAnswers(v: ujson.Value): Either[String, ShadowAnswers] =
    v.arrOpt.toRight("answers: expected an array").flatMap { arr =>
      arr.toVector
        .foldLeft[Either[String, Vector[(Option[QuestionName], Answer)]]](Right(Vector.empty)) {
          (acc, a) =>
            acc.flatMap(done =>
              (nameOf(a), answer(a)).match {
                case (Right(name), Right(answer)) => Right(done :+ (name -> answer))
                case (Left(why), _) => Left(why)
                case (_, Left(why)) => Left(why)
              }
            )
        }
        .flatMap { read =>
          val names = read.flatMap(_._1)
          if (names.isEmpty) Right(ShadowAnswers.Worded(read.map(_._2)))
          else if (names.size < read.size) Left("answers: some named, some not")
          else
            names.diff(names.distinct).headOption match {
              case Some(twice) => Left(s"answers: ${QuestionName.value(twice)} is named twice")
              case None => Right(ShadowAnswers.Named(VectorMap.from(names.zip(read.map(_._2)))))
            }
        }
    }

  /** An answer's question's name; `None` when it has none. */
  private def nameOf(v: ujson.Value): Either[String, Option[QuestionName]] =
    v.objOpt.flatMap(_.get("name")) match {
      case None => Right(None)
      case Some(name) =>
        name.strOpt.toRight("answer: name is not a string").flatMap(QuestionName.read).map(Some(_))
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
