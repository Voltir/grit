package grit.core.classify

import scala.collection.immutable.VectorMap

import grit.core.id.QuestionName

/** How a classifier's answers are stored: a choice as `{"choice", "weights": [{"key", "p"}]}`,
  * a yes/no as `{"yes"}`, a score as `{"score", "levels": [p…]}`, a choice's or a score's
  * confidence not kept (it is computed from its weights); an answer kept under its
  * question's name as the same object with `"name"` first. A workflow's journal and a row
  * read back what an earlier build wrote (ADR 0004), so a form once written is read by every
  * later build.
  */
object AnswersJson {

  def write(answer: Answer): ujson.Obj = answer match {
    case Answer.Choice(choice, weights, _) =>
      ujson.Obj(
        "choice" -> choice,
        "weights" -> ujson.Arr.from(
          weights.map(w => ujson.Obj("key" -> w.key, "p" -> w.probability))
        )
      )
    case Answer.YesNo(yes) => ujson.Obj("yes" -> yes)
    case Answer.Score(score, probabilities, _) =>
      ujson.Obj("score" -> score, "levels" -> ujson.Arr.from(probabilities.map(ujson.Num(_))))
  }

  /** What [[write]] wrote, a `"name"` beside it not read; `Left` when it is neither form. */
  def read(v: ujson.Value): Either[String, Answer] =
    v.objOpt.toRight("answer: expected an object").flatMap { o =>
      (
        o.get("yes").flatMap(_.numOpt),
        o.get("choice").flatMap(_.strOpt),
        o.get("score").flatMap(_.numOpt)
      ) match {
        case (Some(yes), None, None) => Right(Answer.YesNo(yes))
        case (None, None, Some(score)) =>
          o.get("levels")
            .flatMap(_.arrOpt)
            .toRight("answer: missing levels")
            .flatMap(levels =>
              levels.toVector
                .foldLeft[Option[Vector[Double]]](Some(Vector.empty))((acc, p) =>
                  acc.flatMap(done => p.numOpt.map(done :+ _))
                )
                .toRight("answer: a level's weight is not a number")
            )
            .map(ps => Answer.Score(score, ps, Answer.scoreConfidence(ps)))
        case (None, Some(choice), None) =>
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
        case _ => Left("answer: expected {yes}, {choice, weights} or {score, levels}")
      }
    }

  /** `answers` in order, each as [[write]] writes it with its question's `"name"` first. */
  def writeNamed(answers: VectorMap[QuestionName, Answer]): ujson.Value =
    ujson.Arr.from(answers.toVector.map { (name, a) =>
      ujson.Obj.from(("name" -> ujson.Str(QuestionName.value(name))) +: write(a).value.toSeq)
    })

  /** What [[writeNamed]] wrote, in order; `Left` when it is not an array, an answer has no
    * name, a name repeats or does not read ([[QuestionName.read]]), or an answer does not
    * read.
    */
  def readNamed(v: ujson.Value): Either[String, VectorMap[QuestionName, Answer]] =
    v.arrOpt.toRight("answers: expected an array").flatMap { arr =>
      arr.toVector
        .foldLeft[Either[String, Vector[(QuestionName, Answer)]]](Right(Vector.empty)) { (acc, a) =>
          acc.flatMap(done =>
            for {
              name <- a.objOpt
                .flatMap(_.get("name"))
                .toRight("answers: an answer has no name")
                .flatMap(_.strOpt.toRight("answer: name is not a string"))
                .flatMap(QuestionName.read)
              answer <- read(a)
            } yield done :+ (name -> answer)
          )
        }
        .flatMap { read =>
          val names = read.map(_._1)
          names.diff(names.distinct).headOption match {
            case Some(twice) => Left(s"answers: ${QuestionName.value(twice)} is named twice")
            case None => Right(VectorMap.from(read))
          }
        }
    }
}
