package grit.kit.rehearse

import scala.util.Try

import grit.core.classify.{Answer, ClassifierError, Question}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.provider.{ModelRequest, ToolUse}
import grit.models.{StubClassifier, StubProvider}

/** How a dry rehearsal's calls are answered: every answer is shown as the stand-in it is. */
sealed trait Answers extends caps.Pure {

  /** The stand-in reply to `request`, a call made in the step `step`. */
  def reply(step: String, request: ModelRequest): Answers.Reply[Message.Assistant]

  /** The stand-in answers to `questions` about `state`, a classifier's call made in the step
    * `step`; `Unreadable` when the text supplied for it does not read as [[Answers.supplied]]
    * says.
    */
  def judge(
      step: String,
      state: ujson.Value,
      questions: Vector[Question]
  ): Answers.Reply[Either[ClassifierError, grit.core.classify.Answers]]
}

object Answers {

  /** What stood in for a model or a classifier. */
  enum StandIn {

    /** grit.models' stub. */
    case Stub

    /** A text supplied for the step. */
    case Supplied
  }

  /** A stand-in's answer, and which stand-in gave it. */
  final case class Reply[+A](by: StandIn, answer: A)

  /** The model id a supplied answer records. */
  val SuppliedModel: String = "grit/supplied"

  /** Each call answered as grit.models' stub models answer it, under their model ids, but a
    * classifier's choice: its first key at 0.9, the rest sharing 0.1 (the stub classifier's
    * own takes the last), so triage's `gap` reads as asking; [[Rehearsed.StubJudge]] says what
    * that does to the chain.
    */
  val Stubbed: Answers = Supplying(Map.empty)

  /** Each call made in a step `byStep` names answered with its text, under the model id
    * [[SuppliedModel]]; every other call as [[Stubbed]]. A model's call offered a tool it must
    * call ([[ToolUse.Required]]) calls the first with the text as its arguments (JSON, or the
    * raw string when it is not); otherwise the text is its reply. A classifier's call reads
    * the text as one answer per question, separated by `;`, each its options' probabilities
    * written `p1,p2,…` in the question's order: a yes/no question's one number, p(yes); a
    * choice's one per key; a score's one per level.
    */
  def supplied(byStep: Map[String, String]): Answers = Supplying(byStep)

  private final case class Supplying(byStep: Map[String, String]) extends Answers {

    def reply(step: String, request: ModelRequest): Reply[Message.Assistant] =
      byStep.get(step) match {
        case None =>
          Reply(
            StandIn.Stub,
            new StubProvider().complete(request).fold(e => unanswered(e.cause), identity)
          )
        case Some(text) => Reply(StandIn.Supplied, said(text, request))
      }

    def judge(
        step: String,
        state: ujson.Value,
        questions: Vector[Question]
    ): Reply[Either[ClassifierError, grit.core.classify.Answers]] =
      byStep.get(step) match {
        case None => Reply(StandIn.Stub, Right(stubbed(state, questions)))
        case Some(text) => Reply(StandIn.Supplied, judged(text, questions))
      }
  }

  private def stubbed(
      state: ujson.Value,
      questions: Vector[Question]
  ): grit.core.classify.Answers = {
    val stub = StubClassifier.answers(state, questions)
    stub.copy(answers = questions.zip(stub.answers).map {
      case (c: Question.Choice, _) =>
        val rest = 0.1 / (c.keys.size - 1)
        val ps = c.keys.zipWithIndex.map((k, i) => Answer.Weight(k.name, if (i == 0) 0.9 else rest))
        Answer.Choice(c.first.name, ps, Answer.confidence(ps.map(_.probability)))
      case (_, answer) => answer
    })
  }

  // The stub provider answers every request: this is never seen.
  private def unanswered(cause: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(cause)),
      StopReason.Other("stub"),
      Usage.Zero,
      StubProvider.Model
    )

  private def said(text: String, request: ModelRequest): Message.Assistant =
    (request.use, request.tools.headOption) match {
      case (ToolUse.Required, Some(tool)) =>
        Message.Assistant(
          Vector(
            AssistantBlock.ToolCall(
              StubProvider.CallId,
              tool.name,
              Try(ujson.read(text)).getOrElse(ujson.Str(text))
            )
          ),
          StopReason.ToolUse,
          Usage.Zero,
          SuppliedModel
        )
      case _ =>
        Message.Assistant(
          Vector(AssistantBlock.Text(text)),
          StopReason.EndTurn,
          Usage.Zero,
          SuppliedModel
        )
    }

  private def judged(
      text: String,
      questions: Vector[Question]
  ): Either[ClassifierError, grit.core.classify.Answers] = {
    val parts = text.split(";", -1).toVector.map(_.trim)
    if (parts.size != questions.size)
      Left(
        ClassifierError.Unreadable(
          s"supplied ${parts.size} answers for ${questions.size} questions"
        )
      )
    else
      parts
        .zip(questions)
        .foldLeft[Either[ClassifierError, Vector[Answer]]](Right(Vector.empty)) {
          case (acc, (part, question)) =>
            for {
              done <- acc
              ps <- numbers(part)
              answer <- answered(ps, question)
            } yield done :+ answer
        }
        .map(grit.core.classify.Answers(_, Usage.Zero, SuppliedModel))
  }

  private def numbers(part: String): Either[ClassifierError, Vector[Double]] = {
    val read = part.split(",", -1).toVector.map(p => p.trim.toDoubleOption)
    if (read.forall(_.nonEmpty)) Right(read.flatten)
    else Left(ClassifierError.Unreadable(s"not probabilities: $part"))
  }

  private def answered(ps: Vector[Double], question: Question): Either[ClassifierError, Answer] =
    question match {
      case _: Question.YesNo =>
        ps match {
          case Vector(yes) => Right(Answer.YesNo(yes))
          case _ => Left(ClassifierError.Unreadable(s"a yes/no question takes one number"))
        }
      case c: Question.Choice =>
        if (ps.size != c.keys.size)
          Left(ClassifierError.Unreadable(s"a choice of ${c.keys.size} keys takes as many numbers"))
        else
          Answer
            .choice(c.keys.zip(ps).map((k, p) => Answer.Weight(k.name, p)))
            .toRight(ClassifierError.Unreadable("no probability among the keys"))
      case s: Question.Score =>
        if (ps.size != s.levels.size)
          Left(
            ClassifierError.Unreadable(s"a score of ${s.levels.size} levels takes as many numbers")
          )
        else {
          val total = ps.sum
          if (total <= 0) Left(ClassifierError.Unreadable("no probability among the levels"))
          else {
            val weights = ps.map(_ / total)
            Right(
              Answer.Score(
                weights.zipWithIndex.map((p, i) => p * i).sum,
                weights,
                Answer.scoreConfidence(weights)
              )
            )
          }
        }
    }
}
