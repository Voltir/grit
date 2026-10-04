package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Question}
import grit.core.id.{QuestionName, TurnRef, WorkflowId}
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.triage.KnowledgeSources
import grit.dbos.engine.Reader
import grit.eval.harness.corpus.{Digest, TurnCase}
import grit.eval.harness.jev.{Asking, Posed}
import grit.eval.harness.log.Weights
import grit.lifecycle.triage.{TriageInput, TriageQuestions, TriageRecipe}

/** Live triage's question set put to the message a recorded turn
  * answers: `posed` as the shipped call asks it, and the questions it names, in the order
  * asked.
  */
final case class TurnAsk(posed: Posed, questions: VectorMap[QuestionName, Question])

/** Triage's questions of the messages recorded turns answer, as triage would have asked them
  * when each turn started, whether the message was heard or said to grit.
  */
object TurnTriage {

  /** The ask of `t`'s message: its state built by the shipped builder ([[TriageInput.read]],
    * with `tuning` and the shipped recipe, [[TriageRecipe.Shipped]], whose sections it reads as
    * the database stands now) over `reader`'s database as it stood when `t` started
    * ([[AsOf]]), heard or said to grit; asked `questions` (live triage's set for the deployment
    * that recorded it, [[TriageQuestions.shipped]] of its persona), with a question per source
    * of `knowledge` whose place holds the turn's conversation's. `Left` naming what could not be read or built; never a message's
    * text.
    */
  def ask(
      reader: Reader^,
      t: TurnCase,
      knowledge: KnowledgeSources,
      questions: TriageQuestions,
      tuning: Tuning
  ): Either[String, TurnAsk] = {
    val name = WorkflowId.value(t.workflow)
    val view = AsOf(reader, t.started)
    for {
      turn <- TurnRef.fromWorkflowId(t.workflow).toRight(s"$name is not a turn")
      reads = StitchReads(
        view.entries,
        view.conversations,
        view.lifecycle,
        view.stitches,
        view.search,
        view.principals
      )
      read <- TriageInput
        .read(
          reads,
          reader.rooms,
          reader.db,
          turn,
          tuning,
          TriageRecipe.Shipped
        )
        .left
        .map(why => s"triage's question of $name not built: $why")
    } yield {
      val sources = read.place.fold(KnowledgeSources.Empty)(knowledge.at)
      val request = questions.request(read.state, sources)
      TurnAsk(
        Posed(
          Asking.Questions(questions, read.state, sources),
          request,
          Digest.json(request.state)
        ),
        questions.questions(sources)
      )
    }
  }

  /** `answers`, by position, under the names of `ask`'s questions; `None` when they are not
    * one each, of its question's kind.
    */
  def named(ask: TurnAsk, answers: Vector[Weights]): Option[VectorMap[QuestionName, Answer]] = {
    val each = ask.questions.toVector.zip(answers).flatMap { case ((n, q), w) =>
      Weights.answer(q, w).map(n -> _)
    }
    Option
      .when(answers.size == ask.questions.size && each.size == answers.size)(VectorMap.from(each))
  }
}
