package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Question}
import grit.core.id.{
  ConversationId,
  EntryId,
  EntrySeq,
  QuestionName,
  TriageRef,
  TurnRef,
  WorkflowId
}
import grit.core.message.Message
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Entry, EntryStore, Payload, StoreError, Tx}
import grit.core.triage.KnowledgeSources
import grit.dbos.engine.Reader
import grit.eval.harness.corpus.{Capture, Digest, TurnCase}
import grit.eval.harness.jev.{Asking, Posed}
import grit.eval.harness.log.Weights
import grit.lifecycle.triage.{TriageInput, TriageQuestions, TriageRecipe}

/** Live triage's question set ([[TriageQuestions.Shipped]]) put to the message a recorded turn
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
    * ([[AsOf]]), a message said to grit read as one heard, since triage builds its question of
    * heard messages alone; with a question per source of `knowledge` whose place holds the
    * turn's conversation's. `Left` naming what could not be read or built; never a message's
    * text.
    */
  def ask(
      reader: Reader^,
      t: TurnCase,
      knowledge: KnowledgeSources,
      tuning: Tuning
  ): Either[String, TurnAsk] = {
    val name = WorkflowId.value(t.workflow)
    val view = AsOf(reader, t.started)
    for {
      turn <- TurnRef.fromWorkflowId(t.workflow).toRight(s"$name is not a turn")
      period <- reader.db
        .read(reader.periods.of(turn))
        .left
        .map(e => s"period of $name unread: ${Capture.kind(e)}")
        .flatMap(_.toRight(s"$name has no period"))
      reads = StitchReads(
        new AsHeard(view.entries, turn),
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
          TriageRef(period.ref, turn.turnSeq),
          tuning,
          TriageRecipe.Shipped
        )
        .left
        .map(why => s"triage's question of $name not built: $why")
    } yield {
      val sources = read.place.fold(KnowledgeSources.Empty)(knowledge.at)
      val request = TriageQuestions.Shipped.request(read.state, sources)
      TurnAsk(
        Posed(
          Asking.Questions(TriageQuestions.Shipped, read.state, sources),
          request,
          Digest.json(request.state)
        ),
        TriageQuestions.Shipped.questions(sources)
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

  /** `under`, with the first entry of `turn` read as heard when it is a person's message. */
  private final class AsHeard(under: EntryStore, turn: TurnRef) extends EntryStore {
    private def first(es: Vector[Entry]): Option[EntryId] =
      es.filter(_.turnSeq == turn.turnSeq).minByOption(e => EntrySeq.value(e.seq)).map(_.id)
    private def heard(es: Vector[Entry], id: Option[EntryId]): Vector[Entry] =
      es.map(e =>
        e.payload match {
          case Payload.Message(Message.User(text)) if id.contains(e.id) =>
            e.copy(payload = Payload.Heard(text))
          case _ => e
        }
      )
    private def mine(es: Vector[Entry])(using Tx^): Either[StoreError, Vector[Entry]] =
      under.ofTurn(turn).map(own => heard(es, first(own)))

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = under.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] =
      under.get(id).flatMap(e => mine(e.toVector).map(_.headOption))
    def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
      under.list(conversation).flatMap(mine)
    def at(conversation: ConversationId, seqs: Vector[EntrySeq])(using
        Tx^
    ): Either[StoreError, Vector[Entry]] = under.at(conversation, seqs).flatMap(mine)
    def ofTurn(t: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] =
      under.ofTurn(t).flatMap(mine)
    def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      under.lockNext(conversation)
  }
}
