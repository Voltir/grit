package grit.kit.rehearse

import grit.core.classify.{ClassifierError, ClearedClassifier, Question}
import grit.core.message.Message
import grit.core.model.{Catalog, Pinned, Policy}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.store.{Db, ModelSettingStore}
import grit.kit.deployment.Topics
import grit.models.{KeptCatalog, Seed, StubClassifier}
import grit.turn.Turn

/** A dry rehearsal's models and classifier: each call kept on a [[Tape]] with the step it is
  * made in, and answered by [[Answers]] for that step (a call made between operations as the
  * step `""`), never sent.
  */
object Taps {

  /** The seed a deployment of `policy` runs under: the checked-in catalog under `policy`;
    * `Left` when the seed cannot be read ([[Seed.catalog]]).
    */
  def seed(policy: Policy): Either[String, Catalog] = Seed.catalog.map(_.withPolicy(policy))

  /** Models under the catalog in force over `seed` (a deployment's, [[Taps.seed]]): with every
    * setting kept in `settings` laid over it, read through `db` on each call
    * ([[KeptCatalog.read]]). A call made in the turn's own step
    * ([[grit.turn.Turn.Step.answering]]) is answered only when `answers` supplies its step;
    * otherwise it is kept unanswered and throws [[Stop]].
    */
  def models(
      seed: Catalog,
      db: Db^,
      settings: ModelSettingStore,
      answers: Answers,
      tape: Tape^
  ): Models^ = new Tapped(seed, db, settings, answers, tape)

  /** The classifier a deployment of `topics` places messages with, cleared as that one is: its
    * states go to the service the live one's go to ([[Topics.JevService]], or the stub's), or,
    * for [[Topics.Off]], none, every call `Unavailable` and nothing kept.
    */
  def classifier(topics: Topics, answers: Answers, tape: Tape^): ClearedClassifier^ =
    topics match {
      case Topics.Jev => cleared(Topics.JevService, answers, tape)
      case Topics.Stub => cleared(StubClassifier.Service, answers, tape)
      case Topics.Off(reason) => ClearedClassifier.none(reason)
    }

  private def cleared(
      service: grit.core.place.Service,
      standIns: Answers,
      tape: Tape^
  ): ClearedClassifier^ =
    ClearedClassifier.of(
      service,
      new grit.core.classify.Classifier {
        protected def answer(
            state: ujson.Value,
            questions: Vector[Question]
        ): Either[ClassifierError, grit.core.classify.Answers] = {
          val step = tape.step
          val reply = standIns.judge(step.getOrElse(""), state, questions)
          tape.called(Tape.Call.Classifier(step, service, state, questions, reply))
          reply.answer
        }
      }
    )

  private final class Tapped(
      seed: Catalog,
      db: Db^,
      settings: ModelSettingStore,
      answers: Answers,
      tape: Tape^
  ) extends Models {

    def catalog(): Either[String, Catalog] = KeptCatalog.read(seed, db, settings)

    def provider(pinned: Pinned): Provider^ = new Provider {
      def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
        val step = tape.step
        val reply = answers.reply(step.getOrElse(""), request)
        step.filter(Turn.Step.answering) match {
          // The stub never stands in for the turn's own call: only a supplied answer does.
          case Some(own) if reply.by != Answers.StandIn.Supplied =>
            tape.called(Tape.Call.Model(step, pinned, request, None))
            throw Stop(own)
          case _ =>
            tape.called(Tape.Call.Model(step, pinned, request, Some(reply)))
            Right(reply.answer)
        }
      }
    }
  }
}
