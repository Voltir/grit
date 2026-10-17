package grit.kit.rehearse

import java.time.{Instant, LocalDate}

import grit.core.classify.{Ask, ClassifierError, StateJson}
import grit.core.durable.StepRecord
import grit.core.id.WorkflowId
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{Catalog, Known, NameRepair, Profile, Source}
import grit.core.provider.ModelRequest
import grit.core.route.Routed
import grit.core.store.{Db, InMemoryModelSettingStore, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx
import grit.kit.deployment.Topics
import grit.models.{Seed, StubModels}

import utest.*
import Rehearsal.Event
import Tape.{Call, Entry}

/** The dry rehearsal's taps: each call kept on the tape with its step and answered by its
  * stand-in, the turn's own call stopped, and the deployment's catalog read on each call.
  */
object TapsTests extends TestSuite {

  private object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private val seed: Catalog = Seed.catalog.getOrElse(sys.error("the seed"))

  /** A policy other than the seed's own, so the catalog shows whose it is. */
  private val policy = StubModels.Catalog.policy

  private val request = ModelRequest("system", Vector(Message.User("lunch?")))

  private given StateJson[String] = StateJson.instance(s => ujson.Str(s))

  /** The tapped models over `settings`, answering by `answers`, keeping on `tape`. */
  private def tapped(answers: Answers, tape: Tape, settings: InMemoryModelSettingStore) =
    Taps.models(Taps.seed(policy).fold(sys.error, identity), FakeDb, settings, answers, tape)

  val tests = Tests {
    test(
      "the turn's own call is kept unanswered and stops the rehearsal: the step begun, the call, the step ended with no output"
    ) {
      val tape = new Tape
      val models = tapped(Answers.Stubbed, tape, new InMemoryModelSettingStore)
      val pinned = StubModels.Catalog.pin.turn.first
      val d = new Rehearsal(WorkflowId("c:0"), new RehearsalTests.Writes, tape)
      val stopped =
        try {
          d.step("ask:0")(() => models.provider(pinned).complete(request).toString)
          None
        } catch { case s: Stop => Some(s) }
      stopped ==> Some(Stop("ask:0"))
      tape.entries.takeRight(3) ==> Vector(
        Entry.Told(Event.Began("ask:0")),
        Entry.Called(Call.Model(Some("ask:0"), pinned, request, None)),
        Entry.Told(Event.Ended(StepRecord("ask:0", None, None)))
      )
    }

    test(
      "a model's call in any other step is answered by the stand-in for its step and kept with its pin and request"
    ) {
      val tape = new Tape
      val answers = Answers.supplied(Map("ask:query" -> "lunch places"))
      val models = tapped(answers, tape, new InMemoryModelSettingStore)
      val pinned = StubModels.Catalog.pin.query.first
      val d = new Rehearsal(WorkflowId("c:0"), new RehearsalTests.Writes, tape)
      val query = d.step("ask:query")(() =>
        models.provider(pinned).complete(request).fold(_.toString, _.blocks.toString)
      )
      val between = models.provider(pinned).complete(request)
      query ==> Vector(AssistantBlock.Text("lunch places")).toString
      tape.entries.collect { case Entry.Called(c) => c } ==> Vector(
        Call.Model(Some("ask:query"), pinned, request, Some(answers.reply("ask:query", request))),
        Call.Model(None, pinned, request, Some(answers.reply("", request)))
      )
      between.map(_.model) ==> Right(grit.models.StubProvider.Model)
    }

    test(
      "the catalog is the seed under the deployment's policy, with the settings kept by each call laid over it"
    ) {
      val settings = new InMemoryModelSettingStore
      val models = tapped(Answers.Stubbed, new Tape, settings)
      val ours = seed.withPolicy(policy)
      models.catalog().map(_.version) ==> Right(ours.version)
      val asSent =
        Profile(
          policy.turn.first.assignment.ref,
          names = Known.Of(
            NameRepair.AsSent,
            Source.Measured("probe", LocalDate.of(2026, 10, 17), 4, 4)
          )
        )
      settings.keep(asSent, "nick", Instant.EPOCH)(using TestTx.fake)
      models.catalog().map(_.version) ==> Right(ours.overlaid(Vector(asSent)).version)
    }

    test(
      "a classifier's call goes to the service the deployment's topics name, answered by the stand-in for its step and kept"
    ) {
      val tape = new Tape
      val answers = Answers.supplied(Map("ask:triage" -> "0.9"))
      val jev = Taps.classifier(Topics.Jev, answers, tape)
      val routed = Routed.of(using TestTx.fake)
      val asked = Ask.yesNo[String]("Is it about lunch?", None, None)
      val d = new Rehearsal(WorkflowId("c:0"), new RehearsalTests.Writes, tape)
      val got =
        d.step("ask:triage")(() => jev.asking(routed).ask("lunch?", asked).map(_.value).toString)
      jev.upstream ==> Some("service:jev")
      got ==> Right(0.9).toString
      tape.entries.collect { case Entry.Called(c) => c } ==> Vector(
        Call.Classifier(
          Some("ask:triage"),
          Topics.JevService,
          ujson.Str("lunch?"),
          asked.questions,
          answers.judge("ask:triage", ujson.Str("lunch?"), asked.questions)
        )
      )
    }

    test("topics off ask nothing: every call Unavailable, nothing kept") {
      val tape = new Tape
      val off = Taps.classifier(Topics.Off("no topics here"), Answers.Stubbed, tape)
      val asked = Ask.yesNo[String]("Is it about lunch?", None, None)
      off.asking(Routed.of(using TestTx.fake)).ask("lunch?", asked) ==>
        Left(ClassifierError.Unavailable("no topics here"))
      (off.upstream, tape.entries) ==> (None, Vector())
    }
  }
}
