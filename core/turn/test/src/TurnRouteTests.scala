package grit.turn

import grit.core.act.MovesFixtures.FakeDb as LabelledDb
import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.InMemoryDurable
import grit.core.model.{
  Assignment,
  Catalog,
  Grade,
  Ladder,
  ModelId,
  ModelRef,
  Pinned,
  Policy,
  Rung,
  Upstream
}
import grit.core.route.Uncleared
import grit.core.store.{Db, InMemoryEntryStore, StoreError, Tx}
import grit.core.visibility.{Clearance, Cleared, Label, Level, Party, Subject, Visibility}
import grit.dbos.sql.TestTx

import utest.*

/** A turn's calls go only to pairs cleared for what it knows (ADR 0037): the `route` step. */
object TurnRouteTests extends TestSuite {
  import TurnFixtures.*

  private val internal = Label.at(Level.Internal)
  private val restricted = Label.at(Level.Restricted)

  private def ref(model: String, upstream: Option[String]): ModelRef =
    ModelRef(
      ModelId.of(model).getOrElse(throw new java.lang.AssertionError(model)),
      upstream.map(u => Upstream.of(u).getOrElse(throw new java.lang.AssertionError(u)))
    )

  /** OpenRouter picks its upstream: no party, so cleared for public alone. */
  private val open = ref("test/open", None)
  private val flash = ref("test/flash", Some("fireworks"))
  private val summarising = ref("test/summary", None)

  private def ladder(refs: ModelRef*): Ladder =
    Ladder
      .of(refs.toVector.map(r => Rung(Grade.Standard, Assignment(r, 100, None))))
      .getOrElse(throw new java.lang.AssertionError())

  /** The turn's ladder is the open pair, then flash; summary and query the open pair alone. */
  private val catalog: Catalog =
    Catalog.of(
      Policy(ladder(open, flash), ladder(summarising), ladder(open), ladder(summarising)),
      Vector.empty
    )

  private val flashCleared: Visibility =
    labelling(
      Vector.empty,
      Vector(Cleared(Party.of(flash).getOrElse(throw new java.lang.AssertionError()), internal))
    )

  /** Records each request it is asked to assemble, reading once through the db it is given;
    * `before` runs first, as a change landing while the turn assembles.
    */
  private final class Asking(before: () => Unit = () => ()) extends ContextAssembler {
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[AssemblyRequest]
    def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] = {
      asked = asked :+ request
      before()
      db.read(Subject.Turn(request.turn))(Right(Window(Vector.empty)))
        .left
        .map(AssemblyError.Store(_))
    }
  }

  /** Reads at internal, under `visibility`, until [[rise]]; at restricted after. */
  private final class Rising(visibility: Visibility) extends Db {
    @caps.unsafe.untrackedCaptures
    var risen = false
    def rise(): Unit = risen = true
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(Clearance.of(if (risen) restricted else internal), visibility))
  }

  private def pinOf(r: ModelRef, role: Catalog => grit.core.model.Pins): Pinned =
    role(catalog).all.find(_.assignment.ref == r).getOrElse(throw new java.lang.AssertionError())

  private def routeOf(durable: InMemoryDurable, turn: grit.core.id.TurnRef) =
    durable
      .history(turn.workflowId)
      .collectFirst {
        case InMemoryDurable.Step(Turn.Step.Route, InMemoryDurable.Outcome.Output(v)) => v
      }
      .map(TurnJournal.routing.decode)

  val tests = Tests {
    test(
      "a turn answers on the first of its pairs cleared for what it knows, and the uncleared summary and query make no call"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val (provider, summarizer, assembler) =
        (new RecordingProvider, new RecordingProvider, new Asking)
      val durable = new InMemoryDurable
      val ran = durable.run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          summarizer,
          assembler,
          LabelledDb(Clearance.of(internal), flashCleared),
          catalog
        )
      )
      val flashPin = pinOf(flash, _.pin.turn)
      (
        routeOf(durable, turn),
        assembler.asked.map(_.query),
        provider.requests.size,
        summarizer.requests.size
      ) ==> (
        Some(Right(Right(Route(internal, flashPin, None, None)))),
        Vector(None),
        1,
        0
      )
      assert(
        ran.startsWith("replied:"),
        ran.contains(s"no summary: ${TurnFailure.Uncleared(internal)}")
      )
    }

    test(
      "a turn said to grit that no turn pair is cleared for is replied the fixed text, and no call is made"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val provider = new RecordingProvider
      new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          new RecordingProvider,
          new Asking,
          LabelledDb(Clearance.of(restricted), flashCleared),
          catalog
        )
      )
      (texts(entries), provider.requests.size) ==> (
        Vector("user: hello", s"assistant: ${Uncleared.reply(Uncleared(restricted))}"),
        0
      )
    }

    test("a heard turn that no turn pair is cleared for is not replied, and no call is made") {
      val entries = new InMemoryEntryStore
      val turn = hear(entries, "overheard")
      val provider = new RecordingProvider
      val ran = new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          new RecordingProvider,
          new Asking,
          LabelledDb(Clearance.of(restricted), flashCleared),
          catalog
        )
      )
      (texts(entries), provider.requests.size) ==> (Vector("heard: overheard"), 0)
      assert(ran.startsWith(s"failed: ${TurnFailure.Uncleared(restricted)}"))
    }

    test(
      "a read above the classification the turn was routed for fails its assembly, asking again, and no call is made"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val provider = new RecordingProvider
      val db = new Rising(flashCleared)
      val ran = new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          new RecordingProvider,
          new Asking(() => db.rise()),
          db,
          catalog
        )
      )
      (provider.requests.size, ran.startsWith("failed: Assembly("), ran.contains("ask again")) ==>
        (0, true, true)
    }

    test("an offer made where the turn knows more than it was routed for fails, asking again") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val provider = new RecordingProvider
      val ran =
        new InMemoryDurable(resolve = _ => Clearance.of(restricted), visibility = flashCleared)
          .run(turn.workflowId)(
            routedBody(
              entries,
              provider,
              new RecordingProvider,
              new Asking,
              LabelledDb(Clearance.of(internal), flashCleared),
              catalog
            )
          )
      (provider.requests.size, ran.startsWith("failed: Store("), ran.contains("ask again")) ==>
        (0, true, true)
    }

    test("a turn that offered before routing shipped makes each role's calls on its first pin") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val (provider, summarizer, assembler) =
        (new RecordingProvider, new RecordingProvider, new Asking)
      val durable = new InMemoryDurable(unpatched = Set(Turn.Patches.Route))
      val ran = durable.run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          summarizer,
          assembler,
          LabelledDb(Clearance.of(restricted), flashCleared),
          catalog
        )
      )
      (
        durable.recordedSteps(turn.workflowId).contains(Turn.Step.Route),
        assembler.asked.map(_.query),
        provider.requests.size,
        summarizer.requests.size
      ) ==> (false, Vector(Some(pinOf(open, _.pin.query))), 1, 1)
      assert(ran.startsWith("replied:"))
    }
  }
}
