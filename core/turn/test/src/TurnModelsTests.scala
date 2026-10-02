package grit.turn

import java.time.LocalDate

import grit.core.durable.InMemoryDurable
import grit.core.id.{ToolCallId, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{
  AfterToolResult,
  Catalog,
  Known,
  NameRepair,
  Pinned,
  Profile,
  Source,
  StrictSchemas,
  ToolGuidance,
  TurnProfile,
  TurnProfileId
}
import grit.core.provider.{Models, Provider}
import grit.core.store.{Entry, InMemoryEntryStore, InMemoryModelProfileStore, Payload}
import grit.core.tool.Gate
import grit.dbos.sql.TestTx

import utest.*

/** How a turn pins its models: the catalog in force when it starts is its profile, kept, and
  * every call it makes, then or after a restart, is made under that profile's pins.
  */
object TurnModelsTests extends TestSuite {
  import TurnFixtures.*

  /** Models whose catalog in force is `now`, which a test may change between runs; every
    * call goes to `calls`, and each pin a provider was asked for is kept in `asked`.
    */
  final class Switching(
      calls: Provider^,
      @caps.unsafe.untrackedCaptures var now: Either[String, Catalog]
  ) extends Models {
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[Pinned]

    def catalog(): Either[String, Catalog] = now
    def provider(pinned: Pinned): Provider^ = {
      asked = asked :+ pinned
      calls
    }
  }

  /** [[TestCatalog]] with the turn's budget changed: another version, another turn pin. */
  private val Changed: Catalog =
    TestCatalog.withPolicy(
      TestCatalog.policy.copy(turn = TestCatalog.policy.turn.copy(maxTokens = 100))
    )

  private val isWindow: Entry -> Boolean = _.payload match {
    case Payload.Window(_, _, _) => true
    case _ => false
  }

  val tests = Tests {
    test("a turn pins the catalog in force first, keeps it, and calls each role under its pin") {
      val entries = new InMemoryEntryStore
      val profiles = new InMemoryModelProfileStore
      val provider = new RecordingProvider
      val models = new Switching(provider, Right(TestCatalog))
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(modelsBody(entries, models, profiles, NoCheckout, noTools))
      durable.recordedSteps(turn.workflowId).headOption ==> Some("pin-models")
      profiles.of(turn.workflowId)(using TestTx.fake) ==> Right(Some(TestCatalog.pin))
      // The answer under the turn's pin, then the summary under the summary's.
      models.asked ==> Vector(TestCatalog.pin.turn, TestCatalog.pin.summary)
    }

    test("a catalog changed before a restart does not change a crashed turn's calls") {
      val store = new InMemoryEntryStore
      val entries = new CrashOnInsert(store, isWindow)
      val profiles = new InMemoryModelProfileStore
      val provider = new RecordingProvider
      val models = new Switching(provider, Right(TestCatalog))
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      assertThrows[InMemoryDurable.Crash](
        durable.run(turn.workflowId)(modelsBody(entries, models, profiles, NoCheckout, noTools))
      )
      provider.requests ==> Vector.empty
      models.now = Right(Changed)
      durable.run(turn.workflowId)(modelsBody(entries, models, profiles, NoCheckout, noTools))
      models.asked ==> Vector(TestCatalog.pin.turn, TestCatalog.pin.summary)
      profiles.of(turn.workflowId)(using TestTx.fake) ==> Right(Some(TestCatalog.pin))
      // A turn started after the change pins the new catalog.
      val next = say(store, "again")
      durable.run(next.workflowId)(modelsBody(entries, models, profiles, NoCheckout, noTools))
      profiles.of(next.workflowId)(using TestTx.fake) ==> Right(Some(Changed.pin))
    }

    test("pin-models journals the profile's id, and the turn runs under the profile kept by it") {
      val entries = new InMemoryEntryStore
      val profiles = new InMemoryModelProfileStore
      val provider = new RecordingProvider
      val models = new Switching(provider, Right(TestCatalog))
      val durable = new InMemoryDurable
      def pinned(profile: TurnProfile): InMemoryDurable.Step =
        InMemoryDurable.Step(
          "pin-models",
          InMemoryDurable.Outcome.Output(
            ujson.Obj("ok" -> TurnProfileId.value(profile.id)).render()
          )
        )
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(modelsBody(entries, models, profiles, NoCheckout, noTools))
      // The step's stored form: the profile's id; `model_profiles` keeps the profile.
      durable.history(turn.workflowId).headOption ==> Some(pinned(TestCatalog.pin))
      // A turn whose step recorded another profile's id runs under the profile kept by that
      // id, whatever the catalog in force.
      profiles.pin(WorkflowId("another-turn"), Changed.pin)(using TestTx.fake) ==> Right(())
      val later = new Switching(provider, Right(TestCatalog))
      val next = say(entries, "again")
      val _ = durable.replay(next.workflowId, Vector(pinned(Changed.pin)))(
        modelsBody(entries, later, profiles, NoCheckout, noTools)
      )
      later.asked ==> Vector(Changed.pin.turn, Changed.pin.summary)
    }

    test("no catalog: the turn fails at its first step and calls nothing") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val models = new Switching(provider, Left("catalog.json is not JSON"))
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(
        modelsBody(entries, models, new InMemoryModelProfileStore, NoCheckout, noTools)
      ) ==> "failed: Model(no model catalog: catalog.json is not JSON)"
      durable.recordedSteps(turn.workflowId) ==> Vector("pin-models")
      models.asked ==> Vector.empty
    }

    test("a turn's tool schemas are strict only when its pair is known to enforce them") {
      def sent(catalog: Catalog): Vector[Boolean] = {
        val entries = new InMemoryEntryStore
        val provider = new RecordingProvider
        val ws = new Files(Map.empty)
        val durable = new InMemoryDurable
        val turn = say(entries, "hello")
        durable.run(turn.workflowId)(
          modelsBody(
            entries,
            new Switching(provider, Right(catalog)),
            new InMemoryModelProfileStore,
            ws,
            tools(ws)
          )
        )
        provider.requests.headOption.toVector.flatMap(_.tools.map(_.strict))
      }
      def known(s: StrictSchemas) =
        TestCatalog.overlaid(
          Vector(
            Profile(
              TestCatalog.policy.turn.ref,
              strict = Known.Of(s, Source.Declared("nick", LocalDate.of(2026, 9, 25)))
            )
          )
        )
      sent(known(StrictSchemas.Enforced)) ==> Vector(true, true)
      sent(known(StrictSchemas.WhenRequired)) ==> Vector(false, false)
      sent(TestCatalog) ==> Vector(false, false)
    }

    test("a turn guides, reads and tells its model as its pin says") {
      val nick = Source.Declared("nick", LocalDate.of(2026, 9, 25))
      val pair = Profile(
        TestCatalog.policy.turn.ref,
        names = Known.Of(NameRepair.AsSent, nick),
        afterResult = Known.Of(AfterToolResult.InLastResult, nick),
        guidance = Known.Of(ToolGuidance.SystemLines, nick)
      )
      val leaked: Message.Assistant = Message.Assistant(
        Vector(
          AssistantBlock
            .ToolCall(ToolCallId("t1"), "peek<|channel|>x", ujson.Obj("path" -> "a.txt"))
        ),
        StopReason.ToolUse,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
        "m"
      )
      val provider = new Scripted((_, n) => Right(if (n == 0) leaked else said("done")))
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(
        modelsBody(
          entries,
          new Switching(provider, Right(TestCatalog.overlaid(Vector(pair)))),
          new InMemoryModelProfileStore,
          ws,
          tools(ws),
          calls = 2
        )
      )
      provider.requests.headOption.map(_.system) ==>
        Some(
          s"$system\n\nTools you may call:\n- peek: Reads a file.\n" +
            s"- poke: Reads a file, asking first. ${Gate.AsksFirst}"
        )
      // Not cut at `<|`: no tool has the name as sent, so nothing was read.
      ws.reads ==> 0
      provider.requests.lift(1).flatMap(_.messages.lastOption) ==> Some(
        Message.ToolResult(
          ToolCallId("t1"),
          "There is no tool named `peek<|channel|>x`; the tools are `peek`, `poke`." +
            s"\n\n${TurnLoop.LastCall}",
          isError = true
        )
      )
    }
  }
}
