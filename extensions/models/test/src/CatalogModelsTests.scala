package grit.models

import java.time.{Instant, LocalDate}

import grit.core.message.Message
import grit.core.model.{
  Assignment,
  Catalog,
  CatalogVersion,
  Grade,
  Known,
  ModelId,
  ModelRef,
  NameRepair,
  Pinned,
  Profile,
  Settings,
  Source,
  Upstream
}
import grit.core.provider.ModelRequest
import grit.core.secret.Variable
import grit.core.store.{Db, InMemoryModelSettingStore, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

object CatalogModelsTests extends TestSuite {

  private object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private val seed: Catalog = Seed.catalog.getOrElse(sys.error("the seed"))
  private val turnPair = seed.policy.turn.first.assignment.ref

  /** Keys with each of `variables` set. */
  private def keys(variables: Variable*): Keys =
    Keys
      .of(variables.map(Variable.value(_) -> "k").toMap)
      .toOption
      .flatten
      .getOrElse(sys.error("no keys"))

  private def pinned(model: String, upstream: String): Pinned = (for {
    id <- ModelId.of(model)
    at <- Upstream.of(upstream)
  } yield Pinned(
    CatalogVersion("v"),
    Grade.Standard,
    Assignment(ModelRef(id, Some(at)), 64, None),
    Settings.of(None)
  )).getOrElse(sys.error(s"$model @ $upstream"))

  val tests = Tests {
    test(
      "the seed's dream role is its own rung, claude-haiku-5.5 at 8192 tokens, and part of its version"
    ) {
      val haiku = ModelId.of("anthropic/claude-haiku-5.5").map(ModelRef(_, None))
      (
        haiku.map(Assignment(_, 8192, None)),
        seed.dream.first.assignment == seed.pin.summary.first.assignment,
        seed.withPolicy(seed.policy.copy(dream = seed.policy.summary)).version == seed.version
      ) ==> (Some(seed.dream.first.assignment), false, false)
    }

    test(
      "the catalog in force is the seed with every kept model setting laid over it, in the order kept"
    ) {
      val settings = new InMemoryModelSettingStore
      val models = new CatalogModels(keys(OpenRouterConfig.KeyVar), seed, FakeDb, settings)
      models.catalog() ==> Right(seed)
      val on = LocalDate.of(2026, 9, 25)
      val asSent =
        Profile(turnPair, names = Known.Of(NameRepair.AsSent, Source.Measured("probe", on, 4, 4)))
      settings.keep(asSent, "nick", Instant.EPOCH)(using TestTx.fake)
      val now = models.catalog()
      now.map(_.version) ==> Right(seed.overlaid(Vector(asSent)).version)
      assert(now.map(_.version) != Right(seed.version))
      now.map(c => Settings.of(c.profile(turnPair)).names) ==> Right(NameRepair.AsSent)
    }

    test(
      "a pin served by api.anthropic.com goes to Anthropic, any other to OpenRouter; one without its key refuses every call naming the variable"
    ) {
      val direct = pinned("anthropic/claude-haiku-5-5", "api.anthropic.com")
      val routed = pinned("anthropic/claude-haiku-5.5", "anthropic")
      val hi = ModelRequest("s", Vector(Message.User("hi")))
      // Which provider serves `p`; a refusing one says why, with no call made.
      def served(keys: Keys, p: Pinned): String =
        new CatalogModels(keys, seed, FakeDb, new InMemoryModelSettingStore).provider(p) match {
          case _: AnthropicProvider => "Anthropic"
          case _: OpenRouterProvider => "OpenRouter"
          case other => other.complete(hi).fold(_.toString, _ => "answered")
        }
      val both = keys(OpenRouterConfig.KeyVar, AnthropicProvider.KeyVar)
      (
        served(both, direct),
        served(both, routed),
        served(keys(AnthropicProvider.KeyVar), routed),
        served(keys(OpenRouterConfig.KeyVar), direct)
      ) ==> (
        "Anthropic",
        "OpenRouter",
        "Refused(OPENROUTER_API_KEY is not set, but the model catalog calls anthropic/claude-haiku-5.5 @ anthropic, which needs it)",
        "Refused(ANTHROPIC_API_KEY is not set, but the model catalog calls anthropic/claude-haiku-5-5 @ api.anthropic.com, which needs it)"
      )
    }

    test("the stub answers the turn's pin with its delayed provider, every other role's with one") {
      val stub = new StubModels(delayMs = 200)
      val pins = StubModels.Catalog.pin
      val turn = stub.provider(pins.turn.first)
      val summary = stub.provider(pins.summary.first)
      val query = stub.provider(pins.query.first)
      val heard = stub.provider(StubModels.Catalog.heard.first)
      assert(summary ne turn, query eq summary, heard eq summary)
      val hi = grit.core.provider.ModelRequest("s", Vector(grit.core.message.Message.User("hi")))
      def waited(p: grit.core.provider.Provider^): Boolean = {
        val start = System.nanoTime()
        val _ = p.complete(hi)
        System.nanoTime() - start >= 200_000_000L
      }
      // The delay is a floor on the turn's call; the others answer at once, far under it.
      (waited(turn), waited(summary)) ==> (true, false)
    }
  }
}
