package grit.models

import java.time.{Instant, LocalDate}

import grit.core.model.{Catalog, Known, NameRepair, Profile, Settings, Source}
import grit.core.store.{Db, InMemoryModelSettingStore, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

object OpenRouterModelsTests extends TestSuite {

  private object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private val seed: Catalog = Seed.catalog.getOrElse(sys.error("the seed"))
  private val turnPair = seed.policy.turn.ref

  val tests = Tests {
    test(
      "the catalog in force is the seed with every kept model setting laid over it, in the order kept"
    ) {
      val settings = new InMemoryModelSettingStore
      val models = new OpenRouterModels("k", seed, FakeDb, settings)
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
  }
}
