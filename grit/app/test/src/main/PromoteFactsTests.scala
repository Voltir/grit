package grit.app.main

import java.time.LocalDate

import grit.core.model.{CatalogJson, Known, NameRepair, Profile, Settings, Source}
import grit.models.Seed

import utest.*

object PromoteFactsTests extends TestSuite {

  private val seed = Seed.catalog.getOrElse(sys.error("the seed"))

  val tests = Tests {
    test("the promoted seed is the seed with every approved fact laid over it, in the seed's form") {
      val pair = seed.policy.turn.ref
      val fact = Profile(pair, names = Known.Of(NameRepair.AsSent, Source.Measured("probe_pair", LocalDate.of(2026, 9, 25), 3, 3)))
      val text = PromoteFacts.render(seed, Vector(fact))
      val back = CatalogJson.read(ujson.read(text))
      back.map(_.version) ==> Right(seed.overlaid(Vector(fact)).version)
      back.map(c => Settings.of(c.profile(pair)).names) ==> Right(NameRepair.AsSent)
      // Nothing approved: the seed as it is.
      CatalogJson.read(ujson.read(PromoteFacts.render(seed, Vector.empty))).map(_.version) ==> Right(seed.version)
      assert(text.endsWith("\n"))
    }
  }
}
