package grit.app.main

import java.time.LocalDate

import grit.core.model.{CatalogJson, Known, NameRepair, Profile, Settings, Source}
import grit.models.Seed

import utest.*

object PromoteModelSettingsTests extends TestSuite {

  private val seed = Seed.catalog.getOrElse(sys.error("the seed"))

  val tests = Tests {
    test(
      "the promoted seed is the seed with every approved model setting laid over it, in the seed's form"
    ) {
      val pair = seed.policy.turn.ref
      val approved = Profile(
        pair,
        names = Known.Of(
          NameRepair.AsSent,
          Source.Measured("probe_pair", LocalDate.of(2026, 9, 25), 3, 3)
        )
      )
      val text = PromoteModelSettings.render(seed, Vector(approved))
      val back = CatalogJson.read(ujson.read(text))
      back.map(_.version) ==> Right(seed.overlaid(Vector(approved)).version)
      back.map(c => Settings.of(c.profile(pair)).names) ==> Right(NameRepair.AsSent)
      // Nothing approved: the seed as it is.
      CatalogJson
        .read(ujson.read(PromoteModelSettings.render(seed, Vector.empty)))
        .map(_.version) ==> Right(seed.version)
      assert(text.endsWith("\n"))
    }
  }
}
