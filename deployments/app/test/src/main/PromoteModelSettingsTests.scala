package grit.app.main

import java.time.LocalDate

import grit.core.model.{CatalogJson, Known, NameRepair, Profile, Settings, Source}
import grit.models.Seed

import utest.*

object PromoteModelSettingsTests extends TestSuite {

  private val seed = Seed.catalog.getOrElse(sys.error("the seed"))

  val tests = Tests {
    test("a promoted catalog is written in the seed's form, read back as it was, ending a line") {
      val pair = seed.policy.turn.first.assignment.ref
      val approved = Profile(
        pair,
        names = Known.Of(
          NameRepair.AsSent,
          Source.Measured("probe_pair", LocalDate.of(2026, 9, 25), 3, 3)
        )
      )
      val text = PromoteModelSettings.render(seed.overlaid(Vector(approved)))
      val back = CatalogJson.read(ujson.read(text))
      back.map(_.version) ==> Right(seed.overlaid(Vector(approved)).version)
      back.map(c => Settings.of(c.profile(pair)).names) ==> Right(NameRepair.AsSent)
      // Nothing approved: the seed as it is.
      CatalogJson
        .read(ujson.read(PromoteModelSettings.render(seed)))
        .map(_.version) ==> Right(seed.version)
      assert(text.endsWith("\n"))
    }
  }
}
