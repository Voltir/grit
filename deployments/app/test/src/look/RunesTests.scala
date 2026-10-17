package grit.app.look

import utest.*

/** What a person is told a turn's step does, by its name ([[Look.Runes.step]]). */
object RunesTests extends TestSuite {

  val tests = Tests {

    test("the assembly's steps say they search, sift and record, a batch by its family") {
      Vector(
        "ask:query",
        "search",
        "ask:relevance-0",
        "ask:relevance-2",
        "ask:query:record",
        "ask:relevance-1:record",
        "assemble"
      ).map(Look.Runes.step) ==> Vector(
        "ᛟ searching",
        "ᛟ searching",
        "ᛟ sifting what it found",
        "ᛟ sifting what it found",
        "ᛃ recording",
        "ᛃ recording",
        "ᛟ assembling"
      )
    }
  }
}
