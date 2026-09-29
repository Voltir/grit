package grit.core.period

import grit.core.id.LineId

import TestClosings.{balance, line}
import utest.*

object ClosingTests extends TestSuite {

  private def flows(prose: String, outcome: Option[String], changes: Change*): Flows =
    Flows.of(prose, outcome, changes.toVector).getOrElse(throw new java.lang.AssertionError(prose))

  private val backup = line(Section.Open, "How often should the laptop backup run?", 1, 1)

  private val full = Closing(
    flows(
      "We set up the staging deploy.",
      Some("staging deploys from main"),
      Change.Added(
        line(Section.Standing, "Staging deploys with make stage", 3, 3, ground = Ground.Person)
      ),
      Change.Resolved(backup, "daily at 02:00"),
      Change.Dropped(line(Section.Standing, "Deploys are manual", 1, 2), "superseded"),
      Change.Evicted(
        line(Section.Standing, "The old key lived in vault", 1, 1, ground = Ground.Tool)
      ),
      Change.Refused(line(Section.Open, "Too much to keep", 3, 3)),
      Change.Ignored("o9: done", "names no line")
    ),
    balance(
      line(Section.Open, "Prod deploy is not set up", 2, 3),
      line(Section.Standing, "Staging deploys with make stage", 3, 3, ground = Ground.Person),
      line(Section.Topics, "Laptop Backup Setup", 1, 2),
      line(Section.Topics, "Staging Deploy", 2, 3)
    )
  )

  val tests = Tests {
    test("flows need prose; prose and outcome are trimmed and a blank outcome dropped") {
      Flows.of("  ", Some("x"), Vector()) ==> None
      Flows.of(" p ", Some(" "), Vector()).map(f => (f.prose, f.outcome)) ==> Some(("p", None))
    }

    test("a closing's headline is its outcome, or else its prose's first sentence") {
      full.headline ==> "staging deploys from main"
      TestClosings.prose("We set it up. Then we tested it.").headline ==> "We set it up."
      TestClosings.prose("No full stop here").headline ==> "No full stop here"
      TestClosings.prose("Version 1.2 works! It does.").headline ==> "Version 1.2 works!"
    }

    // A pin of the stored form: every closing entry is written in it, and it outlives every
    // raw entry of its period, so a change that would make one unreadable fails here first.
    test("a closing's stored form, version 3: each standing line with its ground") {
      ClosingJson.write(full).render() ==>
        """{"v":3,"flows":{"prose":"We set up the staging deploy.","outcome":"staging deploys from main",""" +
        """"changes":[""" +
        """{"added":{"section":"standing","text":"Staging deploys with make stage","since":3,"touched":3,"ground":"person"}},""" +
        """{"resolved":{"section":"open","text":"How often should the laptop backup run?","since":1,"touched":1},"how":"daily at 02:00"},""" +
        """{"dropped":{"section":"standing","text":"Deploys are manual","since":1,"touched":2,"ground":"claimed"},"why":"superseded"},""" +
        """{"evicted":{"section":"standing","text":"The old key lived in vault","since":1,"touched":1,"ground":"tool"}},""" +
        """{"refused":{"section":"open","text":"Too much to keep","since":3,"touched":3}},""" +
        """{"ignored":"o9: done","why":"names no line"}]},""" +
        """"balance":{"open":[{"text":"Prod deploy is not set up","since":2,"touched":3}],""" +
        """"standing":[{"text":"Staging deploys with make stage","since":3,"touched":3,"ground":"person"}],""" +
        """"topics":[{"text":"Laptop Backup Setup","since":1,"touched":2},""" +
        """{"text":"Staging Deploy","since":2,"touched":3}]}}"""
      ClosingJson.write(TestClosings.prose("Small talk.")).render() ==>
        """{"v":3,"flows":{"prose":"Small talk.","changes":[]},""" +
        """"balance":{"open":[],"standing":[],"topics":[]}}"""
    }

    test("a topic line's summary is stored beside its text, and read back; other lines have none") {
      val t = TestClosings.line(Section.Topics, "Photo Rename", 1, 2, Some("renaming photos"))
      val c = Closing(TestClosings.prose("x").flows, balance(t))
      ClosingJson.writeBalance(c.balance).render() ==>
        """{"open":[],"standing":[],"topics":[{"text":"Photo Rename","since":1,"touched":2,"summary":"renaming photos"}]}"""
      ClosingJson.read(ClosingJson.write(c)).map(_.balance.lines.map(_.summary)) ==>
        Right(Vector(Some("renaming photos")))
      ClosingJson
        .readBalance(
          ujson.read("""{"open":[{"text":"a","since":1,"touched":1,"summary":"s"}]}"""),
          3
        ) ==>
        Left("a open line has a summary: a")
    }

    test("a stored closing reads back, a missing balance or section as empty") {
      ClosingJson.read(ClosingJson.write(full)) ==> Right(full)
      ClosingJson.read(ujson.read("""{"v":2,"flows":{"prose":"Small talk."}}""")) ==>
        Right(TestClosings.prose("Small talk."))
      ClosingJson
        .read(
          ujson.read(
            """{"v":2,"flows":{"prose":"x"},"balance":{"open":[{"text":"a","since":1,"touched":1}]}}"""
          )
        )
        .map(_.balance) ==> Right(balance(line(Section.Open, "a", 1, 1)))
    }

    test("a version-2 closing still reads, its standing lines Claimed") {
      ClosingJson
        .read(
          ujson.read(
            """{"v":2,"flows":{"prose":"x","changes":[{"added":{"section":"standing","text":"s","since":1,"touched":1}}]},""" +
              """"balance":{"standing":[{"text":"s","since":1,"touched":1}]}}"""
          )
        )
        .map(c => (c.balance.lines.map(_.ground), c.flows.changes)) ==>
        Right(
          (
            Vector(Some(Ground.Claimed)),
            Vector(Change.Added(line(Section.Standing, "s", 1, 1, ground = Ground.Claimed)))
          )
        )
    }

    test(
      "a version-3 standing line without a ground, a line of another section with one, or a ground in version 2, does not read"
    ) {
      def at(v: Int, lines: String) =
        ClosingJson.read(ujson.read(s"""{"v":$v,"flows":{"prose":"x"},"balance":$lines}"""))
      at(3, """{"standing":[{"text":"s","since":1,"touched":1}]}""") ==>
        Left("a standing line has no ground: s")
      at(3, """{"open":[{"text":"o","since":1,"touched":1,"ground":"person"}]}""") ==>
        Left("a open line has a ground: o")
      at(3, """{"standing":[{"text":"s","since":1,"touched":1,"ground":"sure"}]}""") ==>
        Left("a line's ground is not one: sure")
      at(2, """{"standing":[{"text":"s","since":1,"touched":1,"ground":"person"}]}""") ==>
        Left("a version-2 line has a ground: s")
      at(3, """{"standing":[{"text":"s","since":1,"touched":1,"ground":"tool"}]}""")
        .map(_.balance.lines.map(_.ground)) ==> Right(Vector(Some(Ground.Tool)))
    }

    test(
      "a stored closing that is not version 2 or 3, or whose prose, change or line is bad, is refused"
    ) {
      ClosingJson.read(ujson.read("""{"v":1,"prose":"x"}""")) ==> Left("unknown closing version: 1")
      ClosingJson.read(ujson.read("""{"v":4,"prose":"x"}""")) ==> Left("unknown closing version: 4")
      ClosingJson.read(ujson.read("""{"flows":{"prose":"x"}}""")) ==> Left("missing field: v")
      ClosingJson.read(ujson.read("""{"v":2,"flows":{"prose":" "}}""")) ==> Left("prose is blank")
      ClosingJson.read(ujson.read("""{"v":2,"flows":{"prose":"x","changes":[{"moved":1}]}}""")) ==>
        Left("""not a change: {"moved":1}""")
      ClosingJson.read(
        ujson.read(
          """{"v":2,"flows":{"prose":"x"},"balance":{"open":[{"text":"a","since":2,"touched":1}]}}"""
        )
      ) ==> Left("a line touched at 1 before its since 2")
      ClosingJson.read(
        ujson.read(
          """{"v":2,"flows":{"prose":"x"},"balance":{"open":[{"text":"a","since":1,"touched":1},{"text":"a","since":1,"touched":1}]}}"""
        )
      ) ==> Left(s"two lines share the id ${LineId.value(line(Section.Open, "a", 1, 1).id)}")
      ClosingJson.read(ujson.Arr()) ==> Left("expected an object")
    }
  }
}
