package grit.core.period

import java.time.Instant

import utest.*

object ClosingTests extends TestSuite {

  private def closing(
      prose: String,
      outcome: Option[String] = None,
      decisions: Vector[String] = Vector.empty,
      facts: Vector[String] = Vector.empty,
      open: Vector[String] = Vector.empty,
      sources: Vector[String] = Vector.empty
  ): Closing =
    Closing
      .of(prose, outcome, decisions, facts, open, sources)
      .getOrElse(throw new java.lang.AssertionError(prose))

  private val full = closing(
    "We set up the staging deploy.",
    Some("staging deploys from main"),
    Vector("deploy with make stage"),
    Vector("the key lives in vault"),
    Vector("prod is not done"),
    Vector("docs/deploy.md")
  )

  val tests = Tests {
    test("a closing needs prose; blank lines and a blank outcome are dropped") {
      Closing.of("  ", Some("x"), Vector(), Vector(), Vector(), Vector()) ==> None
      Closing.of(" p ", Some(" "), Vector("a", " ", ""), Vector(" b "), Vector(), Vector()) ==>
        Some(closing("p", None, Vector("a"), Vector("b")))
    }

    test("a closing is shown as one message: when and why it closed, its prose, its sections") {
      full.shown(Instant.parse("2026-09-20T23:30:00Z"), CloseReason.Resolved) ==>
        """Earlier in this conversation (closed 2026-09-20, resolved): We set up the staging deploy.
          |Outcome: staging deploys from main
          |Decisions:
          |- deploy with make stage
          |Facts:
          |- the key lives in vault
          |Open:
          |- prod is not done
          |Sources:
          |- docs/deploy.md""".stripMargin
      closing("Small talk.").shown(Instant.parse("2026-09-21T00:00:00Z"), CloseReason.Lapsed) ==>
        "Earlier in this conversation (closed 2026-09-21, lapsed): Small talk."
    }

    test("a closing's headline is its outcome, or else its prose's first sentence") {
      full.headline ==> "staging deploys from main"
      closing("We set it up. Then we tested it.").headline ==> "We set it up."
      closing("No full stop here").headline ==> "No full stop here"
      closing("Version 1.2 works! It does.").headline ==> "Version 1.2 works!"
    }

    // A pin of the stored form: every closing entry is written in it, and it outlives every
    // raw entry of its period, so a change that would make one unreadable fails here first.
    test("a closing's stored form, version 1") {
      ClosingJson.write(full).render() ==>
        """{"v":1,"prose":"We set up the staging deploy.","outcome":"staging deploys from main",""" +
        """"decisions":["deploy with make stage"],"facts":["the key lives in vault"],""" +
        """"open":["prod is not done"],"sources":["docs/deploy.md"]}"""
      ClosingJson.write(closing("Small talk.")).render() ==>
        """{"v":1,"prose":"Small talk.","decisions":[],"facts":[],"open":[],"sources":[]}"""
    }

    test("a stored closing reads back, a missing section as empty") {
      ClosingJson.read(ClosingJson.write(full)) ==> Right(full)
      ClosingJson.read(ujson.read("""{"v":1,"prose":"Small talk."}""")) ==>
        Right(closing("Small talk."))
    }

    test("a stored closing that is not version 1, has no prose, or a bad section, is refused") {
      ClosingJson.read(ujson.read("""{"v":2,"prose":"x"}""")) ==> Left("unknown closing version: 2")
      ClosingJson.read(ujson.read("""{"prose":"x"}""")) ==> Left("missing field: v")
      ClosingJson.read(ujson.read("""{"v":1,"prose":" "}""")) ==> Left("prose is blank")
      ClosingJson.read(ujson.read("""{"v":1,"prose":"x","facts":[1]}""")) ==>
        Left("facts is not a list of strings")
      ClosingJson.read(ujson.Arr()) ==> Left("expected an object")
    }
  }
}
