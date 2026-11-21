package grit.core.document

import grit.core.edge.EdgeContracts
import grit.core.edge.ScriptedSessions
import grit.core.either.Got.got
import grit.core.id.DocKey

import utest.*

/** What a brief's data says of its claim, kept and read back by core alone. */
object BriefClaimTests extends TestSuite {

  private def brief(data: ujson.Obj): Document =
    ScriptedSessions.document("Work it.", EdgeContracts.At).copy(data = data)

  val tests = Tests {
    test("a brief's claim is kept under `claim` beside the plugin's own data, and read back") {
      // Briefs in the store hold this form: a key renamed on both sides would pass a round trip
      // and leave every brief already kept naming no claim, so its session opens with the brief.
      val about = BriefClaim(got(DocKey.of("p/1")), Some(Takeover("cli project_claim", "p/1")))
      val bare = BriefClaim(got(DocKey.of("p/2")), None)
      val stored = ujson.Obj(
        "holder" -> "h",
        "claim" -> ujson.Obj(
          "subject" -> "p/1",
          "takeover" -> ujson.Obj("command" -> "cli project_claim", "key" -> "p/1")
        )
      )
      val storedBare = ujson.Obj("claim" -> ujson.Obj("subject" -> "p/2"))
      (
        BriefClaim.kept(ujson.Obj("holder" -> "h"), about),
        BriefClaim.kept(ujson.Obj(), bare),
        BriefClaim.of(brief(stored)),
        BriefClaim.of(brief(storedBare))
      ) ==> (stored, storedBare, Some(about), Some(bare))
    }

    test("a brief whose data names no claim says none") {
      BriefClaim.of(brief(ujson.Obj("holder" -> "h"))) ==> None
    }
  }
}
