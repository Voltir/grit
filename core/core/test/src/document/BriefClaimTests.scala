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
    test("a brief's claim reads back as it was kept, beside the plugin's own data") {
      val about = BriefClaim(got(DocKey.of("p/1")), Some(Takeover("cli project_claim", "p/1")))
      val bare = BriefClaim(got(DocKey.of("p/2")), None)
      val kept = BriefClaim.kept(ujson.Obj("holder" -> "h"), about)
      (
        BriefClaim.of(brief(kept)),
        BriefClaim.of(brief(BriefClaim.kept(ujson.Obj(), bare))),
        kept("holder")
      ) ==> (
        Some(about),
        Some(bare),
        ujson.Str("h")
      )
    }

    test("a brief whose data names no claim says none") {
      BriefClaim.of(brief(ujson.Obj("holder" -> "h"))) ==> None
    }
  }
}
