package grit.core.edge

import java.time.Instant

import grit.core.claim.{Claim, ClaimRole}
import grit.core.document.BriefClaim
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, SessionId}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.visibility.Label

/** The sessions contract, kept by the scripted fake. */
object ScriptedSessionsTests extends EdgeSessionsContract {

  private val script = new ScriptedSessions.Script

  protected def recording[A](body: EdgeSessions^ => A): A =
    body(new ScriptedSessions(Set(EdgeContracts.Reader, EdgeContracts.Other), script))

  protected def brief(holder: ClaimHolder, text: String, written: Instant): Unit =
    script.brief(holder, ScriptedSessions.document(text, written))

  protected def claimedBrief(
      holder: ClaimHolder,
      text: String,
      written: Instant,
      subject: DocKey,
      session: SessionId
  ): Unit = {
    val d = ScriptedSessions.document(text, written)
    script.brief(
      holder,
      d.copy(data =
        BriefClaim.kept(ujson.Obj(), BriefClaim(subject, Some(EdgeSessionsContract.Moving)))
      )
    )
    script.claim(
      Claim(
        subject,
        holder,
        got(ClaimRole.of("builder")),
        new Principal.Person(
          TestAccounts.principalId(TestAccounts.account("slack:T1/U-ann")),
          Set.empty
        ),
        written,
        Label.Public,
        Some(session),
        written
      )
    )
  }
}
