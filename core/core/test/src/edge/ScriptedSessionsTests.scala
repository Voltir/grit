package grit.core.edge

import java.time.Instant

import grit.core.claim.ClaimRole
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, SessionId}

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
      session: Option[SessionId]
  ): Unit =
    script.claimedBrief(
      holder,
      ScriptedSessions.document(text, written),
      subject,
      Some(EdgeSessionsContract.Moving),
      got(ClaimRole.of("builder")),
      session
    )
}
