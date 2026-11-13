package grit.core.edge

import java.time.Instant

import grit.core.id.ClaimHolder

/** The sessions contract, kept by the scripted fake. */
object ScriptedSessionsTests extends EdgeSessionsContract {

  private val script = new ScriptedSessions.Script

  protected def recording[A](body: EdgeSessions^ => A): A =
    body(new ScriptedSessions(Set(EdgeContracts.Reader), script))

  protected def brief(holder: ClaimHolder, text: String, written: Instant): Unit =
    script.brief(holder, ScriptedSessions.document(text, written))
}
