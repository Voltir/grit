package grit.core.edge

import java.time.Instant

import grit.core.document.DocRef
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, SessionId}
import grit.core.place.Place
import grit.core.session.Event

/** The watch contract, kept by the scripted fake. */
object ScriptedWatchTests extends EdgeWatchContract {

  private val script = new ScriptedWatch.Script

  protected def watching[A](body: EdgeWatch^ => A): A =
    body(new ScriptedWatch(Set(EdgeContracts.Reader), script))

  protected def hold(key: DocKey, ref: DocRef, written: Instant): DocumentVersionId =
    script.hold(EdgeWatchContract.Plugin, key, Set(ref), written)

  protected def commit(
      session: SessionId,
      holder: ClaimHolder,
      worktree: Place,
      at: Instant
  ): Event.Shape = {
    val shape = EdgeWatchContract.got(Event.Shape.committed(worktree, "c0ffee", None))
    script.commit(session, Some(holder), worktree, at, shape)
    shape
  }
}
