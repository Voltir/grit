package grit.core.claim

import java.time.Instant

import grit.core.id.{ClaimHolder, SessionId}
import grit.core.identity.Principal

/** The sessions a claim's lease is read from, as the fakes of [[Claims]] keep them. */
object ClaimSessions {

  /** A session working as `holder` when it names one, begun by `by`, a child of `parent` when an
    * agent's, its last event at `last`, ended when `ended`.
    */
  final case class Working(
      holder: Option[ClaimHolder],
      last: Instant,
      parent: Option[SessionId],
      ended: Boolean,
      by: Principal.Person
  )

  /** [[Claim.active]] of `claim`, as a store whose sessions are `sessions` would answer it. */
  def active(claim: Claim, sessions: Map[SessionId, Working]): Instant = {
    val worked = claim.session match {
      case Some(id) =>
        sessions.get(id).toVector.flatMap { own =>
          own.last +: (if (own.ended) Vector.empty
                       else
                         sessions.values.toVector
                           .filter(w => w.parent.contains(id) && w.by.id == own.by.id)
                           .map(_.last))
        }
      case None => sessions.values.toVector.filter(_.holder.contains(claim.holder)).map(_.last)
    }
    (claim.made +: worked).max
  }
}
