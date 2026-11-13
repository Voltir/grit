package grit.commands.routes

import grit.commands.worker.{Holders, WorkerName}
import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.place.Directory
import grit.edge.http.Request

/** What `/holder` reads from a query. */
private[commands] object Briefing {

  /** `holder`: where the holder a session works as is asked for. */
  val Holder: String = "holder"

  /** The holder `/holder`'s query names on `worker` ([[Holders.of]]), or why not:
    *   - `harness`, the harness's name ([[Harness.of]]), and `dir`, the session's directory
    *     ([[Directory.of]]), each sent;
    *   - for an agent, its `type` and the `session` (the harness's own id) that starts it, sent
    *     together, `dir` being that session's directory;
    *   - no other parameter, and none sent twice.
    */
  def holderOf(request: Request, worker: WorkerName): Either[String, ClaimHolder] =
    for {
      q <- Queried.of(request, Vector("harness", "dir", "session", "type"))
      harness <- q.need("harness")(Harness.of)
      dir <- q.need("dir")(Directory.of)
      session <- q.read("session")(SessionId.of(harness, _))
      kind <- q.read("type")(Right(_))
      agent <- (kind, session) match {
        case (Some(k), Some(s)) => Right(Some(Holders.Agent(k, s)))
        case (None, None) => Right(None)
        case _ => Left(Together)
      }
    } yield Holders.of(worker, dir, agent)

  private val Together =
    "type and session come together: an agent's type, and the session starting it"
}
