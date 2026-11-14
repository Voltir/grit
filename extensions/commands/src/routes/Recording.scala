package grit.commands.routes

import java.time.Instant

import grit.commands.events.Lines
import grit.commands.worker.WorkerName
import grit.core.edge.EdgeSessions
import grit.core.id.Harness
import grit.core.store.StoreError
import grit.edge.http.{Request, Response, Status}

/** What `/events` reads from a request, and how it answers what came of the recording. */
private[commands] object Recording {

  /** `events`: where a harness's event lines are posted. */
  val Events: String = "events"

  /** The batch `request` posts from `worker` at `now`, or why not, the post refused whole: a
    * query other than `harness`, the harness's name ([[Harness.of]]), sent once; or what
    * [[Lines.batch]] refuses whole.
    */
  def batchOf(request: Request, worker: WorkerName, now: Instant): Either[String, Lines.Batched] =
    for {
      q <- Queried.of(request, Vector("harness"))
      harness <- q.need("harness")(Harness.of)
      batched <- Lines.batch(request.body, harness, worker, now)
    } yield batched

  /** What `/events` answers for `batched` once recording its batch came to `came`:
    *   - 200 `{"recorded": <lines logged>, "refused": [{"line": <its number>, "why": ...}]}`,
    *     the lines refused left out and the rest kept, a line sent again not logged twice, so
    *     `recorded` is 0 and nothing is recorded when every line was refused;
    *   - 403 when the store refused the batch ([[StoreError.Invalid]]: no person is linked to the
    *     caller, or the session is kept where the caller does not read), nothing kept: not
    *     resent as it is, since it would be refused again;
    *   - 503 when the database could not be written, nothing kept: resending is safe;
    *   - 500 any other store failure, nothing kept.
    */
  def recorded(
      batched: Lines.Batched,
      came: EdgeSessions.Batch => Either[StoreError, Int]
  ): Response = {
    def answer(status: Status, logged: Int) = Response.json(
      status,
      ujson.Obj(
        "recorded" -> logged,
        "refused" -> ujson.Arr.from(
          batched.refused.map((i, why) => ujson.Obj("line" -> i, "why" -> why))
        )
      )
    )
    batched.batch match {
      case None => answer(Status.Ok, 0)
      case Some(batch) =>
        came(batch) match {
          case Right(logged) => answer(Status.Ok, logged)
          case Left(StoreError.Invalid(cause)) =>
            Response.text(Status.Forbidden, s"nothing was recorded: $cause")
          case Left(StoreError.DatabaseError(cause)) =>
            Response.text(
              Status.Unavailable,
              s"nothing was recorded, and resending is safe: $cause"
            )
          case Left(other) =>
            Response.text(Status.Failed, s"nothing was recorded: ${other.described}")
        }
    }
  }
}
