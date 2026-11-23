package grit.tools

import scala.collection.immutable.VectorMap

import grit.core.host.Branch
import grit.core.place.Place
import grit.core.tool.{
  Args,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  SetRefusal,
  ToolName,
  ToolSpec,
  Toolbox,
  Writes,
  Writing
}

/** The tool over a run's branch: `offer_pull_request`, the one path to the forge, and the only
  * one that holds its credential (a [[Branch]]'s). It writes outside grit to the forge
  * repository's place alone, so it is withheld where that place is not written to, and asks no
  * one first.
  */
object Versioning {

  type OfferArgs = (title: String, body: String)

  /** `offer_pull_request`. */
  val Name: ToolName = ToolName("offer_pull_request")

  /** `offer_pull_request` as the engine offers it, writing to `forge`, the place of the forge
    * repository the model names `named` (its full name, `{owner}/{name}`): described, never
    * run. Rerun when its edge died before it answered, since offering twice pushes what was
    * committed and opens no second pull request. Why not: `named` is blank.
    */
  def hosted(forge: Place, named: String): Either[String, Writing[OfferArgs, Place]] =
    for {
      writes <- Writes.of(
        VectorMap(named -> forge),
        identity,
        s"The repository the pull request is opened in: $named."
      )
      tool <- Hosted.writing[OfferArgs, Place](
        ToolSpec[OfferArgs](
          Name,
          s"Push this run's branch to $named and open a pull request from it, with `title` and " +
            "`body`, into the repository's default branch; when its pull request is open " +
            "already, push what was committed since and answer the same address. Only " +
            "committed work is pushed: commit first with `run`'s `git commit`. A push the " +
            "repository refuses opens nothing.",
          Args.of(
            (
              title = Field.text("The pull request's title: one line."),
              body = Field.text("The pull request's description, in markdown.")
            )
          ),
          Retry.Rerun,
          Some("open a pull request")
        ),
        Gate.Free,
        a => a.title,
        writes
      )
    } yield tool

  /** `described` run over `branch`: its answer the pull request's address. */
  def offered(
      branch: Branch^,
      described: Writing[OfferArgs, Place]
  ): Either[SetRefusal, Toolbox[caps.CapSet^{branch}]] =
    Toolbox.of(described.over { (a, _) =>
      branch.offer(a.title, a.body) match {
        case Right(at) => Outcome.Done(s"The branch is pushed; its pull request is open at $at.")
        case Left(e) => Outcome.Failed(s"${e.message} No pull request was offered.")
      }
    })
}
