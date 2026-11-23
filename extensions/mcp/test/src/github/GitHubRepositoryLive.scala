package grit.mcp.github

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.core.host.RemoteRepository
import grit.mcp.client.{Credential, KeyFiles}

/** [[GitHubRepository]] against GitHub itself, opt-in and never run by a tier: skipped, saying
  * so, unless [[Token]] is set.
  *
  * {{{
  * GRIT_REMOTE_REPOSITORY_TOKEN=… ./mill grit.mcp.test.runMain grit.mcp.github.GitHubRepositoryLive \
  *   <owner>/<name> <open-branch> <merged-branch> <closed-branch> [dir]
  * }}}
  *
  * Over a scratch repository holding a pull request open from `<open-branch>`, one merged from
  * `<merged-branch>` and one closed unmerged from `<closed-branch>` (its branch deleted on GitHub,
  * so the probe shows a closed pull request is found without it), it checks that a branch none
  * was opened from has none, that each of the three reads as it stands, and that opening again
  * from `<open-branch>` answers the open one's address. That last is the one write it sends, and
  * GitHub refuses it as one already open; the probe sends it only after checking that the open
  * one's base is the repository's default branch, so it changes nothing. With `dir`, GitHub's
  * answers (the repository, each list of pull requests and the refusal) are written there as
  * they came, to replace the hand-made fixtures; none holds the token, which is never printed.
  * Exits 1 when a check fails or cannot run.
  */
object GitHubRepositoryLive {

  /** The variable whose token the probe sends, as `grit work --in` reads it. */
  val Token: Variable = Variable("GRIT_REMOTE_REPOSITORY_TOKEN")

  def main(args: Array[String]): Unit =
    if (!sys.env.contains(Variable.value(Token)))
      println(s"GitHubRepositoryLive: skipped, ${Variable.value(Token)} unset")
    else
      args.toVector match {
        case Vector(full, open, merged, closed, rest*) =>
          val failures = probe(full, open, merged, closed, rest.headOption.map(Path.of(_)))
          failures.foreach(f => System.err.println(s"GitHubRepositoryLive: $f"))
          println(s"GitHubRepositoryLive: ${failures.size} failed")
          if (failures.nonEmpty) sys.exit(1)
        case _ =>
          System.err.println(
            "GitHubRepositoryLive <owner>/<name> <open-branch> <merged-branch> <closed-branch> [dir]"
          )
          sys.exit(1)
      }

  /** Each check that failed, or why none could run. */
  private def probe(
      full: String,
      open: String,
      merged: String,
      closed: String,
      dir: Option[Path]
  ): Vector[String] =
    Credential.open(Credential.static(Token), sys.env, KeyFiles.Local) match {
      case Left(refused) => Vector(refused.message)
      case Right(authorizer) =>
        GitHubRepository.of(
          URI.create(GitHubRepository.Api),
          full,
          authorizer,
          Clock.system()
        ) match {
          case Left(why) => Vector(why)
          case Right(repository) =>
            val token = sys.env.getOrElse(Variable.value(Token), "")
            def raw(path: String, name: String): Option[ujson.Value] = {
              val request = HttpRequest
                .newBuilder(URI.create(s"${GitHubRepository.Api}repos/$full/$path"))
                .header("Authorization", s"Bearer $token")
                .header("Accept", "application/vnd.github+json")
                .header("X-GitHub-Api-Version", GitHubRepository.ApiVersion)
                .GET()
                .build()
              val body = HttpClient
                .newHttpClient()
                .send(request, HttpResponse.BodyHandlers.ofString())
                .body()
              dir.foreach(d => Files.writeString(d.resolve(name), body))
              scala.util.Try(ujson.read(body)).toOption
            }
            val random = s"grit/probe-${java.util.UUID.randomUUID().toString.take(8)}"
            def expect(what: String, got: Any, wanted: Any => Boolean): Option[String] =
              Option.when(!wanted(got))(s"$what: got $got")
            val default = raw("", "repo.json")
              .flatMap(_.objOpt)
              .flatMap(_.get("default_branch"))
              .flatMap(_.strOpt)
            val openPull = repository.pullRequest(open)
            val base =
              raw(s"pulls?head=${full.takeWhile(_ != '/')}:$open&state=open", "pulls-open.json")
                .flatMap(_.arrOpt)
                .flatMap(_.headOption)
                .flatMap(_.objOpt)
                .flatMap(_.get("base"))
                .flatMap(_.objOpt)
                .flatMap(_.get("ref"))
                .flatMap(_.strOpt)
            val checks = Vector(
              expect(
                "a branch none was opened from",
                repository.pullRequest(random),
                _ == Right(None)
              ),
              expect(
                "the open branch",
                openPull,
                { case Right(Some(_: RemoteRepository.PullRequest.Open)) => true; case _ => false }
              ),
              expect(
                "the merged branch",
                repository.pullRequest(merged),
                {
                  case Right(Some(_: RemoteRepository.PullRequest.Merged)) => true; case _ => false
                }
              ),
              expect(
                "the closed branch, deleted on GitHub",
                repository.pullRequest(closed),
                {
                  case Right(Some(_: RemoteRepository.PullRequest.Closed)) => true; case _ => false
                }
              ),
              expect(
                "the open pull request's base is the default branch",
                base,
                _ => base.isDefined && base == default
              )
            ).flatten
            if (checks.nonEmpty) checks
            else
              expect(
                "opening again from the open branch",
                repository
                  .open(open, "grit probe: not opened", "The probe expects GitHub to refuse this."),
                got =>
                  openPull.exists(
                    _.exists(p =>
                      p == RemoteRepository.PullRequest.Open(got match {
                        case Right(at: URI) => at
                        case _ => URI.create("about:none")
                      })
                    )
                  )
              ).toVector
        }
    }
}
