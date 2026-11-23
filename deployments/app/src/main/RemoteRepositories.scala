package grit.app.main

import java.net.URI
import java.nio.file.Path

import grit.app.config.RepositoriesEnv
import grit.core.clock.Clock
import grit.core.place.Service
import grit.core.secret.{Secret, Variable}
import grit.host.checkout.Redaction
import grit.host.git.PushTarget
import grit.kit.run.Pushing
import grit.mcp.client.{Credential, KeyFiles}
import grit.mcp.github.{GitHubItems, GitHubRepository}

/** How `grit work --in` pushes and offers a declared repository's runs: on the GitHub
  * repository [[RepositoriesEnv.RemoteRepositoriesVar]] names for it, with the token
  * [[RepositoriesEnv.RemoteTokenVar]] holds, read twice: once by grit.mcp's credential for
  * GitHub's REST API, once as a [[Secret]] for git's push.
  */
private[main] object RemoteRepositories {

  /** The GitHub repository `fullName`, `{owner}/{name}`, as a push target, its requests carrying
    * `env`'s token got at `clock`'s time; why not: the token unset or not a token's shape, or
    * `fullName` as [[GitHubRepository.of]] refuses it.
    */
  def target(
      env: Map[String, String],
      fullName: String,
      clock: Clock^
  ): Either[String, PushTarget^] =
    Credential.open(
      Credential.static(grit.core.edge.Variable(RepositoriesEnv.RemoteTokenVar)),
      env,
      KeyFiles.Local
    ) match {
      case Left(refused) => Left(refused.message)
      case Right(authorizer) =>
        GitHubRepository.of(URI.create(GitHubRepository.Api), fullName, authorizer, clock) match {
          case Left(why) => Left(s"${RepositoriesEnv.RemoteRepositoriesVar}: $why")
          case Right(repository) => PushTarget.of(repository)
        }
    }

  /** The runs of the clone at `clone` pushed and offered as `target` says, `fullName`'s
    * branches, each pushed with `env`'s token as a secret, git run under `processes` (the
    * process's own environment, never `env`, which `.env` adds to) with what it shows cleared
    * by `redaction`; why not: the token unset or blank, or `fullName` not `{owner}/{name}`.
    */
  def pushing(
      env: Map[String, String],
      processes: Map[String, String],
      fullName: String,
      clone: Path,
      target: PushTarget^,
      redaction: Redaction
  ): Either[String, Pushing^] =
    (
      Secret.read(env, Variable(RepositoriesEnv.RemoteTokenVar)),
      Service.of("github").flatMap(GitHubItems.repository(_, fullName))
    ) match {
      case (Left(refused), _) => Left(refused.message)
      case (_, Left(why)) => Left(s"${RepositoriesEnv.RemoteRepositoriesVar}: $why")
      case (Right(secret), Right(place)) =>
        Pushing.of(clone, target, secret, processes, redaction, place, fullName)
    }
}
