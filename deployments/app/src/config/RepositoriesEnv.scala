package grit.app.config

import java.nio.file.Path

import grit.core.edge.Console
import grit.core.place.Repository

/** The repositories the environment declares for `grit work --in`, and where their runs'
  * working copies are made.
  */
object RepositoriesEnv {

  /** The variable declaring repositories: `name=path` each, comma-separated, `path` the
    * directory of that repository's clone on this machine.
    */
  val RepositoriesVar = "GRIT_REPOSITORIES"

  /** The variable naming the directory working copies are made under. */
  val CopiesVar = "GRIT_COPIES"

  /** The repositories `env` declares ([[RepositoriesVar]]), each with its clone's path as
    * written; none when unset. Why not, naming the variable: an item that is not `name=path`, a
    * name that is no repository's, or a name declared twice.
    */
  def declared(env: Map[String, String]): Either[String, Vector[(Repository, Path)]] =
    Listed
      .csv(env, RepositoriesVar)
      .foldLeft[Either[String, Vector[(Repository, Path)]]](Right(Vector.empty)) { (kept, item) =>
        kept.flatMap { pairs =>
          item.split('=') match {
            case Array(name, path) if name.trim.nonEmpty && path.trim.nonEmpty =>
              for {
                repository <- Repository.of(name.trim).left.map(why => s"$RepositoriesVar: $why")
                at <- scala.util
                  .Try(Path.of(path.trim))
                  .toEither
                  .left
                  .map(_ => s"$RepositoriesVar: '${path.trim}' is not a path")
                _ <-
                  if (pairs.exists(_._1 == repository))
                    Left(s"$RepositoriesVar declares ${repository.name} twice")
                  else Right(())
              } yield pairs :+ (repository -> at)
            case _ => Left(s"$RepositoriesVar: '$item' is not name=path")
          }
        }
      }

  /** Where working copies are made for `env`: [[CopiesVar]], else `grit/copies` under
    * `XDG_DATA_HOME`, or under `.local/share` in `HOME` when that is unset or empty. Why not,
    * naming the variables: none is set, or the first set is not a path.
    */
  def copies(env: Map[String, String]): Either[String, Path] = {
    def path(variable: String): Option[Either[String, Path]] =
      env
        .get(variable)
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(v => scala.util.Try(Path.of(v)).toOption.toRight(s"$variable: '$v' is not a path"))
    path(CopiesVar)
      .orElse(
        path("XDG_DATA_HOME")
          .orElse(path("HOME").map(_.map(_.resolve(".local").resolve("share"))))
          .map(_.map(_.resolve("grit").resolve("copies")))
      )
      .getOrElse(Left(s"working copies need a directory: set $CopiesVar, XDG_DATA_HOME or HOME"))
  }

  /** Where `declared` says `repository`'s clone is; why not, naming what it declares. */
  def cloneOf(declared: Vector[(Repository, Path)], repository: Repository): Either[String, Path] =
    declared
      .collectFirst { case (r, at) if r == repository => at }
      .toRight {
        val names = declared.map(_._1.name)
        s"$RepositoriesVar declares no repository ${repository.name}; it declares " +
          (if (names.isEmpty) "none" else names.mkString(", "))
      }

  /** `clone`, when it is a directory on this machine; why not otherwise. */
  def existing(clone: Path): Either[String, Path] =
    if (java.nio.file.Files.isDirectory(clone)) Right(clone)
    else Left(s"$RepositoriesVar names $clone as a clone, which is no directory here")

  /** Whether a run of work in a working copy may go on under `runner`: only on an engine this
    * process runs, whose deployment believes the repository's files as this one declares them.
    */
  def startedHere(runner: Console.Runner): Either[String, Unit] = runner match {
    case Console.Runner.Here => Right(())
    case Console.Runner.Elsewhere(_) =>
      Left(
        "grit work --in runs only on an engine it starts, and another process runs this " +
          "database's engine, which may not believe the repository's files as this one does: " +
          "stop it, or run without --in"
      )
  }
}
