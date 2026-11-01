package grit.core.host

import grit.core.secret.{Named, Secret}

/** Starting a program that is handed a secret: the one way a [[Secret]] reaches a program. Not a
  * [[Processes]]: code holding only one cannot hand a secret on.
  */
trait Handing extends caps.SharedCapability {

  /** `body` of the program `argv` names, started in `dir` as [[Processes.started]] starts one,
    * with `variables` added to its environment and `secret` in a file only this user can read,
    * in a directory of its own outside the root; `argv` is given the file's absolute path. The
    * directory is removed when `body` returns or throws. `Left`, `body` not run: as
    * [[Processes.started]]; `Failed` when the file cannot be written, or with `argv`'s refusal.
    */
  def started[A](dir: RelPath, secret: Secret, variables: Variables)(
      argv: String -> Either[String, Argv]
  )(body: Started^ => A): Either[HostError, A]
}

/** Variables added to a program's environment, none named like a secret. */
final class Variables private (val values: Map[String, String]) extends caps.Pure

object Variables {

  /** `values`, or why not: a name [[Named.variable]] holds of, since a secret goes as a
    * [[Secret]] through [[Handing]] or not at all.
    */
  def of(values: Map[String, String]): Either[String, Variables] =
    values.keys.toVector.sorted.find(Named.variable) match {
      case Some(name) =>
        Left(s"$name is named like a secret: a secret is handed as a Secret, or not at all")
      case None => Right(new Variables(values))
    }
}
