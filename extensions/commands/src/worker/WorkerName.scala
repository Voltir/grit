package grit.commands.worker

import grit.core.id.Slug

/** A worker's name: the machine or sandbox a credential is for, by the deployment's
  * configuration until credentials are; a slug ([[Slug.check]], a leading digit allowed) of at
  * most [[WorkerName.Longest]] characters. Never read from a host's name.
  */
opaque type WorkerName = String

object WorkerName {

  /** 32: the most characters a worker's name holds. */
  val Longest: Int = 32

  /** `s` as a worker's name, or why not. */
  def of(s: String): Either[String, WorkerName] =
    Slug.check("a worker's name", s, Some(Longest), leadingDigit = true)

  def value(w: WorkerName): String = w
}
