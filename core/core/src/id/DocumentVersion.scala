package grit.core.id

/** One version of one plugin's document: numbered across every plugin and never reused, so it
  * alone names the version, and a key's later version has the higher number. At least 1.
  */
opaque type DocumentVersion = Long

object DocumentVersion {

  /** `n` as a version, or `None` below 1. */
  def of(n: Long): Option[DocumentVersion] = Option.when(n >= 1)(n)

  def value(v: DocumentVersion): Long = v

}
