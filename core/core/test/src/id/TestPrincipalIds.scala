package grit.core.id

/** A principal's id as core's in-memory stores keep it: they stand in for `grit.dbos`'s, the
  * only main sources that make one ([[PrincipalIds]]). The one test source that names it
  * (scripts/enola-law.sh).
  */
object TestPrincipalIds {
  def stored(text: String): PrincipalId = PrincipalIds.stored(text)
}
