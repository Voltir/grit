package grit.core.identity

/** The accounts one source names: those in `namespace` whose name begins `{within}/`, as a
  * Slack workspace's are `slack:{team}/…`.
  */
final case class Realm private (namespace: String, within: String) {
  def holds(account: Account): Boolean =
    Account.written(account).startsWith(s"$namespace:$within/")
}

object Realm {

  /** Or why not: `namespace` as [[Account.of]] takes one (never `email`); `within` not blank,
    * with no `/` and no whitespace.
    */
  def of(namespace: String, within: String): Either[String, Realm] =
    for {
      ns <- Account.namespace(namespace)
      w <- Either.cond(
        within.nonEmpty && !within.exists(c => c == '/' || c.isWhitespace),
        within,
        s"a realm's scope is not blank and holds no / or whitespace: $within"
      )
    } yield Realm(ns, w)
}
