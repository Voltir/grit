package grit.core.identity

/** How one source names someone, keyed by its realm and the source's stable id
  * (`slack:T0123/U0456`), or [[Account.Local]] or [[Account.Grit]]. Evidence of who wrote
  * something, not of who they are: one person may hold several (ADR 0032). Never an email
  * address, which is a claim a realm attests about an account ([[Standing]]).
  */
opaque type Account = String

object Account {

  /** Whoever runs this machine's local edge. */
  val Local: Account = "local"

  /** grit itself: what a job's run is opened by. */
  val Grit: Account = "grit"

  /** `name` in `namespace`, or why not: the namespace is lowercase letters, digits and `-`,
    * starting with a letter, and is not `email`; the name is not blank and holds no
    * whitespace.
    */
  def of(namespace: String, name: String): Either[String, Sourced] =
    for {
      ns <- Account.namespace(namespace)
      n <- Either.cond(
        name.nonEmpty && !name.exists(_.isWhitespace),
        name,
        s"an account's name is not blank and holds no whitespace: $name"
      )
    } yield s"$ns:$n"

  /** The account `text` writes ([[written]]), or why not, as [[of]] refuses. */
  def read(text: String): Either[String, Account] =
    text match {
      case Local | Grit => Right(text)
      case _ =>
        text.indexOf(':') match {
          case -1 => Left(s"an account is local, grit, or {namespace}:{name}: $text")
          case at => of(text.take(at), text.drop(at + 1))
        }
    }

  def written(a: Account): String = a

  /** An account a source names, `{namespace}:{name}`: any but [[Local]] and [[Grit]]. */
  opaque type Sourced <: Account = String

  object Sourced {

    /** `account`, when a source names it; `None` for [[Local]] and [[Grit]]. */
    def of(account: Account): Option[Sourced] =
      Option.when(account != Local && account != Grit && account.contains(':'))(account)

    /** The source's namespace: `slack` of `slack:T0123/U0456`. */
    def namespace(account: Sourced): String = account.takeWhile(_ != ':')

    /** Its name in that namespace: `T0123/U0456` of `slack:T0123/U0456`. */
    def name(account: Sourced): String = account.dropWhile(_ != ':').drop(1)
  }

  given CanEqual[Account, Account] = CanEqual.derived

  /** `namespace`, or why it names no source's accounts: the rule [[of]] states. */
  private[identity] def namespace(namespace: String): Either[String, String] =
    if (namespace == "email")
      Left("email is no account's namespace: an address is what a realm attests of an account")
    else
      Either.cond(
        namespace.headOption.exists(c => c >= 'a' && c <= 'z') &&
          namespace.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'),
        namespace,
        s"an account's namespace is a lowercase letter, then lowercase letters, digits or -: $namespace"
      )
}
