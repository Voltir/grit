package grit.core.identity

/** How one source names someone: `{namespace}:{name}` (`slack:T0123/U0456`,
  * `email:alice@example.com`), or [[Account.Local]] or [[Account.Grit]]. Evidence of who wrote
  * something, not of who they are: one person may hold several (ADR 0032).
  */
opaque type Account = String

object Account {

  /** Whoever runs this machine's local edge. */
  val Local: Account = "local"

  /** grit itself: what a job's run is opened by. */
  val Grit: Account = "grit"

  private val EmailNamespace = "email"

  /** `name` in `namespace`, or why not: the namespace is lowercase letters, digits and `-`,
    * starting with a letter, and is not `email` ([[email]] makes those); the name is not blank
    * and holds no whitespace.
    */
  def of(namespace: String, name: String): Either[String, Account] =
    for {
      ns <- Account.namespace(namespace)
      n <- Either.cond(
        name.nonEmpty && !name.exists(_.isWhitespace),
        name,
        s"an account's name is not blank and holds no whitespace: $name"
      )
    } yield s"$ns:$n"

  /** The account sharing by address names: `email:{email}`. */
  def email(email: Email): Account = s"$EmailNamespace:${Email.value(email)}"

  /** The account `text` writes ([[written]]), or why not. */
  def read(text: String): Either[String, Account] =
    text match {
      case Local | Grit => Right(text)
      case _ =>
        text.indexOf(':') match {
          case -1 => Left(s"an account is local, grit, or {namespace}:{name}: $text")
          case at =>
            val (namespace, name) = (text.take(at), text.drop(at + 1))
            if (namespace == EmailNamespace) {
              Email
                .of(name)
                .filterOrElse(
                  Email.value(_) == name,
                  s"an email account's address is as Email.of writes it: $name"
                )
                .map(email)
            } else of(namespace, name)
        }
    }

  def written(a: Account): String = a

  /** The address `a` is the account of ([[email]]); `None` for any other account. */
  def address(a: Account): Option[Email] =
    Option
      .when(a.startsWith(s"$EmailNamespace:"))(a.drop(EmailNamespace.length + 1))
      .flatMap(
        Email.of(_).toOption
      )

  given CanEqual[Account, Account] = CanEqual.derived

  /** `namespace`, or why it names no source's accounts: the rule [[of]] states. */
  private[identity] def namespace(namespace: String): Either[String, String] =
    if (namespace == EmailNamespace)
      Left("email accounts are made from an address (Account.email), never named")
    else
      Either.cond(
        namespace.headOption.exists(c => c >= 'a' && c <= 'z') &&
          namespace.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'),
        namespace,
        s"an account's namespace is a lowercase letter, then lowercase letters, digits or -: $namespace"
      )
}
