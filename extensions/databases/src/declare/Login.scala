package grit.databases.declare

import grit.core.secret.Variable

/** How the reader logs in. A secret's variable must be named like one (KEY, TOKEN, SECRET or
  * PASSWORD in its name); no secret's value is shown in a refusal, a row or a log line.
  */
sealed trait Login

object Login {

  /** As `user`'s value, with `password`'s, a secret. */
  final case class Password(user: Variable, password: Variable) extends Login

  /** As the driver authenticates: `plain` passed as written (a socket factory, an instance
    * name, an IAM user), `secrets` each set to its variable's value.
    */
  final case class Driver private (plain: Map[String, String], secrets: Map[String, Variable])
      extends Login

  object Driver {

    /** Or why not: `plain` holds `password` or `sslpassword`, in any case, which is a secret. */
    def of(plain: Map[String, String], secrets: Map[String, Variable]): Either[String, Driver] =
      plain.keys.find(k => Credentials(k.toLowerCase)) match {
        case Some(key) =>
          Left(s"the driver's plain properties hold $key, a secret: give it as a secret's variable")
        case None => Right(new Driver(plain, secrets))
      }
  }

  /** The driver properties that carry a credential, lowercased: the login alone gives them. */
  private[databases] val Credentials: Set[String] = Set("password", "sslpassword")
}
