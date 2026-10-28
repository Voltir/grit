package grit.core.id

/** The one shape a name made to sit in keys and ids takes: lowercase ASCII letters, digits and
  * `-`, not starting with `-`.
  */
object Slug {

  /** `s`, or why it is not `what`: empty, over `longest` characters when there is a cap,
    * starting with `-`, or with a digit unless `leadingDigit`, or holding a character other than
    * a lowercase ASCII letter, a digit or `-`.
    */
  def check(
      what: String,
      s: String,
      longest: Option[Int],
      leadingDigit: Boolean
  ): Either[String, String] =
    if (s.isEmpty) Left(s"$what is empty")
    else if (longest.exists(s.length > _)) Left(s"$what is over ${longest.mkString} characters: $s")
    else if (s.startsWith("-")) Left(s"$what starts with '-': $s")
    else if (!leadingDigit && s.headOption.exists(c => c >= '0' && c <= '9'))
      Left(s"$what starts with a digit: $s")
    else
      s.find(c => !(c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '-')) match {
        case Some(c) => Left(s"$what holds '$c', not a lowercase letter, a digit or '-': $s")
        case None => Right(s)
      }
}
