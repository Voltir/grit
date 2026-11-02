package grit.kit.environment

/** How a command-line program ends on a refusal: one line on standard error, its `tag`, a space
  * and the refusal, then the program's exit status.
  */
trait Exit extends caps.SharedCapability {

  /** Names the program in each line it ends on, such as `[main]`. */
  def tag: String

  /** Writes [[line]] of `why` to standard error and ends the program with its status. Never
    * returns.
    */
  def refuse(why: String): Nothing

  /** `result`'s value; on `Left`, [[refuse]]s with its message. */
  final def apply[A](result: Either[String, A]): A = result match {
    case Right(a) => a
    case Left(why) => refuse(why)
  }

  /** The line [[refuse]] writes for `why`. */
  final def line(why: String): String = s"$tag $why"
}

object Exit {

  /** This process's exit: [[Exit.refuse]] ends the JVM with `status`. Made at a program's
    * composition root.
    */
  def process(tag: String, status: Int): Exit^ = {
    val named = tag
    new Exit {
      def tag: String = named
      def refuse(why: String): Nothing = {
        System.err.println(line(why))
        sys.exit(status)
      }
    }
  }
}
