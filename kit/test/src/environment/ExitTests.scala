package grit.kit.environment

import utest.*

/** [[Exit]]'s shared half, over an exit that throws where a process's would end the JVM. */
object ExitTests extends TestSuite {

  private final class Ended(val line: String) extends java.lang.RuntimeException(line)

  private def throwing(named: String): Exit^ =
    new Exit {
      def tag: String = named
      def refuse(why: String): Nothing = throw new Ended(line(why))
    }

  val tests = Tests {
    test(
      "a Right's value is given back; a Left ends the program on one line, the tag, a space and the refusal"
    ) {
      val exit = throwing("[main]")
      exit(Right(3)) ==> 3
      val ended = assertThrows[Ended](exit(Left("OPENROUTER_API_KEY is empty")))
      ended.line ==> "[main] OPENROUTER_API_KEY is empty"
      Vector("eval:", "[promote]").map(throwing(_).line("--capture is required")) ==>
        Vector("eval: --capture is required", "[promote] --capture is required")
    }
  }
}
