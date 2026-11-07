package grit.core.store

import utest.*

object WentOnTests extends TestSuite {

  private val make = TestWentOn.command("make test")

  val tests = Tests {
    test(
      "a failed check's output is kept clean: no escapes, control characters or trailing blanks, CR as LF"
    ) {
      val output =
        "\u001b[31mred\u001b[0m\r\nbell\u0007 tab\there\rover\u001b]0;title\u0007 \n\n"
      WentOn.failed(make, Some(1), output).tail ==> "red\nbell tab\there\nover"
    }

    test("a failed check keeps its output's last 60 lines, and its last 4 KB") {
      val many = (1 to 100).map(n => s"line $n").mkString("\n")
      val wide = ('a' to 'j').map(_.toString * 1000).mkString("\n")
      (
        WentOn.failed(make, Some(1), many).tail,
        WentOn.failed(make, Some(1), wide).tail
      ) ==> (
        (41 to 100).map(n => s"line $n").mkString("\n"),
        ('g' to 'j').map(_.toString * 1000).mkString("\n")
      )
    }

    test("a command is refused blank or over 1 KB") {
      (
        WentOn.Command.of("  "),
        WentOn.Command.of("x" * 1025),
        WentOn.Command.of("x" * 1024).map(_.text)
      ) ==> (
        Left("the check command is empty"),
        Left("the check command is over 1024 bytes"),
        Right("x" * 1024)
      )
    }

    test("a stored failed check is refused with exit code 0, or output failed would not keep") {
      (
        WentOn.CheckFailed.read(make, Some(0), "ok"),
        WentOn.CheckFailed.read(make, Some(1), "a\u001b[0m"),
        WentOn.CheckFailed.read(make, None, "slow")
      ) ==> (
        Left("a failed check's exit code is not 0"),
        Left("a failed check's output is over its bound or not clean"),
        Right(WentOn.failed(make, None, "slow"))
      )
    }
  }
}
