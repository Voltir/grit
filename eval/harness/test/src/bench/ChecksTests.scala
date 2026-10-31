package grit.eval.harness.bench

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import grit.core.host.RelPath
import grit.host.checkout.LocalProcesses

import utest.*

/** [[Checks]] in a checkout whose scripts are stand-ins: each exits 1 when an argument is `red`,
  * the law when the file `law-red` is there.
  */
object ChecksTests extends TestSuite {

  private val failing = "for a in \"$@\"; do [ \"$a\" = red ] && exit 1; done; exit 0\n"

  /** A checkout under a fresh root with the stand-in scripts, `body` of it, the root deleted after. */
  private def inCheckout[A](body: (LocalProcesses, Path) => A): A = {
    val root = Files.createTempDirectory("grit-checks")
    try {
      val scripts = Files.createDirectories(root.resolve("checkout/scripts"))
      val runnable = PosixFilePermissions.fromString("rwxr-xr-x")
      Seq("check", "it").foreach { name =>
        val f = scripts.resolve(name)
        Files.writeString(f, s"#!/bin/sh\n$failing")
        Files.setPosixFilePermissions(f, runnable)
      }
      Files.writeString(scripts.resolve("fetch-enola.sh"), "exit 0\n")
      Files.writeString(scripts.resolve("enola-law.sh"), "[ -e law-red ] && exit 1; exit 0\n")
      body(new LocalProcesses(root, sys.env), root.resolve("checkout"))
    } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private val checkout = RelPath.of("checkout").getOrElse(RelPath.Root)

  private val checks: Vector[Check] = Vector(
    Check(Runner.Check, Vector("grit.core")),
    Check(Runner.It, Vector("grit.dbos.it.testOnly", "red"))
  )

  val tests = Tests {
    test("each check and the law run in order, by their command lines, whatever an earlier gave") {
      inCheckout { (work, _) =>
        Checks.run(work)(checkout, checks).map(c => (c.runs, c.passed)) ==> Right(
          (
            Vector(
              "scripts/check grit.core" -> true,
              "scripts/it grit.dbos.it.testOnly red" -> false,
              "bash scripts/fetch-enola.sh" -> true,
              "bash scripts/enola-law.sh" -> true
            ),
            false
          )
        )
      }
    }

    test("checks pass only when every check and the law exit 0") {
      inCheckout { (work, dir) =>
        val green = Vector(Check(Runner.Check, Vector("grit.core")))
        val before = Checks.run(work)(checkout, green).map(_.passed)
        Files.writeString(dir.resolve("law-red"), "")
        (before, Checks.run(work)(checkout, green).map(_.passed)) ==> (Right(true), Right(false))
      }
    }
  }
}
