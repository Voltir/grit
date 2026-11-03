package grit.host.checkout

import java.nio.file.{Files, Path}

import grit.core.host.*

import utest.*

/** [[LocalMemory]] against a temporary root, over this machine's own processes. */
object LocalMemoryTests extends TestSuite {

  private def rel(s: String): RelPath =
    RelPath.of(s).fold(e => throw new java.lang.AssertionError(e.toString), identity)

  /** A fresh root holding `busy` and `idle`, given to `body`, deleted after. */
  private def inRoot[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-memory")
    Files.createDirectories(dir.resolve("busy/deeper"))
    Files.createDirectories(dir.resolve("idle"))
    try body(dir)
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  val tests = Tests {
    test(
      "what processes working in a directory or below it hold counts, and none elsewhere; the machine has some to spare"
    ) {
      inRoot { root =>
        val processes = new LocalProcesses(root, sys.env, Redaction.of(Map.empty))
        val memory = new LocalMemory(root)
        val sleep = Argv.of("sleep", "5").fold(e => throw new java.lang.AssertionError(e), identity)
        processes
          .started(sleep, rel("busy/deeper")) { s =>
            val busy = memory.held(rel("busy"))
            val idle = memory.held(rel("idle"))
            s.kill()
            (
              busy.map(h => h.resident > 0 && h.available > 0),
              idle.map(_.resident),
              memory.held(rel("gone")).isLeft
            )
          } ==> Right((Right(true), Right(0L), true))
      }
    }
  }
}
