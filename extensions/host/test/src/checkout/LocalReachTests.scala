package grit.host.checkout

import java.nio.file.{Files, Path}

import grit.core.host.{AbsPath, Reach, ReachContract}

/** [[LocalReach]] held to [[ReachContract]] over a temporary tree laid out as it says. */
object LocalReachTests extends ReachContract {

  protected def over[A](body: (Reach^, AbsPath) => A): A = {
    val base = Files.createTempDirectory("grit-reach").toRealPath()
    try {
      val co = base.resolve("work/co")
      Files.createDirectories(co.resolve("src"))
      Files.createDirectories(base.resolve("work/other"))
      Files.createSymbolicLink(co.resolve("link-out"), base)
      Files.createSymbolicLink(co.resolve("link-in"), Path.of("src"))
      Files.createSymbolicLink(co.resolve("up"), Path.of(".."))
      val abs = AbsPath.of(base.toString).fold(e => throw new java.lang.AssertionError(e), identity)
      body(new LocalReach(co), abs)
    } finally Files.walk(base).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }
}
