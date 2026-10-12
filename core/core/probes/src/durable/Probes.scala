package grit.core.durable

import java.nio.file.Files

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter

/** Probe sources compiled against the module under test with its own flags, as build.mill
  * hands them to its tests (`GRIT_PROBE_CLASSPATH`, `GRIT_PROBE_OPTIONS`):
  * `assertCompileError` never reports capture- or separation-checking errors
  * (docs/capture-checking.md).
  */
object Probes {

  /** The module's run classpath; empty when the environment is not set. */
  val classpath: String = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")

  /** The module's compiler flags, less `-Werror`. */
  val options: List[String] = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  /** The error messages from compiling `code`, one source file, with `flags`. */
  def errors(code: String, flags: List[String] = options): List[String] = {
    val dir = Files.createTempDirectory("grit-probe")
    try {
      val source = Files.writeString(dir.resolve("Probe.scala"), code)
      val args = flags ++ List("-classpath", classpath, "-d", dir.toString, source.toString)
      // The compiler only reads the array; separation checking treats any array as mutable.
      val argv = caps.unsafe.unsafeAssumePure(args.toArray)
      new Driver().process(argv, StoreReporter(), null).allErrors.map(_.message)
    } finally {
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
    }
  }

  /** `source` with its capture sets erased, as code written without capture checking. */
  def erased(source: String): String =
    source
      .replaceAll("Toolbox\\[\\{[^}]*\\}\\]", "Toolbox[?]")
      .replaceAll("\\^\\{[^}]*\\}", "")
      .replace("^", "")

  /** Whether `errs` reject a class for holding a capability its pure self type excludes. */
  def heldImpure(errs: List[String]): Boolean =
    errs.exists(_.contains("is not included in the allowed capture set {} of the self type"))

  /** Whether `errs` reject a capability flowing into a capture set that may not hold it. */
  def flowsInto(set: String)(errs: List[String]): Boolean =
    errs.exists(_.contains(s"cannot flow into capture set $set"))
}
