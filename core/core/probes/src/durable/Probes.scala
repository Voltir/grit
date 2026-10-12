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
  def errors(code: String, flags: List[String] = options): List[String] =
    diagnostics(code, flags).map(_._2)

  /** Each error from compiling `code` with `flags`: its 0-based line, -1 when it has none, and
    * its message.
    */
  private def diagnostics(code: String, flags: List[String]): List[(Int, String)] = {
    val dir = Files.createTempDirectory("grit-probe")
    try {
      val source = Files.writeString(dir.resolve("Probe.scala"), code)
      val args = flags ++ List("-classpath", classpath, "-d", dir.toString, source.toString)
      // The compiler only reads the array; separation checking treats any array as mutable.
      val argv = caps.unsafe.unsafeAssumePure(args.toArray)
      new Driver()
        .process(argv, StoreReporter(), null)
        .allErrors
        .map(d => (if (d.pos.exists) d.pos.line else -1, d.message))
    } finally {
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
    }
  }

  /** `probes`, deduplicated, each compiled inside an object of its own after `prelude`, with
    * `flags`, in as few compiler runs as give each the errors it would get compiled alone. Any
    * error before capture checking (the typer's, say) stops a run there for every probe, so a
    * sentinel probe that checker must reject (the typer, when capture checking is off) shows
    * whether a run got that far. When it did not, the probes that reported errors keep them,
    * since they stopped where alone they would, and those left without any run again; each is
    * compiled alone at worst.
    */
  def batch(prelude: String, probes: List[String], flags: List[String] = options): Batch = {
    val head = prelude + "\n"
    def alone(body: String): List[String] = errors(head + s"object Probe0 {\n$body\n}\n", flags)
    @scala.annotation.tailrec
    def rounds(
        pending: List[String],
        answered: Map[String, List[String]],
        runs: Int
    ): Batch =
      if (pending.isEmpty) Batch(answered, runs, alone)
      else
        once(head, pending, flags) match {
          case Ran.Reached(byProbe) => Batch(answered ++ byProbe, runs + 1, alone)
          case Ran.Stopped(byProbe) =>
            val (failed, clean) = byProbe.partition(_._2.nonEmpty)
            if (failed.isEmpty)
              Batch(answered ++ clean.map((p, _) => p -> alone(p)), runs + 1 + clean.size, alone)
            else rounds(clean.map(_._1), answered ++ failed, runs + 1)
        }
    rounds(probes.distinct, Map.empty, 0)
  }

  /** One compiler run's errors, by probe: whether it reached the checker `flags` enable. */
  private enum Ran {
    case Reached(byProbe: List[(String, List[String])])
    case Stopped(byProbe: List[(String, List[String])])
  }

  /** `probes` and the sentinel, each in an object of its own after `head`, compiled once; an
    * error outside every object (in `head`) counts against each probe.
    */
  private def once(head: String, probes: List[String], flags: List[String]): Ran = {
    val (sentinel, rejected) =
      if (flags.contains("-language:experimental.captureChecking"))
        (
          "def sentinel(c: caps.SharedCapability^): () -> String = () => c.toString",
          "() ->{c} String"
        )
      else ("val sentinel: Int = \"grit-probe-sentinel\"", "\"grit-probe-sentinel\"")
    val wrapped = (probes :+ sentinel).zipWithIndex.map { (body, i) =>
      s"object Probe$i {\n$body\n}\n"
    }
    val starts = wrapped.scanLeft(head.count(_ == '\n'))(_ + _.count(_ == '\n'))
    val spans = starts.zip(starts.drop(1)).map((from, next) => (from, next - 1))
    val errs = diagnostics(head + wrapped.mkString, flags)
    def within(span: (Int, Int))(line: Int): Boolean = line >= span._1 && line <= span._2
    val outside = errs.filter((l, _) => !spans.exists(s => within(s)(l))).map(_._2)
    val byProbe = probes.zip(spans).map { (body, span) =>
      body -> (errs.filter((l, _) => within(span)(l)).map(_._2) ++ outside)
    }
    val reached = spans.lastOption.exists(last =>
      errs.exists((l, m) => within(last)(l) && m.contains(rejected))
    )
    if (reached) Ran.Reached(byProbe) else Ran.Stopped(byProbe)
  }

  /** What compiling a [[batch]]'s probes reported, by probe.
    *
    * @param runs
    *   the compiler runs it took: one when the first reached the checker the batch's flags
    *   enable
    */
  final class Batch private[Probes] (
      byProbe: Map[String, List[String]],
      val runs: Int,
      alone: String -> List[String]
  ) {

    /** The error messages within `probe`'s object, and every error outside all the probes'
      * objects (in the prelude), which counts against each. A probe the batch was not given is
      * compiled alone.
      */
    def errors(probe: String): List[String] = byProbe.getOrElse(probe, alone(probe))
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
