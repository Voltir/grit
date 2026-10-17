package grit.outline.testing

/** Roots built under a temp dir for a test, their sources compiled by dotc in the test's own JVM. */
object Dotc {

  /** A fresh temp dir named from `prefix`, holding an empty `build.mill`, so that MillLayout reads it as a root. */
  def millRoot(prefix: String): os.Path = {
    val dir = os.temp.dir(prefix = prefix)
    os.write(dir / "build.mill", "")
    dir
  }

  /** Where Mill puts module `module`'s classes under `root`: `out/<module>/compile.dest/classes`. */
  def classesOf(root: os.Path, module: os.SubPath): os.Path =
    root / "out" / module / "compile.dest" / "classes"

  /** Compiles `sources` into `classes`, made if missing, against `classpath` then the test's own classpath. Throws when
    * the compiler reports an error, which it has printed.
    */
  def compile(
      classes: os.Path,
      sources: Vector[os.Path],
      classpath: Vector[os.Path] = Vector.empty
  ): Unit = {
    os.makeDir.all(classes)
    val path =
      (classpath.map(_.toString) :+ System.getProperty("java.class.path"))
        .mkString(java.io.File.pathSeparator)
    val reporter = dotty.tools.dotc.Main.process(
      Array("-d", classes.toString, "-classpath", path) ++ sources.map(_.toString)
    )
    if (reporter.hasErrors)
      throw new AssertionError(s"dotc failed on ${sources.mkString(", ")}")
  }
}
