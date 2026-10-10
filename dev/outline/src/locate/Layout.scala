package grit.outline.locate

/** Where a build writes its compiled `.tasty` files under a root. */
trait Layout {

  /** Every directory under `root` holding compiled classes, sorted; empty when nothing is compiled. */
  def classesDirs(root: Root): Vector[os.Path]

  /** Every library jar a compiled class under `root` may refer to, sorted; empty when none are recorded. */
  def libraryJars(root: Root): Vector[os.Path]

  /** The one line telling a person how to compile `root` when `classesDirs` is empty. */
  def notCompiled(root: Root): String

  /** Whether `classesDir` holds only tests' classes. */
  def isTest(classesDir: os.Path): Boolean

  /** Whether the source file `file`, a path relative to the checkout, holds only tests' source. */
  def isTestSource(file: os.RelPath): Boolean
}

/** Mill's layout: `out/**/compile.dest/classes`, and each module's `resolvedMvnDeps.json` beside its `compile.dest`. */
object MillLayout extends Layout {

  /** The walk of `out` that does not enter a classes directory, another module's `.dest`, or a nested Mill output
    * directory: one below `out` holding a `mill-out-lock` file or a `mill-daemon` directory, or any directory named
    * `mill-build`. A nested Mill output directory (another tool's or another pass's) is not this root's build. `out`
    * itself is never pruned.
    */
  private def prune(out: os.Path)(p: os.Path): Boolean =
    (p / os.up).last == "classes" ||
      (p.last.endsWith(".dest") && p.last != "compile.dest") ||
      (p != out && os.isDir(p) && (p.last == "mill-build" || os.isFile(p / "mill-out-lock") || os
        .isDir(p / "mill-daemon")))

  /** A coursier reference as Mill writes it, before the path it names. */
  private val coursierPrefix = raw"^q?ref:v\d+:[0-9a-f]+:".r

  def classesDirs(root: Root): Vector[os.Path] = {
    val out = root.dir / "out"
    if (!os.isDir(out)) Vector.empty
    else
      os.walk(out, skip = prune(out))
        .filter(p => p.last == "classes" && os.isDir(p) && (p / os.up).last == "compile.dest")
        .sortBy(_.toString)
        .toVector
  }

  def libraryJars(root: Root): Vector[os.Path] = {
    val out = root.dir / "out"
    if (!os.isDir(out)) Vector.empty
    else
      os.walk(out, skip = prune(out))
        .filter(p => p.last == "resolvedMvnDeps.json" && os.isFile(p))
        .flatMap(recorded)
        .filter(p => p.ext == "jar" && os.isFile(p))
        .distinct
        .sortBy(_.toString)
        .toVector
  }

  /** The jar paths one `resolvedMvnDeps.json` names; a file that is unreadable or not in that shape names none. */
  private def recorded(file: os.Path): Vector[os.Path] =
    scala.util.Try(ujson.read(os.read(file))).toOption match {
      case Some(ujson.Obj(fields)) =>
        fields.toVector
          .collectFirst { case ("value", ujson.Arr(items)) => items.toVector }
          .getOrElse(Vector.empty)
          .collect { case ujson.Str(s) => coursierPrefix.replaceFirstIn(s, "") }
          .filter(_.startsWith("/"))
          .map(os.Path(_))
      case _ => Vector.empty
    }

  def isTest(classesDir: os.Path): Boolean =
    Set("test", "it").contains((classesDir / os.up / os.up).last)

  def isTestSource(file: os.RelPath): Boolean = {
    val segments = file.segments.toVector
    segments.lift(segments.indexOf("src") - 1).exists(Set("test", "it").contains)
  }

  def notCompiled(root: Root): String =
    s"no compiled classes under ${root.dir / "out"}: run ./mill <module>.compile"
}
