package grit.outline.locate

/** Where a build writes its compiled `.tasty` files under a root. */
trait Layout {

  /** Every directory under `root` holding compiled classes, sorted; empty when nothing is compiled. */
  def classesDirs(root: Root): Vector[os.Path]

  /** Every library jar a compiled class under `root` may refer to, sorted; empty when none are recorded. */
  def libraryJars(root: Root): Vector[os.Path]

  /** The one line telling a person how to compile `root` when `classesDirs` is empty. */
  def notCompiled(root: Root): String
}

/** Mill's layout: `out/**/compile.dest/classes`, and each module's `resolvedMvnDeps.json` beside its `compile.dest`. */
object MillLayout extends Layout {

  /** The walk of `out` that does not enter a classes directory or another module's `.dest`. */
  private def prune(p: os.Path): Boolean =
    (p / os.up).last == "classes" || (p.last.endsWith(".dest") && p.last != "compile.dest")

  /** A coursier reference as Mill writes it, before the path it names. */
  private val coursierPrefix = raw"^q?ref:v\d+:[0-9a-f]+:".r

  def classesDirs(root: Root): Vector[os.Path] = {
    val out = root.dir / "out"
    if (!os.isDir(out)) Vector.empty
    else
      os.walk(out, skip = prune)
        .filter(p => p.last == "classes" && os.isDir(p) && (p / os.up).last == "compile.dest")
        .sortBy(_.toString)
        .toVector
  }

  def libraryJars(root: Root): Vector[os.Path] = {
    val out = root.dir / "out"
    if (!os.isDir(out)) Vector.empty
    else
      os.walk(out, skip = prune)
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

  def notCompiled(root: Root): String =
    s"no compiled classes under ${root.dir / "out"}: run ./mill <module>.compile"
}
