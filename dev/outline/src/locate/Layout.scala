package grit.outline.locate

/** Where a build writes its compiled `.tasty` files under a root. */
trait Layout {

  /** Every directory under `root` holding compiled classes, sorted; empty when nothing is compiled. */
  def classesDirs(root: Root): Vector[os.Path]

  /** The one line telling a person how to compile `root` when `classesDirs` is empty. */
  def notCompiled(root: Root): String
}

/** Mill's layout: `out/**/compile.dest/classes`. */
object MillLayout extends Layout {

  def classesDirs(root: Root): Vector[os.Path] = {
    val out = root.dir / "out"
    if (!os.isDir(out)) Vector.empty
    else
      os.walk(
        out,
        skip = p =>
          (p / os.up).last == "classes" || (p.last.endsWith(".dest") && p.last != "compile.dest")
      ).filter(p => p.last == "classes" && os.isDir(p) && (p / os.up).last == "compile.dest")
        .sortBy(_.toString)
        .toVector
  }

  def notCompiled(root: Root): String =
    s"no compiled classes under ${root.dir / "out"}: run ./mill <module>.compile"
}
