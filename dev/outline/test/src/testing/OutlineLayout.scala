package grit.outline.testing

import grit.outline.locate.{Layout, MillLayout, Root}

/** Mill's layout narrowed to `grit.outline`'s own output, everything under `out/grit/outline`: a query through it over the
  * worktree reads only TASTy built before the test JVM started, never another module's that the same test run is
  * compiling beside it.
  */
object OutlineLayout extends Layout {
  private def own(root: Root)(p: os.Path): Boolean =
    p.startsWith(root.dir / "out" / "grit" / "outline")

  def classesDirs(root: Root): Vector[os.Path] = MillLayout.classesDirs(root).filter(own(root))
  def libraryJars(root: Root): Vector[os.Path] = MillLayout.libraryJars(root)
  def notCompiled(root: Root): String = MillLayout.notCompiled(root)
  def isTest(classesDir: os.Path): Boolean = MillLayout.isTest(classesDir)
  def isTestSource(file: os.RelPath): Boolean = MillLayout.isTestSource(file)
}
