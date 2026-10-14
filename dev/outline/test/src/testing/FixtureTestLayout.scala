package grit.outline.testing

import grit.outline.locate.{Layout, Root}

/** The fixture's layout with its classes dir and every source counted as tests': a query through it finds no main
  * definition, and its test calls name its sites.
  */
object FixtureTestLayout extends Layout {
  def classesDirs(root: Root): Vector[os.Path] = FixtureLayout.classesDirs(root)
  def libraryJars(root: Root): Vector[os.Path] = FixtureLayout.libraryJars(root)
  def notCompiled(root: Root): String = FixtureLayout.notCompiled(root)
  def isTest(classesDir: os.Path): Boolean = true
  def isTestSource(file: os.RelPath): Boolean = true
}
