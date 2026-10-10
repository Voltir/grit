package grit.outline.testing

import grit.outline.locate.{Layout, MillLayout, Root}

/** Mill's layout narrowed to the fixture's classes dir: a query through it over the worktree reads the fixture's TASTy and no other, and finds no test classes dir. Throws when `OUTLINE_FIXTURE_CLASSES`, which the build sets for the tests, is unset. */
object FixtureLayout extends Layout {
  private lazy val fixtureClasses: os.Path = sys.env.get("OUTLINE_FIXTURE_CLASSES") match {
    case Some(path) => os.Path(path)
    case None => throw new Exception("OUTLINE_FIXTURE_CLASSES env var not set")
  }

  def classesDirs(root: Root): Vector[os.Path] = Vector(fixtureClasses)
  def libraryJars(root: Root): Vector[os.Path] = MillLayout.libraryJars(root)
  def notCompiled(root: Root): String = MillLayout.notCompiled(root)
  def isTest(classesDir: os.Path): Boolean = MillLayout.isTest(classesDir)
  def isTestSource(file: os.RelPath): Boolean = MillLayout.isTestSource(file)
}
