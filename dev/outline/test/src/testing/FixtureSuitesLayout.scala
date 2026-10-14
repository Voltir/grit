package grit.outline.testing

import grit.outline.locate.{Layout, Root}

/** The fixture's classes dir as main, and the fixture's suites (`dev/outline/fixture/suites`) as tests: a query through it
  * finds the fixture's definitions, and the tests exercising them at lines no other change moves. Throws when
  * `OUTLINE_FIXTURE_SUITES_CLASSES`, which the build sets for the tests, is unset.
  */
object FixtureSuitesLayout extends Layout {
  private lazy val suitesClasses: os.Path = sys.env.get("OUTLINE_FIXTURE_SUITES_CLASSES") match {
    case Some(path) => os.Path(path)
    case None => throw new Exception("OUTLINE_FIXTURE_SUITES_CLASSES env var not set")
  }

  def classesDirs(root: Root): Vector[os.Path] = FixtureLayout.classesDirs(root) :+ suitesClasses
  def libraryJars(root: Root): Vector[os.Path] = FixtureLayout.libraryJars(root)
  def notCompiled(root: Root): String = FixtureLayout.notCompiled(root)
  def isTest(classesDir: os.Path): Boolean = classesDir == suitesClasses
  def isTestSource(file: os.RelPath): Boolean =
    file.startsWith(os.rel / "dev" / "outline" / "fixture" / "suites")
  def moduleOf(root: Root, classesDir: os.Path): String = FixtureLayout.moduleOf(root, classesDir)
  def compile(modules: Vector[String]): String = FixtureLayout.compile(modules)
}
