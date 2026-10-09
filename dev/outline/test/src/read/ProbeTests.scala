package grit.outline.read

import utest.*

object ProbeTests extends TestSuite {
  def tests = Tests {
    test("the fixture's documented trait reads with its docstring and line range") {
      val fixtureClasses = sys.env.get("OUTLINE_FIXTURE_CLASSES") match {
        case Some(path) => path
        case None => throw new Exception("OUTLINE_FIXTURE_CLASSES env var not set")
      }
      val tastyFiles = os
        .walk(os.Path(fixtureClasses))
        .filter(p => p.ext == "tasty")
        .toVector
      val facts = Probe.facts(tastyFiles, Vector.empty)
      val traitALine = facts.find(_.startsWith("grit.outline.fixture.A |"))
      traitALine match {
        case Some(line) =>
          assert(line.contains("| 7-14 |"))
          assert(line.contains("doc=true"))
        case None => throw new Exception("grit.outline.fixture.A line not found in facts")
      }
    }
  }
}
