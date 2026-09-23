package grit.core.interop

import utest.*

object StepResultTests extends TestSuite {

  val tests = Tests {
    test("a value can be a step result") {
      summon[StepResult[String]]
      summon[StepResult[Int]]
    }

    test("Unit cannot be a step result") {
      // Unit has no Jackson representation, so a step returning it would
      // replay as something DBOS cannot reconstruct.
      val err = compileError("summon[StepResult[Unit]]")
      assert(err.msg.contains("cannot be a step output"))
    }
  }
}
