package grit.projects.method

import grit.projects.method.MethodFixtures.*

import utest.*

object RoleViewTests extends TestSuite {

  val tests = Tests {
    test("depth over 3 is refused") {
      assert(
        RoleView.of(role("builder"), Set(change.name), 4) == Left(
          "a role view's depth is 4, not 0 to 3"
        ),
        RoleView.of(role("builder"), Set(change.name), 3).map(_.depth) == Right(3)
      )
    }
  }
}
