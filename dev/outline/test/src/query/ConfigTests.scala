package grit.outline.query

import grit.outline.locate.Root

import utest.*

object ConfigTests extends TestSuite {

  def tests = Tests {
    test("a test-call line names the test call, and a second one is refused by its line") {
      val one = os.temp.dir()
      os.write(one / ".outline.conf", "test-call it\n")
      assert(Config.read(Root(one)).map(_.testCall) == Right("it"))
      val two = os.temp.dir()
      os.write(two / ".outline.conf", "test-call it\ntest-call sp\n")
      assert(Config.read(Root(two)).left.toOption.exists(_.contains("line 2")))
    }

    test("with no test-call line the test call is test") {
      assert(Config.empty.testCall == "test")
    }
  }
}
