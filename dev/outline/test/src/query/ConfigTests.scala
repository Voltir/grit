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

    test(
      "hides answers for a path under a hidden prefix, not one outside it, and a prefix ending in / does not hide a path sharing only its leading characters"
    ) {
      val config = Config(Vector("dev/"), Vector.empty)
      assert(config.hides("dev/outline/src/A.scala"))
      assert(!config.hides("src/A.scala"))
      assert(!config.hides("devel/A.scala"))
    }

    test(
      "a line naming a setting this outline does not know is ignored and kept, so a newer .outline.conf still reads"
    ) {
      val dir = os.temp.dir()
      os.write(dir / ".outline.conf", "hide a\nfrobnicate x y\nsecondary dev/\n")
      assert(
        Config.read(Root(dir)) == Right(
          Config(
            Vector("a"),
            Vector.empty,
            secondary = Vector("dev/"),
            ignored = Vector("line 2: frobnicate x y")
          )
        )
      )
    }

    test(
      "a line opening with each known word but written wrong is refused, named by its line in the file, comments counted"
    ) {
      val refusals = Vector("hide", "secondary   ", "area store Store", "test-call").map { bad =>
        val dir = os.temp.dir()
        os.write(dir / ".outline.conf", s"hide a/b\n# a comment\n$bad\n")
        Config.read(Root(dir)).left.map(_.stripPrefix(s"${dir / ".outline.conf"}: "))
      }
      def refused(line: String): Either[String, Config] = Left(
        s"line 3 is none of `hide <prefix>`, `secondary <prefix>`, `area <name> \"<purpose>\": <items>` and `test-call <name>`: $line"
      )
      assert(
        refusals == Vector(
          refused("hide"),
          refused("secondary"),
          refused("area store Store"),
          refused("test-call")
        )
      )
    }

    test("a comment line is skipped, not kept as a line this outline does not know") {
      val dir = os.temp.dir()
      os.write(dir / ".outline.conf", "hide a/b\n# a comment\n")
      assert(Config.read(Root(dir)) == Right(Config(Vector("a/b"), Vector.empty)))
    }

    test("with no test-call line the test call is test") {
      assert(Config.empty.testCall == "test")
    }

    test("secondary lines are read in order, beside the other kinds of line") {
      val dir = os.temp.dir()
      os.write(
        dir / ".outline.conf",
        "hide a/b\nsecondary eval/harness/\ntest-call check\nsecondary dev/\n"
      )
      assert(
        Config.read(Root(dir)) == Right(
          Config(Vector("a/b"), Vector.empty, "check", Vector("eval/harness/", "dev/"))
        )
      )
    }
  }
}
