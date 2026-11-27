package grit.core.host

import utest.*

/** What every [[Reach]] does, over one layout: under a base directory `b`, the checkout `b/work/co`
  * (its root) holding the directory `src` and three links, `link-out` to `b` (absolute),
  * `link-in` to `src` and `up` to `..`; and the directory `b/work/other`.
  */
abstract class ReachContract extends TestSuite {

  /** `body` over a [[Reach]] whose checkout is laid out as the class says, with its base. */
  protected def over[A](body: (Reach^, AbsPath) => A): A

  private def at(base: AbsPath, rest: String): Reached =
    Reached.Outside(AbsPath.resolve(base, rest))

  val tests = Tests {
    test("a path inside the checkout, there or not yet, is inside") {
      over { (reach, base) =>
        val co = AbsPath.value(AbsPath.resolve(base, "work/co"))
        assert(
          Vector("src", "src/x", "new/dir/x", ".", "link-in/x", s"$co/src", "a/../src")
            .map(reach.reached) == Vector.fill(7)(Reached.Inside)
        )
      }
    }

    test("a `..` out of the checkout, there or not, leads where it climbs to") {
      over { (reach, base) =>
        assert(reach.reached("../other") == at(base, "work/other"))
        assert(reach.reached("missing/../../other/x") == at(base, "work/other/x"))
        assert(reach.reached("src/../..") == at(base, "work"))
      }
    }

    test("an absolute path outside the checkout leads to itself") {
      over { (reach, base) =>
        assert(reach.reached("/") == Reached.Outside(AbsPath.Root))
        assert(reach.reached(AbsPath.value(base)) == Reached.Outside(base))
      }
    }

    test("a link is followed, and a `..` after it climbs from where it led") {
      over { (reach, base) =>
        assert(reach.reached("link-out") == Reached.Outside(base))
        assert(reach.reached("link-out/x") == at(base, "x"))
        assert(reach.reached("up/other") == at(base, "work/other"))
        assert(reach.reached("src/../link-out/work") == at(base, "work"))
        assert(reach.reached("link-out/work/co/src") == Reached.Inside)
      }
    }
  }
}
