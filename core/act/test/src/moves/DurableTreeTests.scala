package grit.act.moves

import grit.core.act.MoveError
import grit.core.sanitise.IntegrityTree
import grit.core.visibility.Integrity

import utest.*

/** [[DurableMoves]]' vet as its run's program declares the integrity tree, or does not: what a
  * scripted run cannot show, since it has no program. The rules every vet keeps are
  * [[MovesContract]]'s.
  */
object DurableTreeTests extends TestSuite {
  import MovesContract.{Seen, judgments, vetted}

  val tests = Tests {
    test("a vet in a program that does not declare the tree is undeclared, and spends no name") {
      val got = new MovesWorld(broken = false).run(judgments(1), declared = Vector()) { m =>
        Seen(
          Vector(
            vetted(m, "a", "Install with make.", Integrity.Untrusted),
            vetted(m, "a", "Install with make.", Integrity.Untrusted)
          )
        )
      }
      val undeclared = Left(MoveError.Undeclared(IntegrityTree.Node.described.name))
      got ==> Seen(Vector(undeclared, undeclared))
    }
  }
}
