package grit.act.moves

import grit.core.act.MoveError
import grit.core.sanitise.IntegrityTree
import grit.core.stitch.StrandWorld
import grit.core.visibility.Integrity

import utest.*

/** [[DurableMoves]]' vet and declassify as its run's program declares the integrity tree and
  * the declassification, or does not: what a scripted run cannot show, since it has no program.
  * The rules every vet and declassify keeps are [[MovesContract]]'s.
  */
object DurableTreeTests extends TestSuite {
  import MovesContract.{Seen, applied, declassifies, judgments, name, outcome, vetted}

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

    test(
      "a declassify in a program that does not declare the declassification is undeclared, and spends no name"
    ) {
      val shelves = MovesContract.Shelves(needed = Vector(MovesContract.Cards))
      val (source, policy) = applied(shelves, "c1")
      val w = new MovesWorld(
        broken = false,
        MovesContract.Declassing,
        Integrity.Authority,
        documents = shelves.documents,
        compartments = MovesContract.Declared
      )
      val (got, asked) = w.answering(
        declassifies(2, 2),
        new StrandWorld,
        shelves.needs,
        MovesContract.Rewrote,
        new grit.core.act.MovesFixtures.Judging(),
        Vector(IntegrityTree.Node)
      ) { m =>
        Seen(Vector("d", "d").map(n => m.declassify(name(n), source, policy).map(outcome)))
      }
      val undeclared = Left(MoveError.Undeclared("declassification"))
      (got, asked) ==> (Seen(Vector(undeclared, undeclared)), Vector())
    }
  }
}
