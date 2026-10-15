package grit.knowledge.card

import grit.core.act.MoveName
import grit.core.id.CorpusName

import utest.*

object CardMovesTests extends TestSuite {

  private val corpus =
    CorpusName.of("repo").fold(why => throw new java.lang.AssertionError(why), identity)

  val tests = Tests {
    test("a unit's move is named its phase's word, a slash and its key, within 72 characters") {
      val long = "src/" + ("deep/" * 60) + "file.scala"
      val key = CardKey.of(corpus, long)
      val names = Phase.values.toVector.map(p => MoveName.value(CardMoves.of(p, key)))
      // Recorded move names: a changed word stops every run recorded under the old one replaying.
      (names, names.forall(_.length <= 72)) ==> (
        Vector("list", "read", "summary", "write", "relabel").map(w => s"$w/${CardKey.value(key)}"),
        true
      )
    }

    test("a unit's check is named vet, its key and its window, within 79 characters") {
      val key = CardKey.of(corpus, "docs/a.md")
      val name = MoveName.value(CardMoves.vet(key, Int.MaxValue))
      // Recorded move names, as above.
      (name, name.length <= 79) ==> (s"vet/${CardKey.value(key)}/${Int.MaxValue}", true)
    }

    test("the whole-run keeps are held and drop") {
      assert(MoveName.value(CardMoves.Held) == "held")
      assert(MoveName.value(CardMoves.Drop) == "drop")
    }
  }
}
