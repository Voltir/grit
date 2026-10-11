package grit.knowledge.card

import grit.core.act.MoveName
import grit.core.id.CorpusName

import utest.*

object CardMovesTests extends TestSuite {

  private val corpus =
    CorpusName.of("repo").fold(why => throw new java.lang.AssertionError(why), identity)

  val tests = Tests {
    test("a unit's move is named its phase, a slash and its key, within 72 characters") {
      val long = "src/" + ("deep/" * 60) + "file.scala"
      val key = CardKey.of(corpus, long)
      val names = Phase.values.toVector.map(p => MoveName.value(CardMoves.of(p, key)))
      assert(names.contains(s"summary/${CardKey.value(key)}"))
      assert(names.forall(_.length <= 72))
      assert(names.distinct.size == Phase.values.size)
    }

    test("the whole-run keeps are held and drop") {
      assert(MoveName.value(CardMoves.Held) == "held")
      assert(MoveName.value(CardMoves.Drop) == "drop")
    }
  }
}
