package grit.core.either

import utest.*

object TraversalTests extends TestSuite {

  private def even(n: Int): Either[String, Int] = if (n % 2 == 0) Right(n / 2) else Left(s"odd $n")

  val tests = Tests {
    test("traversing by Right is the vector itself, the empty one included") {
      Traversal.traverse(Vector(1, 2, 3))(Right(_)) ==> Right(Vector(1, 2, 3))
      Traversal.traverse(Vector.empty[Int])(even) ==> Right(Vector.empty)
    }
    test("every value Right: each one mapped, in order") {
      Traversal.traverse(Vector(4, 2, 8))(even) ==> Right(Vector(2, 1, 4))
    }
    test("a Left: the first one, and nothing after it is mapped") {
      var mapped = Vector.empty[Int]
      val got = Traversal.traverse(Vector(2, 3, 4, 5)) { n =>
        mapped = mapped :+ n
        even(n)
      }
      (got, mapped) ==> (Left("odd 3"), Vector(2, 3))
    }
    test("sequence is traversing by identity") {
      val es = Vector(Vector(2, 4), Vector(2, 3, 5), Vector.empty[Int]).map(_.map(even))
      es.map(Traversal.sequence) ==> es.map(Traversal.traverse(_)(identity))
      es.map(Traversal.sequence) ==> Vector(Right(Vector(1, 2)), Left("odd 3"), Right(Vector.empty))
    }
  }
}
