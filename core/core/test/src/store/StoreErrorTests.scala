package grit.core.store

import grit.core.id.EntryId

import utest.*

/** [[StoreError]]'s two wordings: what a person or a log is told, and its kind alone. */
object StoreErrorTests extends TestSuite {

  private val errors: Vector[StoreError] = Vector(
    StoreError.DuplicateId(EntryId("ask:c1")),
    StoreError.DatabaseError("connection reset"),
    StoreError.Invalid("bound must be positive"),
    StoreError.Full(3)
  )

  val tests = Tests {
    test("each error is described in the words a person or a log is told, quoting its cause") {
      errors.map(_.described) ==> Vector(
        "entry ask:c1 already exists",
        "connection reset",
        "bound must be positive",
        "its plugin already holds the 3 documents it may, so nothing was kept"
      )
    }

    test("each error's kind names its case and quotes nothing it carries") {
      errors.map(_.kind) ==> Vector("duplicate id", "database error", "invalid", "full")
    }
  }
}
