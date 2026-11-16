package grit.core.json

import utest.*

object FieldsTests extends TestSuite {

  val tests = Tests {
    test("a text is an object only when it is JSON and an object") {
      Vector("""{"a": 1}""", "{}", "[1]", "3", "\"x\"", "{", "").map(Fields.objectOf) ==>
        Vector(Some(ujson.Obj("a" -> 1)), Some(ujson.Obj()), None, None, None, None, None)
    }
  }
}
