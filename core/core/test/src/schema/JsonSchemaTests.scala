package grit.core.schema

import utest.*

/** [[JsonSchema.read]]: each refusal of the strict subset, named at its path, the limits at
  * their edges, and the JSON a schema is sent as.
  */
object JsonSchemaTests extends TestSuite {

  /** An object schema in the subset: `properties` in order, each required. */
  private def obj(properties: (String, ujson.Value)*): ujson.Obj =
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj.from(properties),
      "required" -> ujson.Arr.from(properties.map((n, _) => ujson.Str(n))),
      "additionalProperties" -> false
    )

  private val text: ujson.Obj = ujson.Obj("type" -> "string")

  /** `schema` with `key` set to `value`. */
  private def plus(schema: ujson.Obj, key: String, value: ujson.Value): ujson.Obj = {
    val out = ujson.Obj.from(schema.value)
    out(key) = value
    out
  }

  /** `schema`'s refusal, or a failure naming what it accepted. */
  private def refusal(schema: ujson.Value): SchemaError =
    JsonSchema.read(schema) match {
      case Left(e) => e
      case Right(s) => throw new java.lang.AssertionError(s"accepted: ${s.json}")
    }

  /** `schema` at `name` inside a list's items inside an object, so a refusal's path has depth. */
  private def nested(schema: ujson.Value): ujson.Obj =
    obj("edits" -> ujson.Obj("type" -> "array", "items" -> obj("name" -> schema)))

  private val NestedPath = "properties.edits.items.properties.name"

  /** Objects nested `depth` deep below the root, each holding the next as `next`. */
  private def deep(depth: Int): ujson.Obj =
    (1 to depth).foldLeft(obj("leaf" -> text))((inner, _) => obj("next" -> inner))

  val tests = Tests {
    test("a schema in the subset reads") {
      val schema = obj(
        "about" -> ujson.Obj(
          "type" -> ujson.Arr("string", "null"),
          "description" -> "Which.",
          "enum" -> ujson.Arr("current", "new", ujson.Null)
        ),
        "count" -> ujson.Obj("type" -> "integer", "minimum" -> 1, "maximum" -> 5),
        "ratio" -> ujson.Obj("type" -> "number", "minimum" -> 0.5),
        "sure" -> ujson.Obj("type" -> "boolean"),
        "edits" -> ujson.Obj(
          "type" -> "array",
          "items" -> obj("oldText" -> text),
          "minItems" -> 1,
          "maxItems" -> 3
        )
      )
      JsonSchema.read(schema).map(_.json) ==> Right(schema)
    }

    test("the root must be an object type") {
      refusal(ujson.Obj("type" -> "array", "items" -> text)) ==>
        SchemaError("", "the root must be \"type\": \"object\"")
      refusal(ujson.Obj("type" -> ujson.Arr("object", "null"))) ==>
        SchemaError("", "the root must be \"type\": \"object\"")
      refusal(ujson.Str("object")) ==> SchemaError("", "the root must be \"type\": \"object\"")
    }

    test("a keyword outside the subset is named at its path") {
      val outside = Vector(
        "$ref",
        "anyOf",
        "oneOf",
        "allOf",
        "not",
        "const",
        "pattern",
        "format",
        "default",
        "$schema",
        "title"
      )
      outside.map(k => refusal(nested(plus(text, k, "x")))) ==> outside.map(k =>
        SchemaError(NestedPath, s"`$k` is not in the subset every strict mode accepts")
      )
    }

    test("a type outside the subset, or no type") {
      val why = "`type` must be one of string, number, integer, boolean, object, array, " +
        "or a list of one of those and null"
      refusal(nested(ujson.Obj("type" -> "date"))) ==> SchemaError(
        NestedPath,
        s"$why; got \"date\""
      )
      refusal(nested(ujson.Obj("type" -> ujson.Arr("string", "integer")))) ==>
        SchemaError(NestedPath, s"$why; got [\"string\",\"integer\"]")
      refusal(nested(ujson.Obj("type" -> ujson.Arr("null")))) ==>
        SchemaError(NestedPath, s"$why; got [\"null\"]")
      refusal(nested(ujson.Obj("description" -> "No type."))) ==>
        SchemaError(NestedPath, "has no `type`")
      refusal(nested(ujson.Obj("type" -> "string", "description" -> 3))) ==>
        SchemaError(NestedPath, "`description` must be a string")
    }

    test("an object must close itself and require every property") {
      val inner = obj("a" -> text, "b" -> text)
      refusal(nested(ujson.Obj.from(inner.value.filter(_._1 != "additionalProperties")))) ==>
        SchemaError(NestedPath, "an object must have \"additionalProperties\": false")
      refusal(nested(plus(inner, "additionalProperties", true))) ==>
        SchemaError(NestedPath, "an object must have \"additionalProperties\": false")
      refusal(nested(plus(inner, "required", ujson.Arr("a")))) ==> SchemaError(
        NestedPath,
        "`b` must be in `required`: under strict mode every property is, and one that may " +
          "be left out admits null instead"
      )
      refusal(nested(plus(inner, "required", ujson.Arr("a", "b", "c")))) ==>
        SchemaError(NestedPath, "`required` names `c`, which is not a property")
      refusal(nested(plus(inner, "required", ujson.Arr("a", 2)))) ==>
        SchemaError(NestedPath, "`required` must be a list of property names")
      refusal(nested(plus(inner, "properties", ujson.Arr()))) ==>
        SchemaError(NestedPath, "`properties` must be an object")
    }

    test("an array must have items") {
      refusal(nested(ujson.Obj("type" -> "array"))) ==>
        SchemaError(NestedPath, "an array must have `items`")
    }

    test("a keyword on a type it does not apply to") {
      refusal(nested(plus(text, "minimum", 1))) ==>
        SchemaError(NestedPath, "`minimum` does not apply to a string")
      refusal(nested(ujson.Obj("type" -> "integer", "minItems" -> 1))) ==>
        SchemaError(NestedPath, "`minItems` does not apply to an integer")
      refusal(nested(plus(text, "items", text))) ==>
        SchemaError(NestedPath, "`items` does not apply to a string")
      refusal(
        nested(ujson.Obj("type" -> "array", "items" -> text, "properties" -> ujson.Obj()))
      ) ==>
        SchemaError(NestedPath, "`properties` does not apply to an array")
    }

    test("a bound must be a number, and a minimum not above its maximum") {
      refusal(nested(ujson.Obj("type" -> "number", "minimum" -> "1"))) ==>
        SchemaError(NestedPath, "`minimum` must be a number")
      refusal(nested(ujson.Obj("type" -> "array", "items" -> text, "minItems" -> -1))) ==>
        SchemaError(NestedPath, "`minItems` must be a whole number of at least 0")
      refusal(nested(ujson.Obj("type" -> "integer", "minimum" -> 5, "maximum" -> 1))) ==>
        SchemaError(NestedPath, "`minimum` 5 is above `maximum` 1")
      refusal(
        nested(ujson.Obj("type" -> "array", "items" -> text, "minItems" -> 3, "maxItems" -> 2))
      ) ==> SchemaError(NestedPath, "`minItems` 3 is above `maxItems` 2")
    }

    test("an enum must be a non-empty list its type admits") {
      refusal(nested(plus(text, "enum", ujson.Arr()))) ==>
        SchemaError(NestedPath, "`enum` is empty")
      refusal(nested(plus(text, "enum", "a"))) ==> SchemaError(NestedPath, "`enum` must be a list")
      refusal(nested(plus(text, "enum", ujson.Arr("a", 3)))) ==>
        SchemaError(NestedPath, "`enum` holds 3, which its type refuses")
      refusal(nested(plus(text, "enum", ujson.Arr("a", ujson.Null)))) ==>
        SchemaError(NestedPath, "`enum` holds null, which its type refuses")
      refusal(nested(ujson.Obj("type" -> "integer", "enum" -> ujson.Arr(1, 1.5)))) ==>
        SchemaError(NestedPath, "`enum` holds 1.5, which its type refuses")
    }

    test("the limits, at their edges") {
      JsonSchema.read(deep(JsonSchema.MaxDepth)).isRight ==> true
      val tooDeepPath = (1 to JsonSchema.MaxDepth + 1).map(_ => "properties.next").mkString(".")
      refusal(deep(JsonSchema.MaxDepth + 1)) ==> SchemaError(
        tooDeepPath,
        s"objects and arrays nest more than ${JsonSchema.MaxDepth} deep below the root"
      )

      val many = (n: Int) => obj((1 to n).map(i => s"p$i" -> (text: ujson.Value))*)
      JsonSchema.read(many(JsonSchema.MaxProperties)).isRight ==> true
      refusal(many(JsonSchema.MaxProperties + 1)) ==> SchemaError(
        s"properties.p${JsonSchema.MaxProperties + 1}",
        s"more than ${JsonSchema.MaxProperties} properties in all"
      )

      val options =
        (n: Int) => nested(plus(text, "enum", ujson.Arr.from((1 to n).map(i => s"o$i"))))
      JsonSchema.read(options(JsonSchema.MaxEnum)).isRight ==> true
      refusal(options(JsonSchema.MaxEnum + 1)) ==> SchemaError(
        NestedPath,
        s"`enum` holds ${JsonSchema.MaxEnum + 1} values; at most ${JsonSchema.MaxEnum} are allowed"
      )
    }

    test("the first refusal in document order: a node before what it holds") {
      val schema = obj("a" -> ujson.Obj("type" -> "date"), "b" -> plus(text, "pattern", "x"))
      refusal(plus(schema, "title", "T")) ==>
        SchemaError("", "`title` is not in the subset every strict mode accepts")
      refusal(schema) ==> SchemaError(
        "properties.a",
        "`type` must be one of string, number, integer, boolean, object, array, " +
          "or a list of one of those and null; got \"date\""
      )
    }

    test("its JSON: keywords in a fixed order, properties in the order read, read again the same") {
      val scrambled = ujson.Obj(
        "additionalProperties" -> false,
        "required" -> ujson.Arr("b", "a"),
        "properties" -> ujson.Obj(
          "b" -> ujson.Obj("maximum" -> 5, "type" -> "integer", "description" -> "B."),
          "a" -> ujson.Obj("enum" -> ujson.Arr("x"), "type" -> "string")
        ),
        "type" -> "object"
      )
      val written = JsonSchema.read(scrambled).map(_.json)
      written.map(ujson.write(_)) ==> Right(
        """{"type":"object","properties":{"b":{"type":"integer","description":"B.",""" +
          """"maximum":5},"a":{"type":"string","enum":["x"]}},"required":["b","a"],""" +
          """"additionalProperties":false}"""
      )
      written.flatMap(JsonSchema.read).map(_.json) ==> written
    }

    test("its JSON is a fresh copy: changing it changes nothing kept") {
      val schema = JsonSchema.read(obj("a" -> text))
      schema.foreach(s => s.json("properties")("a")("type") = "integer")
      schema.map(_.json("properties")("a")("type")) ==> Right(ujson.Str("string"))
    }
  }
}
