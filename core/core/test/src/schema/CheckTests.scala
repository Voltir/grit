package grit.core.schema

import grit.core.model.ArgRepair

import utest.*

/** [[JsonSchema.check]]: what conforms is kept as sent, the first mismatch in document order
  * at its path, `null` only where a type lists it, and each repair made only where asked and
  * where the schema directs it.
  */
object CheckTests extends TestSuite {

  private val schema: JsonSchema = JsonSchema.read(
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "about" -> ujson.Obj("type" -> "string", "enum" -> ujson.Arr("current", "new")),
        "label" -> ujson.Obj("type" -> "string"),
        "count" -> ujson.Obj("type" -> "integer", "minimum" -> 1, "maximum" -> 5),
        "ratio" -> ujson.Obj("type" -> ujson.Arr("number", "null")),
        "edits" -> ujson.Obj(
          "type" -> "array",
          "items" -> ujson.Obj(
            "type" -> "object",
            "properties" -> ujson.Obj("oldText" -> ujson.Obj("type" -> "string")),
            "required" -> ujson.Arr("oldText"),
            "additionalProperties" -> false
          ),
          "minItems" -> 1,
          "maxItems" -> 3
        )
      ),
      "required" -> ujson.Arr("about", "label", "count", "ratio", "edits"),
      "additionalProperties" -> false
    )
  ) match {
    case Right(s) => s
    case Left(e) => throw new java.lang.AssertionError(e.message)
  }

  private def read(v: ujson.Value): JsonSchema = JsonSchema.read(v) match {
    case Right(s) => s
    case Left(e) => throw new java.lang.AssertionError(e.message)
  }

  /** A card's summary: its prose and a required list of terms. */
  private val carded: JsonSchema = read(
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "summary" -> ujson.Obj("type" -> "string"),
        "terms" -> ujson.Obj("type" -> "array", "items" -> ujson.Obj("type" -> "string"))
      ),
      "required" -> ujson.Arr("summary", "terms"),
      "additionalProperties" -> false
    )
  )

  /** A topic's write: two prose fields and a nullable meeting. */
  private val written: JsonSchema = read(
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "lead" -> ujson.Obj("type" -> "string"),
        "passage" -> ujson.Obj("type" -> "string"),
        "meeting" -> ujson.Obj(
          "type" -> ujson.Arr("object", "null"),
          "properties" -> ujson.Obj(
            "state" -> ujson.Obj("type" -> "string"),
            "who" -> ujson.Obj("type" -> "array", "items" -> ujson.Obj("type" -> "string"))
          ),
          "required" -> ujson.Arr("state", "who"),
          "additionalProperties" -> false
        )
      ),
      "required" -> ujson.Arr("lead", "passage", "meeting"),
      "additionalProperties" -> false
    )
  )

  private def edit(old: ujson.Value): ujson.Obj = ujson.Obj("oldText" -> old)

  /** A value that conforms, in property order. */
  private def good: ujson.Obj = ujson.Obj(
    "about" -> "new",
    "label" -> "a",
    "count" -> 2,
    "ratio" -> 0.5,
    "edits" -> ujson.Arr(edit("x"))
  )

  /** `good` with `key` set to `value`, in its place, or added last. */
  private def with_(key: String, value: ujson.Value): ujson.Obj = {
    val out = good
    out(key) = value
    out
  }

  private def without(key: String): ujson.Obj = ujson.Obj.from(good.value.filter(_._1 != key))

  private def mismatch(value: ujson.Value, repairs: Set[ArgRepair] = Set.empty): Option[Mismatch] =
    schema.check(value, repairs).left.toOption

  private def checked(value: ujson.Value, repairs: Set[ArgRepair]): Either[Mismatch, ujson.Value] =
    schema.check(value, repairs).map(_.json)

  val tests = Tests {
    test("what conforms is kept as sent, written compactly") {
      // Sent in the reverse of the schema's property order, and kept in the order sent.
      val reversed = ujson.Obj.from(good.value.toVector.reverse)
      schema.check(reversed, Set.empty).map(_.text) ==>
        Right("""{"edits":[{"oldText":"x"}],"ratio":0.5,"count":2,"label":"a","about":"new"}""")
    }

    test("each kind of mismatch, at its path") {
      mismatch(ujson.Str("x")) ==> Some(Mismatch("", "expected an object, got \"x\""))
      mismatch(without("count")) ==> Some(Mismatch("count", "is missing"))
      mismatch(with_("extra", 1)) ==> Some(
        Mismatch(
          "extra",
          "is not a property here; the properties are `about`, `label`, `count`, `ratio`, `edits`"
        )
      )
      mismatch(with_("count", "x")) ==> Some(Mismatch("count", "expected an integer, got \"x\""))
      mismatch(with_("count", 1.5)) ==> Some(Mismatch("count", "expected an integer, got 1.5"))
      mismatch(with_("about", "old")) ==>
        Some(Mismatch("about", "expected one of \"current\", \"new\", got \"old\""))
      mismatch(with_("count", 9)) ==> Some(Mismatch("count", "expected at most 5, got 9"))
      mismatch(with_("count", 0)) ==> Some(Mismatch("count", "expected at least 1, got 0"))
      mismatch(with_("edits", ujson.Arr())) ==>
        Some(Mismatch("edits", "expected at least 1 item, got 0"))
      mismatch(with_("edits", ujson.Arr(edit("a"), edit("b"), edit("c"), edit("d")))) ==>
        Some(Mismatch("edits", "expected at most 3 items, got 4"))
      mismatch(with_("edits", ujson.Arr(edit("a"), edit("b"), edit(3)))) ==>
        Some(Mismatch("edits[2].oldText", "expected a string, got 3"))
    }

    test("the first mismatch in document order") {
      val twoWrong = ujson.Obj(
        "about" -> "old",
        "label" -> "a",
        "count" -> "x",
        "ratio" -> 0.5,
        "edits" -> ujson.Arr(edit("x"))
      )
      mismatch(twoWrong).map(_.path) ==> Some("about")
      val extraFirst = ujson.Obj.from(Vector("extra" -> ujson.Num(1)) ++ with_("count", 0).value)
      mismatch(extraFirst).map(_.path) ==> Some("extra")
      val missingAndWrong = ujson.Obj.from(with_("count", 0).value.filter(_._1 != "about"))
      mismatch(missingAndWrong).map(_.path) ==> Some("count")
    }

    test("null only where the type lists it") {
      checked(with_("ratio", ujson.Null), Set.empty) ==> Right(with_("ratio", ujson.Null))
      mismatch(with_("count", ujson.Null)) ==> Some(
        Mismatch("count", "expected an integer, got null")
      )
      mismatch(with_("ratio", "x")) ==>
        Some(Mismatch("ratio", "expected a number or null, got \"x\""))
      checked(without("ratio"), Set.empty) ==> Right(with_("ratio", ujson.Null))
      mismatch(without("label")) ==> Some(Mismatch("label", "is missing"))
    }

    test("a quoted number is read as its number only when asked, and only where one is asked for") {
      checked(with_("count", "3"), Set(ArgRepair.QuotedNumber)) ==> Right(with_("count", 3))
      checked(with_("ratio", "-4"), Set(ArgRepair.QuotedNumber)).map(_("ratio")) ==>
        Right(ujson.Num(-4))
      mismatch(with_("count", "3"), Set(ArgRepair.QuotedList)) ==>
        Some(Mismatch("count", "expected an integer, got \"3\""))
      checked(with_("label", "42"), ArgRepair.values.toSet) ==> Right(with_("label", "42"))
      mismatch(with_("count", "3.5"), Set(ArgRepair.QuotedNumber)) ==>
        Some(Mismatch("count", "expected an integer, got \"3.5\""))
    }

    test("a quoted list is read as its list only when asked, and checked as one") {
      val quoted = ujson.Str("""[{"oldText":"a"},{"oldText":"b"}]""")
      checked(with_("edits", quoted), Set(ArgRepair.QuotedList)) ==>
        Right(with_("edits", ujson.Arr(edit("a"), edit("b"))))
      mismatch(with_("edits", quoted), Set(ArgRepair.QuotedNumber)) ==> Some(
        Mismatch("edits", s"expected an array, got ${ujson.write(quoted)}")
      )
      mismatch(with_("edits", ujson.Str("""[{"oldText":1}]""")), Set(ArgRepair.QuotedList)) ==>
        Some(Mismatch("edits[0].oldText", "expected a string, got 1"))
      checked(with_("label", ujson.Str("[1]")), ArgRepair.values.toSet) ==>
        Right(with_("label", ujson.Str("[1]")))
    }

    test(
      "a field carrying a required list after its close is cut there, and the list read, only when asked"
    ) {
      // As a provider parsed a model's call: `terms` given inside `summary`, after its close.
      val sent = ujson.Obj(
        "summary" -> ("An order form. Signature blocks are unfilled.</summary>\n" +
          "<parameter name=\"terms\">[\"revenue share\", \"payment terms\"]"),
        "terms" -> ujson.Null
      )
      (
        carded.check(sent, Set(ArgRepair.CarriedParameter)).map(_.json),
        carded.check(sent, Set.empty).left.map(_.path)
      ) ==> (
        Right(
          ujson.Obj(
            "summary" -> "An order form. Signature blocks are unfilled.",
            "terms" -> ujson.Arr("revenue share", "payment terms")
          )
        ),
        Left("terms")
      )
    }

    test("a field carrying a nullable object after its close is cut there, and the object read") {
      val meeting = ujson.Obj("state" -> "proposed", "who" -> ujson.Arr("Ana"))
      val sent = ujson.Obj(
        "lead" -> "Meetings with a partner.",
        "passage" -> (s"Nick suggested inviting Emily.</passage>\n<parameter name=\"meeting\">" +
          ujson.write(meeting)),
        "meeting" -> ujson.Null
      )
      written.check(sent, Set(ArgRepair.CarriedParameter)).map(_.json) ==> Right(
        ujson.Obj(
          "lead" -> "Meetings with a partner.",
          "passage" -> "Nick suggested inviting Emily.",
          "meeting" -> meeting
        )
      )
    }

    test("markup carrying a name that is no property, or one sent already, is left as sent") {
      val stray = "A passage.</passage>\n<parameter name=\"mood\">\"glad\""
      val again = "A passage.</passage>\n<parameter name=\"lead\">\"Another.\""
      Vector(stray, again).map(p =>
        written
          .check(
            ujson.Obj("lead" -> "A lead.", "passage" -> p, "meeting" -> ujson.Null),
            Set(ArgRepair.CarriedParameter)
          )
          .map(_.json("passage").str)
      ) ==> Vector(Right(stray), Right(again))
    }

    test("a conforming value's JSON is a fresh copy: changing it changes nothing kept") {
      val conforming = schema.check(good, Set.empty)
      conforming.foreach(c => c.json("label") = "changed")
      conforming.map(_.json("label")) ==> Right(ujson.Str("a"))
    }

    test("a mismatch's message: its path, then why; why alone at the root") {
      Mismatch("edits[2].oldText", "expected a string, got 3").message ==>
        "edits[2].oldText: expected a string, got 3"
      Mismatch("", "expected an object, got 1").message ==> "expected an object, got 1"
    }
  }
}
