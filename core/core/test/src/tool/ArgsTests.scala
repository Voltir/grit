package grit.core.tool

import grit.core.model.ArgRepair
import grit.core.schema.SchemaError

import utest.*

/** [[Args]] and [[Field]]: the schema each field kind shows, what a read accepts and refuses,
  * the message a refusal gives the model, and that what a read accepts the schema admits.
  */
object ArgsTests extends TestSuite {

  private val args = Args.of(
    (
      about = Field.oneOf("Which.", "current", "earlier", "new"),
      name = Field.text("A name.").optional,
      count = Field.count("How many.", 1, 5),
      sure = Field.flag("Sure?").optional
    )
  )

  /** Whether `schema` admits `value`: its type, enum and bounds; an object's required
    * properties, no others, and each property's schema; an array's length and each item's
    * schema. A hand check over exactly the keywords the schemas here use.
    */
  private def admits(schema: ujson.Value, value: ujson.Value): Boolean = {
    val s = schema.obj
    val types = s.get("type").toVector.flatMap {
      case ujson.Arr(ts) => ts.toVector.map(_.str)
      case t => Vector(t.str)
    }
    val typed = types.exists {
      case "object" => value.objOpt.isDefined
      case "array" => value.arrOpt.isDefined
      case "string" => value.strOpt.isDefined
      case "boolean" => value.boolOpt.isDefined
      case "integer" => value.numOpt.exists(_.isWhole)
      case "null" => value.isNull
      case _ => false
    }
    typed &&
    s.get("enum").forall(_.arr.contains(value)) &&
    s.get("minimum").forall(m => value.numOpt.forall(_ >= m.num)) &&
    s.get("maximum").forall(m => value.numOpt.forall(_ <= m.num)) &&
    value.objOpt.forall { sent =>
      val props = s.get("properties").fold(Map.empty[String, ujson.Value])(_.obj.toMap)
      s.get("required").forall(_.arr.forall(r => sent.contains(r.str))) &&
      sent.forall((k, v) => props.get(k).exists(admits(_, v)))
    } &&
    value.arrOpt.forall { items =>
      s.get("minItems").forall(m => items.size >= m.num) &&
      s.get("items").forall(i => items.forall(admits(i, _)))
    }
  }

  val tests = Tests {
    test("each field kind's schema") {
      args.schema(strict = false) ==> ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj(
          "about" -> ujson.Obj(
            "type" -> "string",
            "enum" -> ujson.Arr("current", "earlier", "new"),
            "description" -> "Which."
          ),
          "name" -> ujson.Obj("type" -> "string", "description" -> "A name."),
          "count" -> ujson.Obj(
            "type" -> "integer",
            "minimum" -> 1,
            "maximum" -> 5,
            "description" -> "How many."
          ),
          "sure" -> ujson.Obj("type" -> "boolean", "description" -> "Sure?")
        ),
        "required" -> ujson.Arr("about", "count"),
        "additionalProperties" -> false
      )
    }

    test("strict: every property required, an optional one nullable") {
      val s = args.schema(strict = true)
      s("required") ==> ujson.Arr("about", "name", "count", "sure")
      s("properties")("name")("type") ==> ujson.Arr("string", "null")
      s("properties")("about")("type") ==> ujson.Str("string")
      val optionalChoice = Args.of((pick = Field.oneOf("P.", "a", "b").optional))
      optionalChoice.schema(strict = true)("properties")("pick")("enum") ==>
        ujson.Arr("a", "b", ujson.Null)
    }

    test("a read gives the named tuple") {
      val read = args.read(ujson.Obj("about" -> "new", "name" -> "Knots", "count" -> 3))
      read.map(a => (a.about, a.name, a.count, a.sure)) ==>
        Right(("new", Some("Knots"), 3, None))
      args
        .read(ujson.Obj("about" -> "current", "name" -> ujson.Null, "count" -> 5.0))
        .map(_.name) ==>
        Right(None)
    }

    test("each refusal, first in order") {
      args.read(ujson.Arr(1)) ==> Left(ArgsError.NotAnObject("[1]"))
      args.read(ujson.Obj("about" -> "new", "count" -> 1, "extra" -> 1)) ==>
        Left(ArgsError.Unexpected("extra", Vector("about", "name", "count", "sure")))
      args.read(ujson.Obj("count" -> 1)) ==>
        Left(ArgsError.Missing("about", "one of `current`, `earlier`, `new`"))
      args.read(ujson.Obj("about" -> ujson.Null, "count" -> 1)) ==>
        Left(ArgsError.Missing("about", "one of `current`, `earlier`, `new`"))
      args.read(ujson.Obj("about" -> "Current", "count" -> 1)) ==>
        Left(ArgsError.Invalid("about", "one of `current`, `earlier`, `new`", "\"Current\""))
      args.read(ujson.Obj("about" -> "new", "count" -> 6)) ==>
        Left(ArgsError.Invalid("count", "a whole number from 1 to 5", "6"))
      args.read(ujson.Obj("about" -> "new", "count" -> 2.5)) ==>
        Left(ArgsError.Invalid("count", "a whole number from 1 to 5", "2.5"))
      args.read(ujson.Obj("about" -> "new", "count" -> 1, "sure" -> "yes")) ==>
        Left(ArgsError.Invalid("sure", "true or false", "\"yes\""))
      args.read(ujson.Obj("about" -> "new", "count" -> 1, "name" -> 7)) ==>
        Left(ArgsError.Invalid("name", "text", "7"))
    }

    test("each repair is made only when asked for") {
      val edit = Args.of((oldText = Field.text("Old."), newText = Field.text("New.")))
      val edits = Args.of((path = Field.text("P."), edits = Field.each("The edits.", edit)))
      val quoted = ujson.Obj("about" -> "new", "count" -> "3")
      val listed =
        ujson.Obj(
          "path" -> "f",
          "edits" -> ujson.Arr(ujson.Obj("oldText" -> "a", "newText" -> "b")).render()
        )
      val none = Set.empty[ArgRepair]
      args.read(quoted, none) ==>
        Left(ArgsError.Invalid("count", "a whole number from 1 to 5", "\"3\""))
      val asText =
        ArgsError.Invalid("edits", "a list of at least 1 objects", listed("edits").render())
      edits.read(listed, none) ==> Left(asText)
      args.read(quoted, Set(ArgRepair.QuotedNumber)).map(_.count) ==> Right(3)
      edits.read(listed, Set(ArgRepair.QuotedNumber)) ==> Left(asText)
      edits.read(listed, Set(ArgRepair.QuotedList)).map(_.edits.size) ==> Right(1)
      // Inside a repaired list, an item's number is repaired only when that is asked for too.
      val counted =
        Args.of((items = Field.each("I.", Args.of((n = Field.count("N.", 1, 5)))))).map(_.items)
      val inner = ujson.Obj("items" -> ujson.Arr(ujson.Obj("n" -> "2")).render())
      counted.read(inner, Set(ArgRepair.QuotedList)) ==>
        Left(ArgsError.Invalid("items[0].n", "a whole number from 1 to 5", "\"2\""))
      counted.read(inner, ArgRepair.values.toSet).map(_.map(_.n)) ==> Right(List(2))
    }

    test("a refusal's message names the field and what it accepts") {
      ArgsError.Invalid("about", "one of `current`, `new`", "\"x\"").message ==>
        "`about` takes one of `current`, `new`, not \"x\"."
      ArgsError.Missing("count", "a whole number from 1 to 5").message ==>
        "`count` is missing: it takes a whole number from 1 to 5."
      ArgsError.Unexpected("extra", Vector("a", "b")).message ==>
        "There is no argument `extra`; the arguments there are `a`, `b`."
      ArgsError.NotAnObject("[1]").message ==> "The arguments must be a JSON object, not [1]."
    }

    test("a one-option choice, and a repeated option listed once") {
      val one = Field.oneOf("Only.", "a", "a")
      one.accepts ==> "`a`"
      Args.of((x = one)).schema(strict = false)("properties")("x")("enum") ==> ujson.Arr("a")
    }

    test("refine: a check across fields, its Left the read's") {
      val refined = Args
        .of((kind = Field.oneOf("K.", "a", "b"), note = Field.text("N.").optional))
        .refine(a =>
          if (a.kind == "b") a.note.toRight(ArgsError.Missing("note", "text")) else Right("a")
        )
      refined.read(ujson.Obj("kind" -> "b")) ==> Left(ArgsError.Missing("note", "text"))
      refined.read(ujson.Obj("kind" -> "b", "note" -> "n")) ==> Right("n")
      refined.read(ujson.Obj("kind" -> "a")) ==> Right("a")
    }

    test("what a read accepts, the schema admits") {
      val samples = Vector(
        ujson.Obj("about" -> "new", "count" -> 1),
        ujson.Obj("about" -> "earlier", "name" -> "x", "count" -> 5, "sure" -> true),
        ujson.Obj("about" -> "current", "count" -> 3.0, "sure" -> false),
        ujson.Obj("about" -> "Current", "count" -> 3),
        ujson.Obj("about" -> "new", "count" -> 0),
        ujson.Obj("about" -> "new", "count" -> 1, "other" -> 1),
        ujson.Obj("count" -> 1),
        ujson.Obj("about" -> "new", "count" -> 1, "sure" -> "yes"),
        ujson.Str("new")
      )
      val accepted = samples.filter(s => args.read(s).isRight)
      accepted.size ==> 3
      assert(accepted.forall(admits(args.schema(strict = false), _)))
      // Under strict, an absent optional property is sent as null, which the read also takes.
      val nulled = accepted.map { s =>
        val o = ujson.Obj.from(s.obj)
        Vector("name", "sure").foreach(k => if (!o.obj.contains(k)) o(k) = ujson.Null)
        o
      }
      assert(nulled.forall(s => args.read(s).isRight))
      assert(nulled.forall(admits(args.schema(strict = true), _)))
      // The check is not vacuous: what the read refuses, it refuses too. The two repairs
      // (the test after next) are the one exception: a read takes them, the schema does not.
      assert(samples.filter(s => args.read(s).isLeft).forall(!admits(args.schema(false), _)))
    }

    test("a list of objects: its schema, and a failure inside an item at its path") {
      val edit = Args.of((oldText = Field.text("Old."), newText = Field.text("New.")))
      val edits = Args.of((path = Field.text("P."), edits = Field.each("The edits.", edit)))
      edits.schema(strict = false)("properties")("edits") ==> ujson.Obj(
        "type" -> "array",
        "items" -> edit.schema(strict = false),
        "minItems" -> 1,
        "description" -> "The edits."
      )
      val one = ujson.Obj("oldText" -> "a", "newText" -> "b")
      edits
        .read(ujson.Obj("path" -> "f", "edits" -> ujson.Arr(one, one)))
        .map(_.edits.map(e => (e.oldText, e.newText))) ==>
        Right(List(("a", "b"), ("a", "b")))
      edits.read(ujson.Obj("path" -> "f", "edits" -> ujson.Arr())) ==>
        Left(ArgsError.Invalid("edits", "a list of at least 1 objects", "[]"))
      edits.read(
        ujson.Obj("path" -> "f", "edits" -> ujson.Arr(one, one, ujson.Obj("oldText" -> "a")))
      ) ==>
        Left(ArgsError.Missing("edits[2].newText", "text"))
      edits.read(
        ujson.Obj(
          "path" -> "f",
          "edits" -> ujson.Arr(one, ujson.Obj("oldText" -> 1, "newText" -> "b"))
        )
      ) ==>
        Left(ArgsError.Invalid("edits[1].oldText", "text", "1"))
      edits.read(ujson.Obj("path" -> "f", "edits" -> ujson.Arr("x"))) ==>
        Left(ArgsError.Invalid("edits[0]", "an object", "\"x\""))
      edits.read(
        ujson.Obj(
          "path" -> "f",
          "edits" -> ujson.Arr(ujson.Obj("oldText" -> "a", "newText" -> "b", "x" -> 1))
        )
      ) ==>
        Left(ArgsError.Unexpected("edits[0].x", Vector("oldText", "newText")))
      Args
        .of((e = Field.each("E.", edit, min = 0)))
        .read(ujson.Obj("e" -> ujson.Arr()))
        .map(_.e) ==>
        Right(Nil)
      assert(
        admits(edits.schema(strict = false), ujson.Obj("path" -> "f", "edits" -> ujson.Arr(one)))
      )
    }

    test("the two repairs: a quoted whole number, and a list sent as a JSON string") {
      val edit = Args.of((oldText = Field.text("Old."), newText = Field.text("New.")))
      val edits = Args.of((path = Field.text("P."), edits = Field.each("The edits.", edit)))
      args.read(ujson.Obj("about" -> "new", "count" -> "3")).map(_.count) ==> Right(3)
      args.read(ujson.Obj("about" -> "new", "count" -> "-3")) ==>
        Left(ArgsError.Invalid("count", "a whole number from 1 to 5", "\"-3\""))
      // Nothing else is coerced: not a number with a point, spaces or a sign, nor a list's
      // string that holds no list, nor a number where text is asked for.
      for (bad <- Vector("3.0", " 3", "+3", "3x", "", "three"))
        args.read(ujson.Obj("about" -> "new", "count" -> bad)) ==>
          Left(ArgsError.Invalid("count", "a whole number from 1 to 5", ujson.Str(bad).render()))
      val one = ujson.Obj("oldText" -> "a", "newText" -> "b")
      edits
        .read(ujson.Obj("path" -> "f", "edits" -> ujson.Arr(one).render()))
        .map(_.edits.map(e => (e.oldText, e.newText))) ==>
        Right(List(("a", "b")))
      // Read as the list, so a failure inside it is still at its item's path.
      edits.read(
        ujson.Obj("path" -> "f", "edits" -> ujson.Arr(ujson.Obj("oldText" -> "a")).render())
      ) ==>
        Left(ArgsError.Missing("edits[0].newText", "text"))
      for (bad <- Vector(one.render(), "[not json", "\"[]\"", "x"))
        edits.read(ujson.Obj("path" -> "f", "edits" -> bad)) ==>
          Left(ArgsError.Invalid("edits", "a list of at least 1 objects", ujson.Str(bad).render()))
      args.read(ujson.Obj("about" -> "new", "count" -> 1, "name" -> 7)) ==>
        Left(ArgsError.Invalid("name", "text", "7"))
    }

    test("a name used twice does not compile") {
      val error = assertCompileError("""Args.of((a = Field.text("x"), a = Field.text("y")))""")
      assert(error.msg.contains("Duplicate tuple element name"))
    }

    test("tool names") {
      ToolName.of("topic").map(ToolName.value) ==> Right("topic")
      ToolName.of("read_file2").isRight ==> true
      assert(
        ToolName.of("").isLeft,
        ToolName.of("Topic").isLeft,
        ToolName.of("2topic").isLeft,
        ToolName.of("read-file").isLeft,
        ToolName.of("a" * 65).isLeft
      )
      ToolName.of("a" * 64).isRight ==> true
      ToolName.value(ToolName("topic")) ==> "topic"
      assertCompileError("""ToolName("Topic")""")
      ()
    }

    test("a spec's schema carries its name, what it does, and strict") {
      val spec = ToolSpec(ToolName("pick"), "Pick one.", Args.of((x = Field.flag("X."))))
      val shown = spec.schema(strict = true)
      (shown.name, shown.description, shown.strict) ==> ("pick", "Pick one.", true)
      shown.parameters ==> spec.args.schema(strict = true)
    }

    test("every field kind's strict schema reads as the schema a typed reply is sent") {
      val every = Args.of(
        (
          about = Field.oneOf("Which.", "current", "new"),
          maybeAbout = Field.oneOf("Which, if any.", "current", "new").optional,
          name = Field.text("A name."),
          maybeName = Field.text("A name, if any.").optional,
          count = Field.count("How many.", 1, 5),
          maybeCount = Field.count("How many, if any.", 0, 9).optional,
          sure = Field.flag("Sure?"),
          maybeSure = Field.flag("Sure, if asked?").optional,
          edits = Field.each(
            "The edits.",
            Args.of((oldText = Field.text("Old."), note = Field.text("Why.").optional))
          ),
          maybeEdits =
            Field.each("More edits.", Args.of((at = Field.count("At.", 0, 9))), 0).optional
        )
      )
      every.typed.map(_.schema.json) ==> Right(every.schema(strict = true))
    }

    test("a typed reply reads as the arguments do, its refusal the same words") {
      val refined = args.refine(a =>
        if (a.about == "new" && a.name.isEmpty) Left(ArgsError.Missing("name", "text"))
        else Right(a)
      )
      val sent = Vector(
        ujson.Obj("about" -> "new", "name" -> "x", "count" -> 2, "sure" -> ujson.Null),
        ujson.Obj("about" -> "new", "name" -> ujson.Null, "count" -> 2, "sure" -> true),
        ujson.Obj("about" -> "current", "name" -> ujson.Null, "count" -> 5, "sure" -> false)
      )
      val read = refined.typed.map(t =>
        sent.map(v => t.schema.check(v, Set.empty).left.map(_.message).flatMap(t.read))
      )
      read ==> Right(sent.map(v => refined.read(v, Set.empty).left.map(_.message)))
      read.map(_(1)) ==> Right(Left("`name` is missing: it takes text."))
    }

    test("arguments whose schema the subset refuses are not typed") {
      Args.raw(ujson.Obj("type" -> "object", "properties" -> ujson.Obj())).typed.map(_ => ()) ==>
        Left(SchemaError("", "an object must have \"additionalProperties\": false"))
      Args.of((n = Field.count("N.", 5, 1))).typed.map(_ => ()) ==>
        Left(SchemaError("properties.n", "`minimum` 5 is above `maximum` 1"))
    }
  }
}
