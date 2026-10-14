package grit.core.flow

import java.time.LocalDate

import utest.*

object ShapeTests extends TestSuite {

  private def oneOf(options: Seq[String]): ujson.Value =
    ujson.Obj("kind" -> "one_of", "options" -> ujson.Arr.from(options.map(ujson.Str(_))))

  private def number(min: Double, max: Double): ujson.Value =
    ujson.Obj("kind" -> "number", "min" -> min, "max" -> max)

  private def token(max: Int): ujson.Value = ujson.Obj("kind" -> "token", "max" -> max)

  val tests = Tests {
    test("of reads each kind") {
      List(
        ujson.Obj("kind" -> "yes_no"),
        ujson.Obj("kind" -> "day"),
        number(-1, 3),
        oneOf(Seq("red", "green")),
        token(16)
      ).map(Shape.of) ==> List(
        Right(Shape.YesNo),
        Right(Shape.Day),
        Right(Shape.Number(-1, 3)),
        Right(Shape.OneOf(Vector("red", "green"))),
        Right(Shape.Token(16))
      )
    }

    test("of takes each bound at its limit") {
      List(
        number(5, 5),
        oneOf(Seq.tabulate(64)(_.toString)),
        oneOf(Seq("a" * 80)),
        token(1)
      ).map(Shape.of(_).isRight) ==> List(true, true, true, true)
    }

    test("of refuses each bound just past its limit, and an empty one") {
      List(
        number(5, 4),
        oneOf(Seq.tabulate(65)(_.toString)),
        oneOf(Seq("a" * 81)),
        oneOf(Nil),
        token(17),
        token(0)
      ).map(Shape.of) ==> List(
        Left("a number shape's range is reversed: 5 to 4"),
        Left("a one_of shape lists 1 to 64 options, not 65"),
        Left("a one_of shape's options are at most 80 characters each"),
        Left("a one_of shape lists 1 to 64 options, not 0"),
        Left("a token shape allows 1 to 16 characters, not 17"),
        Left("a token shape allows 1 to 16 characters, not 0")
      )
    }

    test("of refuses an unknown kind, a missing field and a fractional bound") {
      List(
        ujson.Obj("kind" -> "free_text"),
        ujson.Obj("kind" -> "number", "min" -> 1),
        number(0.5, 2),
        ujson.Str("yes_no")
      ).map(Shape.of) ==> List(
        Left("no shape is called free_text"),
        Left("a shape's max is a whole number"),
        Left("a shape's min is a whole number"),
        Left("a shape is a JSON object")
      )
    }

    test("answer reads a value in its shape") {
      List(
        Shape.answer(Shape.YesNo, ujson.False),
        Shape.answer(Shape.Number(1, 10), ujson.Num(10)),
        Shape.answer(Shape.Day, ujson.Str("2026-10-14")),
        Shape.answer(Shape.OneOf(Vector("red", "green")), ujson.Str("green")),
        Shape.answer(Shape.Token(4), ujson.Str("a_-9"))
      ) ==> List(
        Some(Extracted.Yes(false)),
        Some(Extracted.Number(10)),
        Some(Extracted.Day(LocalDate.of(2026, 10, 14))),
        Some(Extracted.Chosen("green")),
        Some(Extracted.Token("a_-9"))
      )
    }

    test("answer refuses a value outside its shape") {
      List(
        Shape.answer(Shape.YesNo, ujson.Str("yes")),
        Shape.answer(Shape.Number(1, 10), ujson.Num(11)),
        Shape.answer(Shape.Number(1, 10), ujson.Num(2.5)),
        Shape.answer(Shape.Day, ujson.Str("14 October")),
        Shape.answer(Shape.OneOf(Vector("red", "green")), ujson.Str("Green")),
        Shape.answer(Shape.Token(4), ujson.Str("abcde")),
        Shape.answer(Shape.Token(4), ujson.Str("a b")),
        Shape.answer(Shape.Token(4), ujson.Str(""))
      ) ==> List(None, None, None, None, None, None, None, None)
    }
  }
}
