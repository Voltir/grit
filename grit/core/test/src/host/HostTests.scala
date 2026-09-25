package grit.core.host

import utest.*

/** The pure half of the host: [[RelPath]]'s checks, [[Clipped]]'s cuts and hints, and
  * [[Replace.onto]], pi's edit rules without its fuzzy matching.
  */
object HostTests extends TestSuite {

  private def path(s: String): RelPath = RelPath.of(s) match {
    case Right(p) => p
    case Left(e) => throw new java.lang.AssertionError(e.message)
  }

  private val p = path("a.txt")

  private def onto(text: String, edits: (String, String)*): Either[EditError, (String, Edited)] =
    Replace.onto(p, text, edits.map(Replace(_, _)))

  private def hintOf(k: Option[Kept]): Option[String] = k.map(k => s"${k.lines}/${k.total}")

  val tests = Tests {
    test("RelPath") {
      test("normal form") {
        RelPath.of("src//./a/../b.scala").map(RelPath.value) ==> Right("src/b.scala")
        RelPath.of("").map(RelPath.value) ==> Right(".")
        RelPath.of("./").map(RelPath.value) ==> Right(".")
        RelPath.of("a/..").map(RelPath.value) ==> Right(".")
        RelPath.of("dir/").map(RelPath.value) ==> Right("dir")
      }
      test("absolute is refused") {
        RelPath.of("/etc/passwd") ==> Left(PathError.Absolute("/etc/passwd"))
        RelPath.of("~/x") ==> Left(PathError.Absolute("~/x"))
      }
      test("climbing above the root is refused, even when it comes back") {
        RelPath.of("..") ==> Left(PathError.Escapes(".."))
        RelPath.of("../grit/x") ==> Left(PathError.Escapes("../grit/x"))
        RelPath.of("a/../../a") ==> Left(PathError.Escapes("a/../../a"))
      }
      test("secrets files are refused anywhere in the path") {
        RelPath.of(".env") ==> Left(PathError.Secret(".env"))
        RelPath.of("app/.env.local") ==> Left(PathError.Secret("app/.env.local"))
        RelPath.of(".env/x") ==> Left(PathError.Secret(".env/x"))
        RelPath.of(".envrc").map(RelPath.value) ==> Right(".envrc")
        RelPath.of("x.env").map(RelPath.value) ==> Right("x.env")
      }
      test("a NUL is refused") {
        RelPath.of("a\u0000b") ==> Left(PathError.Malformed("a\u0000b"))
      }
      test("messages") {
        PathError.Escapes("..").message ==>
          "`..` leads outside the checkout; paths must stay inside it."
      }
    }

    test("Clipped") {
      test("text that fits is whole, with the hint for nothing cut") {
        Clipped.head("a\nb\n", _ => Some("whole")) ==> Clipped.head("a\nb\n", _ => Some("whole"))
        Clipped.head("a\nb\n", hintOf).text ==> "a\nb\n"
        Clipped.head("a\nb\n", hintOf).hint ==> None
        Clipped.head("a\nb\n", _ => Some("more")).show ==> "a\nb\n\n\n[more]"
      }
      test("head keeps the first lines, tail the last") {
        val text = (1 to 2500).map(i => s"l$i").mkString("\n")
        val head = Clipped.head(text, hintOf)
        head.text ==> (1 to 2000).map(i => s"l$i").mkString("\n")
        head.hint ==> Some("2000/2500")
        val tail = Clipped.tail(text, hintOf)
        tail.text ==> (501 to 2500).map(i => s"l$i").mkString("\n")
        tail.hint ==> Some("2000/2500")
      }
      test("bytes cut before lines do, counting UTF-8 and newlines") {
        val line = "é" * 1000 // 2000 bytes
        val text = Vector.fill(30)(line).mkString("\n")
        val head = Clipped.head(text, hintOf)
        head.hint ==> Some("25/30") // 25 * 2000 + 24 newlines <= 51200 < 26 lines
        assert(head.text.getBytes("UTF-8").length <= Clipped.MaxBytes)
      }
      test("a first line over the limit leaves the head empty") {
        val text = "x" * (Clipped.MaxBytes + 1) + "\nnext"
        Clipped.head(text, hintOf).text ==> ""
        Clipped.head(text, hintOf).hint ==> Some("0/2")
      }
      test("a last line over the limit leaves the tail its end, cut at a character") {
        val text = "first\n" + "é" * Clipped.MaxBytes
        val tail = Clipped.tail(text, k => k.map(_.toString))
        tail.hint ==> Some(Kept(1, 2, partial = true).toString)
        tail.text ==> "é" * (Clipped.MaxBytes / 2)
      }
      test("lines ignore a final newline") {
        Clipped.lines("a\nb\n") ==> Vector("a", "b")
        Clipped.lines("a\n\n") ==> Vector("a", "")
        Clipped.lines("") ==> Vector()
      }
    }

    test("Replace.onto") {
      test("every edit matched against the original, applied together") {
        onto("one two three", "one" -> "two", "two" -> "one") ==>
          Right(("two one three", Edited(Vector(1, 1))))
      }
      test("where each landed, in file order") {
        onto("a\nb\nc\nd\n", "d" -> "D", "b" -> "x\ny\nB") ==>
          Right(("a\nx\ny\nB\nc\nD\n", Edited(Vector(2, 6))))
      }
      test("not found, repeated, overlapping and empty are refused by index") {
        onto("abc", "zz" -> "y") ==> Left(EditError.NotFound(p, 0))
        onto("abab", "c" -> "d", "ab" -> "x") ==> Left(EditError.NotFound(p, 0))
        onto("abab", "a" -> "x", "ab" -> "y") ==> Left(EditError.Repeated(p, 0, 2))
        onto("aaa", "aa" -> "b") ==> Left(EditError.Repeated(p, 0, 2))
        onto("abcdef", "cde" -> "x", "bcd" -> "y") ==> Left(EditError.Overlap(p, 1, 0))
        onto("abc", "b" -> "x", "" -> "y") ==> Left(EditError.EmptyOld(p, 1))
      }
      test("edits that change nothing are refused, and so are none") {
        onto("abc", "b" -> "b") ==> Left(EditError.NoChange(p))
        onto("abc") ==> Left(EditError.NoChange(p))
      }
      test("CRLF is kept, and an edit's LF matches it") {
        onto("a\r\nb\r\nc\r\n", "a\nb" -> "x\ny") ==>
          Right(("x\r\ny\r\nc\r\n", Edited(Vector(1))))
        onto("a\r\nb", "a\r\nb" -> "c") ==> Right(("c", Edited(Vector(1))))
      }
      test("LF is kept when the first line break is one") {
        onto("a\nb\r\nc", "c" -> "d") ==> Right(("a\nb\nd", Edited(Vector(3))))
      }
      test("a byte order mark is kept and never matched") {
        onto("﻿abc", "abc" -> "x") ==> Right(("﻿x", Edited(Vector(1))))
      }
      test("no fuzzy matching: whitespace and quotes count") {
        onto("say “hi”  ", "say \"hi\"" -> "x") ==> Left(EditError.NotFound(p, 0))
        onto("a  b", "a b" -> "x") ==> Left(EditError.NotFound(p, 0))
      }
      test("messages name the edit and the path") {
        EditError.Repeated(p, 2, 3).message ==>
          "edits[2].oldText occurs 3 times in a.txt; it must occur exactly once. Include " +
          "more of the surrounding lines to make it unique."
      }
    }
  }
}
