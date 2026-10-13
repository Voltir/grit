package grit.app.main

import grit.knowledge.github.{Answers, Listing}
import grit.mcp.wire.Answer

import utest.*

/** GitHub's answers as the knowledge plugin reads them in a deployment: through grit.mcp's
  * `Answer.of`, which knowledge's own tests stand in for with `Answers.text`, since knowledge
  * may not name grit.mcp.
  */
object GitHubAnswersTests extends TestSuite {

  private def answered(result: ujson.Value): Either[String, String] =
    result.objOpt
      .toRight("not an object")
      .flatMap(Answer.of(_).left.map(_.toString))
      .map(_.text)

  private def sha(n: Int): String = f"$n%040x"

  /** A directory's result of `n` files, each entry shaped as the recorded listing's first. */
  private def directory(n: Int): ujson.Obj = {
    val recorded = ujson.read(Answers.text("get-file-contents-directory")).arr.toVector
    val shape = recorded.headOption.flatMap(_.objOpt).fold(ujson.Obj())(ujson.Obj.from(_))
    val entries = (0 until n).map { i =>
      val entry = ujson.Obj.from(shape.value)
      entry("path") = s"docs/file-$i.md"
      entry("name") = s"file-$i.md"
      entry("sha") = sha(i)
      entry
    }
    ujson.Obj(
      "content" -> ujson.Arr(
        ujson.Obj("type" -> "text", "text" -> ujson.write(ujson.Arr.from(entries)))
      )
    )
  }

  val tests = Tests {
    test("each recorded GitHub result's answer is the text knowledge's tests read for it") {
      val names = Vector(
        "get-file-contents-directory",
        "get-file-contents-directory-git-url",
        "get-file-contents-file",
        "get-file-contents-match"
      )
      names.map(n => answered(Answers.result(n))) ==> names.map(n => Right(Answers.text(n)))
    }

    test("a listing too long for one answer is cut to its leading entries, read as Partial") {
      val result = directory(1000)
      val Right(Listing.Complete(all)) = Listing.of(result("content")(0)("text").str): @unchecked
      val read = answered(result).flatMap(Listing.of)
      val shown = read match {
        case Right(Listing.Partial(entries)) => entries.size
        case _ => 0
      }
      (read, 0 < shown && shown < all.size) ==> (Right(Listing.Partial(all.take(shown))), true)
    }
  }
}
