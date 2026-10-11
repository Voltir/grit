package grit.knowledge.github

/** The recorded `get_file_contents` results under `test/resources/github/`, as the text a call's
  * answer gives: each content block on lines of its own, a text block's text and an embedded
  * resource's text as they are (the rule `grit.mcp`'s answers follow for these two kinds, the
  * only ones recorded).
  */
object Answers {

  /** The result recorded as `name`. */
  def result(name: String): ujson.Value =
    ujson.read(
      scala.io.Source
        .fromInputStream(getClass.getResourceAsStream(s"/github/$name.json"))
        .mkString
    )

  /** The text of the result recorded as `name`. */
  def text(name: String): String =
    result(name)("content").arr.toVector
      .map(block =>
        block("type").str match {
          case "resource" => block("resource")("text").str
          case _ => block("text").str
        }
      )
      .mkString("\n")
}
