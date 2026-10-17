package grit.outline.model

/** What kind of definition a TASTy entry records. `Param` is a class's constructor parameter, kept as its field. */
enum Kind {
  case Class, CaseClass, Trait, Object, Enum, EnumCase, Def, Val, Var, Param, Given, TypeAlias,
    Opaque
}

object Kind {

  /** A pattern finding, in source text, `name` declared by one of `keywords` (as `object`, or `case class` with any
    * whitespace between its words): keyword and name each a whole identifier, of which a `$` is part, so
    * `object Name$Kept` does not declare `Name`.
    */
  def declaring(keywords: Vector[String], name: String): scala.util.matching.Regex = {
    import java.util.regex.Pattern
    val keyword = keywords.map(_.split(' ').map(Pattern.quote).mkString("\\s+")).mkString("(?:", "|", ")")
    s"(?<![\\w$$])$keyword\\s+${Pattern.quote(name)}(?![\\w$$])".r
  }
}

/** A 1-based, inclusive line range; `start` is the Scaladoc's first line when there is one. */
final case class Lines(start: Int, end: Int)

/** A type a signature names; `topLevel` names the `.tasty` file it lives in. */
final case class Ref(fullName: String, topLevel: String, inRepo: Boolean)

/** A definition's closing source lines from line `start`, verbatim, for an Edit to anchor on: its last line, or its last two
  * when the last holds only closing brackets, since a lone `}` matches too often to Edit on.
  */
final case class Closing(start: Int, text: String)

/** One definition read from the TASTy: `file` is repo-relative. `head` is the whole source lines from its doc (or its first line)
  * through its signature's, and `body` the whole lines after them, both verbatim; `doc` and `signature` are the same text cut to
  * their own spans, for one-line outlines. A definition whose span runs past the end of its current source (cut shorter since
  * the compile) has the compiler's signature as `signature`, `head` that signature on a `--` line saying it is not file text,
  * `lines` as compiled, and no `doc`, `body`, `size` or `closing`. `size` is the UTF-8 bytes of the whole source lines `lines`
  * spans, doc included, newlines between them and none after. `closing` is none when `head` already ends on the last line.
  */
final case class Defn(
    kind: Kind,
    name: String,
    fullName: String,
    file: String,
    lines: Lines,
    doc: Option[String],
    signature: String,
    head: String,
    body: Option[String],
    size: Option[Int],
    closing: Option[Closing],
    members: Vector[Defn],
    refs: Vector[Ref],
    parents: Vector[String],
    isPrivate: Boolean,
    isAbstract: Boolean
)
