package grit.mcp.drive

import scala.collection.mutable
import scala.util.Try

/** Stand-ins for what Drive's answers name, so a recording can be kept as a fixture. One
  * `Neutral` gives one value the same stand-in in every answer it reads, numbered in the order
  * it meets them:
  *   - an id (at `id`, a key ending `Id`, or in `parents`): `drive-a`, `drive-b` … for
  *     `drives`, in their order; `folder-N` for `folders`; `permission-N` in an object with a
  *     `role`; `file-N` otherwise;
  *   - an address: `self@project.iam.example` for `self`, `writer-bot-N@project.iam.example`
  *     for `writers`, `person-N@example.com` in `domain` exactly, `outside-N@gmail.example` at
  *     gmail.com, `service-N@project.iam.example` for another service account, and
  *     `outside-N@elsewhere.example` for anyone else; a `domain` holding a dot is `example.com`
  *     when it is `domain`, else `elsewhere.example` (an error's `domain` of reasons, such as
  *     `global`, is kept as a word);
  *   - a text block that holds JSON: that JSON with its stand-ins, written compact;
  *   - a string at a key naming a name, title, description, snippet, content or text, or of no
  *     other shape kept: `<key>-N`.
  * Kept as they are: numbers, booleans, nulls, and strings shaped as a lowercase word (a type
  * or a role), a media type, a UTC time, or a file's `size` (digits, as Drive writes it).
  */
final class Neutral(
    domain: String,
    self: String,
    writers: Set[String],
    folders: Set[String],
    drives: Vector[String]
) {

  // Scoped mutation: the stand-ins given so far, and the next number of each kind.
  private val stood = mutable.Map.empty[(String, String), String]
  private val counts = mutable.Map.empty[String, Int]

  private val Word = "[a-z][a-zA-Z]{0,30}".r
  private val Media = "[a-z]+/[a-zA-Z0-9.+-]+".r
  private val Time = """\d{4}-\d\d-\d\dT[\d:.]+Z""".r
  private val Digits = """\d{1,19}""".r
  private val Address = """[^@\s]+@([^@\s]+)""".r
  private val Named: Vector[String] =
    Vector("name", "title", "description", "snippet", "content", "text")

  /** `body`, an exported file's text, with each run of letters and digits as `text`. */
  def body(body: String): String = body.replaceAll("[\\p{L}\\p{N}]+", "text")

  /** `result` with its stand-ins. */
  def result(result: ujson.Obj): ujson.Obj = ujson.Obj.from(obj(result).value)

  private def numbered(kind: String, value: String)(make: Int => String): String =
    stood.getOrElseUpdate(
      (kind, value), {
        val n = counts.getOrElse(kind, 0) + 1
        counts(kind) = n
        make(n)
      }
    )

  private def id(value: String, permission: Boolean): String =
    drives.indexOf(value) match {
      case i if i >= 0 => s"drive-${('a' + i).toChar}"
      case _ if folders(value) => numbered("folder", value)(n => s"folder-$n")
      case _ if permission => numbered("permission", value)(n => s"permission-$n")
      case _ => numbered("file", value)(n => s"file-$n")
    }

  private def address(value: String, at: String): String =
    if (value == self) "self@project.iam.example"
    else if (writers(value)) numbered("writer", value)(n => s"writer-bot-$n@project.iam.example")
    else if (at == domain) numbered("person", value)(n => s"person-$n@example.com")
    else if (at == "gmail.com") numbered("outside", value)(n => s"outside-$n@gmail.example")
    else if (at.endsWith(".iam.gserviceaccount.com"))
      numbered("service", value)(n => s"service-$n@project.iam.example")
    else numbered("outside", value)(n => s"outside-$n@elsewhere.example")

  private def text(key: String, value: String): String =
    numbered(key, value)(n => s"$key-$n")

  private def obj(o: ujson.Obj): ujson.Obj = {
    val permission = o.value.contains("role")
    ujson.Obj.from(o.value.map { case (k, v) => k -> value(k, v, permission) })
  }

  private def value(key: String, v: ujson.Value, permission: Boolean): ujson.Value = v match {
    case o: ujson.Obj => obj(o)
    case ujson.Arr(items) if key == "parents" =>
      ujson.Arr.from(items.map {
        case ujson.Str(s) => ujson.Str(id(s, permission = false))
        case other => value(key, other, permission)
      })
    case ujson.Arr(items) => ujson.Arr.from(items.map(value(key, _, permission)))
    case ujson.Str(s) => ujson.Str(string(key, s, permission))
    case other => other
  }

  private def string(key: String, s: String, permission: Boolean): String =
    if (key == "id" || key.endsWith("Id")) id(s, permission)
    else if (key == "domain" && s.contains('.'))
      if (s == domain) "example.com" else "elsewhere.example"
    else
      s match {
        case Address(at) => address(s, at)
        case _ if key == "text" =>
          Try(ujson.read(s)).toOption.filter(j => j.objOpt.isDefined || j.arrOpt.isDefined) match {
            case Some(j) => ujson.write(value(key, j, permission))
            case None => text(key, s)
          }
        case _ if Named.exists(key.toLowerCase.contains) => text(key, s)
        case Word() | Media() | Time() => s
        case Digits() if key == "size" => s
        case _ => text(key, s)
      }
}
