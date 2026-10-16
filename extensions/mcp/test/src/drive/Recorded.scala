package grit.mcp.drive

import java.nio.charset.StandardCharsets.UTF_8

/** Drive's recorded answers, `test/resources/drive/` (extensions/mcp/README.md). */
object Recorded {

  /** `drive/{name}` as recorded, read as UTF-8 (a byte-order mark kept). */
  def text(name: String): String = {
    val in = Option(getClass.getResourceAsStream(s"/drive/$name"))
      .getOrElse(throw new java.lang.AssertionError(s"no recording drive/$name"))
    try new String(in.readAllBytes(), UTF_8)
    finally in.close()
  }

  /** `drive/{name}.json` as JSON. */
  def json(name: String): ujson.Value = ujson.read(text(s"$name.json"))

  /** `id` as a [[FileId]], for an id a test names. */
  def id(id: String): FileId =
    FileId.of(id).fold(e => throw new java.lang.AssertionError(e), identity)
}
