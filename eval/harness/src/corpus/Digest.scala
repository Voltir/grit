package grit.eval.harness.corpus

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import grit.core.classify.Request

/** A SHA-256, as 64 lower-case hex digits: what a corpus keeps of a text or an input in place
  * of it.
  */
final case class Digest private (hex: String)

object Digest {

  /** Of `value` as JSON, rendered compactly. */
  def json(value: ujson.Value): Digest = text(value.render())

  /** Of `text`'s UTF-8 bytes. */
  def text(text: String): Digest =
    new Digest(
      MessageDigest
        .getInstance("SHA-256")
        .digest(text.getBytes(StandardCharsets.UTF_8))
        .map(b => f"${b & 0xff}%02x")
        .mkString
    )

  /** `request`'s own ([[Request.digest]]). */
  def request(request: Request): Digest = new Digest(request.digest)

  /** The digest written `hex`; why not, when it is not 64 lower-case hex digits. */
  def read(hex: String): Either[String, Digest] =
    Either.cond(hex.matches("[0-9a-f]{64}"), new Digest(hex), s"not a digest: $hex")
}
