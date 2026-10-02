package grit.eval.harness.log

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** What a cached answer is kept under: 64 lower-case hex digits. */
final case class CacheKey private (hex: String)

object CacheKey {

  /** What a cached answer is kept under: SHA-256 of `provider` (a tag naming the call's kind,
    * `"jev"`), the exact `request` it sent, as UTF-8 (for Jev, the wire body, which names the
    * model), and the `repeat`, so each repeat is its own call. Any change to one changes it.
    */
  def of(provider: String, request: String, repeat: Int): CacheKey = {
    val tag = provider.getBytes(StandardCharsets.UTF_8)
    val sha = MessageDigest.getInstance("SHA-256")
    // Each part's length first, so no two different splits hash the same bytes.
    sha.update(ByteBuffer.allocate(4).putInt(tag.length).array())
    sha.update(tag)
    sha.update(ByteBuffer.allocate(4).putInt(repeat).array())
    sha.update(request.getBytes(StandardCharsets.UTF_8))
    new CacheKey(sha.digest().map(b => f"${b & 0xff}%02x").mkString)
  }

  /** The key written `hex`; why not, when it is not 64 lower-case hex digits. */
  def read(hex: String): Either[String, CacheKey] =
    Either.cond(hex.matches("[0-9a-f]{64}"), new CacheKey(hex), s"not a cache key: $hex")
}
