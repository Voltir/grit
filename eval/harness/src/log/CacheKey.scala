package grit.eval.harness.log

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import grit.core.id.Sha256

/** What a cached answer is kept under: 64 lower-case hex digits. */
final case class CacheKey private (hex: String)

object CacheKey {

  /** What a cached answer is kept under: SHA-256 of `provider` (a tag naming the call's kind,
    * `"jev"`), the exact `request` it sent, as UTF-8 (for Jev, the wire body, which names the
    * model), and the `repeat`, so each repeat is its own call. Any change to one changes it.
    */
  def of(provider: String, request: String, repeat: Int): CacheKey = {
    val tag = provider.getBytes(StandardCharsets.UTF_8)
    def int(n: Int) = ByteBuffer.allocate(4).putInt(n).array()
    // Each part's length first, so no two different splits hash the same bytes.
    val bytes = int(tag.length) ++ tag ++ int(repeat) ++ request.getBytes(StandardCharsets.UTF_8)
    new CacheKey(Sha256.digest(bytes.toVector).hex)
  }

  /** The key written `hex`; why not, when it is not 64 lower-case hex digits. */
  def read(hex: String): Either[String, CacheKey] =
    Either.cond(hex.matches("[0-9a-f]{64}"), new CacheKey(hex), s"not a cache key: $hex")
}
