package grit.eval.harness.log

/** What a cached answer is kept under: 64 lower-case hex digits. */
final case class CacheKey private (hex: String)

object CacheKey {

  /** The key written `hex`; why not, when it is not 64 lower-case hex digits. */
  def read(hex: String): Either[String, CacheKey] =
    Either.cond(hex.matches("[0-9a-f]{64}"), new CacheKey(hex), s"not a cache key: $hex")
}
