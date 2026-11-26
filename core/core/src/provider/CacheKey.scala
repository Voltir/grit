package grit.core.provider

import grit.core.id.Sha256

/** What a provider may group requests by, so the prefix they share stays cached: requests of
  * one key may be sent to one server. Never empty, and at most [[MaxLength]] characters,
  * OpenRouter's bound on its routing key.
  */
opaque type CacheKey = String

object CacheKey {

  val MaxLength: Int = 256

  /** The key `text` is; `Left` when it is empty or longer than [[MaxLength]]. */
  def of(text: String): Either[String, CacheKey] =
    if (text.isEmpty) Left("a cache key is never empty")
    else if (text.length > MaxLength) Left(s"a cache key is at most $MaxLength characters")
    else Right(text)

  /** The key that is `digest`, as 64 hex digits. */
  def of(digest: Sha256): CacheKey = digest.hex

  def value(key: CacheKey): String = key
}
