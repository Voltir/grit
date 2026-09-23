package grit.tui.model.text

/** Identifies a piece of logical text whose wrapped rows are cached. Opaque so ids
  * cannot be transposed with ordinary strings at a call site.
  */
opaque type ContentId = String

object ContentId {
  def apply(s: String): ContentId = s
  extension (id: ContentId) def value: String = id
}

/** Wrapped rows keyed by content id and revision at a fixed width.
  *
  * The effective key is `(contentId, revision, width)`: the cache is bound to one
  * width and [[atWidth]] discards every entry when it changes, because every row in
  * it is wrong at the new width. A pure value, threaded through `update` and stored
  * back after each lookup -- `view` only reads it, or every frame re-wraps. `hits`
  * and `misses` make "the viewport only wrapped the rows it displayed" an assertable
  * property rather than a hope.
  */
final case class WrapCache(
    width: Int,
    entries: Map[ContentId, WrapCache.Cached],
    hits: Long,
    misses: Long
) {

  /** The wrapped rows of `text`, from the cache while `(id, rev)` is unchanged. The
    * returned cache must be stored back.
    */
  def rowsFor(id: ContentId, rev: Long, text: String): (Vector[Row], WrapCache) =
    entries.get(id) match {
      case Some(c) if c.rev == rev => (c.rows, copy(hits = hits + 1))
      case _ =>
        val rows = Wrap.wrap(text, width)
        val next =
          copy(entries = entries.updated(id, WrapCache.Cached(rev, rows)), misses = misses + 1)
        (rows, next)
    }

  /** The same cache rebound to `w`; every entry is discarded, the counters survive. */
  def atWidth(w: Int): WrapCache =
    if (w == width) this else WrapCache(math.max(1, w), Map.empty, hits, misses)
}

object WrapCache {
  final case class Cached(rev: Long, rows: Vector[Row])

  /** An empty cache bound to `width`. */
  def empty(width: Int): WrapCache = WrapCache(math.max(1, width), Map.empty, 0L, 0L)
}
