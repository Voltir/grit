package grit.outline.query

import scala.annotation.tailrec

import grit.outline.locate.Root

/** Each root's cache, least recently used first; never shares an entry between roots. */
final case class Roots(byRoot: Vector[(Root, Loaded)], maxFiles: Int)

object Roots {

  /** No root cached, with room for `maxFiles` cached entries across all roots, a `.tasty` file's definitions and its references each counting as one (`Loaded.entries`). */
  def empty(maxFiles: Int): Roots = Roots(Vector.empty, maxFiles)

  /** `root`'s cache (empty if none). */
  def of(roots: Roots, root: Root): Loaded =
    roots.byRoot.find(_._1 == root).map(_._2).getOrElse(Loaded.empty)

  /** `root` now holds `loaded` and is the most recently used; least recently used roots are dropped while the total cached entries exceed `maxFiles`, never `root` itself. */
  def put(roots: Roots, root: Root, loaded: Loaded): Roots = {
    @tailrec def trim(v: Vector[(Root, Loaded)]): Vector[(Root, Loaded)] =
      if (v.size > 1 && v.map(_._2.entries).sum > roots.maxFiles && v.head._1 != root)
        trim(v.tail)
      else v
    Roots(trim(roots.byRoot.filterNot(_._1 == root) :+ (root -> loaded)), roots.maxFiles)
  }
}
