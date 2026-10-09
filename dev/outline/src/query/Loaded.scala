package grit.outline.query

import grit.outline.locate.Root
import grit.outline.model.Defn
import grit.outline.read.Read

/** One `.tasty` file's definitions as of its mtime. */
final case class Cached(mtime: Long, defns: Vector[Defn])

/** The definitions read so far, by `.tasty` path; a file is read again only when its mtime changed. */
final case class Loaded(byTasty: Map[os.Path, Cached])

object Loaded {
  val empty: Loaded = Loaded(Map.empty)

  /** `tasty`'s definitions, reading (in one inspector run) only files absent or changed; the new cache; how many files were read. */
  def defns(
      root: Root,
      tasty: Vector[os.Path],
      in: Loaded
  ): Either[String, (Vector[Defn], Loaded, Int)] = {
    val stale = tasty.filter(p => in.byTasty.get(p).forall(_.mtime != os.mtime(p)))
    val read: Either[String, Map[os.Path, Vector[Defn]]] =
      if (stale.isEmpty) Right(Map.empty) else Read.defnsByTasty(root, stale)
    read.map { fresh =>
      val updated = stale.foldLeft(in.byTasty) { (cache, p) =>
        cache.updated(p, Cached(os.mtime(p), fresh.getOrElse(p, Vector.empty)))
      }
      val all = tasty.flatMap(p => updated.get(p).map(_.defns).getOrElse(Vector.empty))
      (all, Loaded(updated), stale.size)
    }
  }
}
