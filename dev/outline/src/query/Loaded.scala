package grit.outline.query

import grit.outline.locate.{Layout, Root}
import grit.outline.model.Defn
import grit.outline.read.Read

/** One `.tasty` file's definitions as of its mtime. */
final case class Cached(mtime: Long, defns: Vector[Defn])

/** The definitions read so far, by `.tasty` path; a file is read again only when its mtime changed. `failed` holds, by path, the mtime at which the inspector failed on a file; such a file is not read again until its mtime changes. `unreadable` holds the message of each file in `failed` whose traversal threw. */
final case class Loaded(
    byTasty: Map[os.Path, Cached],
    failed: Map[os.Path, Long] = Map.empty,
    unreadable: Map[os.Path, String] = Map.empty
)

object Loaded {
  val empty: Loaded = Loaded(Map.empty)

  /** `tasty`'s definitions, reading (in one inspector run) only files absent or changed and not failed at their current mtime; the new cache; how many files were read. */
  def defns(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path],
      in: Loaded
  ): Either[String, (Vector[Defn], Loaded, Int)] = {
    val stale = tasty.filter(p => !(cachedAt(in, p) || failedAt(in, p)))
    val read: Either[String, Read.Batch] =
      if (stale.isEmpty) Right(Read.Batch(Map.empty, Vector.empty))
      else Read.defnsByTasty(root, layout, stale)
    read.map { batch =>
      val updated = stale.foldLeft(in.byTasty) { (cache, p) =>
        cache.updated(p, Cached(os.mtime(p), batch.byTasty.getOrElse(p, Vector.empty)))
      }
      val all = tasty.flatMap(p => updated.get(p).map(_.defns).getOrElse(Vector.empty))
      val failedNow = batch.unreadable.map { case (p, _) => p -> os.mtime(p) }.toMap
      val failed = (in.failed -- stale) ++ failedNow
      val unreadable = (in.unreadable -- stale) ++ batch.unreadable.toMap
      (all, Loaded(updated, failed, unreadable), stale.size)
    }
  }

  /** The files of `tasty` that failed at their current mtime, so are not read again by `defns`. */
  def remembered(tasty: Vector[os.Path], in: Loaded): Vector[os.Path] =
    tasty.filter(p => failedAt(in, p))

  /** `in` with each of `files` remembered as failed at its current mtime. */
  def withFailures(files: Vector[os.Path], in: Loaded): Loaded =
    in.copy(failed = files.foldLeft(in.failed)((failed, p) => failed.updated(p, os.mtime(p))))

  private def cachedAt(in: Loaded, p: os.Path): Boolean =
    in.byTasty.get(p).exists(_.mtime == os.mtime(p))

  private def failedAt(in: Loaded, p: os.Path): Boolean =
    in.failed.get(p).contains(os.mtime(p))
}
