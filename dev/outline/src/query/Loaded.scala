package grit.outline.query

import grit.outline.locate.{Layout, Root}
import grit.outline.model.{Defn, Use}
import grit.outline.read.Read

/** One `.tasty` file's definitions as of its mtime. */
final case class Cached(mtime: Long, defns: Vector[Defn])

/** One `.tasty` file's references as of its mtime. */
final case class Indexed(mtime: Long, uses: Vector[Use])

/** The definitions read so far, by `.tasty` path; a file is read again only when its mtime changed. `failed` holds, by path, the mtime at which the inspector failed on a file; such a file is not read again until its mtime changes. `unreadable` holds the message of each file in `failed` whose traversal threw. `refs` is the reference index `uses` reads from, by `.tasty` path and mtime, kept beside the definitions so that one root's cache bounds both. */
final case class Loaded(
    byTasty: Map[os.Path, Cached],
    failed: Map[os.Path, Long] = Map.empty,
    unreadable: Map[os.Path, String] = Map.empty,
    refs: Map[os.Path, Indexed] = Map.empty
) {

  /** The cached entries: one per `.tasty` file whose definitions are cached and one per file whose references are indexed. */
  def entries: Int = byTasty.size + refs.size
}

object Loaded {
  val empty: Loaded = Loaded(Map.empty)

  /** The `.tasty` files one inspector run reads for `uses`. One run over 1600 files exhausted the CLI's 512 MB heap in the inspector's tree walk, and the 1573 candidates of `grit.core.act.MoveLimits.of` complete in runs of 800. */
  val readBatch: Int = 800

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
      val next = withDefns(in, stale, batch)
      val all = tasty.flatMap(p => next.byTasty.get(p).map(_.defns).getOrElse(Vector.empty))
      (all, next, stale.size)
    }
  }

  /** `in` with each of `files` cached at its current mtime as `batch` read it, and each file `batch` could not read remembered as failed. */
  private def withDefns(in: Loaded, files: Vector[os.Path], batch: Read.Batch): Loaded = {
    val byTasty = files.foldLeft(in.byTasty) { (cache, p) =>
      cache.updated(p, Cached(os.mtime(p), batch.byTasty.getOrElse(p, Vector.empty)))
    }
    val failedNow = batch.unreadable.map { case (p, _) => p -> os.mtime(p) }.toMap
    in.copy(
      byTasty = byTasty,
      failed = (in.failed -- files) ++ failedNow,
      unreadable = (in.unreadable -- files) ++ batch.unreadable.toMap
    )
  }

  /** Every reference in `tasty` to a member of a class or package defined in source under `root`, reading (in runs of `readBatch` files) only files absent from the index or changed since they were indexed; each run reads those files' definitions too, cached as `defns` caches them; the new cache; how many files were read. */
  def uses(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path],
      in: Loaded
  ): Either[String, (Vector[Use], Loaded, Int)] = {
    val stale = tasty.filter(p => !indexedAt(in, p))
    stale
      .grouped(readBatch)
      .foldLeft[Either[String, Loaded]](Right(in)) { (acc, batch) =>
        acc.flatMap(state =>
          Read.definitionsAndReferences(root, layout, batch).map(both => absorb(state, batch, both))
        )
      }
      .map { next =>
        val all = tasty.flatMap(p => next.refs.get(p).map(_.uses).getOrElse(Vector.empty))
        (all, next, stale.size)
      }
  }

  /** `in` with each of `batch`'s files cached as `both` read them: definitions, references, and any file that could not be read. */
  private def absorb(in: Loaded, batch: Vector[os.Path], both: Read.Both): Loaded = {
    val next = withDefns(in, batch, both.batch)
    next.copy(refs = batch.foldLeft(next.refs) { (index, p) =>
      index.updated(p, Indexed(os.mtime(p), both.uses.getOrElse(p, Vector.empty)))
    })
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

  private def indexedAt(in: Loaded, p: os.Path): Boolean =
    in.refs.get(p).exists(_.mtime == os.mtime(p))
}
