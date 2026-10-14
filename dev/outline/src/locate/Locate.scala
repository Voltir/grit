package grit.outline.locate

import java.time.Instant

import scala.util.Try

import grit.outline.model.Staleness

/** A repository checkout whose build output lives under `dir/out`. */
final case class Root(dir: os.Path)

/** Finds build output and sources for a repository; each query returns sorted paths, empty when none match. */
object Locate {

  /** Top-level directories a mentioning search never reads: build output, tooling and VCS state. */
  private val skipped = Set("out", ".git", ".local", ".claude", ".bsp", ".metals", "tools")

  /** The checkout's branch (None when detached) and the first 8 characters of its commit; `head` is "unknown" when git state is unreadable. */
  def revision(root: Root): Revision = {
    val unknown = Revision(None, "unknown")
    val dotGit = root.dir / ".git"
    val gitDir: Option[os.Path] =
      if (os.isDir(dotGit)) Some(dotGit)
      else
        readText(dotGit).flatMap(s =>
          Option.when(s.startsWith("gitdir:"))(os.Path(s.stripPrefix("gitdir:").trim, root.dir))
        )
    gitDir
      .flatMap { dir =>
        readText(dir / "HEAD").flatMap { head =>
          if (head.startsWith("ref: ")) {
            val ref = head.stripPrefix("ref: ")
            refHash(dir, ref).map(hash => Revision(Some(ref.stripPrefix("refs/heads/")), hash))
          } else Some(Revision(None, head))
        }
      }
      .flatMap(r => Option.when(r.head.length >= 8)(r.copy(head = r.head.take(8))))
      .getOrElse(unknown)
  }

  private def readText(p: os.Path): Option[String] = Try(os.read(p).trim).toOption

  /** The commit `ref` names in `gitDir`: its loose file, else its line in the common dir's packed-refs. */
  private def refHash(gitDir: os.Path, ref: String): Option[String] = {
    val common = readText(gitDir / "commondir").fold(gitDir)(c => gitDir / os.RelPath(c))
    readText(gitDir / os.RelPath(ref))
      .orElse(readText(common / os.RelPath(ref)))
      .orElse(
        readText(common / "packed-refs").flatMap(
          _.linesIterator.map(_.split(' ')).collectFirst {
            case Array(hash, name) if name == ref => hash
          }
        )
      )
  }

  /** The `.tasty` files that can hold `sym`, searched in every classes dir `layout` names under `root`; empty when none match. */
  def forTopLevel(root: Root, layout: Layout, sym: String): Vector[os.Path] =
    forTopLevelIn(layout.classesDirs(root), sym)

  /** `forTopLevel` over the classes dirs the caller already found, so a read finds them once. */
  def forTopLevelIn(dirs: Vector[os.Path], sym: String): Vector[os.Path] = {
    val segments = sym.split('.').toVector
    val rest = segments.dropWhile(s => !s.take(1).exists(_.isUpper))
    rest.headOption match {
      case None => Vector.empty
      case Some(top) =>
        val pkg = segments.take(segments.length - rest.length)
        val names = Set(s"$top.tasty", s"$top$$package.tasty")
        val found =
          if (pkg.isEmpty) dirs.flatMap(c => os.walk(c).filter(p => names.contains(p.last)))
          else {
            val rel = os.RelPath(pkg.mkString("/"))
            dirs.flatMap(c => names.toVector.map(n => c / rel / n))
          }
        found.filter(os.isFile).distinct.sortBy(_.toString)
    }
  }

  /** The `.tasty` files directly in the source file's package dir across all classes dirs; empty when the file is missing or has no package line. */
  def inPackageOf(root: Root, layout: Layout, file: os.RelPath): Vector[os.Path] =
    inPackageIn(root, layout.classesDirs(root), file)

  /** `inPackageOf` over the classes dirs the caller already found, so a query finds them once. */
  def inPackageIn(root: Root, classes: Vector[os.Path], file: os.RelPath): Vector[os.Path] = {
    val source = root.dir / file
    val segments =
      if (!os.isFile(source)) Vector.empty
      else packageOf(os.read.lines(source)).flatMap(_.split('.')).toVector
    if (segments.isEmpty) Vector.empty
    else {
      val rel = os.RelPath(segments.mkString("/"))
      classes
        .map(c => c / rel)
        .filter(os.isDir)
        .flatMap(d => os.list(d).filter(p => os.isFile(p) && p.ext == "tasty"))
        .sortBy(_.toString)
        .toVector
    }
  }

  /** Repo `.scala` files under a `src` directory whose text holds `simpleName` as a whole word, outside the root's build and tool dirs. */
  def mentioning(root: Root, simpleName: String): Vector[os.RelPath] =
    mentioningEach(root, Vector(simpleName)).getOrElse(simpleName, Vector.empty)

  /** For each of `words`, the files `mentioning` gives for it, each file read once; a word no file holds maps to an empty vector. */
  def mentioningEach(root: Root, words: Vector[String]): Map[String, Vector[os.RelPath]] = {
    val patterns =
      words.distinct.map(w => w -> ("\\b" + java.util.regex.Pattern.quote(w) + "\\b").r)
    val held = sources(root)
      .map { p =>
        val text = os.read(p)
        p.relativeTo(root.dir) -> patterns.collect {
          case (w, word) if word.findFirstIn(text).isDefined => w
        }.toSet
      }
    patterns.map { case (w, _) =>
      w -> held.collect { case (rel, ws) if ws.contains(w) => rel }
    }.toMap
  }

  /** The files `mentioning` searches whose text holds a match of `word`, sorted. */
  def holding(root: Root, word: scala.util.matching.Regex): Vector[os.RelPath] =
    sources(root).filter(p => word.findFirstIn(os.read(p)).isDefined).map(_.relativeTo(root.dir))

  /** Repo `.scala` files under a `src` directory, outside the root's build and tool dirs, sorted. */
  private def sources(root: Root): Vector[os.Path] =
    os.walk(
      root.dir,
      skip = p => p.relativeTo(root.dir).segments.headOption.exists(skipped.contains)
    ).filter(p =>
      p.ext == "scala" && os.isFile(p) && p.relativeTo(root.dir).segments.contains("src")
    ).sortBy(_.toString)
      .toVector

  /** Each of `files`, a source with the `.tasty` files of its package, judged against the `.tasty` compiled from it, in
    * order. A source no newer than every one of them is `Fresh`, read no further. Otherwise it is `Fresh` when no newer
    * than the newest `.tasty` compiled from it, or when its lines are as many and as long as that `.tasty` records, so a
    * checkout that rewrites a file with the same content leaves it `Fresh`; else `Stale`. An edit that keeps every line's
    * length reads `Fresh`. A source no `.tasty` records as its own is judged by the package's newest. `NoTasty` when its
    * `.tasty` files are none, `NoSource` when the source is missing.
    */
  def staleness(root: Root, files: Vector[(os.RelPath, Vector[os.Path])]): Vector[Staleness] = {
    // Each `.tasty` read once in a call: a package's files share one set.
    val read = scala.collection.mutable.Map.empty[os.Path, Option[TastySource]]
    def recorded(p: os.Path): Option[TastySource] = read.getOrElseUpdate(p, TastySource.of(p))
    files.map { case (file, tasty) =>
      val path = root.dir / file
      val byAge = tasty.map(p => p -> os.mtime(p)).sortBy(-_._2)
      byAge.headOption match {
        case None => Staleness.NoTasty
        case Some(_) if !os.isFile(path) => Staleness.NoSource
        case Some((_, newest)) =>
          val source = os.mtime(path)
          def stale(tasty: Long) =
            Staleness.Stale(Instant.ofEpochMilli(source), Instant.ofEpochMilli(tasty))
          if (byAge.forall(_._2 >= source)) Staleness.Fresh
          else
            byAge.find { case (p, _) => recorded(p).exists(_.isOf(file)) } match {
              case None => if (source > newest) stale(newest) else Staleness.Fresh
              case Some((_, own)) if source <= own => Staleness.Fresh
              case Some((p, own)) =>
                val text = Try(os.read(path)).toOption
                val same =
                  recorded(p).exists(r => text.exists(t => r.lineSizes == TastySource.lineSizes(t)))
                if (same) Staleness.Fresh else stale(own)
            }
      }
    }
  }

  /** The package clause segments that lead a source file, in order; stops at the first line that is not a package clause, blank or a line comment. */
  private def packageOf(lines: Seq[String]): Vector[String] =
    lines
      .map(_.trim)
      .filter(_.nonEmpty)
      .takeWhile(l => l.startsWith("package ") || l.startsWith("//"))
      .collect { case l if l.startsWith("package ") => l.stripPrefix("package ").trim }
      .toVector
}
