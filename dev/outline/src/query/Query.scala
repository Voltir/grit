package grit.outline.query

import scala.collection.mutable

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.model.{Defn, Kind, Staleness, Use}
import grit.outline.read.Read
import grit.outline.render.{Family, Render}
import grit.outline.trace.Trace

enum Status {
  case Found, NoMatch, Failed
}

final case class Answer(text: String, status: Status)

object Query {

  private val usage =
    "usage: show Sym[,Sym…] [--depth 0|1|2] [--body m[,m…]] [--private] [--cap BYTES] [--root DIR]"

  /** The `show` answer for `syms` (each `Name`, `Name.member` or fully qualified), read through `root`'s own cache in `roots`, which then holds `root` as most recently used. Every answer starts with `root`'s `## root` line. */
  def show(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      syms: Vector[String],
      depth: Int,
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int
  ): (Answer, Roots) = {
    val (answer, loaded) =
      showIn(root, layout, config, Roots.of(roots, root), syms, depth, bodies, withPrivate, cap)
    (answer.copy(text = s"${header(root)}\n${answer.text}"), Roots.put(roots, root, loaded))
  }

  /** The `uses` answer: the direct references to `syms`, grouped by file, filtered by `in` and `outside` path prefixes. */
  def uses(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      syms: Vector[String],
      in: Option[String],
      outside: Option[String],
      cap: Int
  ): (Answer, Roots) = {
    val classes = layout.classesDirs(root)
    if (classes.isEmpty) (Answer(layout.notCompiled(root), Status.Failed), roots)
    else {
      // The cache read in this query, threaded through a scoped local, as `showIn` does.
      var state = Roots.of(roots, root)
      val targets = mutable.ListBuffer.empty[Defn]
      val notes = mutable.ListBuffer.empty[String]
      var failure: Option[String] = None

      Resolve.preload(root, layout, Scope.WithTests, syms, state) match {
        case Left(message) => failure = Some(message)
        case Right(next) => state = next
      }
      syms.foreach { sym =>
        Resolve.resolve(root, layout, config, Scope.WithTests, sym, state) match {
          case Left(message) => failure = Some(message)
          case Right(resolved) =>
            state = resolved.loaded
            targets ++= resolved.defns
            notes ++= resolved.notes
        }
      }

      val nextRoots = Roots.put(roots, root, state)
      val names = targets.toVector.map(_.fullName).distinct.toSet
      // A name's references are read in the packages of the files that mention its last segment as a word.
      val candidates = syms
        .map(_.split('.').last)
        .distinct
        .flatMap(simple => Locate.mentioning(root, simple))
        .distinct
      val inPackage = candidates.map(file => file -> Locate.inPackageOf(root, layout, file))
      val tasty = inPackage.flatMap(_._2).distinct
      val unsearched = inPackage.collect {
        case (file, found)
            if found.isEmpty && !config.hidden.exists(prefix => file.toString.startsWith(prefix)) =>
          file
      }
      val read: Either[String, Vector[Use]] =
        if (failure.nonEmpty || names.isEmpty || tasty.isEmpty) Right(Vector.empty)
        else Read.uses(root, layout, tasty, names)
      val answer = failure match {
        case Some(message) => Answer(message, Status.Failed)
        case None =>
          read match {
            case Left(message) => Answer(message, Status.Failed)
            case Right(all) =>
              val kept = all.filter(u =>
                in.forall(prefix => u.file.startsWith(prefix)) &&
                  outside.forall(prefix => !u.file.startsWith(prefix))
              )
              if (kept.isEmpty) Answer(s"no uses of ${syms.mkString(",")}", Status.NoMatch)
              else {
                val overloaded = targets
                  .groupBy(_.fullName)
                  .collect {
                    case (name, ds) if ds.size > 1 => name
                  }
                  .toSet
                Answer(Render.uses(kept, overloaded, cap), Status.Found)
              }
          }
      }
      val noteText = (notes.toVector ++ notSearched(unsearched)).map(_ + "\n").mkString
      (answer.copy(text = s"${header(root)}\n$noteText${answer.text}"), nextRoots)
    }
  }

  /** The `area` answer for the named areas `names`, declared in `config`: at level 0 one line per package, at level 1 each definition with its doc's first sentence, grouped by file; `NoMatch` for a name no area declares, `Failed` for a level outside 0..1; cut at `cap` bytes. */
  def area(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      names: Vector[String],
      level: Int,
      cap: Int
  ): (Answer, Roots) =
    if (level < 0 || level > 1)
      (Answer(s"level must be 0 or 1\n$areaUsage", Status.Failed), roots)
    else {
      val declared = config.areas.map(_.name)
      names.find(n => !declared.contains(n)) match {
        case Some(n) =>
          (Answer(s"no area $n; declared: ${declared.mkString(", ")}", Status.NoMatch), roots)
        case None if layout.classesDirs(root).isEmpty =>
          (Answer(layout.notCompiled(root), Status.Failed), roots)
        case None =>
          val selected = names.distinct.flatMap(n => config.areas.find(_.name == n))
          // The cache read in this query, threaded through a scoped local, as `showIn` does.
          var state = Roots.of(roots, root)
          val listed = mutable.ListBuffer.empty[Render.Listed]
          val notes = mutable.ListBuffer.empty[String]
          var failure: Option[String] = None

          Resolve.preload(root, layout, Scope.Main, selected.flatMap(_.symbols), state) match {
            case Left(message) => failure = Some(message)
            case Right(next) => state = next
          }
          Resolve.preload(
            root,
            layout,
            Scope.WithTests,
            selected.flatMap(_.families),
            state
          ) match {
            case Left(message) => failure = Some(message)
            case Right(next) => state = next
          }
          selected.foreach { a =>
            a.symbols.foreach { sym =>
              if (failure.isEmpty)
                Resolve.resolve(root, layout, config, Scope.Main, sym, state) match {
                  case Left(message) => failure = Some(message)
                  case Right(resolved) =>
                    state = resolved.loaded
                    notes ++= resolved.notes
                    listed ++= resolved.defns.map(d => Render.Listed(d, ""))
                }
            }
            a.families.foreach { name =>
              if (failure.isEmpty)
                areaFamily(root, layout, config, state, name) match {
                  case Left(message) => failure = Some(message)
                  case Right((members, familyNotes, next)) =>
                    state = next
                    notes ++= familyNotes
                    listed ++= members
                }
            }
          }

          val head = (header(root) +: notes.toVector).mkString("\n")
          val nextRoots = Roots.put(roots, root, state)
          failure match {
            case Some(message) => (Answer(s"$head\n$message", Status.Failed), nextRoots)
            case None =>
              val entries = listed.toVector.distinctBy(l =>
                (l.defn.fullName, l.defn.file, l.defn.lines.start, l.prefix)
              )
              val status = if (entries.isEmpty) Status.NoMatch else Status.Found
              (Answer(Render.area(entries, level, head, cap), status), nextRoots)
          }
      }
    }

  private val areaUsage = "usage: area name[,name…] [--level 0|1] [--cap BYTES] [--root DIR]"

  /** The definitions of the family `name` (its trait, implementations and contracts, each with its role as a prefix), the notes its loading raised, and the cache after; `Left` when no trait or abstract class is named `name`. */
  private def areaFamily(
      root: Root,
      layout: Layout,
      config: Config,
      in: Loaded,
      name: String
  ): Either[String, (Vector[Render.Listed], Vector[String], Loaded)] =
    Resolve.resolve(root, layout, config, Scope.WithTests, name, in).flatMap { resolved =>
      val exact = resolved.defns.exists(_.fullName == name)
      val traitName = resolved.defns.map(_.fullName).distinct.sorted.headOption.getOrElse(name)
      val files = Locate
        .mentioning(root, name.split('.').last)
        .filter(file => exact || !config.hidden.exists(prefix => file.toString.startsWith(prefix)))
      val inPackage = files.map(file => file -> Locate.inPackageOf(root, layout, file))
      val tasty = inPackage.flatMap(_._2).distinct
      val unsearched = inPackage.collect { case (file, found) if found.isEmpty => file }
      val (defns, next, missing) = loadAll(root, layout, tasty, resolved.loaded)
      val notes = resolved.notes ++ missing ++ notSearched(unsearched)
      familyOf(defns, traitName, None, false).map { f =>
        val members =
          Vector(Render.Listed(f.trait0, "")) ++
            f.impls.map(d => Render.Listed(d, "impl ")) ++
            f.contracts.map { case (c, _) => Render.Listed(c, "contract ") }
        (members, notes, next)
      }
    }

  /** The `family` answer for `name` (a trait, or an abstract class), read through `root`'s own cache in `roots`, which then holds `root` as most recently used. `member` narrows the trait's and each implementation's text to that member, with its body under `withBody`. Every answer starts with `root`'s `## root` line. */
  def family(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      name: String,
      member: Option[String],
      withBody: Boolean,
      cap: Int
  ): (Answer, Roots) = {
    val (answer, loaded) =
      familyIn(root, layout, config, Roots.of(roots, root), name, member, withBody, cap)
    (answer, Roots.put(roots, root, loaded))
  }

  /** The family of the trait or abstract class `name` among `defns` (top-level, as loaded; their members are read here): its implementations, its contracts (abstract classes that do not extend it and have a member referring to it), and each contract's suites (the definitions extending it), in source order. `Left` when no trait or abstract class is named `name`. */
  def familyOf(
      defns: Vector[Defn],
      name: String,
      member: Option[String],
      withBody: Boolean
  ): Either[String, Family] = {
    val all = everyDefnOf(defns)
    def named(d: Defn): Boolean = d.fullName == name || d.fullName.endsWith("." + name)
    def inSourceOrder(ds: Vector[Defn]): Vector[Defn] = ds.sortBy(d => (d.file, d.lines.start))
    all.find(d =>
      named(d) && (d.kind == Kind.Trait || (d.kind == Kind.Class && d.isAbstract))
    ) match {
      case None => Left(s"no trait or abstract class named $name")
      case Some(t) =>
        val contracts = inSourceOrder(
          all.filter(c =>
            c.kind == Kind.Class && c.isAbstract && !c.parents.contains(t.fullName) &&
              c.members.exists(_.refs.exists(_.fullName == t.fullName))
          )
        )
        val impls = inSourceOrder(
          all.filter(d =>
            (d.kind == Kind.Class || d.kind == Kind.CaseClass || d.kind == Kind.Object) &&
              d.parents.contains(t.fullName)
          )
        )
        val suites =
          contracts.map(c => (c, inSourceOrder(all.filter(_.parents.contains(c.fullName)))))
        Right(Family(t, member, impls, suites, withBody))
    }
  }

  private def everyDefnOf(ds: Vector[Defn]): Vector[Defn] =
    ds.flatMap(d => d +: everyDefnOf(d.members))

  /** `tasty`'s definitions in one inspector run; when that fails, the files are split in halves until each half loads or is a single file. A file that still fails is remembered in `Loaded` and skipped, and every answer that would have included it names it, relative to its classes directory, in one `-- not loaded:` note; the cause is the first line the inspector captured for the first file that failed in this run, and none when every named file was remembered. */
  private[query] def loadAll(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path],
      in: Loaded
  ): (Vector[Defn], Loaded, Vector[String]) = {
    val (defns, next, failed) = bisect(root, layout, tasty, in)
    val remembered = Loaded.remembered(tasty, in)
    val named = tasty.filter(p => remembered.contains(p) || failed.exists(_._1 == p))
    val notes =
      if (named.isEmpty) Vector.empty
      else {
        val cause = failed.headOption.fold("") { case (_, complaint) =>
          val line = captured(complaint)
          if (line.isEmpty) "" else s" ($line)"
        }
        val files = named.map(relativePath(root, layout, _))
        Vector(s"-- not loaded: ${files.mkString(", ")}$cause")
      }
    (defns, next, notes)
  }

  /** `tasty`'s definitions, with each file that fails alone paired with the inspector's complaint about it. */
  private def bisect(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path],
      in: Loaded
  ): (Vector[Defn], Loaded, Vector[(os.Path, String)]) =
    Loaded.defns(root, layout, tasty, in) match {
      case Right((ds, next, _)) => (ds, next, Vector.empty)
      case Left(complaint) if tasty.sizeIs == 1 =>
        (Vector.empty, Loaded.withFailures(tasty, in), tasty.map(_ -> complaint))
      case Left(_) =>
        val (left, right) = tasty.splitAt(tasty.size / 2)
        val (leftDefns, afterLeft, leftFailed) = bisect(root, layout, left, in)
        val (rightDefns, afterRight, rightFailed) = bisect(root, layout, right, afterLeft)
        (leftDefns ++ rightDefns, afterRight, leftFailed ++ rightFailed)
    }

  /** The first line the inspector captured, from a read's complaint, cut at 160 characters; empty when it captured none. */
  private def captured(complaint: String): String =
    complaint.takeWhile(_ != '\n').dropWhile(_ != ':').drop(1).trim.take(160)

  /** The `-- not compiled, not searched:` line naming `files` (at most eight, then how many more), or none when `files` is empty. A file with no `.tasty` has no references read from it. */
  private def notSearched(files: Vector[os.RelPath]): Vector[String] =
    if (files.isEmpty) Vector.empty
    else {
      val more = if (files.size > 8) s" and ${files.size - 8} more" else ""
      Vector(s"-- not compiled, not searched: ${files.take(8).mkString(", ")}$more")
    }

  private def relativePath(root: Root, layout: Layout, file: os.Path): String =
    layout
      .classesDirs(root)
      .find(file.startsWith)
      .fold(file.toString)(c => file.relativeTo(c).toString)

  private def familyIn(
      root: Root,
      layout: Layout,
      config: Config,
      in: Loaded,
      name: String,
      member: Option[String],
      withBody: Boolean,
      cap: Int
  ): (Answer, Loaded) =
    if (layout.classesDirs(root).isEmpty)
      (Answer(s"${header(root)}\n${layout.notCompiled(root)}", Status.Failed), in)
    else
      Resolve.resolve(root, layout, config, Scope.WithTests, name, in) match {
        case Left(message) => (Answer(s"${header(root)}\n$message", Status.Failed), in)
        case Right(resolved) =>
          // A name matched exactly keeps its hidden candidate files; a short name does not reach them.
          val exact = resolved.defns.exists(_.fullName == name)
          val traitName = resolved.defns.map(_.fullName).distinct.sorted.headOption.getOrElse(name)
          val files = Locate
            .mentioning(root, name.split('.').last)
            .filter(file =>
              exact || !config.hidden.exists(prefix => file.toString.startsWith(prefix))
            )
          val inPackage = files.map(file => file -> Locate.inPackageOf(root, layout, file))
          val tasty = inPackage.flatMap(_._2).distinct
          val unsearched = inPackage.collect { case (file, found) if found.isEmpty => file }
          val (defns, next, missing) = loadAll(root, layout, tasty, resolved.loaded)
          val head =
            (Vector(header(root)) ++ resolved.notes ++ missing ++ notSearched(unsearched))
              .mkString("\n")
          familyOf(defns, traitName, member, withBody) match {
            case Left(message) => (Answer(s"$head\n$message", Status.NoMatch), next)
            case Right(f) =>
              val stale = files
                .filter(file =>
                  Locate.staleness(root, file, Locate.inPackageOf(root, layout, file)) match {
                    case Staleness.Stale(_, _) => true
                    case _ => false
                  }
                )
                .map(_.toString)
                .toSet
              (Answer(Render.family(f, everyDefnOf(defns), head, stale, cap), Status.Found), next)
          }
      }

  /** `root`'s `## root` line: its directory, then its branch (or "detached") and HEAD. */
  private def header(root: Root): String = {
    val revision = Locate.revision(root)
    s"## root ${root.dir} (${revision.branch.getOrElse("detached")} @ ${revision.head})"
  }

  private def showIn(
      root: Root,
      layout: Layout,
      config: Config,
      in: Loaded,
      syms: Vector[String],
      depth: Int,
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int
  ): (Answer, Loaded) = {
    val classes = layout.classesDirs(root)
    if (depth < 0 || depth > 2)
      (Answer(s"depth must be 0, 1 or 2\n$usage", Status.Failed), in)
    else if (classes.isEmpty)
      (Answer(layout.notCompiled(root), Status.Failed), in)
    else {
      // The cache and the definitions read in this query, threaded through a scoped local.
      var state = in
      val companions = mutable.ListBuffer.empty[Defn]
      val notes = mutable.ListBuffer.empty[String]
      var failure: Option[String] = None

      def loadTop(tasty: Vector[os.Path]): Either[String, Vector[Defn]] =
        Loaded.defns(root, layout, tasty, state) match {
          case Left(message) => Left(message)
          case Right((defns, next, _)) =>
            state = next
            companions ++= defns
            Right(defns)
        }

      Resolve.preload(root, layout, Scope.Main, syms, state) match {
        case Left(message) => failure = Some(message)
        case Right(next) => state = next
      }
      val matches = mutable.ListBuffer.empty[Defn]
      syms.foreach { sym =>
        Resolve.resolve(root, layout, config, Scope.Main, sym, state) match {
          case Left(message) => failure = Some(message)
          case Right(resolved) =>
            state = resolved.loaded
            companions ++= resolved.seen
            notes ++= resolved.notes
            matches ++= resolved.defns
        }
      }

      val noteText = notes.toVector.map(_ + "\n").mkString
      failure match {
        case Some(message) => (Answer(message, Status.Failed), state)
        case None if matches.isEmpty =>
          // resolve's `-- no match for X` note is the one line naming a miss
          (Answer(noteText.stripSuffix("\n"), Status.NoMatch), state)
        case None =>
          val tops = matches.toVector.distinct
          val traced = Trace.trace(
            tops,
            depth,
            top => {
              val tasty = Locate.forTopLevel(root, layout, top)
              if (tasty.isEmpty) Left(s"no tasty for $top") else loadTop(tasty)
            }
          )
          val loadedDefns = companions.toVector
          // A file is stale when it is newer than the tasty it is read from, as the package's tasty files say.
          val stale = loadedDefns
            .map(_.file)
            .distinct
            .filter(file =>
              Locate.staleness(
                root,
                os.RelPath(file),
                Locate.inPackageOf(root, layout, os.RelPath(file))
              ) match {
                case Staleness.Stale(_, _) => true
                case _ => false
              }
            )
            .toSet
          val rendered = Render.show(
            named = tops,
            traced = traced,
            companions = loadedDefns,
            stale = stale,
            bodies = bodies,
            withPrivate = withPrivate,
            cap = cap
          )
          val text = noteText + rendered
          (Answer(text, Status.Found), state)
      }
    }
  }
}
