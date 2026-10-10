package grit.outline.query

import scala.collection.mutable

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.model.Lines
import grit.outline.model.{Defn, Kind, Staleness, Use}
import grit.outline.read.Read
import grit.outline.render.{Family, Render}
import grit.outline.trace.Trace

enum Status {
  case Found, NoMatch, Failed
}

final case class Answer(text: String, status: Status)

object Query {

  /** The `show` answer for `syms` (each `Name`, `Name.member` or fully qualified), read through `root`'s own cache in `roots`, which then holds `root` as most recently used. Every answer starts with `root`'s `## root` line. A name found only in test sources is a `NoMatch` whose note names the `tests` query for its suite. */
  def show(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      syms: Vector[String],
      depth: Int,
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int,
      withTests: Boolean = true
  ): (Answer, Roots) = {
    val (answer, loaded) =
      showIn(
        root,
        layout,
        config,
        Roots.of(roots, root),
        syms,
        depth,
        bodies,
        withPrivate,
        cap,
        withTests
      )
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
      val seen = mutable.ListBuffer.empty[Defn]
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
            seen ++= resolved.seen
            notes ++= resolved.notes
        }
      }

      val names = targets.toVector.map(_.fullName).distinct.toSet
      val (read, unsearched, next) = referencesIn(
        root,
        layout,
        config,
        classes,
        if (failure.nonEmpty) Set.empty else names,
        // With no target, the search by each name's last segment still names the files not compiled.
        if (targets.isEmpty) syms.map(sym => Search(sym.split('.').last, None)).distinct
        else searchesFor(targets.toVector, seen.toVector),
        _ => true,
        state
      )
      state = next
      val nextRoots = Roots.put(roots, root, state)
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
      (Answer(s"level must be 0 or 1\n${Help.usage(Help.area)}", Status.Failed), roots)
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

  /** The `tests` answer for the suite `suite`, named as `show` names a symbol: its helpers (the members whose range holds no test call, private ones included, and the other top-level definitions of its file beside it, by line) and its tests with their line ranges, read from the file the suite is defined in. `test` is a prefix whose tests print their verbatim bodies. `NoMatch` when no class or object is named `suite`; `Failed` when the layout is not compiled; cut at `cap` bytes. */
  def tests(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      suite: String,
      test: Option[String],
      cap: Int
  ): (Answer, Roots) =
    if (layout.classesDirs(root).isEmpty)
      (Answer(layout.notCompiled(root), Status.Failed), roots)
    else
      Resolve.resolve(root, layout, config, Scope.WithTests, suite, Roots.of(roots, root)) match {
        case Left(message) => (Answer(message, Status.Failed), roots)
        case Right(resolved) =>
          val nextRoots = Roots.put(roots, root, resolved.loaded)
          resolved.defns.find(d => d.kind == Kind.Class || d.kind == Kind.Object) match {
            case None => (Answer(s"no class or object $suite", Status.NoMatch), nextRoots)
            case Some(d) =>
              val path = root.dir / os.RelPath(d.file)
              if (!os.isFile(path))
                (Answer(s"no source for $suite at ${d.file}", Status.Failed), nextRoots)
              else {
                val src = os.read(path)
                def holdsTests(member: Defn): Boolean = {
                  val (from, to) = TestCalls.within(src, member.lines)
                  TestCalls.find(src, from, to, config.testCall).nonEmpty
                }
                val (suiteFrom, suiteTo) = TestCalls.within(src, d.lines)
                val tests = TestCalls.find(src, suiteFrom, suiteTo, config.testCall)
                // Beside the suite, not inside it: a definition in its range is a member's part, not another top-level one.
                val otherTops = resolved.seen.filter(t =>
                  t.file == d.file && (t.lines.end < d.lines.start || d.lines.end < t.lines.start)
                )
                val helpers = (d.members.filterNot(holdsTests) ++ otherTops).sortBy(_.lines.start)
                val notes = test
                  .filterNot(p => tests.exists(_.name.startsWith(p)))
                  .map(p => "-- no test starting \"" + p + "\"")
                val head = (Vector(header(root)) ++ notes).mkString("\n")
                (Answer(Render.tests(d, helpers, tests, test, head, cap), Status.Found), nextRoots)
              }
          }
      }

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
    val remembered = Loaded.remembered(tasty, next)
    val named = tasty.filter(p => remembered.contains(p) || failed.exists(_._1 == p))
    val notes =
      if (named.isEmpty) Vector.empty
      else {
        val cause = failed.headOption.fold("") { case (_, complaint) =>
          val line = captured(complaint)
          if (line.isEmpty) "" else s" ($line)"
        }
        val files = named.map { p =>
          val name = relativePath(root, layout, p)
          next.unreadable.get(p).fold(name) { message =>
            s"$name (${message.takeWhile(_ != '\n').take(160)})"
          }
        }
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

  /** A word a source file must hold to be searched for references to a target, and the simple name of the object that owns the target, which the file must hold too when there is one. */
  private[query] final case class Search(name: String, owner: Option[String])

  /** The searches for references to `targets`, whose owners are found among `seen` and their members. A member of an object is searched for only in files that also name the object (its own file does): it is named through the object, or through an import that names it, a renaming import included. Missed: a reference through a value whose type is the object, such as `val o = Obj; o.f`, or through an `export` elsewhere. A member of a class or trait is not narrowed, since a call through a value of its type, such as `ctx.inbox.hear(...)`, need not name the type in its file; nor is a top-level definition. */
  private[query] def searchesFor(targets: Vector[Defn], seen: Vector[Defn]): Vector[Search] = {
    def every(ds: Vector[Defn]): Vector[Defn] = ds.flatMap(d => d +: every(d.members))
    val objects = every(seen).filter(d => d.kind == Kind.Object && !d.name.endsWith("$package"))
    targets.map { t =>
      Search(t.name, objects.find(o => o.members.exists(_.fullName == t.fullName)).map(_.name))
    }.distinct
  }

  /** The source files under `root` that hold a search's name as a word and, when it has an owner, the owner's too; sorted, each once. */
  private[query] def candidates(root: Root, searches: Vector[Search]): Vector[os.RelPath] = {
    val holding = Locate.mentioningEach(root, searches.flatMap(s => s.name +: s.owner.toVector))
    def files(word: String): Vector[os.RelPath] = holding.getOrElse(word, Vector.empty)
    searches
      .flatMap { s =>
        s.owner.fold(files(s.name)) { owner =>
          val naming = files(owner).toSet
          files(s.name).filter(naming.contains)
        }
      }
      .distinct
      .sortBy(_.toString)
  }

  /** The references in `classes`' `.tasty` to `names` (full names), read through the root's cache from the source files `candidates` gives for `searches` that satisfy `keep`; with them, those files whose package has no `.tasty` in `classes`, and the cache after. The references are `Left` when their index cannot be read. */
  private def referencesIn(
      root: Root,
      layout: Layout,
      config: Config,
      classes: Vector[os.Path],
      names: Set[String],
      searches: Vector[Search],
      keep: os.RelPath => Boolean,
      in: Loaded
  ): (Either[String, Vector[Use]], Vector[os.RelPath], Loaded) = {
    val kept = candidates(root, searches).filter(keep)
    val inPackage = kept.map(file => file -> Locate.inPackageIn(root, classes, file))
    val tasty = inPackage.flatMap(_._2).distinct
    val unsearched = inPackage.collect {
      case (file, found)
          if found.isEmpty && !config.hidden.exists(prefix => file.toString.startsWith(prefix)) =>
        file
    }
    if (names.isEmpty || tasty.isEmpty) (Right(Vector.empty), unsearched, in)
    else
      Loaded.uses(root, layout, tasty, in) match {
        case Left(message) => (Left(message), unsearched, in)
        case Right((refs, next, _)) => (Right(Read.targeting(refs, names)), unsearched, next)
      }
  }

  /** The tests in test sources that hold a reference to `tops` or to one of their members, by file: each test by its line range, or a suite's helper where a reference lies outside every test. Notes name a tasty that could not be read; the cache after. */
  private def exercisedBy(
      root: Root,
      layout: Layout,
      config: Config,
      tops: Vector[Defn],
      seen: Vector[Defn],
      in: Loaded
  ): (Vector[Render.Exercised], Vector[String], Loaded) = {
    val testClasses = layout.classesDirs(root).filter(layout.isTest)
    val names = tops.flatMap(d => d.fullName +: d.members.map(_.fullName)).toSet
    val searches = searchesFor(tops.flatMap(d => d +: d.members), seen ++ tops)
    if (testClasses.isEmpty || names.isEmpty) (Vector.empty, Vector.empty, in)
    else {
      val (refs, _, found) =
        referencesIn(root, layout, config, testClasses, names, searches, layout.isTestSource, in)
      refs match {
        case Left(message) => (Vector.empty, Vector(message), found)
        case Right(sites) =>
          val fileNames = sites.map(_.file).distinct
          val tasty =
            fileNames
              .flatMap(file => Locate.inPackageIn(root, testClasses, os.RelPath(file)))
              .distinct
          Loaded.defns(root, layout, tasty, found) match {
            case Left(message) => (Vector.empty, Vector(message), found)
            case Right((defns, next, _)) =>
              val files = fileNames.map { file =>
                exercisedIn(root, config, file, defns, sites.filter(_.file == file))
              }
              (files.filter(_.entries.nonEmpty), Vector.empty, next)
          }
      }
    }
  }

  /** One file's entries for the sites in it: each site's test by line range, else the suite's helper it lies in, sorted and named once. Reads the file's source; `defns` are its tasty's definitions. */
  private def exercisedIn(
      root: Root,
      config: Config,
      file: String,
      defns: Vector[Defn],
      sites: Vector[Use]
  ): Render.Exercised = {
    val src = os.read(root.dir / os.RelPath(file))
    val suites =
      defns.filter(d => d.file == file && (d.kind == Kind.Class || d.kind == Kind.Object))
    val entries = sites.flatMap { u =>
      suites.find(s => s.lines.start <= u.line && u.line <= s.lines.end).map { suite =>
        val (from, to) = TestCalls.within(src, suite.lines)
        TestCalls
          .find(src, from, to, config.testCall)
          .find(t => t.lines.start <= u.line && u.line <= t.lines.end) match {
          case Some(t) => Render.Exercise(t.lines, s"${suite.name}: ${t.name}")
          case None =>
            suite.members.find(m => m.lines.start <= u.line && u.line <= m.lines.end) match {
              case Some(m) => Render.Exercise(m.lines, s"${suite.name}: helper ${m.name}")
              case None =>
                val enclosing = u.enclosing.split('.').last
                Render.Exercise(Lines(u.line, u.line), s"${suite.name}: helper $enclosing")
            }
        }
      }
    }
    Render.Exercised(
      file,
      suites.map(_.fullName).distinct,
      entries.distinct.sortBy(e => (e.lines.start, e.lines.end, e.label))
    )
  }

  /** The note for each `--body` name that no shown definition has: `tops` and their members (those withPrivate
    * or public), whose simple names it lists, sorted; a name only a private member has is named as private.
    */
  private def bodyMisses(
      bodies: Set[String],
      tops: Vector[Defn],
      withPrivate: Boolean
  ): Vector[String] = {
    val shown = tops.flatMap(d => d +: d.members.filter(m => withPrivate || !m.isPrivate))
    val names = shown.map(_.name).distinct.sorted
    val privateNames = tops.flatMap(_.members.filter(_.isPrivate)).map(_.name).toSet
    bodies.toVector.sorted
      .filterNot(name => names.contains(name))
      .map { name =>
        if (privateNames.contains(name)) s"-- --body $name is private: add --private"
        else s"-- --body $name matches nothing shown; names here: ${names.mkString(", ")}"
      }
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
      cap: Int,
      withTests: Boolean
  ): (Answer, Loaded) = {
    val classes = layout.classesDirs(root)
    if (depth < 0 || depth > 2)
      (Answer(s"depth must be 0, 1 or 2\n${Help.usage(Help.show)}", Status.Failed), in)
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
      // One note per distinct suite: the outermost top-level definition of the match's file that holds it.
      def testSuiteNotes(sym: String, found: Resolved): Vector[String] =
        found.defns
          .map(m =>
            found.seen
              .find(t =>
                t.file == m.file && t.lines.start <= m.lines.start && m.lines.end <= t.lines.end
              )
              .getOrElse(m)
              .fullName
          )
          .distinct
          .sorted
          .map(suite => s"-- $sym is in test sources: tests $suite")
      syms.foreach { sym =>
        Resolve.resolve(root, layout, config, Scope.Main, sym, state) match {
          case Left(message) => failure = Some(message)
          // A miss pays for one more resolve, over the test-only classes dirs; a name found there is named with its suite instead.
          case Right(resolved) if resolved.defns.isEmpty =>
            state = resolved.loaded
            companions ++= resolved.seen
            Resolve.resolve(root, layout, config, Scope.WithTests, sym, state) match {
              case Left(message) => failure = Some(message)
              case Right(tests) =>
                state = tests.loaded
                if (tests.defns.isEmpty) notes ++= resolved.notes
                else notes ++= testSuiteNotes(sym, tests)
            }
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
          val (exercised, testNotes, afterTests) =
            if (withTests) exercisedBy(root, layout, config, tops, companions.toVector, state)
            else (Vector.empty, Vector.empty, state)
          state = afterTests
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
            cap = cap,
            exercising = Render.exercising(syms.mkString(","), exercised)
          )
          val bodyNotes =
            (bodyMisses(bodies, tops, withPrivate) ++ testNotes).map(_ + "\n").mkString
          val text = noteText + bodyNotes + rendered
          (Answer(text, Status.Found), state)
      }
    }
  }
}
