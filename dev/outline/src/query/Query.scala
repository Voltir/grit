package grit.outline.query

import scala.collection.mutable

import grit.outline.locate.{Layout, Locate, Root, SourceWords, TastySource, WordGlob}
import grit.outline.model.Lines
import grit.outline.model.{Defn, Kind, Staleness, TestCase, Use}
import grit.outline.read.Read
import grit.outline.render.{Family, Render, TestsShown, TestsSummary}
import grit.outline.trace.Trace

enum Status {
  case Found, NoMatch, Failed
}

final case class Answer(text: String, status: Status)

object Query {

  /** The `show` answer for `syms` (each `Name`, `Name.member` or fully qualified), read through `root`'s own cache in `roots`, which then holds `root` as most recently used. Every answer starts with `root`'s `## root` line. A name main sources lack is looked up in test sources and shown from there, under a `-- in test sources: <full name>` note, with no tests line. */
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
      tests: TestsShown = TestsShown.Summary,
      fullDocs: Boolean = false
  ): (Answer, Roots) = staled(root, layout, config, roots) { roots =>
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
        tests,
        fullDocs
      )
    (answer.copy(text = s"${header(root)}\n${answer.text}"), Roots.put(roots, root, loaded))
  }

  /** The `find` answer for `pattern`: every definition in main sources, members included, whose full name ends in segments matching `pattern`'s dot-separated segments, each matched whole, with `*` standing for any run of characters within one segment (`*Shelf*`, `Corp*`, `*.placed`, `Moves.a*`). `config.hidden` sources are left out, and so are private members and what they hold unless `withPrivate`; without it, a note counts the private definitions matching when main sources hold no other match. One line per match, `<full name>  <file>:<lines>  <collapsed signature>`, sorted by full name, under `root`'s `## root` line. When no main definition matches, the test sources' matches, under a note saying so; when none matches there either, the definitions of each target a renaming import gives a name matching the last segment (`renamingImports`), under a note naming each import, unless the pattern's segments before its last match a method whose body holds a word matching the last segment, when a note per such method says so in place of them. `NoMatch` when nothing matches, `Failed` for a pattern with an empty segment or when the layout is not compiled; the method and import notes and the matches are cut at `cap` bytes, between lines. Reads, into `root`'s cache in `roots`, the `.tasty` of each package whose sources hold a word matching `pattern`'s last segment: every package for a last segment of `*` alone. */
  def find(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      pattern: String,
      withPrivate: Boolean,
      cap: Int
  ): (Answer, Roots) = staled(root, layout, config, roots) { roots =>
    val segments = pattern.split("\\.", -1).toVector
    val classes = layout.classesDirs(root)
    if (segments.exists(_.isEmpty))
      (
        Answer(
          s"${header(root)}\n$pattern has an empty segment\n${Help.usage(Help.find)}",
          Status.Failed
        ),
        roots
      )
    else if (classes.isEmpty) (Answer(layout.notCompiled(root), Status.Failed), roots)
    else {
      def hidden(d: Defn): Boolean = config.hidden.exists(prefix => d.file.startsWith(prefix))
      def visible(ds: Vector[Defn]): Vector[Defn] =
        ds.filter(d => withPrivate || !d.isPrivate).flatMap(d => d +: visible(d.members))

      // The visible definitions in `scope` whose names match `globs`, read from the packages of `files`, and how many
      // definitions left out as private, or inside a private one, match them too.
      def matching(
          globs: Vector[scala.util.matching.Regex],
          scope: Scope,
          files: Vector[os.RelPath],
          in: Loaded
      ): (Vector[Defn], Loaded, Vector[String], Int) = {
        def matches(d: Defn): Boolean = {
          val names = d.fullName.split('.').toVector
          names.size >= globs.size &&
          names.takeRight(globs.size).zip(globs).forall { case (n, g) => g.matches(n) }
        }
        val tasty = Resolve.usable(
          root,
          layout,
          scope,
          files.flatMap(Locate.inPackagesIn(root, classes)).distinct
        )
        val (defns, loaded, notes) =
          tasty
            .grouped(Loaded.readBatch)
            .foldLeft((Vector.empty[Defn], in, Vector.empty[String])) {
              case ((ds, state, ns), batch) =>
                val (more, next, moreNotes) = loadAll(root, layout, batch.toVector, state)
                (ds ++ more, next, ns ++ moreNotes)
            }
        val found = visible(defns)
          .filter(d => matches(d) && !hidden(d))
          .distinct
          .sortBy(d => (d.fullName, d.file, d.lines.start))
        val all = everyDefnOf(defns).filter(d => matches(d) && !hidden(d)).distinct
        (found, loaded, notes, all.size - found.size)
      }

      // A definition's name is a word of its source, so only the packages holding one are read.
      val word = WordGlob(segments.last)
      val files = Locate.holding(root, word.foundIn)
      val (inMain, afterMain, mainNotes, mainPrivate) =
        matching(segments.map(segmentGlob), Scope.Main, files, Roots.of(roots, root))
      def privateNote(n: Int): Vector[String] =
        if (n == 0) Vector.empty
        else Vector(s"-- $n private match${if (n == 1) "" else "es"}: add --private")
      val (found, loaded, notes, lead, listed) =
        if (inMain.nonEmpty)
          (inMain, afterMain, mainNotes, Vector.empty[String], Vector.empty[String])
        else {
          val (inTests, afterTests, testNotes, testPrivate) =
            matching(segments.map(segmentGlob), Scope.WithTests, files, afterMain)
          if (inTests.nonEmpty)
            (
              inTests,
              afterTests,
              mainNotes ++ testNotes,
              privateNote(mainPrivate) ++
                Vector(s"-- no definition in main sources matches $pattern; in test sources:"),
              Vector.empty[String]
            )
          else {
            // A pattern under a method names its locals, which no .tasty lists as definitions; a local is a word of its
            // method's body, so only a method whose body holds one is named, and an owner glob of `*` names no other.
            val (methods, afterMethods, _, _) =
              if (segments.size < 2) (Vector.empty, afterTests, Vector.empty, 0)
              else
                matching(segments.dropRight(1).map(segmentGlob), Scope.WithTests, files, afterTests)
            val defs = methods.filter(d => d.kind == Kind.Def && d.body.exists(word.foundIn))
            val renamings =
              if (defs.nonEmpty) Vector.empty else renamingImports(root, files, word)
            val (targets, afterTargets, targetNotes) =
              renamings
                .map(_.target)
                .distinct
                .foldLeft((Vector.empty[Defn], afterMethods, testNotes)) {
                  case ((ds, state, ns), target) =>
                    val exact = target.split('.').toVector.map(java.util.regex.Pattern.quote(_).r)
                    val (more, next, moreNotes, _) = matching(
                      exact,
                      Scope.WithTests,
                      Locate.holding(root, WordGlob(target.split('.').last).foundIn),
                      state
                    )
                    (ds ++ more, next, ns ++ moreNotes)
                }
            (
              targets.distinct,
              afterTargets,
              mainNotes ++ targetNotes,
              privateNote(testPrivate),
              defs.map(_.fullName).distinct.map { m =>
                val owner = m.split('.').dropRight(1).mkString(".")
                val name = m.split('.').lastOption.getOrElse(m)
                s"-- $m is a def: find lists no definition local to a method; show $owner --body $name --private prints its body"
              } ++ renamings.map(r =>
                s"-- ${r.alias} is a renaming import of ${r.target} at ${r.file}:${r.line}"
              )
            )
          }
        }
      val head = (header(root) +: (notes ++ lead)).mkString("\n")
      val answer =
        if (found.isEmpty)
          Answer(
            s"${Render.linesUnder(listed, head, cap)}no definition matches $pattern",
            Status.NoMatch
          )
        else Answer(Render.find(listed, found, head, cap), Status.Found)
      (answer, Roots.put(roots, root, loaded))
    }
  }

  /** An import that renames `target` (a full name) to `alias`, at `file`'s `line`. */
  private final case class Renaming(alias: String, target: String, file: os.RelPath, line: Int)

  /** The renaming imports in `files` whose new name is a whole match of `word`: `import a.b.X as Y`, and `X as Y` or
    * `X => Y` among an import's braced selectors, the braces spanning lines or not, each at the line its selector starts
    * on. Read from source text, so the target is the path as written, not resolved.
    */
  private def renamingImports(
      root: Root,
      files: Vector[os.RelPath],
      word: WordGlob
  ): Vector[Renaming] = {
    val importStart = raw"^\s*import\s+(.*)$$".r
    val renamed = raw"^\s*([\w$$]+)\s+as\s+([\w$$]+)\s*$$".r
    val arrowed = raw"^\s*([\w$$]+)\s*=>\s*([\w$$]+)\s*$$".r
    val plain = raw"^\s*([\w$$.]+)\.([\w$$]+)\s+as\s+([\w$$]+)\s*$$".r
    // `text` split at the commas outside braces, each part with its offset in `text`.
    def parts(text: String, from: Int): Vector[(String, Int)] = {
      val cuts = text.indices
        .foldLeft((0, Vector.empty[Int])) { case ((depth, at), i) =>
          text.charAt(i) match {
            case '{' => (depth + 1, at)
            case '}' => (depth - 1, at)
            case ',' if depth == 0 => (depth, at :+ i)
            case _ => (depth, at)
          }
        }
        ._2
      val bounds = (-1 +: cuts) :+ text.length
      bounds.zip(bounds.drop(1)).map { case (a, b) => (text.substring(a + 1, b), from + a + 1) }
    }
    files.flatMap { file =>
      val path = root.dir / file
      val lines = if (os.isFile(path)) os.read(path).split("\n", -1).toVector else Vector.empty
      lines.zipWithIndex.flatMap {
        case (importStart(first), index) =>
          // The statement runs on while its braces are open.
          def open(t: String): Boolean = t.count(_ == '{') > t.count(_ == '}')
          val more =
            lines.iterator.drop(index + 1).scanLeft(first)(_ + "\n" + _).takeWhile(open).size
          val clause = (first +: lines.slice(index + 1, index + 1 + more)).mkString("\n")
          def lineAt(offset: Int): Int = index + 1 + clause.take(offset).count(_ == '\n')
          parts(clause, 0)
            .flatMap { case (part, at) =>
              val brace = part.indexOf('{')
              if (brace < 0) part match {
                case plain(prefix, name, alias) =>
                  Vector(Renaming(alias, s"$prefix.$name", file, lineAt(at)))
                case _ => Vector.empty
              }
              else {
                val prefix = part.take(brace).trim.stripSuffix(".")
                val inside = part.drop(brace + 1).takeWhile(_ != '}')
                parts(inside, at + brace + 1).flatMap { case (selector, from) =>
                  val start = from + selector.indexWhere(!_.isWhitespace).max(0)
                  selector match {
                    case renamed(name, alias) =>
                      Vector(Renaming(alias, s"$prefix.$name", file, lineAt(start)))
                    case arrowed(name, alias) =>
                      Vector(Renaming(alias, s"$prefix.$name", file, lineAt(start)))
                    case _ => Vector.empty
                  }
                }
              }
            }
            .filter(r => word.matches(r.alias) && !r.target.endsWith("." + r.alias))
        case _ => Vector.empty
      }
    }
  }

  /** `segment` as a regular expression matching a whole name segment, each `*` any run of characters. */
  private def segmentGlob(segment: String): scala.util.matching.Regex =
    segment.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*").r

  /** The shortest name `uses` searches for without a qualifier: a shorter bare name (`of`, `get`) is in so many files that a search for it reads most of the project. */
  val shortestBare: Int = 4

  /** The `uses` answer: the direct references to `syms`, grouped by file and owner (a site inside a test call in a test source is owned by its test), filtered by `in` and `outside` path prefixes. A name `X.apply` or `X.new` that resolves to nothing, where `X` resolves to a class or an enum case, stands for `X`'s construction sites: calls to its synthetic `apply` and its `new`s, under a note naming it. A name `X.unapply` that resolves to nothing, where `X` resolves to a class, an enum case or a trait, stands for `X`'s pattern sites: `case X(…)` for a case class or an enum case, and `case _: X`, under a note naming it. Where `X` resolves to something else, the answer is `NoMatch` under one note naming what `X` is. A name with no qualifier shorter than `shortestBare` refuses the whole call, `NoMatch`, reading nothing: its note says to qualify the name with its owner and names the owners of that name among the definitions `roots` already holds for `root`, at most 10. */
  def uses(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      syms: Vector[String],
      in: Option[String],
      outside: Option[String],
      cap: Int,
      allSites: Boolean = false
  ): (Answer, Roots) = staled(root, layout, config, roots) { roots =>
    val bare = syms.filter(sym => !sym.contains('.') && sym.length < shortestBare)
    lazy val classes = layout.classesDirs(root)
    if (bare.nonEmpty)
      (Answer(s"${header(root)}\n${refusal(bare, Roots.of(roots, root))}", Status.NoMatch), roots)
    else if (classes.isEmpty) (Answer(layout.notCompiled(root), Status.Failed), roots)
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
      // The classes whose construction sites a name like `X.apply` asks for, when `X` has no apply of its own in source.
      val constructed = mutable.ListBuffer.empty[Defn]
      // The classes whose pattern sites a name like `X.unapply` asks for, when `X` has no unapply of its own in source.
      val matched = mutable.ListBuffer.empty[Defn]
      syms.foreach { sym =>
        Resolve.resolve(root, layout, config, Scope.WithTests, sym, state) match {
          case Left(message) => failure = Some(message)
          case Right(resolved) if resolved.defns.isEmpty && sitesOf(sym).nonEmpty =>
            val (owner, wanted) = sitesOf(sym).getOrElse((sym, Sites.Built))
            Resolve.resolve(root, layout, config, Scope.WithTests, owner, resolved.loaded) match {
              case Left(message) => failure = Some(message)
              case Right(classes) =>
                state = classes.loaded
                val kinds = wanted match {
                  case Sites.Built => Set(Kind.CaseClass, Kind.Class, Kind.EnumCase)
                  case Sites.Matched => Set(Kind.CaseClass, Kind.Class, Kind.EnumCase, Kind.Trait)
                }
                val sited = classes.defns.filter(d => kinds.contains(d.kind))
                seen ++= resolved.seen ++ classes.seen
                if (classes.defns.isEmpty) notes ++= resolved.notes
                else if (sited.isEmpty)
                  notes ++= classes.defns.map(d =>
                    s"-- $sym: ${d.fullName} is ${article(kindName(d.kind))}; X.apply and X.unapply find a class's or an enum case's construction and pattern sites"
                  )
                else
                  wanted match {
                    case Sites.Built =>
                      constructed ++= sited
                      notes ++= sited.map(c =>
                        s"-- $sym is ${c.fullName}'s constructor: its apply and new sites"
                      )
                    case Sites.Matched =>
                      matched ++= sited
                      notes ++= sited.map { c =>
                        val extractor =
                          if (c.kind == Kind.CaseClass || c.kind == Kind.EnumCase)
                            s"case ${c.name}(…) and "
                          else ""
                        s"-- $sym is ${c.fullName}'s pattern sites: ${extractor}case _: ${c.name}"
                      }
                  }
            }
          case Right(resolved) =>
            state = resolved.loaded
            targets ++= resolved.defns
            seen ++= resolved.seen
            notes ++= resolved.notes
        }
      }

      val names = (targets.toVector.map(_.fullName) ++ constructed.toVector.flatMap(c =>
        Vector(s"${c.fullName}.apply", s"${c.fullName}.<init>")
      ) ++ matched.toVector.map(c => s"${c.fullName}.unapply")).distinct.toSet
      val (read, unsearched, next) = referencesIn(
        root,
        layout,
        config,
        classes,
        if (failure.nonEmpty) Set.empty else names,
        // With no target, the search by each name's last segment still names the files not compiled.
        if (targets.isEmpty && constructed.isEmpty && matched.isEmpty)
          syms.map(sym => Search(sym.split('.').last, None)).distinct
        else
          searchesFor(targets.toVector, seen.toVector) ++
            (constructed ++ matched).toVector.map(c => Search(c.name, None)).distinct,
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
                val holding = testsHolding(root, layout, config, kept)
                Answer(
                  Render.uses(
                    kept,
                    overloaded,
                    cap,
                    holding.view.mapValues(_.name).toMap,
                    staleFiles(root, state, kept.map(_.file)),
                    ownerSizes(kept, holding, state),
                    allSites
                  ),
                  Status.Found
                )
              }
          }
      }
      val noteText = (notes.toVector ++ notSearched(unsearched)).map(_ + "\n").mkString
      (answer.copy(text = s"${header(root)}\n$noteText${answer.text}"), nextRoots)
    }
  }

  /** The innermost test call holding each of `sites` that lies in a test source, read from its file's source text. */
  private def testsHolding(
      root: Root,
      layout: Layout,
      config: Config,
      sites: Vector[Use]
  ): Map[Use, TestCase] =
    sites
      .groupBy(_.file)
      .toVector
      .flatMap { case (file, here) =>
        val path = root.dir / os.RelPath(file)
        if (!layout.isTestSource(os.RelPath(file)) || !os.isFile(path)) Vector.empty
        else {
          val src = os.read(path)
          val calls = TestCalls.find(src, 0, src.length, config.testCall)
          here.flatMap { u =>
            calls
              .filter(t => t.lines.start <= u.line && u.line <= t.lines.end)
              .maxByOption(_.lines.start)
              .map(t => u -> t)
          }
        }
      }
      .toMap

  /** The size in bytes of what owns each of `sites`: the test in `tests` holding it, else the innermost definition `loaded`
    * holds in its file under its enclosing name whose lines span it; a site with neither has none.
    */
  private def ownerSizes(
      sites: Vector[Use],
      tests: Map[Use, TestCase],
      loaded: Loaded
  ): Map[Use, Int] = {
    val files = sites.map(_.file).toSet
    val defns = everyDefnOf(
      loaded.byTasty.values.toVector.flatMap(_.defns).filter(d => files.contains(d.file))
    ).groupBy(d => (d.file, d.fullName))
    sites.flatMap { u =>
      val size = tests.get(u) match {
        case Some(t) => Some(t.text.getBytes(java.nio.charset.StandardCharsets.UTF_8).length)
        case None =>
          defns
            .getOrElse((u.file, u.enclosing), Vector.empty)
            .filter(d => d.lines.start <= u.line && u.line <= d.lines.end)
            .minByOption(d => d.lines.end - d.lines.start)
            .flatMap(_.size)
      }
      size.map(u -> _)
    }.toMap
  }

  /** What a name `X.apply` or `X.new` (construction) or `X.unapply` (patterns) asks for of a class `X`. */
  private enum Sites {
    case Built, Matched
  }

  /** The class `sym` names the construction or the pattern sites of, as `X.apply`, `X.new` or `X.unapply`: `X` and which,
    * or none for any other name.
    */
  private def sitesOf(sym: String): Option[(String, Sites)] =
    Vector(".apply" -> Sites.Built, ".new" -> Sites.Built, ".unapply" -> Sites.Matched)
      .collectFirst {
        case (suffix, sites) if sym.endsWith(suffix) && sym.length > suffix.length =>
          (sym.dropRight(suffix.length), sites)
      }

  private def kindName(k: Kind): String = k match {
    case Kind.CaseClass => "case class"
    case Kind.EnumCase => "enum case"
    case Kind.TypeAlias => "type alias"
    case Kind.Opaque => "opaque type"
    case other => other.toString.toLowerCase
  }

  private def article(word: String): String =
    if (word.headOption.exists("aeiou".contains(_))) s"an $word" else s"a $word"

  /** The `area` answer for the named areas `names`, declared in `config`; with no names, one line per declared area, its name and its purpose. For names: at level 0 one line per package, at level 1 each definition with its doc's first sentence, grouped by file; `Failed` for a level outside 0..1; cut at `cap` bytes. A name no area declares stands for what `standFor` gives, under its note, and is `NoMatch` when that is `Left`. */
  def area(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      names: Vector[String],
      level: Int,
      cap: Int
  ): (Answer, Roots) = staled(root, layout, config, roots) { roots =>
    if (level < 0 || level > 1)
      (Answer(s"level must be 0 or 1\n${Help.usage(Help.area)}", Status.Failed), roots)
    else if (names.isEmpty) {
      val listed = config.areas.map(a => a.purpose.fold(a.name)(p => s"${a.name}  $p"))
      if (listed.isEmpty)
        (Answer(s"${header(root)}\nno area declared in .outline.conf", Status.NoMatch), roots)
      else (Answer((header(root) +: listed).mkString("\n"), Status.Found), roots)
    } else {
      val classes = layout.classesDirs(root)
      val stands = names.distinct.map(n => standFor(layout, config, classes, n))
      stands.collectFirst { case Left(message) => message } match {
        case Some(message) => (Answer(s"${header(root)}\n$message", Status.NoMatch), roots)
        case None if classes.isEmpty =>
          (Answer(layout.notCompiled(root), Status.Failed), roots)
        case None =>
          val chosen = stands.collect { case Right(stand) => stand }
          val selected = chosen.flatMap(_.area).distinct
          // The cache read in this query, threaded through a scoped local, as `showIn` does.
          var state = Roots.of(roots, root)
          val listed = mutable.ListBuffer.empty[Render.Listed]
          val notes = mutable.ListBuffer.empty[String]
          notes ++= chosen.flatMap(_.note)
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
          val packaged = chosen.flatMap(_.tasty).distinct
          if (failure.isEmpty && packaged.nonEmpty) {
            val (defns, next, loadNotes) = loadAll(root, layout, packaged, state)
            state = next
            notes ++= loadNotes
            listed ++= defns
              .filter(d =>
                !d.isPrivate && !config.hidden.exists(prefix => d.file.startsWith(prefix))
              )
              .map(d => Render.Listed(d, ""))
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
  }

  /** What an `area` name stands for: a declared area, or the `.tasty` files of whole packages, with the note saying which when the name is not declared. */
  private final case class Stand(area: Option[Area], tasty: Vector[os.Path], note: Option[String])

  /** What `name` stands for: the area `config` declares by that name; else the one area with a symbol or family under a package segment equal to `name`, ignoring case; else the packages in `classes` holding only main classes with such a segment, each package's `.tasty` directly in it. `Left` is the NoMatch message: several areas naming it, or nothing at all. */
  private def standFor(
      layout: Layout,
      config: Config,
      classes: Vector[os.Path],
      name: String
  ): Either[String, Stand] =
    config.areas.find(_.name == name) match {
      case Some(declared) => Right(Stand(Some(declared), Vector.empty, None))
      case None =>
        def underIt(full: String): Boolean =
          full.split('.').toVector.dropRight(1).exists(_.equalsIgnoreCase(name))
        val naming = config.areas.filter(a => (a.symbols ++ a.families).exists(underIt))
        if (naming.size == 1)
          Right(
            Stand(
              naming.headOption,
              Vector.empty,
              naming.headOption.map(a => s"-- no area $name; area ${a.name} names $name")
            )
          )
        else if (naming.size > 1)
          Left(s"no area $name; areas naming $name: ${naming.map(_.name).mkString(", ")}")
        else {
          val packages = classes.filterNot(layout.isTest).flatMap { dir =>
            os.walk(dir)
              .filter(os.isDir)
              .filter(_.relativeTo(dir).segments.exists(_.equalsIgnoreCase(name)))
              .flatMap { pkg =>
                val tasty = os.list(pkg).filter(_.ext == "tasty")
                if (tasty.isEmpty) None
                else Some(pkg.relativeTo(dir).segments.mkString(".") -> tasty.toVector)
              }
          }
          if (packages.isEmpty)
            Left(s"no area $name; declared: ${config.areas.map(_.name).mkString(", ")}")
          else
            Right(
              Stand(
                None,
                packages.flatMap(_._2).sorted,
                Some(
                  s"-- no area $name declared; packages: ${packages.map(_._1).distinct.sorted.mkString(", ")}"
                )
              )
            )
        }
    }

  /** The files among `files` that `Locate.staleness` judges `Stale` against the `.tasty` files of `loaded.consulted` they
    * were read from: what every query's file tags and its stale line say. A file no consulted `.tasty` was read from is
    * not stale.
    */
  private[query] def staleFiles(root: Root, loaded: Loaded, files: Vector[String]): Set[String] = {
    val distinct = files.distinct
    val wanted = distinct.toSet
    val readFrom: Map[String, Vector[os.Path]] = loaded.consulted.toVector
      .flatMap(p => loaded.sourceOf(p).filter(wanted.contains).map(_ -> p))
      .groupMap(_._1)(_._2)
    val judged = Locate.staleness(
      root,
      distinct.map(file => os.RelPath(file) -> readFrom.getOrElse(file, Vector.empty))
    )
    distinct
      .zip(judged)
      .collect { case (file, Staleness.Stale(_, _)) => file }
      .toSet
  }

  /** The `file` answer for `target`, a repo-relative path, a unique path suffix or a definition's name: the file's loose
    * lines (outside every top-level definition) as file text, each top-level definition and its members as a header line,
    * and its end lines. `NoMatch` when nothing or several files match, naming them; `Failed` when the file has no `.tasty`;
    * cut at `cap` bytes.
    */
  def file(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      target: String,
      cap: Int
  ): (Answer, Roots) = staled(root, layout, config, roots) { roots =>
    val classes = layout.classesDirs(root)
    if (classes.isEmpty) (Answer(layout.notCompiled(root), Status.Failed), roots)
    else {
      val start = Roots.of(roots, root)
      // A path names its file; a name is resolved as show resolves it, main sources first, then test sources.
      val found: Either[String, (Vector[String], Vector[String], Loaded)] =
        if (target.contains('/') || target.endsWith(".scala"))
          Right((Locate.endingWith(root, target).map(_.toString), Vector.empty, start))
        else
          Resolve
            .resolve(root, layout, config, Scope.Main, target, start)
            .flatMap { inMain =>
              if (inMain.defns.nonEmpty) Right(inMain)
              else Resolve.resolve(root, layout, config, Scope.WithTests, target, inMain.loaded)
            }
            .map(r => (r.defns.map(_.file).distinct, r.notes, r.loaded))
      found match {
        case Left(message) => (Answer(message, Status.Failed), roots)
        case Right((files, notes, loaded)) =>
          val head = (Vector(header(root)) ++ notes).mkString("\n")
          files match {
            case Vector(file) =>
              val rel = os.RelPath(file)
              val tasty = recording(root, classes, rel)
              if (tasty.isEmpty) {
                val how = compiling(root, layout, classes, Vector(rel))
                (
                  Answer(s"$head\nno .tasty records $file: compile it first: $how", Status.Failed),
                  Roots.put(roots, root, loaded)
                )
              } else
                Loaded.defns(root, layout, tasty, loaded) match {
                  case Left(message) =>
                    (Answer(message, Status.Failed), Roots.put(roots, root, loaded))
                  case Right((defns, next, _)) =>
                    val src = os.read(root.dir / rel)
                    val stale = staleFiles(root, next, Vector(file)).nonEmpty
                    (
                      Answer(
                        s"$head\n${Render.file(file, defns.filter(_.file == file), src, stale, cap)}",
                        Status.Found
                      ),
                      Roots.put(roots, root, next)
                    )
                }
            case Vector() =>
              (
                Answer(s"$head\nno file matches $target", Status.NoMatch),
                Roots.put(roots, root, loaded)
              )
            case several =>
              (
                Answer(
                  s"$head\n-- $target matches ${several.size} files; name one: ${several.mkString(", ")}",
                  Status.NoMatch
                ),
                Roots.put(roots, root, loaded)
              )
          }
      }
    }
  }

  /** The `.tasty` files in `classes` compiled from the source `file`. */
  private def recording(root: Root, classes: Vector[os.Path], file: os.RelPath): Vector[os.Path] =
    Locate.inPackageIn(root, classes, file).filter(t => TastySource.of(t).exists(_.isOf(file)))

  /** The command compiling the modules whose classes hold `.tasty` files of `files`' packages, or `layout`'s line for an
    * uncompiled root when none does.
    */
  private def compiling(
      root: Root,
      layout: Layout,
      classes: Vector[os.Path],
      files: Vector[os.RelPath]
  ): String = {
    val modules = files
      .flatMap(Locate.inPackagesIn(root, classes))
      .flatMap(t => classes.find(t.startsWith))
      .distinct
      .map(layout.moduleOf(root, _))
      .distinct
    if (modules.isEmpty) layout.notCompiled(root) else layout.compile(modules)
  }

  /** For a class or object `name` no `.tasty` holds: the note naming the sources whose text defines it (in its package,
    * when `name` is a full name) and no `.tasty` records, with the command compiling their modules; none when there are none.
    */
  private def uncompiled(root: Root, layout: Layout, name: String): Option[String] = {
    val classes = layout.classesDirs(root)
    val simple = name.split('.').last
    val pkg = name.split('.').dropRight(1).mkString(".")
    val defines = ("\\b(?:object|class)\\s+" + java.util.regex.Pattern.quote(simple) + "\\b").r
    val files = Locate
      .holding(root, text => text.contains(simple) && defines.findFirstIn(text).isDefined)
      .filter(f =>
        (pkg.isEmpty || Locate.packageIn(root, f) == pkg) && recording(root, classes, f).isEmpty
      )
    Option.when(files.nonEmpty) {
      val (define, those) =
        if (files.size == 1) ("defines", "that file") else ("define", "those files")
      s"no class or object $simple compiled: ${files.mkString(", ")} $define it, but no .tasty records $those, so its " +
        s"last compile failed or never ran; compile it first: ${compiling(root, layout, classes, files)}"
    }
  }

  /** The `tests` answer for the suite `suite`, named as `show` names a symbol: its helpers (the members whose range holds no test call, private ones included, and the other top-level definitions of its file beside it, by line) and its tests with their line ranges, read from the file the suite is defined in. `test` is a prefix whose tests print their verbatim bodies, or, when it starts no test as written, comma-separated prefixes. `NoMatch` when no class or object is named `suite`; `Failed` when the layout is not compiled, or when the only sources defining a class or object `suite` are ones no `.tasty` records, naming them and the command compiling their modules; cut at `cap` bytes. */
  def tests(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      suite: String,
      test: Option[String],
      bodies: Set[String],
      cap: Int,
      fixtures: Boolean = false
  ): (Answer, Roots) = staled(root, layout, config, roots) { roots =>
    if (layout.classesDirs(root).isEmpty)
      (Answer(layout.notCompiled(root), Status.Failed), roots)
    else
      Resolve.resolve(root, layout, config, Scope.WithTests, suite, Roots.of(roots, root)) match {
        case Left(message) => (Answer(message, Status.Failed), roots)
        case Right(resolved) =>
          val nextRoots = Roots.put(roots, root, resolved.loaded)
          def isSuite(d: Defn): Boolean = d.kind == Kind.Class || d.kind == Kind.Object
          val ambiguous = resolved.defns.filter(isSuite).map(_.fullName).distinct.sizeIs > 1
          resolved.defns.find(isSuite) match {
            case None =>
              uncompiled(root, layout, suite) match {
                case Some(note) =>
                  (Answer(s"${header(root)}\n$note", Status.Failed), nextRoots)
                case None =>
                  (Answer(s"${header(root)}\nno class or object $suite", Status.NoMatch), nextRoots)
              }
            case Some(_) if ambiguous =>
              (
                Answer((Vector(header(root)) ++ resolved.notes).mkString("\n"), Status.NoMatch),
                nextRoots
              )
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
                val (used, afterFixtures) = fixturesOf(root, layout, d, resolved.loaded)
                // A value that starts no test as written is comma-separated prefixes.
                val prefixes = test.toVector.flatMap { t =>
                  if (tests.exists(_.name.startsWith(t))) Vector(t)
                  else t.split(',').toVector.map(_.trim).filter(_.nonEmpty)
                }
                val notes = prefixes
                  .filterNot(p => tests.exists(_.name.startsWith(p)))
                  .map(p => "-- no test starting \"" + p + "\"")
                val helperNames = helpers.map(_.name).distinct.sorted
                val fixtureNames = used.flatMap(_.used.map(_.name)).distinct.sorted
                val bodyNotes = bodies.toVector.sorted
                  .filterNot(b => helperNames.contains(b) || fixtureNames.contains(b))
                  .map(b =>
                    s"-- --body $b matches nothing shown; names here: ${(helperNames ++ fixtureNames).distinct.mkString(", ")}"
                  )
                val head =
                  (Vector(header(root)) ++ resolved.notes ++ notes ++ bodyNotes).mkString("\n")
                val stale = staleFiles(root, afterFixtures, Vector(d.file)).nonEmpty
                (
                  Answer(
                    Render.tests(
                      d,
                      d +: otherTops,
                      src,
                      helpers,
                      used,
                      tests,
                      prefixes,
                      bodies,
                      fixtures,
                      head,
                      cap,
                      stale
                    ),
                    Status.Found
                  ),
                  Roots.put(roots, root, afterFixtures)
                )
              }
          }
      }
  }

  /** The test-source fixtures `suite` uses: each top-level definition in a test classes dir, in another file than
    * `suite`'s, holding a member `suite`'s `.tasty` refers to, with those members in file order; and the cache after.
    * A read that fails leaves none.
    */
  private def fixturesOf(
      root: Root,
      layout: Layout,
      suite: Defn,
      in: Loaded
  ): (Vector[Render.Fixture], Loaded) = {
    val own = Locate.forTopLevel(root, layout, suite.fullName)
    Loaded.uses(root, layout, own, in) match {
      case Left(_) => (Vector.empty, in)
      case Right((refs, afterRefs, _)) =>
        val targets = refs.map(_.target).toSet
        val testDirs = layout.classesDirs(root).filter(layout.isTest)
        val tasty = targets.toVector
          .flatMap(t => Locate.forTopLevelIn(testDirs, t))
          .distinct
          .filterNot(own.contains)
        Loaded.defns(root, layout, tasty, afterRefs) match {
          case Left(_) => (Vector.empty, afterRefs)
          case Right((defns, next, _)) =>
            def named(m: Defn): Boolean =
              targets.contains(m.fullName) || targets.exists(_.startsWith(m.fullName + "."))
            val found = defns
              .filter(_.file != suite.file)
              .sortBy(d => (d.file, d.lines.start))
              .flatMap { top =>
                val used =
                  top.members.filter(m => m.kind != Kind.Param && named(m)).sortBy(_.lines.start)
                if (used.isEmpty) None else Some(Render.Fixture(top, used))
              }
            (found, next)
        }
    }
  }

  /** The definitions of the family `name` (its trait, implementations and contracts, each with its role as a prefix), the notes its loading raised, and the cache after; `Left` when no trait or abstract class is named `name`, saying what a definition of that name is instead. */
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
      val (mentioning, words) =
        Locate.mentioning(root, name.split('.').last, resolved.loaded.words)
      val files = mentioning
        .filter(file => exact || !config.hidden.exists(prefix => file.toString.startsWith(prefix)))
      val inPackages = Locate.inPackagesIn(root, layout.classesDirs(root))
      val inPackage = files.map(file => file -> inPackages(file))
      val tasty = inPackage.flatMap(_._2).distinct
      val unsearched = inPackage.collect { case (file, found) if found.isEmpty => file }
      val (defns, next, missing) =
        loadAll(root, layout, tasty, resolved.loaded.copy(words = words))
      val notes = resolved.notes ++ missing ++ notSearched(unsearched)
      familyOf(defns, traitName, None, false).map { f =>
        val members =
          Vector(Render.Listed(f.trait0, "")) ++
            f.impls.map(d => Render.Listed(d, "impl ")) ++
            f.contracts.map { case (c, _) => Render.Listed(c, "contract ") }
        (members, notes, next)
      }
    }

  /** The `family` answer for `name` (a trait, or an abstract class), read through `root`'s own cache in `roots`, which then holds `root` as most recently used. `member` narrows the trait's and each implementation's text to that member, with its body under `withBody`. A `name` `Resolve.resolve` leaves naming several definitions is `Status.NoMatch`, its notes listing them and no family picked. Every answer starts with `root`'s `## root` line. */
  def family(
      root: Root,
      layout: Layout,
      config: Config,
      roots: Roots,
      name: String,
      member: Option[String],
      withBody: Boolean,
      cap: Int
  ): (Answer, Roots) = staled(root, layout, config, roots) { roots =>
    val (answer, loaded) =
      familyIn(root, layout, config, Roots.of(roots, root), name, member, withBody, cap)
    (answer, Roots.put(roots, root, loaded))
  }

  /** The family of the trait or abstract class `name` among `defns` (top-level, as loaded; their members are read here): its implementations, its contracts (abstract classes that do not extend it and have a member referring to it), and each contract's suites (the definitions extending it), in source order. `Left` when no trait or abstract class is named `name`, saying what a definition of that name is instead. */
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
      case None =>
        Left(all.find(named).fold(s"no trait or abstract class named $name") { d =>
          val n = d.fullName
          s"$n is ${article(kindName(d.kind))}, not a trait or abstract class, so it has no family: show $n reads it, uses $n lists what names it"
        })
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
    val notes = notLoaded(root, layout, named, next, failed.headOption.map(_._2)).toVector
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
  /** The one line naming `files`, which the inspector could not read, each with the first line of its message in
    * `loaded`, then `complaint`'s cause when there is one, then the command compiling their modules; none for no file.
    */
  private def notLoaded(
      root: Root,
      layout: Layout,
      files: Vector[os.Path],
      loaded: Loaded,
      complaint: Option[String]
  ): Option[String] =
    Option.when(files.nonEmpty) {
      val cause = complaint.map(captured).filter(_.nonEmpty).fold("")(line => s" ($line)")
      val named = files.map { p =>
        val name = relativePath(root, layout, p)
        loaded.unreadable.get(p).fold(name) { message =>
          s"$name (${Read.plain(message).takeWhile(_ != '\n').take(160)})"
        }
      }
      val modules = files
        .flatMap(p => layout.classesDirs(root).find(p.startsWith))
        .distinct
        .map(layout.moduleOf(root, _))
        .distinct
      val compile =
        if (modules.isEmpty) "" else s"; compile to read them: ${layout.compile(modules)}"
      s"-- not loaded: ${named.mkString(", ")}$cause$compile"
    }

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
        case Right(resolved) if resolved.defns.map(_.fullName).distinct.size > 1 =>
          // A family is one trait's: an ambiguous name answers its candidates, as resolve lists them.
          (Answer((header(root) +: resolved.notes).mkString("\n"), Status.NoMatch), resolved.loaded)
        case Right(resolved) =>
          // A name matched exactly keeps its hidden candidate files; a short name does not reach them.
          val exact = resolved.defns.exists(_.fullName == name)
          val traitName = resolved.defns.map(_.fullName).headOption.getOrElse(name)
          val (mentioning, words) =
            Locate.mentioning(root, name.split('.').last, resolved.loaded.words)
          val files = mentioning.filter(file =>
            exact || !config.hidden.exists(prefix => file.toString.startsWith(prefix))
          )
          val inPackages = Locate.inPackagesIn(root, layout.classesDirs(root))
          val inPackage = files.map(file => file -> inPackages(file))
          val tasty = inPackage.flatMap(_._2).distinct
          val unsearched = inPackage.collect { case (file, found) if found.isEmpty => file }
          val (defns, next, missing) =
            loadAll(root, layout, tasty, resolved.loaded.copy(words = words))
          val head =
            (Vector(header(root)) ++ resolved.notes ++ missing ++ notSearched(unsearched))
              .mkString("\n")
          familyOf(defns, traitName, member, withBody) match {
            case Left(message) => (Answer(s"$head\n$message", Status.NoMatch), next)
            case Right(f) =>
              val stale = staleFiles(root, next, files.map(_.toString))
              (Answer(Render.family(f, everyDefnOf(defns), head, stale, cap), Status.Found), next)
          }
      }

  /** `query`'s answer, run over `roots` with `root`'s consulted files cleared, and after the answer's `## root` line the
    * line naming `config`'s ignored lines and `staleLine`'s line, each when there is one.
    */
  private def staled(root: Root, layout: Layout, config: Config, roots: Roots)(
      query: Roots => (Answer, Roots)
  ): (Answer, Roots) = {
    val (answer, next) =
      query(Roots.put(roots, root, Roots.of(roots, root).copy(consulted = Set.empty)))
    val loaded = Roots.of(next, root)
    // The files this query asked for that the inspector could not read, named once whichever query read them.
    val unread =
      if (answer.text.linesIterator.exists(_.startsWith("-- not loaded:"))) None
      else
        notLoaded(
          root,
          layout,
          Loaded.remembered(loaded.consulted.toVector.sortBy(_.toString), loaded),
          loaded,
          None
        )
    val ignored = Option.when(config.ignored.nonEmpty)(
      s"-- .outline.conf lines this outline does not know, ignored (a newer outline may read them): ${config.ignored.mkString(", ")}"
    )
    val lines = ignored.toVector ++ staleLine(root, layout, loaded) ++ unread
    if (lines.isEmpty || !answer.text.startsWith("## root")) (answer, next)
    else {
      val (first, rest) = answer.text.span(_ != '\n')
      (answer.copy(text = s"$first\n${lines.mkString("\n")}$rest"), next)
    }
  }

  /** Files a module's part of the stale line names before it counts the rest as `+N`. */
  private val staleNamed: Int = 3

  /** The line naming each module that holds a `.tasty` in `loaded.consulted` compiled from a source `staleFiles` judges
    * stale, with those sources' file names (`staleNamed` at most, then `+N`), and the command compiling every such
    * module; none when no consulted file is stale.
    */
  private def staleLine(root: Root, layout: Layout, loaded: Loaded): Option[String] = {
    val dirs = layout.classesDirs(root)
    // Each consulted file's (module, source).
    val compiled: Vector[(String, String)] = loaded.consulted.toVector.flatMap { p =>
      for {
        cached <- loaded.byTasty.get(p)
        d <- cached.defns.headOption
        dir <- dirs.find(p.startsWith)
      } yield (layout.moduleOf(root, dir), d.file)
    }.distinct
    val staleSources = staleFiles(root, loaded, compiled.map(_._2))
    val stale = compiled.filter { case (_, file) => staleSources.contains(file) }
    val byModule = stale.groupMap(_._1)(_._2).toVector.sortBy(_._1)
    if (byModule.isEmpty) None
    else {
      val listed = byModule.map { case (module, files) =>
        val names = files.distinct.sorted.map(_.split('/').last)
        val more = if (names.size > staleNamed) s" +${names.size - staleNamed}" else ""
        s"$module (${names.take(staleNamed).mkString(", ")}$more)"
      }
      Some(
        s"-- stale since the last compile: ${listed.mkString(", ")}: ${layout.compile(byModule.map(_._1))}"
      )
    }
  }

  /** `root`'s `## root` line: its directory, then its branch (or "detached") and HEAD. */
  private def header(root: Root): String = {
    val revision = Locate.revision(root)
    s"## root ${root.dir} (${revision.branch.getOrElse("detached")} @ ${revision.head})"
  }

  /** Why `uses` refuses each of `bare`, with the owners `in` already holds a definition of it in, at most 10. */
  private def refusal(bare: Vector[String], in: Loaded): String = {
    def every(ds: Vector[Defn]): Vector[Defn] = ds.flatMap(d => d +: every(d.members))
    val held = every(in.byTasty.values.toVector.flatMap(_.defns))
    bare
      .map { sym =>
        val owners = held.filter(_.name == sym).map(_.fullName).distinct.sorted
        val known =
          if (owners.isEmpty) ""
          else s"; defined, among the files already read, as ${owners.take(10).mkString(", ")}"
        s"-- uses refuses $sym: a name with no qualifier must be at least $shortestBare characters; qualify it with at least its owner, as in MoveLimits.of$known"
      }
      .mkString("\n")
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

  /** The source files under `root` that hold a search's name as a word and, when it has an owner, the owner's too; sorted, each once. With them, the source words read, as `Locate.mentioningEach` gives them from `known`. */
  private[query] def candidates(
      root: Root,
      searches: Vector[Search],
      known: SourceWords
  ): (Vector[os.RelPath], SourceWords) = {
    val (holding, words) =
      Locate.mentioningEach(root, searches.flatMap(s => s.name +: s.owner.toVector), known)
    def files(word: String): Vector[os.RelPath] = holding.getOrElse(word, Vector.empty)
    val found = searches
      .flatMap { s =>
        s.owner.fold(files(s.name)) { owner =>
          val naming = files(owner).toSet
          files(s.name).filter(naming.contains)
        }
      }
      .distinct
      .sortBy(_.toString)
    (found, words)
  }

  /** The `.tasty` files in `classes` a search for `searches` reads: those in the package dirs of the files `candidates` gives that satisfy `keep`; with them, those files whose package has no `.tasty` in `classes` and that `config` does not hide; and the source words `candidates` read from `known`. */
  private[query] def toRead(
      root: Root,
      config: Config,
      classes: Vector[os.Path],
      searches: Vector[Search],
      keep: os.RelPath => Boolean,
      known: SourceWords
  ): (Vector[os.Path], Vector[os.RelPath], SourceWords) = {
    val (found, words) = candidates(root, searches, known)
    val kept = found.filter(keep)
    val inPackages = Locate.inPackagesIn(root, classes)
    val inPackage = kept.map(file => file -> inPackages(file))
    val unsearched = inPackage.collect {
      case (file, found)
          if found.isEmpty && !config.hidden.exists(prefix => file.toString.startsWith(prefix)) =>
        file
    }
    (inPackage.flatMap(_._2).distinct, unsearched, words)
  }

  /** The references in `classes`' `.tasty` to `names` (full names), read through the root's cache from the files `toRead` gives; with them, the files `toRead` names as not searched, and the cache after. The references are `Left` when their index cannot be read. */
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
    val (tasty, unsearched, words) = toRead(root, config, classes, searches, keep, in.words)
    val read = in.copy(words = words)
    if (names.isEmpty || tasty.isEmpty) (Right(Vector.empty), unsearched, read)
    else
      Loaded.uses(root, layout, tasty, read) match {
        case Left(message) => (Left(message), unsearched, read)
        case Right((refs, next, _)) => (Right(Read.targeting(refs, names)), unsearched, next)
      }
  }

  /** A suite holding references to a shown symbol: its simple name, its file, and how many references it holds. */
  final case class SuiteHits(name: String, file: String, sites: Int)

  /** The names of the suites nearest the symbol defined in `file`, at most three: suites under the symbol's own module
    * directory (the path before its last `src` segment, else its directory) first, then those holding the most
    * references, then by name.
    */
  private[query] def nearest(file: String, suites: Vector[SuiteHits]): Vector[String] = {
    val segments = file.split('/').toVector
    val home = segments.lastIndexOf("src") match {
      case -1 => segments.dropRight(1)
      case i => segments.take(i)
    }
    def own(s: SuiteHits): Boolean = s.file.split('/').toVector.startsWith(home)
    suites.sortBy(s => (!own(s), -s.sites, s.name)).map(_.name).distinct.take(3)
  }

  /** A reference in a test source to a shown symbol: the suite holding it, and its test or helper as `show` lists it. */
  private final case class Hit(site: Use, suite: Defn, entry: Render.Exercise, inTest: Boolean)

  /** `hits` by file, as `show`'s tests section lists them: each test or helper once, in line order. */
  private def listing(hits: Vector[Hit]): Vector[Render.Exercised] =
    hits.map(_.site.file).distinct.map { file =>
      val here = hits.filter(_.site.file == file)
      Render.Exercised(
        file,
        here.map(_.suite.fullName).distinct,
        here.map(_.entry).distinct.sortBy(e => (e.lines.start, e.lines.end, e.label))
      )
    }

  /** One summary per distinct full name among `tops`, in their order, counting the `hits` on it or its members. */
  private def summaries(tops: Vector[Defn], hits: Vector[Hit]): Vector[TestsSummary] =
    tops.map(_.fullName).distinct.map { name =>
      val same = tops.filter(_.fullName == name)
      val names = same.flatMap(d => d.fullName +: d.members.map(_.fullName)).toSet
      val on = hits.filter(h => names.contains(h.site.target))
      val suites = on.groupBy(h => (h.site.file, h.suite.fullName)).toVector.map {
        case ((file, _), hs) => SuiteHits(hs.headOption.fold("")(_.suite.name), file, hs.size)
      }
      TestsSummary(
        name,
        on.filter(_.inTest).map(h => (h.site.file, h.entry)).distinct.size,
        suites.size,
        nearest(same.headOption.fold("")(_.file), suites)
      )
    }

  /** The tests in test sources that hold a reference to `tops` or to one of their members, by file: each test by its line range, or a suite's helper where a reference lies outside every test. Notes name a tasty that could not be read; the cache after. */
  private def exercisedBy(
      root: Root,
      layout: Layout,
      config: Config,
      tops: Vector[Defn],
      seen: Vector[Defn],
      in: Loaded
  ): (Vector[Hit], Vector[String], Loaded) = {
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
          val inTestPackages = Locate.inPackagesIn(root, testClasses)
          val tasty =
            fileNames
              .flatMap(file => inTestPackages(os.RelPath(file)))
              .distinct
          Loaded.defns(root, layout, tasty, found) match {
            case Left(message) => (Vector.empty, Vector(message), found)
            case Right((defns, next, _)) =>
              val hits = fileNames.flatMap { file =>
                exercisedIn(root, config, file, defns, sites.filter(_.file == file))
              }
              (hits, Vector.empty, next)
          }
      }
    }
  }

  /** One file's hits for the sites in it: each site's test by line range, else the suite's helper it lies in; a site in no suite has none. Reads the file's source; `defns` are its tasty's definitions. */
  private def exercisedIn(
      root: Root,
      config: Config,
      file: String,
      defns: Vector[Defn],
      sites: Vector[Use]
  ): Vector[Hit] = {
    val src = os.read(root.dir / os.RelPath(file))
    val suites =
      defns.filter(d => d.file == file && (d.kind == Kind.Class || d.kind == Kind.Object))
    sites.flatMap { u =>
      suites.find(s => s.lines.start <= u.line && u.line <= s.lines.end).map { suite =>
        val (from, to) = TestCalls.within(src, suite.lines)
        TestCalls
          .find(src, from, to, config.testCall)
          .find(t => t.lines.start <= u.line && u.line <= t.lines.end) match {
          case Some(t) => Hit(u, suite, Render.Exercise(t.lines, s"${suite.name}: ${t.name}"), true)
          case None =>
            suite.members.find(m => m.lines.start <= u.line && u.line <= m.lines.end) match {
              case Some(m) =>
                Hit(u, suite, Render.Exercise(m.lines, s"${suite.name}: helper ${m.name}"), false)
              case None =>
                val enclosing = u.enclosing.split('.').last
                val entry =
                  Render.Exercise(Lines(u.line, u.line), s"${suite.name}: helper $enclosing")
                Hit(u, suite, entry, false)
            }
        }
      }
    }
  }

  /** The note for each `--body` name that no shown definition has: `tops` and their members (those withPrivate
    * or public), whose simple names it lists, sorted; a name only a private member has is named as private. A name
    * none of them has, defined in a val's or def's body among them, is named with its line; else, when `tops` hold a
    * type, of `elsewhere(name, typeNames)`, main-source definitions of that simple name, those whose signature names one
    * of those types are listed, each with the `show` printing it, and when none does the note names the `find` listing
    * every one.
    */
  private def bodyMisses(
      bodies: Set[String],
      tops: Vector[Defn],
      withPrivate: Boolean,
      elsewhere: (String, Vector[String]) => Vector[Defn]
  ): Vector[String] = {
    val shown = tops.flatMap(d => d +: d.members.filter(m => withPrivate || !m.isPrivate))
    val names = shown.map(_.name).distinct.sorted
    val privateNames = tops.flatMap(_.members.filter(_.isPrivate)).map(_.name).toSet
    val nested = tops.map(Render.nestedBodies(_, bodies, withPrivate))
    // A bare name or a path a nested member answers, or one several nested owners hold, which its own note names.
    val reached = nested.flatMap(_._1).flatMap(m => Vector(m.name, m.fullName)).toSet
    val several = nested.flatMap(_._2)
    def answered(name: String): Boolean =
      reached.contains(name) || reached.exists(_.endsWith("." + name)) ||
        several.exists(_.startsWith(s"-- --body $name is in several owners"))
    // A definition in a named val's or def's body, in an anonymous class or local, which no .tasty lists: its first line,
    // read from the body's text.
    def anonymous(name: String): Option[String] = {
      val defining =
        raw"^\s*(?:override\s+)?(?:def|val|var|lazy\s+val)\s+${java.util.regex.Pattern.quote(name)}\b.*".r
      def withMembers(ds: Vector[Defn]): Vector[Defn] =
        ds.flatMap(d => d +: withMembers(d.members.filter(m => withPrivate || !m.isPrivate)))
      withMembers(tops)
        .filter(d => d.kind == Kind.Val || d.kind == Kind.Def)
        .flatMap { d =>
          val text = (d.head +: d.body.toVector).mkString("\n").split("\n", -1).toVector
          Option(text.indexWhere(defining.matches))
            .filter(_ >= 0)
            .map(at => d -> (d.lines.start + at))
        }
        .headOption
        .map { case (d, line) =>
          val owner = d.fullName.split('.').dropWhile(_.headOption.exists(_.isLower)).mkString(".")
          s"-- --body $name is defined in $owner's body, in an anonymous class or as a local, at line $line: --body ${d.name} prints it whole"
        }
    }
    // A type's reader may be another type's member; a shown def or val has none.
    val typeNames = tops
      .filter(d =>
        d.kind != Kind.Def && d.kind != Kind.Val && d.kind != Kind.Var && d.kind != Kind.Given
      )
      .map(_.name)
      .distinct
    val whose = typeNames.mkString(" or ")
    def foreign(name: String): String = {
      val here = s"-- --body $name matches nothing shown; names here: ${names.mkString(", ")}"
      val naming =
        if (typeNames.isEmpty) Vector.empty
        else
          elsewhere(name, typeNames)
            .filterNot(tops.contains)
            .filter(d => typeNames.exists(t => WordGlob(t).foundIn(Render.signatureLine(d))))
      def showing(d: Defn): String =
        s"show ${d.fullName.split('.').dropRight(1).mkString(".")} --body $name${
            if (d.isPrivate && !withPrivate) " --private" else ""
          }"
      naming match {
        case Vector(d) =>
          s"-- --body $name is not $whose's; defined elsewhere naming $whose: ${Render.listing(d)}: ${showing(d)} prints it"
        case Vector() if typeNames.isEmpty => here
        case Vector() =>
          s"$here; no $name elsewhere names $whose in its signature: find '*.$name' lists every $name"
        case several =>
          (s"-- --body $name is not $whose's; defined elsewhere naming $whose, each printed by its show:" +:
            several.map(d => s"--   ${Render.listing(d)}: ${showing(d)}")).mkString("\n")
      }
    }
    several ++ bodies.toVector.sorted
      .filterNot(name => names.contains(name) || answered(name))
      .map { name =>
        if (privateNames.contains(name)) s"-- --body $name is private: add --private"
        else anonymous(name).getOrElse(foreign(name))
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
      tests: TestsShown,
      fullDocs: Boolean
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
      // The matches found in test sources only, which no tests line counts.
      val inTests = mutable.ListBuffer.empty[Defn]
      syms.foreach { sym =>
        Resolve.resolve(root, layout, config, Scope.Main, sym, state) match {
          case Left(message) => failure = Some(message)
          // A miss pays for one more resolve, over the test-only classes dirs; a name found there is shown under a note.
          case Right(resolved) if resolved.defns.isEmpty =>
            state = resolved.loaded
            companions ++= resolved.seen
            Resolve.resolve(root, layout, config, Scope.WithTests, sym, state) match {
              case Left(message) => failure = Some(message)
              case Right(tests) =>
                state = tests.loaded
                companions ++= tests.seen
                if (tests.defns.isEmpty) notes ++= resolved.notes
                else {
                  notes ++= tests.notes ++ tests.defns
                    .map(_.fullName)
                    .distinct
                    .map(n => s"-- in test sources: $n")
                  matches ++= tests.defns
                  inTests ++= tests.defns
                }
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
          // resolve's `-- no match for X; try find *X*` note is the one line naming a miss
          (Answer(noteText.stripSuffix("\n"), Status.NoMatch), state)
        case None =>
          val tops = matches.toVector.distinct
          val (hits, testNotes, afterTests) = tests match {
            case TestsShown.Dropped => (Vector.empty, Vector.empty, state)
            case _ =>
              exercisedBy(
                root,
                layout,
                config,
                tops.filterNot(inTests.contains),
                companions.toVector,
                state
              )
          }
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
          val stale = staleFiles(root, state, loadedDefns.map(_.file))
          val rendered = Render.show(
            named = tops,
            traced = traced,
            companions = loadedDefns,
            stale = stale,
            bodies = bodies,
            withPrivate = withPrivate,
            cap = cap,
            exercising = tests match {
              case TestsShown.Listed => Render.exercising(syms.mkString(","), listing(hits))
              case _ => Vector.empty
            },
            summaries = tests match {
              case TestsShown.Summary => summaries(tops.filterNot(inTests.contains), hits)
              case _ => Vector.empty
            },
            fullDocs = fullDocs
          )
          // Read only for a --body name nothing shown has: the packages of the main sources holding it and a shown
          // type's name as words, since a signature naming that type is written in such a file.
          def elsewhere(name: String, types: Vector[String]): Vector[Defn] = {
            val holdingType = types.flatMap(t => Locate.holding(root, WordGlob(t).foundIn)).toSet
            val tasty = Resolve.usable(
              root,
              layout,
              Scope.Main,
              Locate
                .holding(root, WordGlob(name).foundIn)
                .filter(holdingType.contains)
                .flatMap(Locate.inPackagesIn(root, classes))
                .distinct
            )
            val (defns, next, _) = loadAll(root, layout, tasty, state)
            state = next
            everyDefnOf(defns)
              .filter(d => d.name == name && !config.hidden.exists(p => d.file.startsWith(p)))
              .distinct
          }
          val bodyNotes =
            (bodyMisses(bodies, tops, withPrivate, elsewhere) ++ testNotes).map(_ + "\n").mkString
          val text = noteText + bodyNotes + rendered
          (Answer(text, Status.Found), state)
      }
    }
  }
}
