package grit.outline.render

import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import grit.outline.model.Lines
import grit.outline.model.{Defn, Kind, TestCase, Use}
import grit.outline.trace.Traced

/** How much of `show`'s tests section prints: one line per named symbol, every test and helper listed, or nothing. */
enum TestsShown {
  case Summary, Listed, Dropped
}

/** What `show`'s tests line says of the named symbol `symbol`: how many tests in test sources hold a reference to it or to
  * one of its members, how many suites hold one in a test or a helper, and the nearest of those suites, at most three.
  */
final case class TestsSummary(symbol: String, tests: Int, suites: Int, nearest: Vector[String])

/** A trait's family: the trait, its implementations, and each contract with the suites extending it. */
final case class Family(
    trait0: Defn,
    member: Option[String],
    impls: Vector[Defn],
    contracts: Vector[(Defn, Vector[Defn])],
    withBody: Boolean
)

object Render {

  /** One line for a traced type: its lines and collapsed signature, then for an Enum its cases, and for a Trait or abstract Class the members of `companions` that extend it in `d`'s own file, each as its collapsed signature and start line; then `d`'s size tag. */
  def oneLine(d: Defn, companions: Vector[Defn]): String = {
    val head = s"→ ${d.lines.start}-${d.lines.end} ${collapse(withoutDocs(d.signature))}"
    val items: Vector[Defn] =
      if (d.kind == Kind.Enum) d.members.filter(_.kind == Kind.EnumCase)
      else if (d.kind == Kind.Trait || (d.kind == Kind.Class && d.isAbstract))
        companions
          .flatMap(c => c +: c.members)
          .filter(i => i.file == d.file && i.parents.contains(d.fullName))
      else Vector.empty
    (if (items.isEmpty) head
     else head + ": " + items.map(caseItem).mkString(" | ")) + sizeTag(d.size)
  }

  /** A file's `== file  package` header line, flagged when its source is stale. */
  def fileLine(file: String, pkg: String, stale: Boolean): String =
    s"== $file  $pkg" + staleTag(stale)

  /** What a file's `==` line ends with when its source is stale, so what is printed from it may not match it. */
  private def staleTag(stale: Boolean): String =
    if (stale) "  [stale: source newer than .tasty]" else ""

  /** The `show` answer: `named` (verbatim; a named type with its public members, or all with `withPrivate`, but not its constructor vals, which its head holds), `bodies` (members whose `name` is in it get their body, and the nested members `nestedBodies` picks for it print whole, under their owner-qualified names), the traced types as one-liners, grouped by file; each named definition's header line ends with its size tag; a type printed without its body ends with its closing lines, after its members; `stale` files' headers flagged; cut at `cap` bytes between whole entries, a type of two members or more between its members, a type's closing lines printed only when none of its members was cut; a type's members named in `bodies` follow its head, before its other members, and they, that head and a named definition in `bodies` print whatever `cap`. */
  def show(
      named: Vector[Defn],
      traced: Traced,
      companions: Vector[Defn],
      stale: Set[String],
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int,
      exercising: Vector[Vector[String]] = Vector.empty,
      summaries: Vector[TestsSummary] = Vector.empty,
      fullDocs: Boolean = false
  ): String = {
    val everything = named ++ traced.types
    val files = everything.map(_.file).distinct

    // A top-level definition, or a member under its type, is in its owner's context; a named member is not.
    def headed(d: Defn, inContext: Boolean): Vector[String] =
      Vector(headerLine(d, inContext), d.head)

    // A definition named by a path below the shown type: its header names it, and its text prints whole.
    def entryWhole(d: Defn): Vector[String] =
      headed(d, inContext = false) ++ (if (d.body.nonEmpty) d.body.toVector else closingLines(d))

    def entry(d: Defn, inContext: Boolean): Vector[String] =
      headed(d, inContext) ++
        (if (bodies.contains(d.name) && d.body.nonEmpty) d.body.toVector else closingLines(d))

    // A member not named in --body: its doc cut to its first sentence, unless every doc prints whole.
    def memberEntry(d: Defn): Vector[String] =
      if (fullDocs || bodies.contains(d.name)) entry(d, inContext = true)
      else Vector(headerLine(d, inContext = true)) ++ briefHead(d) ++ closingLines(d)

    // A type with two members or more is its head's piece, then one piece per member, its closing lines riding on the
    // last, so a budget cuts between members and prints the closing lines only when no member was cut.
    def namedEntry(d: Defn): Vector[Piece] = {
      val topLevel = !ownerRelative(d).contains('.')
      if (isType(d)) {
        // A constructor val's lines are the type's own head, already shown.
        val (askedOwn, others) = d.members
          .filter(m => (withPrivate || !m.isPrivate) && m.kind != Kind.Param)
          .partition(m => bodies.contains(m.name))
        // A nested member --body names is printed whole under its owner-qualified name.
        val (deeper, _) = nestedBodies(d, bodies, withPrivate)
        val asked = (askedOwn.map(m => (m, memberEntry(m))) ++ deeper.map(m => (m, entryWhole(m))))
          .sortBy(_._1.lines.start)
          .map { case (m, lines) => Piece(lines, ownerRelative(m), asked = true) }
        // What --body names comes first, so a budget spends on it before the rest.
        val members = asked ++ others.map(m => Piece(memberEntry(m), ownerRelative(m)))
        val head = Piece(headed(d, topLevel), ownerRelative(d), asked = asked.nonEmpty)
        val closing = closingLines(d)
        if (members.size <= 1)
          Vector(Piece(head.lines ++ members.flatMap(_.lines) ++ closing, head.name, head.asked))
        else
          Vector(head) ++ members.dropRight(1) ++
            members.takeRight(1).map(m => m.copy(lines = m.lines ++ closing))
      } else Vector(Piece(entry(d, topLevel), ownerRelative(d), asked = bodies.contains(d.name)))
    }

    // Each entry is its pieces; a file's header rides on its first, so a cut never leaves a bare header.
    val groups: Vector[Vector[Vector[Piece]]] = files.map { f =>
      val namedHere = named.filter(_.file == f).map(namedEntry)
      val tracedHere = traced.types
        .filter(_.file == f)
        .sortBy(_.lines.start)
        .map(d => Vector(Piece(Vector(oneLine(d, companions)), ownerRelative(d))))
      namedHere ++ tracedHere
    }

    val entries: Vector[Vector[Piece]] = files.zip(groups).flatMap { case (f, here) =>
      val pkg = everything.find(_.file == f).map(d => pkgOf(d.fullName)).getOrElse("")
      val header = fileLine(f, pkg, stale.contains(f))
      here.zipWithIndex.map { case (pieces, i) =>
        if (i == 0) pieces.zipWithIndex.map { case (p, j) =>
          if (j == 0) p.copy(lines = header +: p.lines) else p
        }
        else pieces
      }
    }
    val pieces = entries.flatten
    val all = entries ++ exercising.map(c => Vector(Piece(c, "")))

    // One budget over the definitions and the tests list, so a cut line counts both.
    val keep = keptUnder(all.flatten, cap)

    val library = traced.library.filterNot(_.contains("<"))
    val notLoaded = traced.missing
    val trailer =
      (if (library.nonEmpty) Vector(s"-- library: ${library.mkString(", ")}") else Vector.empty) ++
        (if (notLoaded.nonEmpty) Vector(s"-- not loaded: ${notLoaded.mkString(", ")}")
         else Vector.empty)
    def keptOf(ps: Vector[Piece], flags: Vector[Boolean]) =
      ps.zip(flags).collect { case (p, true) => p.lines }.flatten
    val kept = keptOf(pieces, keep) ++ trailer ++ keptOf(
      all.flatten.drop(pieces.size),
      keep.drop(pieces.size)
    )
    val body =
      (kept ++ cutBetween(all, keep, cap, hint = bodies.isEmpty) ++
        summaries.map(testsLine))
        .map(_ + "\n")
        .mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  private def pkgOf(fullName: String): String =
    fullName.split('.').takeWhile(s => s.nonEmpty && s.charAt(0).isLower).mkString(".")

  private def ownerRelative(d: Defn): String = ownerRelativeName(d.fullName)

  private def ownerRelativeName(fullName: String): String = {
    val pkg = pkgOf(fullName)
    if (pkg.isEmpty) fullName else fullName.drop(pkg.length + 1)
  }

  private def kindWord(k: Kind): String = k match {
    case Kind.Class => "class"
    case Kind.CaseClass => "case class"
    case Kind.Trait => "trait"
    case Kind.Object => "object"
    case Kind.Enum => "enum"
    case Kind.EnumCase => "case"
    case Kind.Def => "def"
    case Kind.Val => "val"
    case Kind.Var => "var"
    case Kind.Param => "val"
    case Kind.Given => "given"
    case Kind.TypeAlias => "type"
    case Kind.Opaque => "opaque type"
  }

  /** What `bodies` names below `d`'s own members: `picked`, the members a dotted path from `d` names (`Inner.deep`, or
    * `Outer.Inner.deep` from `Outer`), or that a bare name names when none of `d`'s own members has it and the members of
    * one nested owner do; `several`, a note for each bare name the members of more than one nested owner have, naming
    * them. Private members only `withPrivate`.
    */
  def nestedBodies(
      d: Defn,
      bodies: Set[String],
      withPrivate: Boolean
  ): (Vector[Defn], Vector[String]) = {
    def visible(ds: Vector[Defn]): Vector[Defn] = ds.filter(m => withPrivate || !m.isPrivate)
    def below(ds: Vector[Defn]): Vector[Defn] =
      ds.flatMap(m => visible(m.members) ++ below(visible(m.members)))
    val own = visible(d.members)
    val nested = below(own)
    val picks = bodies.toVector.sorted.map { name =>
      val path = name.split('.').toVector match {
        case first +: rest if first == d.name && rest.nonEmpty => rest
        case all => all
      }
      if (path.size > 1)
        Left(path.drop(1).foldLeft(own.filter(_.name == path.headOption.getOrElse(""))) {
          (owners, segment) =>
            owners.flatMap(o => visible(o.members).filter(_.name == segment))
        })
      else if (own.exists(_.name == name)) Left(Vector.empty)
      else {
        val found = nested.filter(_.name == name)
        val owners = found
          .map(m => ownerRelativeName(m.fullName).split('.').dropRight(1).mkString("."))
          .distinct
        if (owners.size > 1)
          Right(
            s"-- --body $name is in several owners: ${owners.map(o => s"$o.$name").mkString(", ")}; name one as Owner.$name"
          )
        else Left(found)
      }
    }
    (
      picks.collect { case Left(ms) => ms }.flatten.distinct,
      picks.collect { case Right(note) => note }
    )
  }

  /** The `family` answer: `f`'s trait, then each implementation as one line, then each contract with its non-private members and the suites that run it, each definition's line ending with its size tag, grouped by file under the `header` line; cut at `cap` bytes. The trait ends with its closing lines, and so does each member it shows without its body that is itself a type. With `f.member`, the trait shows that member (its body with `f.withBody`) and each implementation its overloads of it (or their bodies). */
  def family(f: Family, all: Vector[Defn], header: String, stale: Set[String], cap: Int): String = {
    def headerOf(d: Defn): String =
      s"${d.lines.start}-${d.lines.end} ${kindWord(d.kind)} ${ownerRelative(d)}"

    // Under the trait, a member is in its owner's context; one --member names is not.
    def block(d: Defn, body: Boolean): Vector[String] =
      Vector(headerLine(d, inContext = f.member.isEmpty), d.head) ++
        (if (body && d.body.nonEmpty) d.body.toVector else closingLines(d))

    def signatureOf(d: Defn): String = collapse(withoutDocs(d.signature))

    // The trait's closing lines come after its members' entries.
    val traitLines: Vector[String] =
      Vector(
        headerLine(f.trait0, inContext = !ownerRelative(f.trait0).contains('.')),
        f.trait0.head
      ) ++ (f.member match {
        case None => f.trait0.members.filter(!_.isPrivate).flatMap(block(_, false))
        case Some(m) => f.trait0.members.filter(_.name == m).flatMap(block(_, f.withBody))
      }) ++ closingLines(f.trait0)

    def implLines(d: Defn): Vector[String] = {
      val head = s"${headerOf(d)}  ${signatureOf(d)}${sizeTag(d.size)}"
      f.member match {
        case None => Vector(head)
        case Some(m) =>
          val overloads = d.members.filter(_.name == m)
          if (f.withBody)
            Vector(head) ++ overloads.flatMap(o => Vector(o.head) ++ o.body.toVector)
          else
            Vector(head) ++ overloads.map(o =>
              s"  ${o.lines.start}-${o.lines.end} $m${sizeTag(o.size)}"
            )
      }
    }

    def contractLines(c: Defn, suites: Vector[Defn]): Vector[String] = {
      val runBy =
        if (suites.isEmpty) "  run by nothing"
        else s"  run by ${suites.map(s => s"${s.file}:${s.lines.start}").mkString(", ")}"
      Vector(
        s"${c.lines.start}-${c.lines.end} contract ${ownerRelative(c)}  ${signatureOf(c)}${sizeTag(c.size)}"
      ) ++
        c.members
          .filter(!_.isPrivate)
          .map(m => s"  ${m.lines.start}-${m.lines.end} ${signatureOf(m)}${sizeTag(m.size)}") ++
        Vector(runBy)
    }

    // Each entry is (file, start line, its lines); a file's header rides on its first entry.
    val pieces: Vector[(String, Int, Vector[String])] =
      Vector((f.trait0.file, f.trait0.lines.start, traitLines)) ++
        f.impls.map(d => (d.file, d.lines.start, implLines(d))) ++
        f.contracts.map { case (c, suites) => (c.file, c.lines.start, contractLines(c, suites)) }

    val files = pieces.map(_._1).distinct
    val chunks: Vector[Vector[String]] = files.flatMap { file =>
      val here = pieces.filter(_._1 == file).sortBy(_._2).map(_._3)
      val pkg = all.find(_.file == file).map(d => pkgOf(d.fullName)).getOrElse("")
      val staleTag = if (stale.contains(file)) "  [stale: source newer than .tasty]" else ""
      val fileHeader = s"== $file  $pkg$staleTag"
      here.zipWithIndex.map { case (lines, i) => if (i == 0) fileHeader +: lines else lines }
    }

    val (kept, truncated) = fit(chunks, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** The `uses` answer, grouped by file under `== file`, flagged as `show` flags it when the file is in `stale`: each run of sites with one owner as an `in <owner>` line, ending with the size `sizes` gives its first site, then each site as `<line>  <text>`; then the site and file counts. A site's owner is its enclosing definition without its package, or `<Suite>: <test name>` for a site `tests` names the test of, the suite being the owner's first segment. `overloaded` names the targets whose sites carry the `[→ name :line]` marker to their target's definition line after the line number; cut at `cap` bytes. */
  def uses(
      sites: Vector[Use],
      overloaded: Set[String],
      cap: Int,
      tests: Map[Use, String],
      stale: Set[String],
      sizes: Map[Use, Int],
      allSites: Boolean = false
  ): String = {
    def owner(u: Use): String = {
      val pkg = pkgOf(u.enclosing)
      val member = if (pkg.isEmpty) u.enclosing else u.enclosing.drop(pkg.length + 1)
      tests.get(u).fold(member)(name => s"${member.takeWhile(_ != '.')}: $name")
    }
    def siteLine(u: Use): String = {
      val marker =
        if (overloaded.contains(u.target)) s" [→ ${u.target.split('.').last} :${u.targetLine}]"
        else ""
      s"    ${u.line}$marker  ${u.text}"
    }
    // Consecutive sites with one owner, in line order: an owner line once per run.
    def runs(inFile: Vector[Use]): Vector[Vector[Use]] =
      inFile.foldLeft(Vector.empty[Vector[Use]]) { (acc, u) =>
        acc.lastOption match {
          case Some(run) if run.headOption.exists(first => owner(first) == owner(u)) =>
            acc.init :+ (run :+ u)
          case _ => acc :+ Vector(u)
        }
      }
    def count(n: Int): String = if (n == 1) "1 site" else s"$n sites"
    val files = sites.map(_.file).distinct
    val listed = allSites || sites.size <= SitesListed
    val chunks: Vector[Vector[String]] = files.map { f =>
      val here = sites.filter(_.file == f)
      if (listed)
        s"== $f${staleTag(stale.contains(f))}" +: runs(here).flatMap(run =>
          run.headOption.map(u => s"  in ${owner(u)}${sizeTag(sizes.get(u))}").toVector ++ run.map(
            siteLine
          )
        )
      else
        s"== $f  ${count(here.size)}${staleTag(stale.contains(f))}" +: here
          .groupBy(owner)
          .toVector
          .sortBy { case (_, us) => us.headOption.fold(0)(_.line) }
          .map { case (o, us) =>
            s"  in $o  ${count(us.size)}${sizeTag(us.headOption.flatMap(sizes.get))}"
          }
    }
    val (kept, truncated) = fit(chunks, cap)
    val summary =
      if (listed) s"[${sites.size} sites in ${files.size} files]"
      else
        s"-- ${count(sites.size)} in ${files.size} files; --sites to list them all, or --in/--outside to narrow"
    val body = (kept ++ truncated :+ summary).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** The most sites `uses` lists one by one without `allSites`; past it, each file and owner prints its count instead. */
  val SitesListed: Int = 20

  /** The `find` answer under `header`: `notes`, a line each, then each of `found`, in its order, as `<full name>  <file>:<lines>  <collapsed signature>` and its size tag; cut at `cap` bytes between lines. */
  def find(notes: Vector[String], found: Vector[Defn], header: String, cap: Int): String = {
    val body = linesUnder(notes ++ found.map(listing), header, cap)
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** `d` on one line, as `find` lists it: `<full name>  <file>:<lines>  <collapsed signature>` and its size tag. */
  def listing(d: Defn): String =
    s"${d.fullName}  ${d.file}:${d.lines.start}-${d.lines.end}  ${collapse(withoutDocs(d.signature))}${sizeTag(d.size)}"

  /** `header`, then the leading `lines` that fit in `cap` bytes, then the cut line saying how many more there are and the
    * budget that shows them all; each line ends with its newline.
    */
  def linesUnder(lines: Vector[String], header: String, cap: Int): String = {
    val (kept, truncated) = fit(lines.map(Vector(_)), cap)
    (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
  }

  /** A definition of an area, with the role its family gives it: `prefix` is "" for a symbol or a trait, "impl " for an implementation, "contract " for a contract. */
  final case class Listed(defn: Defn, prefix: String)

  /** The `area` answer under `header`: at level 0 one line per package, `<package>  <short names>`; at level 1 each definition as `<lines> <prefix><kind> <owner>`, then ` — ` and its doc's first sentence when it has one, grouped by file; cut at `cap` bytes. */
  def area(listed: Vector[Listed], level: Int, header: String, cap: Int): String = {
    val sorted = listed.sortBy(l => (l.defn.file, l.defn.lines.start))

    def entryLine(l: Listed): String = {
      val head =
        s"${l.defn.lines.start}-${l.defn.lines.end} ${l.prefix}${kindWord(l.defn.kind)} ${ownerRelative(l.defn)}"
      firstSentence(l.defn.doc).fold(head)(s => s"$head — $s")
    }

    val chunks: Vector[Vector[String]] =
      if (level == 0)
        sorted
          .groupBy(l => pkgOf(l.defn.fullName))
          .toVector
          .sortBy(_._1)
          .map { case (pkg, ls) => Vector(s"$pkg  ${ls.map(_.defn.name).distinct.mkString(", ")}") }
      else
        sorted.map(_.defn.file).distinct.map { file =>
          val here = sorted.filter(_.defn.file == file)
          val pkg = here.map(l => pkgOf(l.defn.fullName)).distinct.mkString(", ")
          s"== $file  $pkg" +: here.map(entryLine)
        }

    val (kept, truncated) = fit(chunks, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** A top-level definition in test sources, in another file than a suite's, whose members `used` the suite refers to. */
  final case class Fixture(top: Defn, used: Vector[Defn])

  /** The `tests` answer: the suite's `== file  package` line, flagged when `stale`; the lines of its file `src` outside
    * every one of `tops` (its file's top-level definitions) as file text; the suite's header line and head, its doc cut to
    * its first sentence as `show` cuts a member's; the suite's own loose lines; its helpers as their collapsed signatures;
    * the fixtures under one header line, one line per fixture name (a class and its companion on one) naming its
    * file, ranges and the members used, or with `listFixtures` each used member's collapsed signature under it; then `tests (n):` and each test's name with its line range; then the suite's closing
    * lines. A test whose name starts with one of `prefixes` prints its verbatim text after its line; a name in `bodies` prints a
    * helper's own text after its signature, or, when no helper has that name, a used fixture member's own text after the
    * fixtures. Each line naming a definition or a test ends with its size tag. `header` leads. The answer is cut at `cap`
    * bytes between entries, what `prefixes` and `bodies` name printed whatever `cap`, and ends with its KB line.
    */
  def tests(
      suite: Defn,
      tops: Vector[Defn],
      src: String,
      helpers: Vector[Defn],
      fixtures: Vector[Fixture],
      tests: Vector[TestCase],
      prefixes: Vector[String],
      bodies: Set[String],
      listFixtures: Boolean,
      header: String,
      cap: Int,
      stale: Boolean
  ): String = {
    def signatureOf(d: Defn): String = collapse(withoutDocs(d.signature))
    val lines = src.split("\n", -1).toVector

    val fileLoose = loose(lines, 1, lines.size, tops.map(_.lines)).flatMap { case (from, to) =>
      looseBlock(lines, from, to, "")
    }
    val opening = Piece(
      Vector(fileLine(suite.file, pkgOf(suite.fullName), stale)) ++ fileLoose ++
        Vector(headerLine(suite, inContext = true)) ++ briefHead(suite),
      ownerRelative(suite)
    )
    val headEnd = suite.lines.start + suite.head.linesIterator.size - 1
    val inside =
      if (suite.closing.isEmpty) Vector.empty
      else
        loose(lines, headEnd + 1, suite.lines.end - 1, suite.members.map(_.lines)).flatMap {
          case (from, to) =>
            looseBlock(lines, from, to, "  ")
        }
    val helperNames = helpers.map(_.name).toSet
    val helperPieces = helpers.sortBy(_.lines.start).map { h =>
      val named = bodies.contains(h.name)
      Piece(
        Vector(s"  ${h.lines.start}-${h.lines.end} ${signatureOf(h)}${sizeTag(h.size)}") ++
          (if (named) Vector(h.head) ++ h.body.toVector else Vector.empty),
        h.name,
        asked = named
      )
    }
    // One line per fixture name: a class and its companion are one fixture, their ranges side by side.
    val byName =
      fixtures.map(_.top.fullName).distinct.map(n => fixtures.filter(_.top.fullName == n))
    val fixtureLines = byName.flatMap { same =>
      val used = same.flatMap(_.used).sortBy(_.lines.start)
      val where = same.headOption.fold("") { f =>
        s"${f.top.fullName}  ${f.top.file}:${same.map(g => s"${g.top.lines.start}-${g.top.lines.end}").mkString(", ")}"
      }
      if (listFixtures)
        Vector(s"    $where") ++
          used
            .map(m => s"      ${m.lines.start}-${m.lines.end} ${signatureOf(m)}${sizeTag(m.size)}")
      else Vector(s"    $where  ${used.map(_.name).distinct.mkString(", ")}")
    }
    val fixtureHeader =
      if (listFixtures) "  fixtures:" else "  fixtures (--fixtures for their signatures):"
    val fixturePieces =
      (if (fixtureLines.isEmpty) Vector.empty
       else Vector(Piece(fixtureHeader +: fixtureLines, "fixtures"))) ++
        fixtures
          .flatMap(_.used)
          .filter(m => bodies.contains(m.name) && !helperNames.contains(m.name))
          .map { m =>
            Piece(
              Vector(headerLine(m, inContext = false), m.head) ++ m.body.toVector,
              m.name,
              asked = true
            )
          }
    val testPieces = tests.map { t =>
      val line = s"    ${t.lines.start}-${t.lines.end} ${t.name}${sizeTag(Some(bytesOf(t.text)))}"
      val named = prefixes.exists(t.name.startsWith)
      Piece(if (named) Vector(line, t.text) else Vector(line), t.name, asked = named)
    }
    val closing = closingLines(suite)
    val pieces: Vector[Piece] =
      Vector(opening) ++ (if (inside.isEmpty) Vector.empty
                          else Vector(Piece(inside, ownerRelative(suite)))) ++
        helperPieces ++ fixturePieces ++ Vector(
          Piece(Vector(s"  tests (${tests.size}):"), "")
        ) ++ testPieces ++
        (if (closing.isEmpty) Vector.empty else Vector(Piece(closing, ownerRelative(suite))))

    val keep = keptUnder(pieces, cap)
    val kept = pieces.zip(keep).collect { case (p, true) => p.lines }.flatten
    val body =
      (Vector(header) ++ kept ++ cutBetween(pieces.map(Vector(_)), keep, cap, hint = false))
        .map(_ + "\n")
        .mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** The doc's first sentence: its text without the comment delimiters, whitespace collapsed, cut after the first `. ` and at 160 characters; none when the doc has no text. */
  private def firstSentence(doc: Option[String]): Option[String] = sentenceOf(doc).map(_.take(160))

  /** The doc's first sentence, uncut: its text without the comment delimiters, whitespace collapsed, through the first `. `;
    * none when the doc has no text.
    */
  private def sentenceOf(doc: Option[String]): Option[String] =
    doc
      .map { d =>
        val text = collapse(
          d.replace("/**", " ")
            .replace("*/", " ")
            .linesIterator
            .map(_.trim.stripPrefix("*"))
            .mkString(" ")
        )
        text.indexOf(". ") match {
          case -1 => text
          case i => text.take(i + 1)
        }
      }
      .filter(_.nonEmpty)

  /** The `file` answer for `file`, whose source is `src` and whose top-level definitions are `tops`: its `== file` line,
    * flagged when `stale`, with its line count and size; then, in line order, each run of lines outside every top-level
    * definition (`loose`) and each top-level definition as `NN-MM <kind> <name>` and its size tag, with its members one
    * level down and the loose lines inside it among them; then its last lines under an `NN-MM end of file` line, its last
    * line or its last two when the last holds only closing brackets. Cut at `cap` bytes between top-level definitions.
    */
  def file(file: String, tops: Vector[Defn], src: String, stale: Boolean, cap: Int): String = {
    val lines = src.split("\n", -1).toVector.reverse.dropWhile(_.isEmpty).reverse
    val ordered = tops.filter(_.kind != Kind.Param).sortBy(_.lines.start)
    val pkg = ordered.headOption.fold("")(d => pkgOf(d.fullName))
    val heading =
      s"${fileLine(file, pkg, stale)}  ${lines.size} lines${sizeTag(Some(bytesOf(src)))}"

    def placed(d: Defn, depth: String): String =
      s"$depth${d.lines.start}-${d.lines.end} ${kindWord(d.kind)} ${d.name}${sizeTag(d.size)}"

    // A type's members, one level down, among the loose lines inside it: those after its head and before its last line.
    def withMembers(d: Defn): Vector[String] = {
      val members = d.members.filter(_.kind != Kind.Param).sortBy(_.lines.start)
      val headEnd = d.lines.start + d.head.linesIterator.size - 1
      val inside =
        if (!isType(d) || d.closing.isEmpty) Vector.empty
        else
          loose(lines, headEnd + 1, d.lines.end - 1, members.map(_.lines)).map(b =>
            (b._1, b._2, "  ")
          )
      val entries =
        members.map(m => m.lines.start -> Vector(placed(m, "  "))) ++
          inside.map { case (from, to, depth) => from -> looseBlock(lines, from, to, depth) }
      Vector(placed(d, "")) ++ entries.sortBy(_._1).flatMap(_._2)
    }

    val last = lines.size
    val endFrom =
      if (last > 1 && lines.lift(last - 1).exists(_.trim.matches("""[)}\]]+"""))) last - 1 else last
    val outside = loose(lines, 1, last, ordered.map(_.lines))
    val body: Vector[(Int, Vector[String])] =
      ordered.map(d => d.lines.start -> withMembers(d)) ++
        outside.map { case (from, to) => from -> looseBlock(lines, from, to, "") }
    val chunks = body.sortBy(_._1).map(_._2) :+
      (Vector(s"$endFrom-$last end of file") ++ lines.slice(endFrom - 1, last))
    val (kept, truncated) = fit(chunks.headOption.map(heading +: _).toVector ++ chunks.drop(1), cap)
    val text = (kept ++ truncated).map(_ + "\n").mkString
    text + s"[${kilobytes(bytesOf(text))} KB]"
  }

  /** The runs of `lines` (1-based) from `from` to `to` that no range of `covered` holds, each trimmed of its leading and
    * trailing blank lines, its inner ones kept, as first and last line numbers; a run of blank lines alone is none.
    */
  def loose(
      lines: Vector[String],
      from: Int,
      to: Int,
      covered: Vector[Lines]
  ): Vector[(Int, Int)] = {
    val free = (from to to).filterNot(n => covered.exists(r => r.start <= n && n <= r.end)).toVector
    val runs = free.foldLeft(Vector.empty[Vector[Int]]) { (acc, n) =>
      acc.lastOption match {
        case Some(run) if run.lastOption.contains(n - 1) => acc.dropRight(1) :+ (run :+ n)
        case _ => acc :+ Vector(n)
      }
    }
    def blank(n: Int): Boolean = lines.lift(n - 1).forall(_.trim.isEmpty)
    runs.flatMap { run =>
      val kept = run.dropWhile(blank).reverse.dropWhile(blank).reverse
      kept.headOption.zip(kept.lastOption)
    }
  }

  /** The loose lines `from` to `to` of `lines`, under their `NN-MM` line at `depth`, the lines themselves verbatim. */
  private def looseBlock(lines: Vector[String], from: Int, to: Int, depth: String): Vector[String] =
    s"$depth$from-$to" +: lines.slice(from - 1, to)

  /** One file of `show`'s tests section: the full names of its suites, and its entries in line order. */
  final case class Exercised(file: String, suites: Vector[String], entries: Vector[Exercise])

  /** One test or helper that holds a reference to a shown name, at `lines`; `label` is `Suite: test name` or `Suite: helper name`. */
  final case class Exercise(lines: Lines, label: String)

  /** `s` as `show`'s tests line: `-- tests of <name>: N in M suites (nearest: A, B, C); --tests to list`, the name without
    * its package; `-- tests of <name>: none` when no suite holds a reference to it.
    */
  def testsLine(s: TestsSummary): String = {
    val name = ownerRelativeName(s.symbol)
    if (s.suites == 0) s"-- tests of $name: none"
    else {
      val suites = if (s.suites == 1) "1 suite" else s"${s.suites} suites"
      s"-- tests of $name: ${s.tests} in $suites (nearest: ${s.nearest.mkString(", ")}); --tests to list"
    }
  }

  /** The tests section of `show`, one chunk per file: the `## tests exercising <syms>` line leads the first, then each file's `== file  package` line and its entries as `NN-MM` then their label. Empty when no file has an entry. */
  def exercising(syms: String, files: Vector[Exercised]): Vector[Vector[String]] = {
    val header = s"## tests exercising $syms"
    files.zipWithIndex.map { case (f, i) =>
      val lines =
        Vector(s"== ${f.file}  ${f.suites.map(pkgOf).distinct.mkString(", ")}") ++
          f.entries.map(e => s"  ${e.lines.start}-${e.lines.end} ${e.label}")
      if (i == 0) header +: lines else lines
    }
  }

  /** The leading `chunks` whose bytes, each line with its newline, fit in `cap`, then the cut line when some do not. */
  private def fit(chunks: Vector[Vector[String]], cap: Int): (Vector[String], Vector[String]) = {
    val shown = fitting(chunks, cap)
    (chunks.take(shown).flatten, cut(chunks, shown, cap))
  }

  /** How many leading `chunks` fit in `cap` bytes, each line with its newline. */
  private def fitting(chunks: Vector[Vector[String]], cap: Int): Int =
    chunks.map(chunkBytes).scanLeft(0)(_ + _).drop(1).takeWhile(_ <= cap).length

  private def chunkBytes(c: Vector[String]): Int = bytesOf(c.mkString("\n") + "\n")

  /** The line saying `chunks` were cut after the first `shown`: `-- cut at N KB: M more entries, ~X KB; rerun with
    * --max-kb X or narrow`, X the KB all of them need, rounded up; none when every chunk is shown.
    */
  private def cut(chunks: Vector[Vector[String]], shown: Int, cap: Int): Vector[String] =
    if (shown >= chunks.size) Vector.empty
    else {
      val whole = kilobytesUp(chunks.map(chunkBytes).sum)
      Vector(
        s"-- cut at ${budgetOf(cap)} KB: ${counted(chunks.size - shown, "more entry", "more entries")}, ~$whole KB; rerun with --max-kb $whole or narrow"
      )
    }

  /** One piece of an entry the budget may cut after: its lines, the owner-relative name of what it shows, and whether
    * it was asked for by name, which a budget never cuts.
    */
  private final case class Piece(lines: Vector[String], name: String, asked: Boolean = false)

  /** Which of `pieces` print under `cap` bytes: every asked one, then the others in order while they fit in what the
    * asked ones leave, none after the first that does not.
    */
  private def keptUnder(pieces: Vector[Piece], cap: Int): Vector[Boolean] = {
    val sized = pieces.map(p => (chunkBytes(p.lines), p.asked))
    val left = cap - sized.collect { case (bytes, true) => bytes }.sum
    val spent = sized
      .scanLeft(0) { case (total, (bytes, asked)) => if (asked) total else total + bytes }
      .drop(1)
    val over = sized.zip(spent).indexWhere { case ((_, asked), total) => !asked && total > left }
    sized.zipWithIndex.map { case ((_, asked), i) => asked || over < 0 || i < over }
  }

  /** The line saying which pieces of `entries` were cut, `keep` flagging each piece in order; none when every piece is
    * kept. Inside an entry, after a piece: `-- cut at N KB after <last piece kept before the cut>: M more members[ and E
    * more entries], ~R KB; rerun with --max-kb X[, or --body <member> for one]`, R the KB of the pieces cut, X the KB of
    * all, the `--body` hint only with `hint`; otherwise `cut`'s line, counting the entries cut whole.
    */
  private def cutBetween(
      entries: Vector[Vector[Piece]],
      keep: Vector[Boolean],
      cap: Int,
      hint: Boolean
  ): Vector[String] = {
    val starts = entries.map(_.size).scanLeft(0)(_ + _)
    val marked = entries.zip(starts).map { case (e, at) => e.zip(keep.slice(at, at + e.size)) }
    val bytes = entries.flatten.map(p => chunkBytes(p.lines))
    val all = kilobytesUp(bytes.sum)
    val whole = counted(marked.count(!_.exists(_._2)), "more entry", "more entries")
    val members = marked.filter(_.exists(_._2)).map(_.count(!_._2)).sum
    marked.find(_.exists(!_._2)) match {
      case None => Vector.empty
      case Some(cutIn) if cutIn.exists(_._2) =>
        val after = cutIn.takeWhile(_._2).lastOption.fold("")(_._1.name)
        val later = marked.count(!_.exists(_._2))
        val more =
          counted(members, "more member", "more members") + (if (later > 0) s" and $whole" else "")
        val cutBytes = bytes.zip(keep).collect { case (b, false) => b }.sum
        Vector(
          s"-- cut at ${budgetOf(cap)} KB after $after: $more, ~${kilobytesUp(cutBytes)} KB; rerun with --max-kb $all" +
            (if (hint) ", or --body <member> for one" else "")
        )
      case Some(_) =>
        val alsoMembers =
          if (members > 0) " and " + counted(members, "more member", "more members") else ""
        Vector(
          s"-- cut at ${budgetOf(cap)} KB: $whole$alsoMembers, ~$all KB; rerun with --max-kb $all or narrow"
        )
    }
  }

  private def budgetOf(cap: Int): String =
    if (cap % 1024 == 0) (cap / 1024).toString else kilobytes(cap)

  private def kilobytesUp(bytes: Int): Int = (bytes + 1023) / 1024

  private def counted(n: Int, one: String, many: String): String =
    if (n == 1) s"1 $one" else s"$n $many"

  /** Whether `d` is a class, trait, object or enum: a definition an Edit may add a member to. */
  private def isType(d: Defn): Boolean =
    d.kind == Kind.Class || d.kind == Kind.CaseClass || d.kind == Kind.Trait ||
      d.kind == Kind.Object || d.kind == Kind.Enum

  private def collapse(s: String): String =
    s.trim.split("\\s+").mkString(" ").replaceAll("([(\\[]) ", "$1").replaceAll(" ([)\\]])", "$1")

  private def withoutDocs(s: String): String = s.replaceAll("(?s)/\\*\\*.*?\\*/", " ")

  /** The words a case or class header carries before its name and constructor, dropped from a one-liner. */
  private val Modifiers =
    Set("case", "final", "sealed", "abstract", "open", "implicit", "private", "protected", "class")

  private def caseItem(d: Defn): String = {
    // A case Read found no signature for falls back to its name, which is what the signature would be without `case `.
    val signature = collapse(withoutDocs(d.signature))
    val source = if (signature.isEmpty) d.name else signature
    val cut = source.indexOf(" extends ")
    val header = if (cut >= 0) source.take(cut) else source
    // An access modifier between the name and the constructor, `Impl private (`, is dropped with the space before the `(`.
    val bare = header
      .split(' ')
      .dropWhile(Modifiers.contains)
      .mkString(" ")
      .replaceFirst("\\s+(private|protected)(\\[[^\\]]*\\])?\\s*(?=\\()", "")
    s"$bare :${d.lines.start}"
  }

  private def bytesOf(s: String): Int = s.getBytes(StandardCharsets.UTF_8).length

  /** The `NN-MM` line over `d`'s file text, ending with its size tag: its kind and owner-relative name between them unless
    * `inContext` (`d` is top-level, or shown under its owner) and its signature already shows its kind and name.
    */
  private def headerLine(d: Defn, inContext: Boolean): String = {
    val lines = s"${d.lines.start}-${d.lines.end}"
    if (inContext && namesItself(d)) s"$lines${sizeTag(d.size)}"
    else s"$lines ${kindWord(d.kind)} ${ownerRelative(d)}${sizeTag(d.size)}"
  }

  /** Whether `d`'s head is file text whose lines after its doc hold its kind's keyword then its name, as in `def at`,
    * `case class Lines` or `case Hearts, Spades` for `Hearts` (not for `Spades`).
    */
  private def namesItself(d: Defn): Boolean = d.size.nonEmpty && {
    val kind = kindWord(d.kind).split(' ').map(Pattern.quote).mkString("\\s+")
    val code = d.head.linesIterator.drop(d.doc.fold(0)(_.linesIterator.size)).mkString("\n")
    s"(?<![\\w$$])$kind\\s+${Pattern.quote(d.name)}(?![\\w$$])".r.findFirstIn(code).nonEmpty
  }

  /** `d`'s head with its doc cut to its first sentence: when that sentence is the whole of the doc's first line, that line
    * then a `-- … N more doc lines` line; otherwise a `-- <first sentence> …` line in place of the doc; then the head's
    * lines after the doc. Whole when the doc is one line, when there is none, or when code follows the doc on its last line.
    */
  private def briefHead(d: Defn): Vector[String] = {
    val head = d.head.linesIterator.toVector
    val docLines = d.doc.fold(0)(_.linesIterator.size)
    val endsClean = head.lift(docLines - 1).exists(_.trim.endsWith("*/"))
    if (docLines <= 1 || head.size <= docLines || !endsClean) Vector(d.head)
    else {
      val sentence = sentenceOf(d.doc).getOrElse("")
      val firstLine = head.headOption.getOrElse("")
      val firstText = collapse(firstLine.trim.stripPrefix("/**"))
      val doc =
        if (firstText.nonEmpty && firstText == sentence)
          Vector(firstLine, s"-- … ${docLines - 1} more doc lines")
        else Vector(s"-- $sentence …")
      doc ++ head.drop(docLines)
    }
  }

  /** `d`'s closing lines under an `NN-MM end of <name>` header, for a type printed without its body, where an Edit adds a
    * member; none for any other definition, or a type that has none.
    */
  private def closingLines(d: Defn): Vector[String] =
    d.closing.toVector
      .filter(_ => isType(d))
      .flatMap(c => Vector(s"${c.start}-${d.lines.end} end of ${ownerRelative(d)}", c.text))

  /** What a definition line ends with: its `size` in bytes as `  [N.N KB]`, to one decimal and at least 0.2; nothing when the size is unknown. */
  private def sizeTag(size: Option[Int]): String =
    size.fold("") { bytes =>
      val kb = (BigDecimal(bytes) / 1024).setScale(1, BigDecimal.RoundingMode.HALF_UP)
      s"  [${kb.max(BigDecimal("0.2"))} KB]"
    }

  private def kilobytes(bytes: Int): String =
    (BigDecimal(bytes) / 1024).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
}
