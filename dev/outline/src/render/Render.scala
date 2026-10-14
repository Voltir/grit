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

  /** A file's `== file  package` header line, flagged when its source is newer than its tasty. */
  def fileLine(file: String, pkg: String, stale: Boolean): String =
    s"== $file  $pkg" + staleTag(stale)

  /** What a file's `==` line ends with when its source is newer than its tasty, so what is printed from it may not match it. */
  private def staleTag(stale: Boolean): String =
    if (stale) "  [stale: source newer than .tasty]" else ""

  /** The `show` answer: `named` (verbatim; a named type with its public members, or all with `withPrivate`, but not its constructor vals, which its head holds), `bodies` (members whose `name` is in it get their body), the traced types as one-liners, grouped by file; each named definition's header line ends with its size tag; each definition printed without its body ends with its closing lines, a type's after its members; `stale` files' headers flagged; cut at `cap` bytes. */
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

    def entry(d: Defn, inContext: Boolean): Vector[String] =
      headed(d, inContext) ++
        (if (bodies.contains(d.name) && d.body.nonEmpty) d.body.toVector else closingLines(d))

    // A member not named in --body: its doc cut to its first sentence, unless every doc prints whole.
    def memberEntry(d: Defn): Vector[String] =
      if (fullDocs || bodies.contains(d.name)) entry(d, inContext = true)
      else Vector(headerLine(d, inContext = true)) ++ briefHead(d) ++ closingLines(d)

    def namedEntry(d: Defn): Vector[String] = {
      val typeKind = d.kind == Kind.Class || d.kind == Kind.CaseClass || d.kind == Kind.Trait ||
        d.kind == Kind.Object || d.kind == Kind.Enum
      val members =
        // A constructor val's lines are the type's own head, already shown.
        if (typeKind)
          d.members
            .filter(m => (withPrivate || !m.isPrivate) && m.kind != Kind.Param)
            .flatMap(memberEntry)
        else Vector.empty
      val topLevel = !ownerRelative(d).contains('.')
      if (typeKind) headed(d, topLevel) ++ members ++ closingLines(d) else entry(d, topLevel)
    }

    // Each entry is its lines; a file's header rides on its first entry, so a cut never leaves a bare header.
    val groups: Vector[Vector[Vector[String]]] = files.map { f =>
      val namedHere = named.filter(_.file == f).map(namedEntry)
      val tracedHere = traced.types
        .filter(_.file == f)
        .sortBy(_.lines.start)
        .map(d => Vector(oneLine(d, companions)))
      namedHere ++ tracedHere
    }

    val chunks: Vector[Vector[String]] = files.zip(groups).flatMap { case (f, entries) =>
      val pkg = everything.find(_.file == f).map(d => pkgOf(d.fullName)).getOrElse("")
      val header = fileLine(f, pkg, stale.contains(f))
      entries.zipWithIndex.map { case (lines, i) => if (i == 0) header +: lines else lines }
    }

    // One budget over the definitions and the tests list, so a cut line counts both.
    val shown = fitting(chunks ++ exercising, cap)

    val library = traced.library.filterNot(_.contains("<"))
    val notLoaded = traced.missing
    val trailer =
      (if (library.nonEmpty) Vector(s"-- library: ${library.mkString(", ")}") else Vector.empty) ++
        (if (notLoaded.nonEmpty) Vector(s"-- not loaded: ${notLoaded.mkString(", ")}")
         else Vector.empty)
    val kept = chunks.take(shown).flatten ++ trailer ++ exercising.take(shown - chunks.size).flatten
    val body =
      (kept ++ cut(chunks ++ exercising, shown, cap) ++ summaries.map(testsLine))
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

  /** The `family` answer: `f`'s trait, then each implementation as one line, then each contract with its non-private members and the suites that run it, each definition's line ending with its size tag, grouped by file under the `header` line; cut at `cap` bytes. The trait ends with its closing lines, and each member it shows without its body with the member's. With `f.member`, the trait shows that member (its body with `f.withBody`) and each implementation its overloads of it (or their bodies). */
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

  /** The `find` answer under `header`: each of `found`, in its order, as `<full name>  <file>:<lines>  <collapsed signature>` and its size tag; cut at `cap` bytes. */
  def find(found: Vector[Defn], header: String, cap: Int): String = {
    val chunks = found.map { d =>
      Vector(
        s"${d.fullName}  ${d.file}:${d.lines.start}-${d.lines.end}  ${collapse(withoutDocs(d.signature))}${sizeTag(d.size)}"
      )
    }
    val (kept, truncated) = fit(chunks, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
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

    val total = if (level == 0) chunks.size else listed.size
    val (kept, truncated) = fit(chunks, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** The `tests` answer: the suite's `== file  package` block with its own line, then its non-private helpers as their collapsed signatures, then `tests (n):` and each test's name with its line range, each of these lines ending with its definition's size tag; a test whose name starts with `prefix` also prints its verbatim text after its line, and a helper named in `bodies` its own text after its signature. `header` leads; the file line is flagged when `stale`; the answer is cut at `cap` bytes and ends with its KB line. */
  def tests(
      suite: Defn,
      helpers: Vector[Defn],
      tests: Vector[TestCase],
      prefix: Option[String],
      bodies: Set[String],
      header: String,
      cap: Int,
      stale: Boolean
  ): String = {
    def signatureOf(d: Defn): String = collapse(withoutDocs(d.signature))

    val suiteLine =
      s"${suite.lines.start}-${suite.lines.end} ${kindWord(suite.kind)} ${ownerRelative(suite)}  ${signatureOf(suite)}${sizeTag(suite.size)}"
    val helperChunks = helpers.sortBy(_.lines.start).map { h =>
      Vector(s"  ${h.lines.start}-${h.lines.end} ${signatureOf(h)}${sizeTag(h.size)}") ++
        (if (bodies.contains(h.name)) Vector(h.head) ++ h.body.toVector else Vector.empty)
    }
    val testChunks = tests.map { t =>
      val line = s"    ${t.lines.start}-${t.lines.end} ${t.name}${sizeTag(Some(bytesOf(t.text)))}"
      if (prefix.exists(t.name.startsWith)) Vector(line, t.text) else Vector(line)
    }
    val chunks: Vector[Vector[String]] =
      Vector(
        Vector(fileLine(suite.file, pkgOf(suite.fullName), stale), suiteLine)
      ) ++ helperChunks ++
        Vector(Vector(s"  tests (${tests.size}):")) ++ testChunks

    val (kept, truncated) = fit(chunks, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
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
      val more = chunks.size - shown
      val whole = (chunks.map(chunkBytes).sum + 1023) / 1024
      val budget = if (cap % 1024 == 0) (cap / 1024).toString else kilobytes(cap)
      val entries = if (more == 1) "1 more entry" else s"$more more entries"
      Vector(s"-- cut at $budget KB: $entries, ~$whole KB; rerun with --max-kb $whole or narrow")
    }

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

  /** `d`'s closing lines under an `NN-MM end of <name>` header, for a definition printed without its body; none when it has none. */
  private def closingLines(d: Defn): Vector[String] =
    d.closing.toVector.flatMap(c =>
      Vector(s"${c.start}-${d.lines.end} end of ${ownerRelative(d)}", c.text)
    )

  /** What a definition line ends with: its `size` in bytes as `  [N.N KB]`, to one decimal and at least 0.2; nothing when the size is unknown. */
  private def sizeTag(size: Option[Int]): String =
    size.fold("") { bytes =>
      val kb = (BigDecimal(bytes) / 1024).setScale(1, BigDecimal.RoundingMode.HALF_UP)
      s"  [${kb.max(BigDecimal("0.2"))} KB]"
    }

  private def kilobytes(bytes: Int): String =
    (BigDecimal(bytes) / 1024).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
}
