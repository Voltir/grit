package grit.outline.render

import java.nio.charset.StandardCharsets

import grit.outline.model.{Defn, Kind, TestCase, Use}
import grit.outline.trace.Traced

/** A trait's family: the trait, its implementations, and each contract with the suites extending it. */
final case class Family(
    trait0: Defn,
    member: Option[String],
    impls: Vector[Defn],
    contracts: Vector[(Defn, Vector[Defn])],
    withBody: Boolean
)

object Render {

  /** One line for a traced type: its lines and collapsed signature, then for an Enum its cases, and for a Trait or abstract Class the members of `companions` that extend it, each as its collapsed signature and start line. */
  def oneLine(d: Defn, companions: Vector[Defn]): String = {
    val head = s"→ ${d.lines.start}-${d.lines.end} ${collapse(withoutDocs(d.signature))}"
    val items: Vector[Defn] =
      if (d.kind == Kind.Enum) d.members.filter(_.kind == Kind.EnumCase)
      else if (d.kind == Kind.Trait || (d.kind == Kind.Class && d.isAbstract))
        companions.flatMap(c => c +: c.members).filter(_.parents.contains(d.fullName))
      else Vector.empty
    if (items.isEmpty) head
    else head + ": " + items.map(caseItem).mkString(" | ")
  }

  /** The `show` answer: `named` (verbatim; a named type with its public members, or all with `withPrivate`), `bodies` (members whose `name` is in it get their body), the traced types as one-liners, grouped by file; `stale` files' headers flagged; cut at `cap` bytes. */
  def show(
      named: Vector[Defn],
      traced: Traced,
      companions: Vector[Defn],
      stale: Set[String],
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int
  ): String = {
    val everything = named ++ traced.types
    val files = everything.map(_.file).distinct

    def entry(d: Defn): Vector[String] =
      Vector(s"${d.lines.start}-${d.lines.end} ${kindWord(d.kind)} ${ownerRelative(d)}") ++
        d.doc.toVector ++
        Vector(d.signature) ++
        (if (bodies.contains(d.name)) d.body.toVector else Vector.empty)

    def namedEntry(d: Defn): Vector[String] = {
      val typeKind = d.kind == Kind.Class || d.kind == Kind.CaseClass || d.kind == Kind.Trait ||
        d.kind == Kind.Object || d.kind == Kind.Enum
      val members =
        if (typeKind) d.members.filter(m => withPrivate || !m.isPrivate).flatMap(entry)
        else Vector.empty
      entry(d) ++ members
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
      val staleTag = if (stale.contains(f)) "  [stale: source newer than .tasty]" else ""
      val header = s"== $f  $pkg$staleTag"
      entries.zipWithIndex.map { case (lines, i) => if (i == 0) header +: lines else lines }
    }

    val total = named.size + traced.types.size
    val (kept, truncated) = fit(chunks, total, cap)

    val library = traced.library.filterNot(_.contains("<"))
    val notLoaded = traced.missing
    val trailer =
      (if (library.nonEmpty) Vector(s"-- library: ${library.mkString(", ")}") else Vector.empty) ++
        (if (notLoaded.nonEmpty) Vector(s"-- not loaded: ${notLoaded.mkString(", ")}")
         else Vector.empty)
    val body = (kept ++ trailer ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  private def pkgOf(fullName: String): String =
    fullName.split('.').takeWhile(s => s.nonEmpty && s.charAt(0).isLower).mkString(".")

  private def ownerRelative(d: Defn): String = {
    val pkg = pkgOf(d.fullName)
    if (pkg.isEmpty) d.fullName else d.fullName.drop(pkg.length + 1)
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
    case Kind.Given => "given"
    case Kind.TypeAlias => "type"
    case Kind.Opaque => "opaque type"
  }

  /** The `family` answer: `f`'s trait, then each implementation as one line, then each contract with its non-private members and the suites that run it, grouped by file under the `header` line; cut at `cap` bytes. With `f.member`, the trait shows that member (its body with `f.withBody`) and each implementation its overloads of it (or their bodies). */
  def family(f: Family, all: Vector[Defn], header: String, stale: Set[String], cap: Int): String = {
    def headerOf(d: Defn): String =
      s"${d.lines.start}-${d.lines.end} ${kindWord(d.kind)} ${ownerRelative(d)}"

    def block(d: Defn, body: Boolean): Vector[String] =
      Vector(headerOf(d)) ++ d.doc.toVector ++ Vector(d.signature) ++
        (if (body) d.body.toVector else Vector.empty)

    def signatureOf(d: Defn): String = collapse(withoutDocs(d.signature))

    val traitLines: Vector[String] = f.member match {
      case None =>
        block(f.trait0, false) ++ f.trait0.members.filter(!_.isPrivate).flatMap(block(_, false))
      case Some(m) =>
        Vector(headerOf(f.trait0), f.trait0.signature) ++
          f.trait0.members.filter(_.name == m).flatMap(block(_, f.withBody))
    }

    def implLines(d: Defn): Vector[String] = {
      val head = s"${headerOf(d)}  ${signatureOf(d)}"
      f.member match {
        case None => Vector(head)
        case Some(m) =>
          val overloads = d.members.filter(_.name == m)
          if (f.withBody)
            Vector(head) ++ overloads.flatMap(o => Vector(o.signature) ++ o.body.toVector)
          else Vector(head) ++ overloads.map(o => s"  ${o.lines.start}-${o.lines.end} $m")
      }
    }

    def contractLines(c: Defn, suites: Vector[Defn]): Vector[String] = {
      val runBy =
        if (suites.isEmpty) "  run by nothing"
        else s"  run by ${suites.map(s => s"${s.file}:${s.lines.start}").mkString(", ")}"
      Vector(s"${c.lines.start}-${c.lines.end} contract ${ownerRelative(c)}  ${signatureOf(c)}") ++
        c.members
          .filter(!_.isPrivate)
          .map(m => s"  ${m.lines.start}-${m.lines.end} ${signatureOf(m)}") ++
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

    val (kept, truncated) = fit(chunks, pieces.size, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** The chunks that fit in `cap` bytes, flattened, and the truncation line when some do not; `total` counts the entries the line reports. */
  /** The `uses` answer: each site as `line  in enclosing   text`, grouped by file under `== file`, then the site and file counts; `overloaded` names the targets whose sites add the `[→ name :line]` marker to their target's definition line; cut at `cap` bytes. */
  def uses(sites: Vector[Use], overloaded: Set[String], cap: Int): String = {
    def siteLine(u: Use): String = {
      val pkg = pkgOf(u.enclosing)
      val enclosing = if (pkg.isEmpty) u.enclosing else u.enclosing.drop(pkg.length + 1)
      val marker =
        if (overloaded.contains(u.target)) s"  [→ ${u.target.split('.').last} :${u.targetLine}]"
        else ""
      s"  ${u.line}  in $enclosing$marker   ${u.text}"
    }
    val files = sites.map(_.file).distinct
    val chunks: Vector[Vector[String]] = files.map { f =>
      s"== $f" +: sites.filter(_.file == f).map(siteLine)
    }
    val (kept, truncated) = fit(chunks, sites.size, cap)
    val summary = s"[${sites.size} sites in ${files.size} files]"
    val body = (kept ++ truncated :+ summary).map(_ + "\n").mkString
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
    val (kept, truncated) = fit(chunks, total, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** The `tests` answer: the suite's `== file  package` block with its own line, then its non-private helpers as their collapsed signatures, then `tests (n):` and each test's name with its line range; a test whose name starts with `prefix` also prints its verbatim text after its line. `header` leads; the answer is cut at `cap` bytes and ends with its KB line. */
  def tests(
      suite: Defn,
      helpers: Vector[Defn],
      tests: Vector[TestCase],
      prefix: Option[String],
      header: String,
      cap: Int
  ): String = {
    def signatureOf(d: Defn): String = collapse(withoutDocs(d.signature))

    val fileLine = s"== ${suite.file}  ${pkgOf(suite.fullName)}"
    val suiteLine =
      s"${suite.lines.start}-${suite.lines.end} ${kindWord(suite.kind)} ${ownerRelative(suite)}  ${signatureOf(suite)}"
    val helperChunks = helpers.sortBy(_.lines.start).map { h =>
      Vector(s"  ${h.lines.start}-${h.lines.end} ${signatureOf(h)}")
    }
    val testChunks = tests.map { t =>
      val line = s"    ${t.lines.start}-${t.lines.end} ${t.name}"
      if (prefix.exists(t.name.startsWith)) Vector(line, t.text) else Vector(line)
    }
    val chunks: Vector[Vector[String]] =
      Vector(Vector(fileLine, suiteLine)) ++ helperChunks ++
        Vector(Vector(s"  tests (${tests.size}):")) ++ testChunks

    val (kept, truncated) = fit(chunks, chunks.size, cap)
    val body = (Vector(header) ++ kept ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
  }

  /** The doc's first sentence: its text without the comment delimiters, whitespace collapsed, cut after the first `. ` and at 160 characters; none when the doc has no text. */
  private def firstSentence(doc: Option[String]): Option[String] =
    doc
      .map { d =>
        val text = collapse(
          d.replace("/**", " ")
            .replace("*/", " ")
            .linesIterator
            .map(_.trim.stripPrefix("*"))
            .mkString(" ")
        )
        val sentence = text.indexOf(". ") match {
          case -1 => text
          case i => text.take(i + 1)
        }
        sentence.take(160)
      }
      .filter(_.nonEmpty)

  private def fit(
      chunks: Vector[Vector[String]],
      total: Int,
      cap: Int
  ): (Vector[String], Vector[String]) = {
    val sizes = chunks.map(c => bytesOf(c.mkString("\n") + "\n")).scanLeft(0)(_ + _).drop(1)
    val shown = sizes.takeWhile(_ <= cap).length
    val kept = chunks.take(shown).flatten
    val truncated =
      if (shown < chunks.size) Vector(s"[truncated: $shown of $total entries; narrow the query]")
      else Vector.empty
    (kept, truncated)
  }

  private def collapse(s: String): String = s.trim.split("\\s+").mkString(" ")

  private def withoutDocs(s: String): String = s.replaceAll("/\\*\\*.*?\\*/", " ")

  /** The words a case or class header carries before its name and constructor, dropped from a one-liner. */
  private val Modifiers =
    Set("case", "final", "sealed", "abstract", "open", "implicit", "private", "protected", "class")

  private def caseItem(d: Defn): String = {
    // Read leaves a parameterless enum case's signature empty; its name is what the signature would be without `case `.
    val signature = collapse(withoutDocs(d.signature))
    val source = if (signature.isEmpty) d.name else signature
    val cut = source.indexOf(" extends ")
    val header = if (cut >= 0) source.take(cut) else source
    val bare = header.split(' ').dropWhile(Modifiers.contains).mkString(" ")
    s"$bare :${d.lines.start}"
  }

  private def bytesOf(s: String): Int = s.getBytes(StandardCharsets.UTF_8).length

  private def kilobytes(bytes: Int): String =
    (BigDecimal(bytes) / 1024).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
}
