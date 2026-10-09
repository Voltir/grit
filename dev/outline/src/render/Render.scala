package grit.outline.render

import java.nio.charset.StandardCharsets

import grit.outline.model.{Defn, Kind}
import grit.outline.trace.Traced

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

    def pkgOf(fullName: String): String =
      fullName.split('.').takeWhile(s => s.nonEmpty && s.charAt(0).isLower).mkString(".")

    def ownerRelative(d: Defn): String = {
      val pkg = pkgOf(d.fullName)
      if (pkg.isEmpty) d.fullName else d.fullName.drop(pkg.length + 1)
    }

    def kindWord(k: Kind): String = k match {
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
    val sizes = chunks.map(c => bytesOf(c.mkString("\n") + "\n")).scanLeft(0)(_ + _).drop(1)
    val shown = sizes.takeWhile(_ <= cap).length
    val kept = chunks.take(shown).flatten

    val library = traced.library.filterNot(_.contains("<"))
    val notLoaded = traced.missing
    val trailer =
      (if (library.nonEmpty) Vector(s"-- library: ${library.mkString(", ")}") else Vector.empty) ++
        (if (notLoaded.nonEmpty) Vector(s"-- not loaded: ${notLoaded.mkString(", ")}")
         else Vector.empty)
    val truncated =
      if (shown < chunks.size) Vector(s"[truncated: $shown of $total entries; narrow the query]")
      else Vector.empty

    val body = (kept ++ trailer ++ truncated).map(_ + "\n").mkString
    body + s"[${kilobytes(bytesOf(body))} KB]"
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
