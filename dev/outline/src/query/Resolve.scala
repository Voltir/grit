package grit.outline.query

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.model.{Defn, Kind}
import grit.outline.render.Render

/** Which classes a name is looked up in: `Main` skips the classes directories holding only tests' classes. */
enum Scope {
  case Main, WithTests
}

/** The definitions a query's name picked, notes for the reader, the cache after, and every top-level definition loaded. */
final case class Resolved(
    defns: Vector[Defn],
    notes: Vector[String],
    loaded: Loaded,
    seen: Vector[Defn]
)

object Resolve {

  /** Where a candidate for a short name lies, most likely meant first: a primary source, under one of `.outline.conf`'s `secondary` prefixes, or a test source. */
  private enum Standing {
    case Primary
    case Secondary(prefix: String)
    case Test

    def rank: Int = this match {
      case Primary => 0
      case Secondary(_) => 1
      case Test => 2
    }

    /** What a note says after a candidate's name; none for a primary one. */
    def tag: Option[String] = this match {
      case Primary => None
      case Secondary(prefix) => Some(s"secondary: $prefix")
      case Test => Some("test sources")
    }
  }

  private def standing(layout: Layout, config: Config, d: Defn): Standing =
    if (layout.isTestSource(os.RelPath(d.file))) Standing.Test
    else
      config.secondary
        .find(prefix => d.file.startsWith(prefix))
        .fold(Standing.Primary)(Standing.Secondary(_))

  /** The definitions `sym` picks among `suffix`, the definitions whose full name ends with `.sym`, and the notes saying so. */
  private def bySuffix(
      layout: Layout,
      config: Config,
      sym: String,
      suffix: Vector[Defn]
  ): (Vector[Defn], Vector[String]) = {
    val names = suffix.map(_.fullName).distinct
    if (names.sizeIs == 1) (suffix, Vector.empty)
    else {
      // One row per full name: a class or trait over its companion object.
      val rows = names
        .flatMap { name =>
          val same = suffix.filter(_.fullName == name)
          same.find(_.kind != Kind.Object).orElse(same.headOption)
        }
        .map(d => (d, standing(layout, config, d)))
        .sortBy { case (d, s) => (s.rank, d.fullName) }
      def tagged(name: String, s: Standing): String = s.tag.fold(name)(t => s"$name ($t)")
      rows.filter { case (_, s) => s == Standing.Primary } match {
        case Vector((chosen, _)) =>
          val others = rows.collect {
            case (d, s) if d.fullName != chosen.fullName => tagged(d.fullName, s)
          }
          (
            suffix.filter(_.fullName == chosen.fullName),
            Vector(
              s"-- $sym taken as ${chosen.fullName}, the only one outside test and secondary sources; also ${others
                  .mkString(", ")}: name it fully to read it"
            )
          )
        case _ =>
          (
            suffix,
            s"-- ambiguous $sym, name one fully:" +: rows.map { case (d, s) =>
              s"--   ${Render.listing(d)}${s.tag.fold("")(t => s"  ($t)")}"
            }
          )
      }
    }
  }

  /** The `.tasty` a `scope` searches among `tasty`: `Scope.Main` drops the classes directories holding only tests' classes. */
  private[query] def usable(
      root: Root,
      layout: Layout,
      scope: Scope,
      tasty: Vector[os.Path]
  ): Vector[os.Path] =
    scope match {
      case Scope.Main =>
        val testDirs = layout.classesDirs(root).filter(layout.isTest)
        tasty.filterNot(t => testDirs.exists(dir => t.startsWith(dir)))
      case Scope.WithTests => tasty
    }

  /** `in` with the top-level definitions of every name in `syms` read, in one inspector run; `in` unchanged when no name has a file to read. Afterwards each of those names resolves with no read. */
  def preload(
      root: Root,
      layout: Layout,
      scope: Scope,
      syms: Vector[String],
      in: Loaded
  ): Either[String, Loaded] = {
    val tasty = syms
      .flatMap(sym => usable(root, layout, scope, Locate.forTopLevel(root, layout, sym)))
      .distinct
    if (tasty.isEmpty) Right(in)
    else Loaded.defns(root, layout, tasty, in).map { case (_, next, _) => next }
  }

  /** `sym`'s definitions under `root`: those whose fullName is exactly `sym`; else those whose fullName ends with `.sym`: when they hold more than one distinct fullName and exactly one lies outside test sources and `config.secondary`, only that one's, with the note `-- <sym> taken as <it>, …; also <each other, tagged (test sources) or (secondary: <prefix>)>: …`; with more than one such, or none, all of them, with the note `-- ambiguous <sym>, name one fully:` and a `--   ` line per fullName as `Render.listing` gives it, tagged likewise, primary first; else, by `sym`'s last segment among the packages of files mentioning it. When `sym` has a qualifier, that step keeps the candidates whose owner (the fullName without its last segment) has the qualifier's last segment as its simple name; when none does, every candidate is listed and nothing resolves. One distinct fullName left resolves to its definitions, with the note `-- no <sym> as written; by last segment: <full names>`; more than one, or a qualifier that matches no candidate's owner, resolves to none, with the note `-- no <sym> as written; by last segment, name one fully: <full names, comma-separated>`. Only an exact match sees `config.hidden` sources; `Scope.Main` skips test-only classes dirs. Empty `defns` when nothing matches, with the note `-- no match for <sym>; try find *<last segment>*`. */
  def resolve(
      root: Root,
      layout: Layout,
      config: Config,
      scope: Scope,
      sym: String,
      in: Loaded
  ): Either[String, Resolved] = {
    def hidden(d: Defn): Boolean = config.hides(d.file)
    def everyDefn(ds: Vector[Defn]): Vector[Defn] = ds.flatMap(d => d +: everyDefn(d.members))
    def simple(name: String): String = name.split('.').last
    def owner(d: Defn): String = d.fullName.take(math.max(d.fullName.lastIndexOf('.'), 0))
    def load(tasty: Vector[os.Path], from: Loaded): Either[String, (Vector[Defn], Loaded)] =
      if (tasty.isEmpty) Right((Vector.empty, from))
      else Loaded.defns(root, layout, tasty, from).map { case (ds, next, _) => (ds, next) }

    load(usable(root, layout, scope, Locate.forTopLevel(root, layout, sym)), in).flatMap {
      case (tops, loaded) =>
        val all = everyDefn(tops)
        val exact = all.filter(_.fullName == sym)
        val suffix = all.filter(d => d.fullName.endsWith("." + sym) && !hidden(d))
        if (exact.nonEmpty) Right(Resolved(exact, Vector.empty, loaded, tops))
        else if (suffix.nonEmpty) {
          val (defns, notes) = bySuffix(layout, config, sym, suffix)
          Right(Resolved(defns, notes, loaded, tops))
        } else {
          val last = simple(sym)
          val classes = layout.classesDirs(root)
          val (mentioning, words) = Locate.mentioning(root, last, loaded.words)
          val tasty = usable(
            root,
            layout,
            scope,
            mentioning.flatMap(Locate.inPackagesIn(root, classes)).distinct
          )
          load(tasty, loaded.copy(words = words)).map { case (more, after) =>
            val byLast = everyDefn(more).filter(d => simple(d.fullName) == last && !hidden(d))
            val qualifier = sym.split('.').toVector.dropRight(1).lastOption
            val owned = qualifier.fold(byLast)(q => byLast.filter(d => simple(owner(d)) == q))
            // A qualifier naming no candidate's owner resolves to none; its note lists them all.
            val unmatched = qualifier.nonEmpty && owned.isEmpty
            val listed = if (unmatched) byLast else owned
            val names = listed.map(_.fullName).distinct.sorted
            val notes =
              if (listed.isEmpty) Vector(s"-- no match for $sym; try find *$last*")
              else if (names.size == 1 && !unmatched)
                Vector(s"-- no $sym as written; by last segment: ${names.mkString(", ")}")
              else
                Vector(
                  s"-- no $sym as written; by last segment, name one fully: ${names.mkString(", ")}"
                )
            val defns = if (names.size == 1 && !unmatched) owned else Vector.empty
            Resolved(defns, notes, after, (tops ++ more).distinct)
          }
        }
    }
  }
}
