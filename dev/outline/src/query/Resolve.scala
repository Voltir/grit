package grit.outline.query

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.model.Defn

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

  /** The `.tasty` a `scope` searches among `tasty`: `Scope.Main` drops the classes directories holding only tests' classes. */
  private def usable(
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

  /** `sym`'s definitions under `root`: those whose fullName is exactly `sym`; else those whose fullName ends with `.sym` (when more than one distinct fullName, the note `-- ambiguous <sym>: <full names, comma-separated> (name one fully)`); else, by `sym`'s last segment among the packages of files mentioning it. When `sym` has a qualifier, that step keeps the candidates whose owner (the fullName without its last segment) has the qualifier's last segment as its simple name; when none does, every candidate is listed and nothing resolves. One distinct fullName left resolves to its definitions, with the note `-- no <sym> as written; by last segment: <full names>`; more than one, or a qualifier that matches no candidate's owner, resolves to none, with the note `-- no <sym> as written; by last segment, name one fully: <full names, comma-separated>`. Only an exact match sees `config.hidden` sources; `Scope.Main` skips test-only classes dirs. Empty `defns` when nothing matches. */
  def resolve(
      root: Root,
      layout: Layout,
      config: Config,
      scope: Scope,
      sym: String,
      in: Loaded
  ): Either[String, Resolved] = {
    def hidden(d: Defn): Boolean = config.hidden.exists(prefix => d.file.startsWith(prefix))
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
          val names = suffix.map(_.fullName).distinct.sorted
          val notes =
            if (names.size > 1)
              Vector(s"-- ambiguous $sym: ${names.mkString(", ")} (name one fully)")
            else Vector.empty
          Right(Resolved(suffix, notes, loaded, tops))
        } else {
          val last = simple(sym)
          val classes = layout.classesDirs(root)
          val tasty = usable(
            root,
            layout,
            scope,
            Locate
              .mentioning(root, last)
              .flatMap(file => Locate.inPackageIn(root, classes, file))
              .distinct
          )
          load(tasty, loaded).map { case (more, after) =>
            val byLast = everyDefn(more).filter(d => simple(d.fullName) == last && !hidden(d))
            val qualifier = sym.split('.').toVector.dropRight(1).lastOption
            val owned = qualifier.fold(byLast)(q => byLast.filter(d => simple(owner(d)) == q))
            // A qualifier naming no candidate's owner resolves to none; its note lists them all.
            val unmatched = qualifier.nonEmpty && owned.isEmpty
            val listed = if (unmatched) byLast else owned
            val names = listed.map(_.fullName).distinct.sorted
            val notes =
              if (listed.isEmpty) Vector(s"-- no match for $sym")
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
