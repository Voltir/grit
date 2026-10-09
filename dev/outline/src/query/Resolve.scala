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

  /** `sym`'s definitions under `root`: those whose fullName is exactly `sym`; else those whose fullName ends with `.sym` (when more than one distinct fullName, the note `-- ambiguous <sym>: <full names, comma-separated> (name one fully)`); else, by `sym`'s last segment alone among the packages of files mentioning it, the note `-- no <sym> as written; by last segment: <full names>`. Only an exact match sees `config.hidden` sources; `Scope.Main` skips test-only classes dirs. Empty `defns` when nothing matches. */
  def resolve(
      root: Root,
      layout: Layout,
      config: Config,
      scope: Scope,
      sym: String,
      in: Loaded
  ): Either[String, Resolved] = {
    val testDirs = layout.classesDirs(root).filter(layout.isTest)
    def usable(tasty: Vector[os.Path]): Vector[os.Path] = scope match {
      case Scope.Main => tasty.filterNot(t => testDirs.exists(dir => t.startsWith(dir)))
      case Scope.WithTests => tasty
    }
    def hidden(d: Defn): Boolean = config.hidden.exists(prefix => d.file.startsWith(prefix))
    def everyDefn(ds: Vector[Defn]): Vector[Defn] = ds.flatMap(d => d +: everyDefn(d.members))
    def simple(name: String): String = name.split('.').last
    def load(tasty: Vector[os.Path], from: Loaded): Either[String, (Vector[Defn], Loaded)] =
      if (tasty.isEmpty) Right((Vector.empty, from))
      else Loaded.defns(root, layout, tasty, from).map { case (ds, next, _) => (ds, next) }

    load(usable(Locate.forTopLevel(root, layout, sym)), in).flatMap { case (tops, loaded) =>
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
        val tasty = usable(
          Locate
            .mentioning(root, last)
            .flatMap(file => Locate.inPackageOf(root, layout, file))
            .distinct
        )
        load(tasty, loaded).map { case (more, after) =>
          val byLast = everyDefn(more).filter(d => simple(d.fullName) == last && !hidden(d))
          val names = byLast.map(_.fullName).distinct.sorted
          val notes =
            if (byLast.isEmpty) Vector.empty
            else Vector(s"-- no $sym as written; by last segment: ${names.mkString(", ")}")
          Resolved(byLast, notes, after, (tops ++ more).distinct)
        }
      }
    }
  }
}
