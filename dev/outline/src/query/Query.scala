package grit.outline.query

import scala.collection.mutable

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.model.{Defn, Kind, Staleness}
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
      roots: Roots,
      syms: Vector[String],
      depth: Int,
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int
  ): (Answer, Roots) = {
    val (answer, loaded) =
      showIn(root, layout, Roots.of(roots, root), syms, depth, bodies, withPrivate, cap)
    (answer.copy(text = s"${header(root)}\n${answer.text}"), Roots.put(roots, root, loaded))
  }

  /** The `family` answer for `name` (a trait, or an abstract class), read through `root`'s own cache in `roots`, which then holds `root` as most recently used. `member` narrows the trait's and each implementation's text to that member, with its body under `withBody`. Every answer starts with `root`'s `## root` line. */
  def family(
      root: Root,
      layout: Layout,
      roots: Roots,
      name: String,
      member: Option[String],
      withBody: Boolean,
      cap: Int
  ): (Answer, Roots) = {
    val (answer, loaded) =
      familyIn(root, layout, Roots.of(roots, root), name, member, withBody, cap)
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

  private def familyIn(
      root: Root,
      layout: Layout,
      in: Loaded,
      name: String,
      member: Option[String],
      withBody: Boolean,
      cap: Int
  ): (Answer, Loaded) =
    if (layout.classesDirs(root).isEmpty)
      (Answer(s"${header(root)}\n${layout.notCompiled(root)}", Status.Failed), in)
    else {
      val files = Locate.mentioning(root, name.split('.').last)
      val tasty = files.flatMap(file => Locate.inPackageOf(root, layout, file)).distinct
      // One file at a time: a file the inspector cannot read (its library is not among the classes directories) is named, not fatal.
      val (defns, next, missing) =
        tasty.foldLeft((Vector.empty[Defn], in, Vector.empty[String])) {
          case ((read, state, unread), path) =>
            Loaded.defns(root, layout, Vector(path), state) match {
              case Left(_) => (read, state, unread :+ path.last)
              case Right((ds, after, _)) => (read ++ ds, after, unread)
            }
        }
      val head =
        (Vector(header(root)) ++
          (if (missing.isEmpty) Vector.empty
           else Vector(s"-- not loaded: ${missing.mkString(", ")}"))).mkString("\n")
      familyOf(defns, name, member, withBody) match {
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
      (
        Answer(
          layout.notCompiled(root),
          Status.Failed
        ),
        in
      )
    else {
      // The cache and the definitions read in this query, threaded through a scoped local.
      var state = in
      val companions = mutable.ListBuffer.empty[Defn]
      val stale = mutable.Set.empty[String]
      var failure: Option[String] = None

      def loadTop(tasty: Vector[os.Path]): Either[String, Vector[Defn]] =
        Loaded.defns(root, layout, tasty, state) match {
          case Left(message) => Left(message)
          case Right((defns, next, _)) =>
            state = next
            companions ++= defns
            defns.map(_.file).distinct.foreach { file =>
              Locate.staleness(root, os.RelPath(file), tasty) match {
                case Staleness.Stale(_, _) => stale += file
                case _ => ()
              }
            }
            Right(defns)
        }

      def everyDefn(ds: Vector[Defn]): Vector[Defn] = ds.flatMap(d => d +: everyDefn(d.members))

      val lines = mutable.ListBuffer.empty[String]
      val matches = mutable.ListBuffer.empty[Defn]
      syms.foreach { sym =>
        val tasty = Locate.forTopLevel(root, layout, sym)
        val found: Vector[Defn] =
          if (tasty.isEmpty) Vector.empty
          else
            loadTop(tasty) match {
              case Left(message) =>
                failure = Some(message)
                Vector.empty
              case Right(defns) =>
                everyDefn(defns).filter(d => d.fullName == sym || d.fullName.endsWith("." + sym))
            }
        if (found.isEmpty) lines += s"no match for $sym"
        matches ++= found
      }

      failure match {
        case Some(message) => (Answer(message, Status.Failed), in)
        case None if matches.isEmpty => (Answer(lines.mkString("\n"), Status.NoMatch), state)
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
          val rendered = Render.show(
            named = tops,
            traced = traced,
            companions = companions.toVector,
            stale = stale.toSet,
            bodies = bodies,
            withPrivate = withPrivate,
            cap = cap
          )
          val text = (lines.toVector :+ rendered).mkString("\n")
          (Answer(text, Status.Found), state)
      }
    }
  }
}
