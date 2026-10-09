package grit.outline.query

import scala.collection.mutable

import grit.outline.locate.{Locate, Root}
import grit.outline.model.{Defn, Staleness}
import grit.outline.render.Render
import grit.outline.trace.Trace

enum Status {
  case Found, NoMatch, Failed
}

final case class Answer(text: String, status: Status)

object Query {

  private val usage =
    "usage: show Sym[,Sym…] [--depth 0|1|2] [--body m[,m…]] [--private] [--cap BYTES] [--root DIR]"

  /** The `show` answer for `syms` (each `Name`, `Name.member` or fully qualified), and the cache after it. */
  def show(
      root: Root,
      in: Loaded,
      syms: Vector[String],
      depth: Int,
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int
  ): (Answer, Loaded) = {
    val classes = Locate.classesDirs(root)
    if (depth < 0 || depth > 2)
      (Answer(s"depth must be 0, 1 or 2\n$usage", Status.Failed), in)
    else if (classes.isEmpty)
      (
        Answer(
          s"no compiled classes under ${root.dir / "out"}: run ./mill grit.<module>.compile",
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
        Loaded.defns(root, tasty, state) match {
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
        val tasty = Locate.forTopLevel(root, sym)
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
        case Some(message) => (Answer(message, Status.Failed), state)
        case None if matches.isEmpty => (Answer(lines.mkString("\n"), Status.NoMatch), state)
        case None =>
          val roots = matches.toVector.distinct
          val traced = Trace.trace(
            roots,
            depth,
            top => {
              val tasty = Locate.forTopLevel(root, top)
              if (tasty.isEmpty) Left(s"no tasty for $top") else loadTop(tasty)
            }
          )
          val rendered = Render.show(
            named = roots,
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
