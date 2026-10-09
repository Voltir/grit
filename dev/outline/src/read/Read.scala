package grit.outline.read

import scala.collection.mutable
import scala.quoted.Quotes
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}
import scala.util.control.NonFatal

import grit.outline.locate.{Locate, Root}
import grit.outline.model.{Defn, Kind, Lines}

/** Reads TASTy files into definitions, each with its source slices. */
object Read {

  /** The top-level definitions in `tasty`, each with its public and private members nested under it.
    *
    * Only definitions whose source lies under `root.dir` are kept; each `file` is relative to it.
    * `Left` carries the complaint when the TASTy inspector reports errors, and the exception's
    * message when it throws.
    */
  def defns(root: Root, tasty: Vector[os.Path]): Either[String, Vector[Defn]] = {
    val buffer = mutable.ListBuffer[Defn]()
    val sources = mutable.Map[String, String]()
    val prefix = root.dir.toString + "/"
    def source(path: String): String = sources.getOrElseUpdate(path, os.read(os.Path(path)))
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*

        def keep(tree: Tree, sym: Symbol, p: Position): Boolean = {
          val flags = sym.flags
          val isModuleVal = tree match {
            case v: ValDef => v.symbol.flags.is(Flags.Module)
            case _ => false
          }
          !(flags.is(Flags.Synthetic) || flags.is(Flags.Artifact) || flags.is(
            Flags.ParamAccessor
          ) ||
            flags.is(Flags.Param) || sym.isClassConstructor || isModuleVal ||
            !sym.pos.exists(_.sourceFile.path.startsWith(prefix)))
        }

        def kindOf(tree: Tree, sym: Symbol): Kind = {
          val flags = sym.flags
          tree match {
            case _: ClassDef =>
              if (flags.is(Flags.Trait)) Kind.Trait
              else if (flags.is(Flags.Module)) Kind.Object
              else if (flags.is(Flags.Enum) && flags.is(Flags.Case)) Kind.EnumCase
              else if (flags.is(Flags.Enum)) Kind.Enum
              else if (flags.is(Flags.Case)) Kind.CaseClass
              else Kind.Class
            case _: ValDef =>
              if (flags.is(Flags.Enum)) Kind.EnumCase
              else if (flags.is(Flags.Given)) Kind.Given
              else if (flags.is(Flags.Mutable)) Kind.Var
              else Kind.Val
            case _: DefDef =>
              if (flags.is(Flags.Given)) Kind.Given else Kind.Def
            case _ =>
              if (flags.is(Flags.Opaque)) Kind.Opaque else Kind.TypeAlias
          }
        }

        def nameOf(sym: Symbol): String = sym.name.stripSuffix("$")

        def fullNameOf(sym: Symbol): String =
          sym.fullName.split('.').filterNot(_ == "$package").map(_.stripSuffix("$")).mkString(".")

        /** The index of the first character of `i`'s line when only spaces precede `i` on it, else `i`. */
        def lineStart(src: String, i: Int): Int = {
          def spaces(j: Int): Int = if (j > 0 && src(j - 1) == ' ') spaces(j - 1) else j
          val j = spaces(i)
          if (j == 0 || src(j - 1) == '\n') j else i
        }

        /** The 1-based line holding offset `i`. */
        def lineOf(src: String, i: Int): Int = src.take(i).count(_ == '\n') + 1

        // The offset of the doc comment opening that ends just before `start`, and the offset after its close.
        def docOf(src: String, start: Int): Option[(Int, Int)] = {
          def skipBack(i: Int): Int = if (i >= 0 && src(i).isWhitespace) skipBack(i - 1) else i
          val close = skipBack(start - 1)
          if (close >= 1 && src(close) == '/' && src(close - 1) == '*') {
            val open = src.lastIndexOf("/**", close - 1)
            if (open < 0) None else Some((open, close + 1))
          } else None
        }

        def trimTrailing(s: String): String = s.replaceAll("\\s+$", "")

        def emit(
            tree: Tree,
            sym: Symbol,
            p: Position,
            cut: Int,
            hasBody: Boolean,
            members: Vector[Defn],
            isAbstract: Boolean
        ): Defn = {
          val src = source(p.sourceFile.path)
          val rawSignature = trimTrailing(src.substring(lineStart(src, p.start), cut))
          val unassigned =
            if (rawSignature.endsWith("=")) rawSignature.dropRight(1) else rawSignature
          val signature = trimTrailing(unassigned)
          val doc = docOf(src, p.start)
          val docStart = doc.map(_._1).getOrElse(p.start)
          val docText = doc.map { case (open, closeEnd) =>
            src.substring(lineStart(src, open), closeEnd)
          }
          val body = if (hasBody) Some(src.substring(cut, p.end)) else None
          val flags = sym.flags
          Defn(
            kind = kindOf(tree, sym),
            name = nameOf(sym),
            fullName = fullNameOf(sym),
            file = p.sourceFile.path.substring(prefix.length),
            lines = Lines(lineOf(src, docStart), lineOf(src, p.end - 1)),
            doc = docText,
            signature = signature,
            body = body,
            members = members,
            refs = Vector.empty,
            parents = Vector.empty,
            isPrivate = flags.is(Flags.Private) || sym.privateWithin.isDefined,
            isAbstract =
              flags.is(Flags.Deferred) || flags.is(Flags.Abstract) || flags.is(Flags.Trait)
          )
        }

        def defn(tree: Tree): Option[Defn] = {
          val sym = tree.symbol
          val p = tree.pos
          if (!keep(tree, sym, p)) None
          else
            tree match {
              case c: ClassDef =>
                val src = source(p.sourceFile.path)
                val newline = src.indexOf('\n', p.start)
                val cut = if (newline < 0) src.length else newline
                Some(
                  emit(
                    c,
                    sym,
                    p,
                    cut,
                    hasBody = false,
                    members = members(c.body),
                    isAbstract = false
                  )
                )
              case d: DefDef =>
                val rhs = d.rhs
                val cut = rhs.map(_.pos.start).getOrElse(p.end)
                Some(emit(d, sym, p, cut, rhs.isDefined, Vector.empty, isAbstract = false))
              case v: ValDef =>
                val rhs = v.rhs
                val cut = rhs.map(_.pos.start).getOrElse(p.end)
                Some(emit(v, sym, p, cut, rhs.isDefined, Vector.empty, isAbstract = false))
              case t: TypeDef =>
                Some(emit(t, sym, p, p.end, hasBody = false, Vector.empty, isAbstract = false))
              case _ => None
            }
        }

        def members(stats: List[Tree]): Vector[Defn] = stats.toVector.flatMap(defn)

        for (tasty <- tastys) {
          tasty.ast match {
            case pkg: PackageClause =>
              buffer ++= pkg.stats.toVector.flatMap {
                case holder: ClassDef if holder.name.endsWith("$package") => members(holder.body)
                case other => defn(other).toVector
              }
            case _ => ()
          }
        }
      }
    }
    val ok =
      try {
        TastyInspector.inspectAllTastyFiles(
          tasty.map(_.toString).toList,
          Nil,
          Locate.classesDirs(root).map(_.toString).toList
        )(inspector)
      } catch {
        case NonFatal(e) => return Left(Option(e.getMessage).getOrElse(e.toString))
      }
    if (ok) Right(buffer.toVector) else Left("the TASTy inspector reported errors")
  }
}
