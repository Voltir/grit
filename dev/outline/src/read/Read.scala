package grit.outline.read

import scala.collection.mutable
import scala.quoted.Quotes
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}
import scala.util.control.NonFatal

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.model.{Defn, Kind, Lines}

/** Reads TASTy files into definitions, each with its source slices. Each inspector call is a fresh compiler run, so no compiler context is shared between roots. */
object Read {

  /** The dependency classpath handed to the inspector for `root`: exactly its compiled classes directories. */
  def classpath(root: Root, layout: Layout): Vector[os.Path] = layout.classesDirs(root)

  /** The top-level definitions in `tasty`, each with its public and private members nested under it, in input order.
    *
    * Only definitions whose source lies under `root.dir` are kept; each `file` is relative to it.
    * `Left` carries the complaint when the TASTy inspector reports errors, and the exception's
    * message when it throws.
    */
  def defns(root: Root, layout: Layout, tasty: Vector[os.Path]): Either[String, Vector[Defn]] =
    defnsByTasty(root, layout, tasty).map(byFile =>
      tasty.flatMap(p => byFile.getOrElse(p, Vector.empty))
    )

  /** The same definitions as `defns`, keyed by the `.tasty` file each came from; every input path is a key, empty when it holds none. */
  def defnsByTasty(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path]
  ): Either[String, Map[os.Path, Vector[Defn]]] = {
    val byPath = mutable.Map[os.Path, Vector[Defn]]()
    val sources = mutable.Map[String, String]()
    val inRepoByTop = mutable.Map[String, Boolean]()
    val foreign = mutable.ListBuffer.empty[String]
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
          sym.fullName
            .split('.')
            .map(_.stripSuffix("$"))
            .filterNot(_.endsWith("$package"))
            .mkString(".")

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

        def symbolsIn(t: TypeRepr): List[Symbol] = t match {
          case ref: TypeRef => List(ref.typeSymbol)
          case ref: TermRef => List(ref.typeSymbol)
          case AppliedType(tycon, args) => symbolsIn(tycon) ++ args.flatMap(symbolsIn)
          case AndType(left, right) => symbolsIn(left) ++ symbolsIn(right)
          case OrType(left, right) => symbolsIn(left) ++ symbolsIn(right)
          case TypeBounds(lo, hi) => symbolsIn(lo) ++ symbolsIn(hi)
          case MethodType(_, params, result) => params.flatMap(symbolsIn) ++ symbolsIn(result)
          case PolyType(_, bounds, result) => bounds.flatMap(symbolsIn) ++ symbolsIn(result)
          case AnnotatedType(underlying, _) => symbolsIn(underlying)
          case ByNameType(result) => symbolsIn(result)
          case Refinement(parent, _, info) => symbolsIn(parent) ++ symbolsIn(info)
          case _ => Nil
        }

        def treeSymbols(t: Tree): List[Symbol] = t match {
          case tt: TypeTree => symbolsIn(tt.tpe)
          case term: Term => symbolsIn(term.tpe)
          case _ => Nil
        }

        def topOf(s: Symbol): Symbol = if (s.owner.isPackageDef) s else topOf(s.owner)

        def refOf(s: Symbol): grit.outline.model.Ref = {
          val topLevel = topOf(s).fullName.split('.').map(_.stripSuffix("$")).mkString(".")
          grit.outline.model.Ref(
            fullName = fullNameOf(s),
            topLevel = topLevel,
            inRepo = inRepoByTop.getOrElseUpdate(
              topLevel,
              Locate.forTopLevel(root, layout, topLevel).nonEmpty
            )
          )
        }

        def paramTrees(c: ClassDef): List[Tree] = c.constructor.paramss.flatMap {
          case clause: TermParamClause => clause.params
          case clause: TypeParamClause => clause.params
        }

        // The types a definition's signature names: its own symbol and its type parameters are not refs.
        def refsOf(tree: Tree, sym: Symbol): Vector[grit.outline.model.Ref] = {
          val named: List[Symbol] = tree match {
            case c: ClassDef =>
              paramTrees(c).flatMap {
                case v: ValDef => symbolsIn(v.tpt.tpe)
                case _ => Nil
              } ++ c.parents.flatMap(treeSymbols)
            case _: DefDef => symbolsIn(sym.info)
            case _: ValDef => symbolsIn(sym.info)
            case t: TypeDef =>
              t.rhs match {
                case tt: TypeTree => symbolsIn(tt.tpe)
                case _ => Nil
              }
            case _ => Nil
          }
          val excluded = Set("scala.Any", "scala.Nothing", "java.lang.Object")
          named
            .filter(s =>
              s != Symbol.noSymbol && s != sym && s.owner != sym && !s.flags.is(Flags.Param) &&
                !excluded.contains(fullNameOf(s))
            )
            .distinct
            .map(refOf)
            .distinct
            .toVector
        }

        def parentsOf(sym: Symbol): Vector[String] = {
          val excluded = Set("scala.Any", "java.lang.Object", "scala.Matchable")
          sym.typeRef.baseClasses
            .filter(_ != sym)
            .map(fullNameOf)
            .filterNot(excluded.contains)
            .distinct
            .toVector
        }

        // A class's signature ends at its body's brace, or where its header does: after its last
        // constructor, parent or self-type tree, or after its name.
        def classCut(c: ClassDef, p: Position, src: String): Int = {
          def within(t: Tree): Boolean =
            t.pos.start >= p.start && t.pos.end <= p.end && t.pos.start < t.pos.end
          val ends = (paramTrees(c) ++ c.parents ++ c.self.toList).filter(within).map(_.pos.end)
          val nameEnd = scala.util.matching.Regex
            .quote(c.name)
            .r
            .unanchored
            .findFirstMatchIn(src.substring(p.start, p.end))
            .fold(p.start)(m => p.start + m.end)
          val base = if (ends.isEmpty) nameEnd else ends.max
          val brace = src.indexOf('{', base)
          if (brace >= 0 && brace < p.end) brace else p.end
        }

        def isEnum(t: Tree): Boolean = t match {
          case c: ClassDef => c.symbol.flags.is(Flags.Enum) && !c.symbol.flags.is(Flags.Case)
          case _ => false
        }

        // An enum's cases sit in its companion object's body, which the compiler marks synthetic: they become
        // the enum's first members, and the companion is kept only for the members it holds besides the cases.
        def withCases(stats: List[Tree]): Vector[Defn] = {
          val enumNames = stats.collect {
            case e: ClassDef if isEnum(e) => fullNameOf(e.symbol)
          }.toSet
          def companionOf(e: ClassDef): Option[ClassDef] = stats.collectFirst {
            case m: ClassDef
                if m.symbol.flags
                  .is(Flags.Module) && fullNameOf(m.symbol) == fullNameOf(e.symbol) =>
              m
          }
          stats.toVector.flatMap {
            case e: ClassDef if isEnum(e) =>
              val cases = companionOf(e).toVector
                .flatMap(c => members(c.body))
                .filter(_.kind == Kind.EnumCase)
              defn(e).map(d => d.copy(members = cases ++ d.members)).toVector
            case m: ClassDef
                if m.symbol.flags.is(Flags.Module) && enumNames.contains(fullNameOf(m.symbol)) =>
              defn(m).flatMap { d =>
                val rest = d.members.filterNot(_.kind == Kind.EnumCase)
                if (rest.isEmpty && d.members.nonEmpty) None else Some(d.copy(members = rest))
              }.toVector
            case other => defn(other).toVector
          }
        }

        def emit(
            tree: Tree,
            sym: Symbol,
            p: Position,
            cut: Int,
            hasBody: Boolean,
            members: Vector[Defn],
            isAbstract: Boolean,
            refs: Vector[grit.outline.model.Ref],
            parents: Vector[String]
        ): Defn = {
          val src = source(p.sourceFile.path)
          // A parameterless enum case has its right-hand side at `p.start` itself, so the slice is only indentation.
          val sliced = trimTrailing(src.substring(lineStart(src, p.start), cut))
          val rawSignature =
            if (sliced.isEmpty) {
              val eol = src.indexOf('\n', p.start) match {
                case -1 => src.length
                case i => i
              }
              trimTrailing(src.substring(lineStart(src, p.start), eol))
            } else sliced
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
            refs = refs,
            parents = parents,
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
                Some(
                  emit(
                    c,
                    sym,
                    p,
                    classCut(c, p, src),
                    hasBody = false,
                    members = members(c.body),
                    isAbstract = false,
                    refs = refsOf(c, sym),
                    parents = parentsOf(sym)
                  )
                )
              case d: DefDef =>
                val rhs = d.rhs
                val cut = rhs.map(_.pos.start).getOrElse(p.end)
                Some(
                  emit(
                    d,
                    sym,
                    p,
                    cut,
                    rhs.isDefined,
                    Vector.empty,
                    isAbstract = false,
                    refsOf(d, sym),
                    Vector.empty
                  )
                )
              case v: ValDef =>
                val rhs = v.rhs
                val cut = rhs.map(_.pos.start).getOrElse(p.end)
                Some(
                  emit(
                    v,
                    sym,
                    p,
                    cut,
                    rhs.isDefined,
                    Vector.empty,
                    isAbstract = false,
                    refsOf(v, sym),
                    Vector.empty
                  )
                )
              case t: TypeDef =>
                Some(
                  emit(
                    t,
                    sym,
                    p,
                    p.end,
                    hasBody = false,
                    Vector.empty,
                    isAbstract = false,
                    refsOf(t, sym),
                    Vector.empty
                  )
                )
              case _ => None
            }
        }

        def members(stats: List[Tree]): Vector[Defn] = withCases(stats)

        for (tasty <- tastys) {
          if (!tasty.ast.pos.sourceFile.path.startsWith(prefix))
            foreign += s"${tasty.path} records its source at ${tasty.ast.pos.sourceFile.path}, outside ${root.dir}: this out/ was built in another checkout; rebuild it here"
          tasty.ast match {
            case pkg: PackageClause =>
              val (holders, others) = pkg.stats.partition {
                case holder: ClassDef => holder.name.stripSuffix("$").endsWith("$package")
                case _ => false
              }
              val defs = withCases(others) ++ holders.toVector.flatMap {
                case holder: ClassDef => members(holder.body)
                case _ => Vector.empty
              }
              val path = os.Path(tasty.path)
              byPath.update(path, byPath.getOrElse(path, Vector.empty) ++ defs)
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
          classpath(root, layout).map(_.toString).toList
        )(inspector)
      } catch {
        case NonFatal(e) => return Left(Option(e.getMessage).getOrElse(e.toString))
      }
    if (foreign.nonEmpty) Left(foreign.mkString("\n"))
    else if (ok) Right(tasty.map(p => p -> byPath.getOrElse(p, Vector.empty)).toMap)
    else Left("the TASTy inspector reported errors")
  }
}
