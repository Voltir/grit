package grit.outline.read

import java.io.{ByteArrayOutputStream, PrintStream}

import scala.collection.mutable
import scala.quoted.Quotes
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}
import scala.util.control.NonFatal

import grit.outline.locate.{Layout, Locate, Root}
import grit.outline.model.{Defn, Kind, Lines, Use}

/** Reads TASTy files into definitions, each with its source slices. Each inspector call is a fresh compiler run, so no compiler context is shared between roots. */
object Read {

  /** The classpath handed to the inspector for `root`: its compiled classes directories, then the library jars its
    * compiled classes record, so that TASTy referring to a third-party type can be read.
    */
  def classpath(root: Root, layout: Layout): Vector[os.Path] =
    layout.classesDirs(root) ++ layout.libraryJars(root)

  /** The definitions read from a set of `.tasty` files: each file's, keyed by path, and each file the inspector could not read, with the exception's message. */
  final case class Batch(byTasty: Map[os.Path, Vector[Defn]], unreadable: Vector[(os.Path, String)])

  /** `s` from `from` to `to`, each end clamped into `0..s.length`; empty when `to <= from`. */
  private def slice(s: String, from: Int, to: Int): String = {
    val lo = from.max(0).min(s.length)
    val hi = to.max(0).min(s.length)
    if (hi <= lo) "" else s.substring(lo, hi)
  }

  /** Each unit's results in input order, and each unit whose `each` threw, with its exception's message. */
  private[read] def traverseUnits[U, A](
      units: Vector[U],
      each: U => Vector[A]
  ): (Vector[(U, Vector[A])], Vector[(U, String)]) = {
    val kept = Vector.newBuilder[(U, Vector[A])]
    val failed = Vector.newBuilder[(U, String)]
    units.foreach { u =>
      try kept.addOne(u -> each(u))
      catch { case NonFatal(e) => failed.addOne(u -> Option(e.getMessage).getOrElse(e.toString)) }
    }
    (kept.result(), failed.result())
  }

  /** The top-level definitions in `tasty`, each with its public and private members nested under it, in input order.
    *
    * Only definitions whose source lies under `root.dir` are kept; each `file` is relative to it.
    * `Left` carries the complaint when the TASTy inspector reports errors, and the exception's
    * message when it throws.
    */
  def defns(root: Root, layout: Layout, tasty: Vector[os.Path]): Either[String, Vector[Defn]] =
    defnsByTasty(root, layout, tasty).map(batch =>
      tasty.flatMap(p => batch.byTasty.getOrElse(p, Vector.empty))
    )

  /** The same definitions as `defns`, keyed by the `.tasty` file each came from; every input path is a key, empty when it holds none.
    *
    * A file whose traversal throws is listed in `unreadable` and contributes no definitions. `Left` is only for an inspector run that
    * fails as a whole, or a file outside `root`'s checkout.
    */
  def defnsByTasty(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path]
  ): Either[String, Batch] = {
    val byPath = mutable.Map[os.Path, Vector[Defn]]()
    val sources = mutable.Map[String, String]()
    val inRepoByTop = mutable.Map[String, Boolean]()
    val foreign = mutable.ListBuffer.empty[String]
    val unreadable = mutable.ListBuffer.empty[(os.Path, String)]
    val prefix = root.dir.toString + "/"
    def source(path: String): String =
      sources.getOrElseUpdate(
        path,
        if (path.startsWith(prefix)) os.read(os.Path(path))
        else {
          foreign += s"a source file recorded at '$path' is outside ${root.dir}: this out/ was built in another checkout; rebuild it here"
          ""
        }
      )
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*

        def keep(tree: Tree, sym: Symbol): Boolean = {
          val flags = sym.flags
          val isModuleVal = tree match {
            case v: ValDef => v.symbol.flags.is(Flags.Module)
            case _ => false
          }
          !(flags.is(Flags.Synthetic) || flags.is(Flags.Artifact) || sym.name.contains(
            "$default$"
          ) || flags.is(
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

        def fullNameOf(sym: Symbol): String = cleanFullName(sym.fullName)

        /** The index of the first character of `i`'s line when only spaces precede `i` on it, else `i`. */
        def lineStart(src: String, i: Int): Int = {
          def spaces(j: Int): Int = if (j > 0 && src(j - 1) == ' ') spaces(j - 1) else j
          val j = spaces(i)
          if (j == 0 || src(j - 1) == '\n') j else i
        }

        /** The 1-based line holding offset `i`. */
        def lineOf(src: String, i: Int): Int = src.take(i).count(_ == '\n') + 1

        /** The index of the last non-space character at or before `i`, or 0 when there is none. */
        def lastNonSpace(src: String, i: Int): Int =
          if (i > 0 && src(i).isWhitespace) lastNonSpace(src, i - 1) else math.max(i, 0)

        /** The whole lines `from` to `to`, 1-based and both included, as the file has them: joined by newlines, nothing trimmed. */
        def wholeLines(src: String, from: Int, to: Int): String =
          src.split("\n", -1).slice(from - 1, to).mkString("\n")

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
            .findFirstMatchIn(slice(src, p.start, p.end))
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
            refs: Vector[grit.outline.model.Ref],
            parents: Vector[String]
        ): Defn = {
          val src = source(p.sourceFile.path)
          // A cut outside the definition's own span (a pattern-bound val's right-hand side starts before it) leaves its
          // whole source line as the signature, and no body.
          val inSpan = p.start <= cut && cut <= p.end
          val lineBegin = src.lastIndexOf('\n', p.start - 1) + 1
          val lineEnd = src.indexOf('\n', p.start) match {
            case -1 => src.length
            case i => i
          }
          // A parameterless enum case has its right-hand side at `p.start` itself, so the slice is only indentation.
          val sliced = if (inSpan) trimTrailing(slice(src, lineStart(src, p.start), cut)) else ""
          val rawSignature =
            if (!inSpan) trimTrailing(slice(src, lineBegin, lineEnd))
            else if (sliced.isEmpty) trimTrailing(slice(src, lineStart(src, p.start), lineEnd))
            else sliced
          val unassigned =
            if (rawSignature.endsWith("=")) rawSignature.dropRight(1) else rawSignature
          val signature = trimTrailing(unassigned)
          val doc = docOf(src, p.start)
          val docStart = doc.map(_._1).getOrElse(p.start)
          val docText = doc.map { case (open, closeEnd) =>
            slice(src, lineStart(src, open), closeEnd)
          }
          val firstLine = lineOf(src, docStart)
          val lastLine = lineOf(src, p.end - 1)
          // The head ends on the line of the signature's last character, before the cut; the body is the whole lines after it.
          val headEnd =
            if (inSpan) lineOf(src, math.max(p.start, lastNonSpace(src, cut - 1)))
            else lineOf(src, p.start)
          val head = wholeLines(src, firstLine, headEnd)
          val body =
            if (hasBody && inSpan && headEnd < lastLine)
              Some(wholeLines(src, headEnd + 1, lastLine))
            else None
          val flags = sym.flags
          Defn(
            kind = kindOf(tree, sym),
            name = nameOf(sym),
            fullName = fullNameOf(sym),
            file = slice(p.sourceFile.path, prefix.length, p.sourceFile.path.length),
            lines = Lines(firstLine, lastLine),
            doc = docText,
            signature = signature,
            head = head,
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
          if (!keep(tree, sym)) None
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
                    refsOf(t, sym),
                    Vector.empty
                  )
                )
              case _ => None
            }
        }

        def members(stats: List[Tree]): Vector[Defn] = withCases(stats)

        // One unit's definitions: none unless the unit is a package's TASTy.
        def unit(tasty: Tasty[q.type]): Vector[Defn] = {
          if (!tasty.ast.pos.sourceFile.path.startsWith(prefix))
            foreign += s"${tasty.path} records its source at ${tasty.ast.pos.sourceFile.path}, outside ${root.dir}: this out/ was built in another checkout; rebuild it here"
          tasty.ast match {
            case pkg: PackageClause =>
              val (holders, others) = pkg.stats.partition {
                case holder: ClassDef => holder.name.stripSuffix("$").endsWith("$package")
                case _ => false
              }
              withCases(others) ++ holders.toVector.flatMap {
                case holder: ClassDef => members(holder.body)
                case _ => Vector.empty
              }
            case _ => Vector.empty
          }
        }
        val (kept, failed) = traverseUnits(tastys.toVector, unit)
        for ((tasty, defs) <- kept) {
          val path = os.Path(tasty.path)
          byPath.update(path, byPath.getOrElse(path, Vector.empty) ++ defs)
        }
        unreadable ++= failed.map { case (tasty, message) => os.Path(tasty.path) -> message }
      }
    }
    val captured = new ByteArrayOutputStream()
    val ran: Either[String, Boolean] = redirected(captured) {
      try
        Right(
          TastyInspector.inspectAllTastyFiles(
            tasty.map(_.toString).toList,
            Nil,
            classpath(root, layout).map(_.toString).toList
          )(inspector)
        )
      catch { case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString)) }
    }
    ran match {
      case Left(message) => Left(message)
      case Right(ok) =>
        if (foreign.nonEmpty) Left(foreign.mkString("\n"))
        else if (ok)
          Right(
            Batch(tasty.map(p => p -> byPath.getOrElse(p, Vector.empty)).toMap, unreadable.toVector)
          )
        else Left(complaint(captured, tasty.size))
    }
  }

  /** A full name without the `$` suffixes of modules and classes, and without package object segments. */
  private def cleanFullName(fullName: String): String =
    fullName
      .split('.')
      .map(_.stripSuffix("$"))
      .filterNot(_.endsWith("$package"))
      .mkString(".")

  /** The references in `tasty` to any of `targets` (full names), sorted by file and line, each with the definition holding it.
    *
    * A reference is the compiler's own resolution, so a different method of the same name never matches. The enclosing definition
    * is the innermost named `def`, `val` or class around it; lambdas and anonymous classes are passed over. `targetLine` is the line
    * where the referenced symbol's definition starts, 0 when it has no position. Only references in source under `root.dir` are
    * kept. `Left` carries the same complaints as `defns`.
    */
  def uses(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path],
      targets: Set[String]
  ): Either[String, Vector[Use]] =
    references(root, layout, tasty, targets.contains).map(byFile =>
      targeting(byFile.values.flatten.toVector, targets)
    )

  /** The references among `refs` to any of `targets` (full names), each once, sorted by file and line: what `uses` reports of a read by `references`. */
  def targeting(refs: Vector[Use], targets: Set[String]): Vector[Use] =
    refs.filter(u => targets.contains(u.target)).distinct.sortBy(u => (u.file, u.line))

  /** Every reference in each of `tasty` to a member of a class or package defined in source under `root.dir`, each with the definition holding it, when `keep` holds for its target's full name; a file with none maps to none. Only references in source under `root.dir` are kept. `Left` carries the same complaints as `defns`, and a run that complains returns no file's references. */
  def references(
      root: Root,
      layout: Layout,
      tasty: Vector[os.Path],
      keep: String => Boolean
  ): Either[String, Map[os.Path, Vector[Use]]] = {
    val byFile = mutable.Map.empty[os.Path, Vector[Use]]
    val found = mutable.ListBuffer.empty[Use]
    val sources = mutable.Map[String, String]()
    val foreign = mutable.ListBuffer.empty[String]
    val prefix = root.dir.toString + "/"
    def source(path: String): String =
      sources.getOrElseUpdate(
        path,
        if (path.startsWith(prefix)) os.read(os.Path(path))
        else {
          foreign += s"a source file recorded at '$path' is outside ${root.dir}: this out/ was built in another checkout; rebuild it here"
          ""
        }
      )
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*

        def named(sym: Symbol): Boolean =
          !(sym.flags.is(Flags.Synthetic) || sym.isClassConstructor || sym.name.startsWith("$"))

        val walk = new TreeTraverser {
          // The named definitions around the tree being walked, innermost first.
          private var enclosing: List[String] = Nil

          private def record(ref: Tree): Unit = {
            val sym = ref.symbol
            val name = cleanFullName(sym.fullName)
            val ownName = sym.pos.exists(p =>
              p.start == ref.pos.start && p.sourceFile.path == ref.pos.sourceFile.path
            )
            if (
              keep(name) && ref.pos.startLine >= 0 && !ownName && ref.pos.sourceFile.path
                .startsWith(prefix) &&
              sym.pos.exists(_.sourceFile.path.startsWith(prefix)) &&
              (sym.maybeOwner.isClassDef || sym.maybeOwner.isPackageDef)
            ) {
              val path = ref.pos.sourceFile.path
              // A Select starts at its qualifier, so its line is the one holding its name, the position's last character.
              val line = ref match {
                case _: Select => ref.pos.endLine + 1
                case _ => ref.pos.startLine + 1
              }
              val text =
                source(path).linesIterator.drop(line - 1).nextOption().map(_.trim).getOrElse("")
              found += Use(
                target = name,
                targetLine = sym.pos.map(_.startLine + 1).getOrElse(0),
                file = path.stripPrefix(prefix),
                line = line,
                enclosing = enclosing.headOption.getOrElse(""),
                text = text
              )
            }
          }

          override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
            case d @ (_: DefDef | _: ValDef | _: ClassDef) if named(d.symbol) =>
              enclosing = cleanFullName(d.symbol.fullName) :: enclosing
              super.traverseTree(tree)(owner)
              enclosing = enclosing.tail
            case ref @ (_: Ident | _: Select) =>
              record(ref)
              super.traverseTree(tree)(owner)
            case _ => super.traverseTree(tree)(owner)
          }
        }

        for (tasty <- tastys) {
          if (!tasty.ast.pos.sourceFile.path.startsWith(prefix))
            foreign += s"${tasty.path} records its source at ${tasty.ast.pos.sourceFile.path}, outside ${root.dir}: this out/ was built in another checkout; rebuild it here"
          val start = found.size
          // A unit whose traversal throws keeps the references it found before the throw, and no more.
          try walk.traverseTree(tasty.ast)(Symbol.noSymbol)
          catch { case NonFatal(_) => () }
          byFile += os.Path(tasty.path.toString) -> found.drop(start).toVector
        }
      }
    }
    val captured = new ByteArrayOutputStream()
    val ran: Either[String, Boolean] = redirected(captured) {
      try
        Right(
          TastyInspector.inspectAllTastyFiles(
            tasty.map(_.toString).toList,
            Nil,
            classpath(root, layout).map(_.toString).toList
          )(inspector)
        )
      catch { case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString)) }
    }
    ran match {
      case Left(message) => Left(message)
      case Right(ok) =>
        if (foreign.nonEmpty) Left(foreign.mkString("\n"))
        else if (ok) Right(byFile.toMap)
        else Left(complaint(captured, tasty.size))
    }
  }

  /** Runs `body` with stdout and stderr, and the console's, sent to `buffer`; they are restored after, even when `body` throws. */
  private def redirected[A](buffer: ByteArrayOutputStream)(body: => A): A = {
    val sink = new PrintStream(buffer, true)
    // Console is entered before System is swapped: it takes its default streams on first use, and those must be the real ones.
    Console.withOut(sink)(Console.withErr(sink) {
      val (oldOut, oldErr) = (System.out, System.err)
      try {
        System.setOut(sink)
        System.setErr(sink)
        body
      } finally {
        System.setOut(oldOut)
        System.setErr(oldErr)
      }
    })
  }

  /** What the inspector printed is never shown: its first three non-blank lines, each cut to 200 characters, name the failure. */
  private def complaint(captured: ByteArrayOutputStream, files: Int): String = {
    val lines =
      captured.toString.linesIterator.filter(_.trim.nonEmpty).take(3).map(_.take(200)).toVector
    s"the TASTy inspector could not read $files file(s): ${lines.mkString("\n")}"
  }
}
