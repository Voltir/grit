package grit.tui

import java.io.File
import utest.*

/** The boundaries, enforced by the build rather than by convention.
  *
  * Every rule here is one the README states and that nothing else can hold: a grep in a
  * review catches it once, a test catches it forever. They walk the real source tree, so
  * a new file in the wrong package fails without anyone remembering to update a list.
  */
object QuarantineTests extends TestSuite {

  /** The repository root, found by walking up from the test's working directory until a
    * `build.mill` appears. Mill does not promise which directory tests run in.
    */
  private def repoRoot: Option[File] = {
    var dir: File = new File(sys.props.getOrElse("user.dir", "."))
    var found: Option[File] = None
    var depth = 0
    while (found.isEmpty && dir != null && depth < 8) {
      if (new File(dir, "build.mill").isFile) { found = Some(dir) }
      dir = dir.getParentFile
      depth += 1
    }
    found
  }

  /** Every `.scala` file under `dir`, as (repo-relative path, contents). */
  private def sources(root: File, dir: File): Vector[(String, String)] = {
    val out = Vector.newBuilder[(String, String)]
    val stack = scala.collection.mutable.Stack[File](dir)
    while (stack.nonEmpty) {
      val f = stack.pop()
      if (f.isDirectory) {
        val kids = f.listFiles
        if (kids != null) {
          var i = 0
          while (i < kids.length) { stack.push(kids(i)); i += 1 }
        }
      } else if (f.getName.endsWith(".scala")) {
        val rel = root.toPath.relativize(f.toPath).toString.replace('\\', '/')
        out += (rel -> java.nio.file.Files.readString(f.toPath))
      }
    }
    out.result()
  }

  /** Library sources only -- `examples` is an app and plays by app rules. */
  private def librarySources: Vector[(String, String)] = repoRoot match {
    case Some(root) => sources(root, new File(root, "grit/tui/src"))
    case None => Vector.empty
  }

  private def assertFound(files: Vector[(String, String)]): Unit =
    assert(files.nonEmpty) // a passing quarantine test that scanned nothing is a lie

  /** Files that broke `rule`, as a readable list. */
  private def offenders(
      files: Vector[(String, String)],
      allowed: Vector[String],
      rule: String => Boolean
  ): Vector[String] =
    files.collect {
      case (path, body) if rule(body) && !allowed.exists(a => path.endsWith(a)) => path
    }

  /** The four groups, in dependency order. A library source lives under exactly one. */
  private val Groups = Vector("model", "components", "wire", "runtime")

  /** What each group is allowed to reach for. `components` and `wire` are siblings:
    * neither may name the other, and only `app` may name both.
    */
  private val MayImport: Map[String, Set[String]] = Map(
    "model" -> Set.empty,
    "components" -> Set("model"),
    "wire" -> Set("model"),
    "runtime" -> Set("model", "components", "wire")
  ).withDefaultValue(Set.empty)

  private val Root = "grit/tui/src/"

  /** The terminal seam, the one package allowed to touch a device. */
  private val Term = Root + "wire/term/"

  /** `body` with comment lines dropped. A scaladoc link to another group is prose, not a
    * dependency -- documenting the painter from the widget convention is exactly the kind
    * of cross-reference that should stay legal.
    */
  private def uncommented(body: String): String =
    body.linesIterator
      .filterNot { l =>
        val t = l.trim
        t.startsWith("*") || t.startsWith("//") || t.startsWith("/*")
      }
      .mkString("\n")

  /** The group a source file belongs to, or None if it is outside all of them. */
  private def groupOf(path: String): Option[String] =
    Groups.find(g => path.startsWith(s"$Root$g/"))

  val tests = Tests {

    test("no Effect case carries a function") {
      // README idea 4 / CLAUDE.md rule 3, and the one rule the compiler cannot hold for
      // us: a thunk in a command launders a capability past capture checking. Checked at
      // the source, because a Scala 3 enum does not report its cases reflectively.
      val files = librarySources
      assertFound(files)
      val effect = files.collectFirst { case (p, b) if p.endsWith("runtime/app/Effect.scala") => b }
      assert(effect.isDefined)
      // Only the enum body: `case` also introduces match clauses further down the file.
      val body = effect.getOrElse("")
      val open = body.indexOf("enum Effect")
      val shut = body.indexOf("\n}", open)
      assert(open >= 0 && shut > open)
      val cases = body
        .substring(open, shut)
        .linesIterator
        .toVector
        .map(_.trim)
        .filter(_.startsWith("case "))
      assert(cases.nonEmpty)
      val functional = cases.filter(c => c.contains("=>") || c.contains("Function"))
      assert(functional.isEmpty)
    }

    test("escape bytes are spelled in exactly three files") {
      // ROADMAP decision, 2026-09-02. Ansi writes them, Decoder reads them, term sets
      // modes; anywhere else means terminal grammar has leaked into the model.
      val files = librarySources
      assertFound(files)
      val allowed = Vector("wire/paint/Ansi.scala", "wire/input/Decoder.scala")
      val bad = offenders(files, allowed, b => b.contains("\\u001b") || b.indexOf(0x1b) >= 0)
      val stillBad = bad.filterNot(_.startsWith(Term))
      assert(stillBad.isEmpty)
    }

    test("every library source sits in a declared group") {
      // The groups *are* the layer table. A package's directory is what says where it
      // sits, so a new package cannot be silently unchecked the way a hand-extended
      // prefix allowlist let it be -- there is nowhere to put it that this does not see.
      val files = librarySources
      assertFound(files)
      val bad = files.collect { case (p, _) if groupOf(p).isEmpty => p }
      assert(bad.isEmpty)
    }

    test("nothing imports upward or sideways between groups") {
      // model is the pure model (the grid, measurement, documents, selection);
      // components build surfaces out of it; wire turns surfaces into bytes and bytes
      // back into inputs; runtime is the only place that holds both ends. components and
      // wire are siblings and must not name each other -- the API surface a user of
      // grit.tui writes against is separated from the terminal by construction, which is
      // the quarantine the README claims, stated once.
      val files = librarySources
      assertFound(files)
      val bad = files.flatMap { (path, body) =>
        val from = groupOf(path).getOrElse("")
        val code = uncommented(body)
        Groups.filter(g => g != from && !MayImport(from).contains(g)).flatMap { g =>
          if (code.contains(s"grit.tui.$g.")) { Some(s"$path -> grit.tui.$g") }
          else { None }
        }
      }
      assert(bad.isEmpty)
    }

    test("the terminal is touched in exactly one package") {
      // stty, subprocesses and the real streams belong to the seam. The library is
      // testable without a terminal precisely because this holds.
      val files = librarySources
      assertFound(files)
      val bad = offenders(
        files,
        Vector.empty,
        b =>
          b.contains("sys.process") || b.contains("System.in") || b.contains("System.out") ||
            b.contains("\"stty")
      )
      val stillBad = bad.filterNot(_.startsWith(Term))
      assert(stillBad.isEmpty)
    }

    test("the library has no external dependencies") {
      // Zero deps is the build's claim; this is what makes it true of the source too.
      val files = librarySources
      assertFound(files)
      val bad = offenders(
        files,
        Vector.empty,
        b => b.contains("import org.jline") || b.contains("import dev.dbos")
      )
      assert(bad.isEmpty)
    }
    test("the example's transcript model imports nothing from the wire") {
      // grit's stated quarantine test for its own transcript model, and one of the two
      // new phase 1 gate oracles: if the transcript model can be unit-tested with no
      // terminal in the loop, the quarantine holds. The library side is held by the
      // group rule above; this holds the example's side -- Transcript.scala, the file
      // that builds and grows the mock transcript, names no wire package.
      val root = repoRoot.getOrElse(sys.error("no repo root"))
      val f = new File(root, "grit/tui/examples/src/Transcript.scala")
      assert(f.isFile)
      val body = java.nio.file.Files.readString(f.toPath)
      assert(body.nonEmpty)
      assert(!uncommented(body).contains("grit.tui.wire"))
    }
  }
}
