package grit.host

import java.nio.file.{Files, Path}

import grit.core.host.{InstructionFile, Instructions}
import grit.core.place.Directory

import utest.*

/** [[LocalInstructions]] over a temporary tree. The walk goes on up to `/`, so each test looks
  * only at the files under its own tree.
  */
object LocalInstructionsTests extends TestSuite {

  /** A fresh tree holding `files` (name to bytes), given to `body` as its real path. */
  private def inTree[A](files: (String, List[Byte])*)(body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-instructions").toRealPath()
    try {
      Files.createDirectories(dir.resolve("a/b"))
      files.foreach { (name, bytes) =>
        // A fresh array, read by Files.write only and dropped after: nothing aliases it.
        Files.write(dir.resolve(name), caps.unsafe.unsafeAssumePure(bytes.toArray))
      }
      body(dir)
    } finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private def utf8(s: String): List[Byte] = s.getBytes("UTF-8").toList

  private def dir(p: Path): Directory =
    Directory.of(p.toString).fold(e => throw new java.lang.AssertionError(e), identity)

  /** What `around(at)` found under `root`, as paths relative to it. */
  private def found(root: Path, at: Path): Vector[(String, String, Boolean)] =
    new LocalInstructions()
      .around(dir(at))
      .filter(_.path.startsWith(root.toString + "/"))
      .map(f => (root.relativize(Path.of(f.path)).toString, f.text, f.cut))

  val tests = Tests {
    test("each directory gives its AGENTS.md, else its CLAUDE.md, the farthest first") {
      inTree(
        "CLAUDE.md" -> utf8("root claude"),
        "a/AGENTS.md" -> utf8("a agents"),
        "a/CLAUDE.md" -> utf8("a claude")
      ) { root =>
        found(root, root.resolve("a/b")) ==>
          Vector(("CLAUDE.md", "root claude", false), ("a/AGENTS.md", "a agents", false))
      }
    }

    test("a file over the cap is cut before the character the cap would split") {
      val over = "x" * (Instructions.MaxBytes - 1) + "é" + "tail"
      inTree("a/AGENTS.md" -> utf8(over)) { root =>
        found(root, root.resolve("a")).map((p, text, cut) =>
          (p, text.length, text.forall(_ == 'x'), cut)
        ) ==>
          Vector(("a/AGENTS.md", Instructions.MaxBytes - 1, true, true))
      }
    }

    test("a directory whose file is not UTF-8 gives nothing, not its other file") {
      inTree("a/AGENTS.md" -> List[Byte](0xc3.toByte, 0x28), "a/CLAUDE.md" -> utf8("fine")) {
        root => found(root, root.resolve("a")) ==> Vector()
      }
    }

    test("a path that is not a directory has no instructions") {
      inTree("a/AGENTS.md" -> utf8("a agents")) { root =>
        new LocalInstructions().around(dir(root.resolve("a/AGENTS.md"))) ==> Vector
          .empty[InstructionFile]
      }
    }
  }
}
