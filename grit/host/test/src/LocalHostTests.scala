package grit.host

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.host.*

import utest.*

/** [[LocalWorkspace]], [[LocalEdits]] and [[LocalShell]] against a temporary checkout. */
object LocalHostTests extends TestSuite {

  private def path(s: String): RelPath = RelPath.of(s) match {
    case Right(p) => p
    case Left(e) => throw new java.lang.AssertionError(e.message)
  }

  /** A fresh checkout holding `files`, given to `body`, deleted after. */
  private def inCheckout[A](files: (String, String)*)(body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-host")
    try {
      files.foreach { (name, text) =>
        val f = dir.resolve(name)
        Files.createDirectories(f.getParent)
        Files.writeString(f, text)
      }
      body(dir)
    } finally {
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
    }
  }

  private def shown(r: Either[HostError, Clipped]): Either[HostError, String] = r.map(_.show)

  val tests = Tests {
    test("read") {
      test("a whole file, and a window of it") {
        inCheckout("a.txt" -> "one\ntwo\nthree\n") { dir =>
          val ws = new LocalWorkspace(dir)
          shown(ws.read(path("a.txt"), Lines.All)) ==> Right("one\ntwo\nthree")
          shown(ws.read(path("a.txt"), Lines.of(2, Some(1)))) ==>
            Right("two\n\n[1 more lines in the file. Use offset=3 to continue.]")
          shown(ws.read(path("a.txt"), Lines.of(3, None))) ==> Right("three")
        }
      }
      test("a long file is clipped, with the offset to continue from") {
        val text = (1 to 2500).map(i => s"line $i").mkString("\n")
        inCheckout("big.txt" -> text) { dir =>
          val clip = new LocalWorkspace(dir).read(path("big.txt"), Lines.All)
          clip.map(_.hint) ==>
            Right(Some("Showing lines 1-2000 of 2500. Use offset=2001 to continue."))
        }
      }
      test("failures") {
        inCheckout("a.txt" -> "one\n", "d/b.txt" -> "x") { dir =>
          val ws = new LocalWorkspace(dir)
          ws.read(path("nope"), Lines.All) ==> Left(HostError.NotFound(path("nope")))
          ws.read(path("d"), Lines.All) ==> Left(HostError.NotAFile(path("d")))
          ws.read(path("a.txt"), Lines.of(5, None)) ==>
            Left(HostError.PastEnd(path("a.txt"), 5, 1))
          // Two bytes, 0xff 0xfe, which begin no UTF-8 character.
          Files.writeString(
            dir.resolve("bin"),
            "\u00ff\u00fe",
            java.nio.charset.StandardCharsets.ISO_8859_1
          )
          ws.read(path("bin"), Lines.All) ==> Left(HostError.NotText(path("bin")))
        }
      }
      test("an empty file reads as empty at offset 1") {
        inCheckout("e" -> "") { dir =>
          shown(new LocalWorkspace(dir).read(path("e"), Lines.All)) ==> Right("")
        }
      }
      test("a link leading outside the checkout, or to a secrets file, is refused") {
        inCheckout("a.txt" -> "x", ".env" -> "SECRET=1") { dir =>
          inCheckout("outside.txt" -> "private") { other =>
            Files.createSymbolicLink(dir.resolve("out"), other.resolve("outside.txt"))
            Files.createSymbolicLink(dir.resolve("config"), dir.resolve(".env"))
            Files.createSymbolicLink(dir.resolve("ok"), dir.resolve("a.txt"))
            val ws = new LocalWorkspace(dir)
            ws.read(path("out"), Lines.All) ==> Left(HostError.Outside(path("out")))
            ws.read(path("config"), Lines.All) ==> Left(HostError.Outside(path("config")))
            shown(ws.read(path("ok"), Lines.All)) ==> Right("x")
          }
        }
      }
    }

    test("list") {
      inCheckout(
        "src/a.scala" -> "",
        "src/deep/b.scala" -> "",
        "README.md" -> "",
        ".git/HEAD" -> "",
        "out/x" -> ""
      ) { dir =>
        val ws = new LocalWorkspace(dir)
        shown(ws.list(RelPath.Root, 1)) ==> Right(".git/\nREADME.md\nout/\nsrc/")
        shown(ws.list(RelPath.Root, 3)) ==> Right(
          ".git/\nREADME.md\nout/\nsrc/\nsrc/a.scala\nsrc/deep/\nsrc/deep/b.scala"
        )
        shown(ws.list(path("src"), 0)) ==> Right("a.scala\ndeep/")
        ws.list(path("README.md"), 1) ==> Left(HostError.NotADirectory(path("README.md")))
        ws.list(path("gone"), 1) ==> Left(HostError.NotFound(path("gone")))
        Files.createDirectory(dir.resolve("empty"))
        shown(ws.list(path("empty"), 1)) ==> Right("(empty)")
      }
    }

    test("search") {
      inCheckout(
        "src/a.scala" -> "val x = 1\nval y = 2\n",
        "src/b.scala" -> "def x = 3\n",
        ".env" -> "x=secret\n",
        ".git/config" -> "x\n",
        "out/gen.scala" -> "val x\n"
      ) { dir =>
        val ws = new LocalWorkspace(dir)
        shown(ws.search("x", RelPath.Root)) ==>
          Right("src/a.scala:1: val x = 1\nsrc/b.scala:1: def x = 3")
        shown(ws.search("^val", path("src/a.scala"))) ==>
          Right("src/a.scala:1: val x = 1\nsrc/a.scala:2: val y = 2")
        shown(ws.search("zzz", RelPath.Root)) ==> Right("No matches.")
        ws.search("(", RelPath.Root).left.map(_.getClass.getSimpleName) ==> Left("BadPattern")
        ws.search("x", path("gone")) ==> Left(HostError.NotFound(path("gone")))
      }
    }

    test("a long matching line is cut") {
      inCheckout("a" -> ("x" * 600)) { dir =>
        val line = new LocalWorkspace(dir).search("x", RelPath.Root).map(_.text)
        line.map(_.length) ==> Right(
          "a:1: ".length + Workspace.MaxLineChars + "… [line cut]".length
        )
      }
    }

    test("write") {
      inCheckout("a.txt" -> "old", "d/x" -> "") { dir =>
        val edits = new LocalEdits(dir)
        edits.write(path("a.txt"), "new") ==> Right(())
        Files.readString(dir.resolve("a.txt")) ==> "new"
        edits.write(path("n/e/w.txt"), "made") ==> Right(())
        Files.readString(dir.resolve("n/e/w.txt")) ==> "made"
        edits.write(path("d"), "x") ==> Left(HostError.NotAFile(path("d")))
        assert(edits.write(path("a.txt/under"), "x").isLeft)
        Files.list(dir).toList.toString.contains(".grit-") ==> false
      }
    }

    test("write keeps a file's permissions") {
      inCheckout("run.sh" -> "echo") { dir =>
        val f = dir.resolve("run.sh")
        Files.setPosixFilePermissions(f, PosixFilePermissions.fromString("rwxr-x---"))
        new LocalEdits(dir).write(path("run.sh"), "echo hi") ==> Right(())
        PosixFilePermissions.toString(Files.getPosixFilePermissions(f)) ==> "rwxr-x---"
      }
    }

    test("edit") {
      inCheckout("a.txt" -> "one\r\ntwo\r\nthree\r\n") { dir =>
        val edits = new LocalEdits(dir)
        edits.edit(path("a.txt"), Vector(Replace("two", "2"), Replace("three", "3"))) ==>
          Right(Edited(Vector(2, 3)))
        Files.readString(dir.resolve("a.txt")) ==> "one\r\n2\r\n3\r\n"
        edits.edit(path("a.txt"), Vector(Replace("two", "x"))) ==>
          Left(EditError.NotFound(path("a.txt"), 0))
        Files.readString(dir.resolve("a.txt")) ==> "one\r\n2\r\n3\r\n"
        edits.edit(path("gone"), Vector(Replace("a", "b"))) ==>
          Left(EditError.Host(HostError.NotFound(path("gone"))))
      }
    }

    test("an edit through a link changes the file it names, and keeps the link") {
      inCheckout("real.txt" -> "a") { dir =>
        Files.createSymbolicLink(dir.resolve("link"), dir.resolve("real.txt"))
        new LocalEdits(dir).edit(path("link"), Vector(Replace("a", "b"))) ==>
          Right(Edited(Vector(1)))
        Files.readString(dir.resolve("real.txt")) ==> "b"
        Files.isSymbolicLink(dir.resolve("link")) ==> true
      }
    }

    test("run") {
      inCheckout("a.txt" -> "x") { dir =>
        val shell = new LocalShell(dir)
        shell.run("echo hi; ls", 10.seconds).map(r => (r.exit, r.output.show)) ==>
          Right((0, "hi\na.txt\n"))
        shell
          .run("echo out; echo err >&2; exit 3", 10.seconds)
          .map(r => (r.exit, r.output.show)) ==>
          Right((3, "out\nerr\n"))
        shell.run("cat", 10.seconds).map(_.exit) ==> Right(0)
      }
    }

    test("run clips from the tail") {
      inCheckout() { dir =>
        val ran = new LocalShell(dir).run("seq 1 3000", 10.seconds)
        ran.map(_.output.text.split("\n").headOption) ==> Right(Some("1001"))
        ran.map(_.output.hint) ==> Right(
          Some(
            "Showing the last 2000 of 3000 lines. Narrow the command's output, for example " +
              "with grep, head or tail, to see the rest."
          )
        )
      }
    }

    test("a timeout kills the command and what it started, and keeps the output so far") {
      inCheckout() { dir =>
        val marker = dir.resolve("late")
        val started = System.nanoTime()
        val ran = new LocalShell(dir).run(
          "echo before; (sleep 2; touch late) & sleep 30",
          500.millis
        )
        val took = (System.nanoTime() - started).nanos
        ran.left.map {
          case HostError.TimedOut(after, output) => (after, output.show)
          case other => (0.seconds, other.message)
        } ==> Left((500.millis, "before\n"))
        assert(took < 5.seconds)
        Thread.sleep(2500)
        Files.exists(marker) ==> false
      }
    }
  }
}
