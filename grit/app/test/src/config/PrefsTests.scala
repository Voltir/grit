package grit.app.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import utest.*

/** The preferences file: read, written, and found, only ever under a temporary directory. */
object PrefsTests extends TestSuite {

  /** `body` given a fresh directory under the system's temporary one, removed afterwards. */
  private def inTemp[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-prefs-test")
    try body(dir)
    finally {
      val all = Files.walk(dir)
      try all.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
      finally all.close()
    }
  }

  val tests = Tests {
    test("the theme is read back as written") {
      Prefs.parse(Prefs.render(Prefs(Some("abyss")))) ==> Prefs(Some("abyss"))
      Prefs.parse(Prefs.render(Prefs.empty)) ==> Prefs.empty
    }

    test("a line grit does not know is ignored; the last theme wins") {
      Prefs.parse("# hi\nfont=large\n  theme = nightshade \nnonsense\ntheme=frost\n") ==>
        Prefs(Some("frost"))
      Prefs.parse("theme=\n") ==> Prefs.empty
      Prefs.parse("") ==> Prefs.empty
    }

    test("it lives under XDG_CONFIG_HOME, or else under HOME's .config") {
      Prefs.path(Map("XDG_CONFIG_HOME" -> "/x", "HOME" -> "/h")) ==> Some(Path.of("/x/grit/prefs"))
      Prefs.path(Map("XDG_CONFIG_HOME" -> "", "HOME" -> "/h")) ==>
        Some(Path.of("/h/.config/grit/prefs"))
      Prefs.path(Map.empty) ==> None
    }

    test("saved, its directories made, and loaded again; a missing file is nothing") {
      inTemp { dir =>
        val file = dir.resolve("config").resolve("grit").resolve("prefs")
        Prefs.load(file) ==> Prefs.empty
        Prefs.save(file, Prefs(Some("tokyo-moon"))) ==> Right(file)
        Prefs.load(file) ==> Prefs(Some("tokyo-moon"))
        Prefs.save(file, Prefs(Some("abyss"))) ==> Right(file)
        Prefs.load(file) ==> Prefs(Some("abyss"))
        // Nothing staged is left beside it.
        Files.list(file.getParent).count() ==> 1L
      }
    }

    // Not "says why": for a directory that is a file, the JDK's message is the bare path.
    test("a file that cannot be written is an error, not a throw") {
      inTemp { dir =>
        val blocked = dir.resolve("a-file")
        val _ = Files.writeString(blocked, "not a directory")
        assert(Prefs.save(blocked.resolve("prefs"), Prefs(Some("abyss"))).isLeft)
      }
    }

    test("a file that cannot be read, or is not a file, is nothing") {
      inTemp { dir =>
        // A theme line, then an é in Latin-1, a byte that is not UTF-8: the whole file is
        // unreadable, the theme line included.
        val garbled = dir.resolve("prefs")
        val _ = Files.writeString(garbled, "theme=abyss\né\n", StandardCharsets.ISO_8859_1)
        Prefs.load(garbled) ==> Prefs.empty
        Prefs.load(dir) ==> Prefs.empty
      }
    }
  }
}
