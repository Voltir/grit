package grit.knowledge.github

import grit.knowledge.card.{UnitKind, UnitVersion}

import utest.*

object ListingTests extends TestSuite {

  private def version(sha: String): UnitVersion =
    UnitVersion.of(sha).fold(why => throw new java.lang.AssertionError(why), identity)

  private val directory = Answers.text("get-file-contents-directory")

  val tests = Tests {
    test("a recorded directory's answer lists every entry with its kind and SHA") {
      val Right(Listing.Complete(entries)) = Listing.of(directory): @unchecked
      assert(entries.size == 16)
      assert(
        entries.headOption == Some(
          Entry(
            "docs/error-handling.md",
            UnitKind.File,
            version("d483ebdec2b55d509f3ad89a83cd713e45d36337")
          )
        )
      )
      assert(
        entries.filter(_.kind == UnitKind.Directory) == Vector(
          Entry(
            "docs/installation-guides",
            UnitKind.Directory,
            version("06826bd559e13531873832a7be3b4b9ccd27f4a7")
          )
        )
      )
    }

    test("a directory entry's SHA is its tree's, as its git_url names it") {
      val recorded = ujson.read(Answers.text("get-file-contents-directory-git-url")).arr.toVector
      val Right(Listing.Complete(entries)) =
        Listing.of(Answers.text("get-file-contents-directory-git-url")): @unchecked
      val trees = recorded.filter(_("type").str == "dir").map(e => e("git_url").str)
      val dirs = entries.filter(_.kind == UnitKind.Directory)
      assert(dirs.size == 1)
      assert(
        trees == dirs.map(d =>
          s"https://api.github.com/repos/octocat/Hello-World/git/trees/${UnitVersion.value(d.version)}"
        )
      )
    }

    test("an answer cut to its leading entries is Partial, showing them") {
      val Right(Listing.Complete(all)) = Listing.of(directory): @unchecked
      val items = ujson.read(directory).arr.toVector
      val cut = ujson.write(ujson.Arr.from(items.take(5))) +
        "\n\n[Showing the first 5 of the 16 items in this answer; call again asking for fewer, " +
        "or for the next page, to see the rest.]"
      assert(Listing.of(cut) == Right(Listing.Partial(all.take(5))))
    }

    test("an answer naming possible path matches is Partial, showing none") {
      assert(
        Listing.of(Answers.text("get-file-contents-match")) == Right(Listing.Partial(Vector.empty))
      )
    }

    test("a symlink or submodule is left out") {
      val sha = "d483ebdec2b55d509f3ad89a83cd713e45d36337"
      val answer = ujson.write(
        ujson.Arr(
          ujson.Obj("type" -> "symlink", "path" -> "link", "sha" -> sha),
          ujson.Obj("type" -> "submodule", "path" -> "vendor/lib", "sha" -> sha),
          ujson.Obj("type" -> "file", "path" -> "a.md", "sha" -> sha)
        )
      )
      assert(
        Listing.of(answer) == Right(
          Listing.Complete(Vector(Entry("a.md", UnitKind.File, version(sha))))
        )
      )
    }

    test("a file's answer, an error's, or an entry without a SHA is no listing") {
      assert(
        Listing.of(Answers.text("get-file-contents-file")) == Left("not a directory's listing")
      )
      assert(Listing.of("Failed to get file contents.") == Left("not a directory's listing"))
      val noSha = """[{"type":"file","path":"a.md"}]"""
      assert(
        Listing.of(noSha) == Left(
          """a listing's entry has no string sha: {"type":"file","path":"a.md"}"""
        )
      )
    }
  }
}
