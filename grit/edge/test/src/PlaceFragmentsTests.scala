package grit.edge

import grit.core.host.InstructionFile
import grit.core.message.{Message, Tokens}
import grit.core.place.Service
import grit.core.prompt.{Fragment, Layer}
import grit.core.provider.TokenEstimator

import utest.*

/** [[PlaceFragments]]: the bounds on a place's instruction files, and declared instructions. */
object PlaceFragmentsTests extends TestSuite {

  /** A token per character, so the layer's cap is a count of characters. */
  private object PerChar extends TokenEstimator {
    def message(message: Message): Tokens = Tokens(0)
    def system(prompt: String): Tokens = Tokens(prompt.length.toLong)
  }

  private def file(path: String, text: String): InstructionFile =
    InstructionFile(path, text, cut = false)

  /** Each fragment's source and the length of its text past the heading. */
  private def shape(files: Vector[InstructionFile]): Vector[(String, Int, Boolean)] =
    PlaceFragments.of(files, PerChar).map { f =>
      val body = f.text.stripPrefix(s"Instructions from ${f.source}:\n\n")
      val cut = body.endsWith("\n\n[The rest of this file is not shown.]")
      (f.source, body.stripSuffix("\n\n[The rest of this file is not shown.]").length, cut)
    }

  val tests = Tests {
    test(
      "the nearest file is kept as read; a farther one is cut at 8 KiB before a character it would split, and says so"
    ) {
      // One byte, then two a character: 8 KiB holds the one and 4095 of them, 8191 bytes; the
      // next would straddle the cap.
      val far = file("/AGENTS.md", "a" + "é" * 5000)
      val near = file("/x/AGENTS.md", "n" * 6000)
      shape(Vector(far, near)) ==> Vector(("/AGENTS.md", 4096, true), ("/x/AGENTS.md", 6000, false))
    }

    test("the nearest file is kept even when it alone is over the layer's tokens") {
      shape(Vector(file("/AGENTS.md", "far"), file("/x/AGENTS.md", "n" * 20000))) ==>
        Vector(("/x/AGENTS.md", 20000, false))
    }

    test("a nearest file cut when it was read still says so") {
      shape(Vector(InstructionFile("/x/AGENTS.md", "read up to the cap", cut = true))) ==>
        Vector(("/x/AGENTS.md", 18, true))
    }

    test("a file whose text a farther one already holds is left out") {
      val files = Vector(
        file("/AGENTS.md", "same"),
        file("/x/CLAUDE.md", "same"),
        file("/x/y/AGENTS.md", "other")
      )
      shape(files).map(_._1) ==> Vector("/AGENTS.md", "/x/y/AGENTS.md")
    }

    test("while the layer is over its tokens, the farthest file left is left out") {
      // Each is about 5,000 tokens here, so three are over 12,000 and two are not.
      val files = Vector(
        file("/AGENTS.md", "a" * 5000),
        file("/x/AGENTS.md", "b" * 5000),
        file("/x/y/AGENTS.md", "c" * 5000)
      )
      shape(files).map(_._1) ==> Vector("/x/AGENTS.md", "/x/y/AGENTS.md")
    }

    test("a declared text is one Place fragment from the place, headed with it; blank is refused") {
      val github =
        Service.of("github").fold(why => throw new java.lang.AssertionError(why), _.place)
      (
        PlaceFragments.declared(github, "  ours is octocat/Hello-World\n"),
        PlaceFragments.declared(github, " \n\t")
      ) ==> (
        Right(
          Fragment(
            Layer.Place,
            "service:github",
            "Instructions for service:github:\n\nours is octocat/Hello-World"
          )
        ),
        Left("the instructions declared for service:github are blank")
      )
    }
  }
}
