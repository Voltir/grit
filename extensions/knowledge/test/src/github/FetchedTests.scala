package grit.knowledge.github

import grit.knowledge.card.UnitVersion

import utest.*

object FetchedTests extends TestSuite {

  private val sha = "3068d045f21e723190d01fed918ed7e6651bf59a"
  private val Right(version) = UnitVersion.of(sha): @unchecked

  val tests = Tests {
    test("a recorded file's answer is its text and its blob's SHA") {
      val text = Answers.result("get-file-contents-file")("content")(1)("resource")("text").str
      assert(Fetched.of(Answers.text("get-file-contents-file")) == Fetched.Text(text, version))
    }

    test("a text file's answer with nothing after its first line is its empty text") {
      // An empty blob is sent as the first line alone, or with an empty text after it.
      assert(
        Fetched.of(s"successfully downloaded text file (SHA: $sha)") == Fetched.Text("", version)
      )
      assert(
        Fetched.of(s"successfully downloaded text file (SHA: $sha)\n") == Fetched.Text("", version)
      )
    }

    test("a binary file's answer, holding no text, is Unversioned as a binary file") {
      val answer = s"successfully downloaded binary file (SHA: $sha)\n" +
        "[application/octet-stream repo://octocat/Hello-World/contents/logo.png omitted]"
      assert(Fetched.of(answer) == Fetched.Unversioned("a binary file"))
    }

    test("a file too large to send, named with its SHA, is Unversioned as too large") {
      val answer = "File big.bin is too large to display (2000000 bytes). Use the download URL " +
        s"to fetch the content: https://example.com/big.bin (SHA: $sha)\n" +
        "[resource big.bin: https://example.com/big.bin]"
      assert(Fetched.of(answer) == Fetched.Unversioned("too large to send"))
    }

    test("a file read at the default branch, its ref missing, is Unversioned as another ref's") {
      val answer = s"successfully downloaded text file (SHA: $sha) Note: the provided ref 'v9' " +
        "does not exist, default branch 'main' was used instead.\n# Title\n"
      assert(Fetched.of(answer) == Fetched.Unversioned("read at another ref than the source's"))
    }

    test("a text file's answer whose SHA is no git SHA is Unversioned saying so") {
      val answer = "successfully downloaded text file (SHA: 3068d0)\n# Title\n"
      assert(Fetched.of(answer) == Fetched.Unversioned("its SHA, 3068d0, is no git SHA"))
    }

    test("an answer with no SHA is Unversioned as no file's answer") {
      assert(Fetched.of("# Title\nsome text") == Fetched.Unversioned("not a file's answer"))
    }
  }
}
