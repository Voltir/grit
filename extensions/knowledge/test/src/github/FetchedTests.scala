package grit.knowledge.github

import grit.knowledge.card.UnitVersion

import utest.*

object FetchedTests extends TestSuite {

  private val sha = "3068d045f21e723190d01fed918ed7e6651bf59a"

  val tests = Tests {
    test("a recorded file's answer is its text and its blob's SHA") {
      val Right(version) = UnitVersion.of(sha): @unchecked
      val text = Answers.result("get-file-contents-file")("content")(1)("resource")("text").str
      assert(Fetched.of(Answers.text("get-file-contents-file")) == Fetched.Text(text, version))
    }

    test("a binary file's answer, holding no text, is Unversioned") {
      val answer = s"successfully downloaded binary file (SHA: $sha)\n" +
        "[application/octet-stream repo://octocat/Hello-World/contents/logo.png omitted]"
      assert(Fetched.of(answer) == Fetched.Unversioned)
    }

    test("a file too large to send, named with its SHA, is Unversioned") {
      val answer = "File big.bin is too large to display (2000000 bytes). Use the download URL " +
        s"to fetch the content: https://example.com/big.bin (SHA: $sha)\n" +
        "[resource big.bin: https://example.com/big.bin]"
      assert(Fetched.of(answer) == Fetched.Unversioned)
    }

    test("a file read at the default branch, its ref missing, is Unversioned") {
      val answer = s"successfully downloaded text file (SHA: $sha) Note: the provided ref 'v9' " +
        "does not exist, default branch 'main' was used instead.\n# Title\n"
      assert(Fetched.of(answer) == Fetched.Unversioned)
    }

    test("an answer with no SHA is Unversioned") {
      assert(Fetched.of("# Title\nsome text") == Fetched.Unversioned)
    }
  }
}
