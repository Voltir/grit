package grit.outline.locate

import utest.*

object RevisionTests extends TestSuite {

  private val commit = "0123456789abcdef0123456789abcdef01234567"

  def tests = Tests {
    test("a git dir on a branch gives the branch and the first 8 characters of its commit") {
      val dir = os.temp.dir(prefix = "outline-git-")
      os.makeDir.all(dir / ".git" / "refs" / "heads")
      os.write(dir / ".git" / "HEAD", "ref: refs/heads/main\n")
      os.write(dir / ".git" / "refs" / "heads" / "main", commit + "\n")
      assert(Locate.revision(Root(dir)) == Revision(Some("main"), "01234567"))
    }

    test("a .git file naming a git dir gives the same revision") {
      val dir = os.temp.dir(prefix = "outline-worktree-")
      val gitDir = os.temp.dir(prefix = "outline-gitdir-")
      os.makeDir.all(gitDir / "refs" / "heads")
      os.write(gitDir / "HEAD", "ref: refs/heads/main\n")
      os.write(gitDir / "refs" / "heads" / "main", commit + "\n")
      os.write(dir / ".git", s"gitdir: $gitDir\n")
      assert(Locate.revision(Root(dir)) == Revision(Some("main"), "01234567"))
    }
  }
}
