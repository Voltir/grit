package grit.dbos.engine

import utest.*

/** The build-info resource as Mill writes it (`build.mill`'s `buildInfo`), read back. */
object BuildTests extends TestSuite {

  private val Sha = "8f0615074f511e91cc295e367d7b883352f915a2"

  val tests = Tests {
    test("a commit and its dirty flag read as that build") {
      Build.of(s"commit=$Sha\ndirty=true\n") ==> Build.Known(Sha, dirty = true)
      Build.of(s"commit=$Sha\ndirty=false\n") ==> Build.Known(Sha, dirty = false)
    }

    test("a build outside git reads as Unknown, never as a commit named unknown") {
      Build.of("commit=unknown\n") ==> Build.Unknown
    }

    test("a resource without its dirty flag, or with a malformed commit, reads as Unknown") {
      Build.of(s"commit=$Sha\n") ==> Build.Unknown
      Build.of("commit=HEAD\ndirty=false\n") ==> Build.Unknown
      Build.of("") ==> Build.Unknown
    }

    test("a unit test's classpath has no resource, so this process's build is Unknown") {
      Build.current ==> Build.Unknown
    }
  }
}
