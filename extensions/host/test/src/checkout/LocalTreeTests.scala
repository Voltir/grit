package grit.host.checkout

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import utest.*

/** [[LocalTree]] against a temporary directory. */
object LocalTreeTests extends TestSuite {

  val tests: Tests = Tests {
    test("a tree's removal carries on past an entry it cannot delete, and answers what it left") {
      val dir = Files.createTempDirectory("grit-tree")
      val locked = dir.resolve("locked")
      def write(name: String): Path = {
        val f = dir.resolve(name)
        Files.createDirectories(f.getParent)
        Files.writeString(f, name)
      }
      Vector("a.txt", "sub/b.txt", "z.txt", "locked/kept").foreach(write)
      Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("r-x------"))
      try {
        val left = LocalTree.removed(dir)
        val there = {
          val walk = Files.walk(dir)
          try walk.toArray.toVector.collect { case p: Path => dir.relativize(p).toString }.sorted
          finally walk.close()
        }
        (left, there) ==> (
          Vector(locked.resolve("kept"), locked, dir),
          Vector("", "locked", "locked/kept")
        )
      } finally {
        Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("rwx------"))
        val _ = LocalTree.removed(dir)
      }
    }
  }
}
