package grit.host.checkout

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import utest.*

/** [[LocalTree]] against a temporary directory. */
object LocalTreeTests extends TestSuite {

  val tests: Tests = Tests {
    test("a free lock is removed with its directory, which before sees first; a held one stays") {
      val dir = Files.createTempDirectory("grit-free")
      try {
        Files.createDirectories(dir.resolve("left/sub"))
        Files.writeString(dir.resolve("left/sub/f"), "f")
        Files.writeString(dir.resolve("left.lock"), "")
        Files.createDirectories(dir.resolve("held"))
        Files.writeString(dir.resolve("held.lock"), "")
        val channel = java.nio.channels.FileChannel
          .open(dir.resolve("held.lock"), java.nio.file.StandardOpenOption.WRITE)
        try {
          val _ = channel.lock()
          var seen = Vector.empty[Boolean]
          val freed = LocalTree.free(dir.resolve("left.lock")) { d =>
            seen = seen :+ Files.exists(d.resolve("sub/f"))
          }
          val stayed = LocalTree.free(dir.resolve("held.lock"))(_ => seen = seen :+ false)
          (
            freed,
            stayed,
            seen,
            Files.exists(dir.resolve("left")),
            Files.exists(dir.resolve("held"))
          ) ==>
            (true, false, Vector(true), false, true)
        } finally channel.close()
      } finally { val _ = LocalTree.removed(dir) }
    }

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
