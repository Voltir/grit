package grit.host.checkout

import java.io.IOException
import java.nio.channels.{FileChannel, OverlappingFileLockException}
import java.nio.file.attribute.{
  BasicFileAttributes,
  FileAttribute,
  PosixFilePermission,
  PosixFilePermissions
}
import java.nio.file.{
  FileVisitResult,
  Files,
  LinkOption,
  Path,
  SimpleFileVisitor,
  StandardOpenOption
}

/** A tree of files on this machine: made readable by this user alone, removed, and found left by
  * a process that ended.
  */
private[host] object LocalTree {

  /** `rwx------`: a directory this user alone may read, write or enter. */
  val OwnerOnly: FileAttribute[java.util.Set[PosixFilePermission]] =
    PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))

  /** `dir` and everything under it deleted, deepest first, a link as itself and never what it
    * leads to. A path that cannot be deleted, or a directory whose entries cannot be read, is
    * left and the rest still go. The paths left, deepest first: empty when `dir` is gone or was
    * never there.
    */
  def removed(dir: Path): Vector[Path] = {
    // Appended by the visitor below, on this thread, and read once the walk returns.
    val left = Vector.newBuilder[Path]
    def kept(p: Path): Unit = { val _ = left.addOne(p) }
    def deleted(p: Path): Unit =
      try { val _ = Files.deleteIfExists(p) }
      catch { case _: IOException => kept(p) }
    if (Files.exists(dir, LinkOption.NOFOLLOW_LINKS)) {
      try {
        val _ = Files.walkFileTree(
          dir,
          new SimpleFileVisitor[Path] {
            override def visitFile(p: Path, attrs: BasicFileAttributes): FileVisitResult = {
              deleted(p)
              FileVisitResult.CONTINUE
            }
            override def visitFileFailed(p: Path, e: IOException): FileVisitResult = {
              kept(p)
              FileVisitResult.CONTINUE
            }
            // A directory whose entries could not all be read still holds them, so its
            // deletion fails and it is kept.
            override def postVisitDirectory(d: Path, e: IOException): FileVisitResult = {
              deleted(d)
              FileVisitResult.CONTINUE
            }
          }
        )
      } catch { case _: IOException => kept(dir) }
    }
    left.result()
  }

  /** Whether `lock` was free: when no process holds a lock on it, `before` is given the directory
    * beside it of the same name less `.lock`, then that directory and `lock` are removed. A lock
    * held, one that cannot be opened, and one whose removal fails, answer false. Opening a lock
    * file this process holds lets go of its lock (POSIX locks): never call it on one's own.
    */
  def free(lock: Path)(before: Path => Unit): Boolean =
    try {
      val channel = FileChannel.open(lock, StandardOpenOption.WRITE)
      try {
        val taken =
          try Option(channel.tryLock())
          catch { case _: OverlappingFileLockException => None }
        taken.exists { _ =>
          val dir = lock.resolveSibling(lock.getFileName.toString.stripSuffix(".lock"))
          before(dir)
          val _ = removed(dir)
          Files.deleteIfExists(lock)
        }
      } finally channel.close()
    } catch { case _: IOException => false }
}
