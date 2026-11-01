package grit.host.checkout

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import grit.core.host.{Argv, Handing, HostError, RelPath, Started, Variables}
import grit.core.secret.{Reveal, Secret}
import grit.host.checkout.Checkout.attempt

/** [[Handing]] under `root`, starting a program as [[LocalProcesses]] does over `environment`.
  * The secret's file is `secret` in a directory of its own under the system temp directory,
  * the directory readable by this user alone (0700) and the file too (0600). What the program
  * shows is cleared by `redaction`.
  */
final class LocalHanding(root: Path, environment: Map[String, String], redaction: Redaction)
    extends Handing {

  private val processes = new LocalProcesses(root, environment, redaction)

  def started[A](dir: RelPath, secret: Secret, variables: Variables)(
      argv: String -> Either[String, Argv]
  )(body: Started^ => A): Either[HostError, A] =
    attempt(Files.createTempDirectory("grit-handed-", LocalHanding.Owner)).flatMap { at =>
      try {
        for {
          file <- attempt {
            val file = Files.createFile(at.resolve("secret"), LocalHanding.Readable)
            Files.writeString(file, Reveal.text(secret), StandardCharsets.UTF_8)
          }
          command <- argv(file.toAbsolutePath.toString).left.map(HostError.Failed(_))
          ran <- processes.startedWith(command, dir, variables.values)(body)
        } yield ran
      } finally LocalHanding.removed(at)
    }
}

private object LocalHanding {

  /** `dir` and all in it deleted, as far as it can be; a file left is one this user alone reads. */
  def removed(dir: Path): Unit = {
    val _ = attempt {
      val walk = Files.walk(dir)
      try walk.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
      finally walk.close()
    }
  }

  val Owner = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))
  val Readable = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
}
