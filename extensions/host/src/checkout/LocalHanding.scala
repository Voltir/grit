package grit.host.checkout

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import grit.core.host.{Argv, Handing, HostError, RelPath, Started, Variables}
import grit.core.secret.{Reveal, Secret}
import grit.host.checkout.Checkout.attempt

/** [[Handing]] under `root`, starting a program as [[LocalProcesses]] does over `environment`.
  * The secrets' files are `secret-0`, `secret-1`, … in a directory of their own under the system
  * temp directory, the directory readable by this user alone (0700) and each file too (0600). What the program
  * shows is cleared by `redaction`.
  */
final class LocalHanding(root: Path, environment: Map[String, String], redaction: Redaction)
    extends Handing {

  private val processes = new LocalProcesses(root, environment, redaction)

  def startedAll[A](dir: RelPath, secrets: Vector[Secret], variables: Variables)(
      argv: Vector[String] -> Either[String, Argv]
  )(body: Started^ => A): Either[HostError, A] =
    attempt(Files.createTempDirectory("grit-handed-", LocalTree.OwnerOnly)).flatMap { at =>
      try {
        for {
          files <- attempt {
            secrets.zipWithIndex.map { (secret, i) =>
              val file = Files.createFile(at.resolve(s"secret-$i"), LocalHanding.Readable)
              Files.writeString(file, Reveal.text(secret), StandardCharsets.UTF_8)
            }
          }
          command <- argv(files.map(_.toAbsolutePath.toString)).left.map(HostError.Failed(_))
          ran <- processes.startedWith(command, dir, variables.values)(body)
        } yield ran
      } finally { val _ = LocalTree.removed(at) }
    }
}

private object LocalHanding {

  val Readable = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
}
