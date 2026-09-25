package grit.host

import java.lang.ProcessBuilder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

import grit.core.host.{Clipped, HostError, Kept, Ran, Shell, Workspace}

/** Running commands in the checkout whose root is `root`, as this JVM's user and with its
  * environment. Output goes to a temporary file, not a pipe, so a command that writes more
  * than a pipe holds never blocks on it. A timeout kills the command and every process it
  * started that is still its descendant; one that detached itself survives.
  */
final class LocalShell(root: Path) extends Shell {
  import Checkout.attempt

  private val checkout = new Checkout(root)

  def run(command: String, timeout: FiniteDuration): Either[HostError, Ran] =
    checkout.real.flatMap { top =>
      attempt(Files.createTempFile("grit-run-", ".out")).flatMap { out =>
        try {
          val argv = new java.util.ArrayList[String]()
          argv.add("sh")
          argv.add("-c")
          argv.add(command)
          attempt(
            new ProcessBuilder(argv)
              .directory(top.toFile)
              .redirectErrorStream(true)
              .redirectOutput(out.toFile)
              .start()
          ).flatMap { process =>
            process.getOutputStream.close()
            val finished =
              try process.waitFor(timeout.toMillis, TimeUnit.MILLISECONDS)
              catch {
                case e: InterruptedException =>
                  kill(process)
                  throw e
              }
            if (!finished) kill(process)
            attempt(tail(out)).map { text =>
              val output = Clipped.tail(text, hint)
              if (finished) Right(Ran(process.exitValue(), output))
              else Left(HostError.TimedOut(timeout, output))
            }.flatten
          }
        } finally {
          val _ = attempt(Files.deleteIfExists(out))
        }
      }
    }

  /** The text of the file `out`, or of its last [[Workspace.MaxFileBytes]] when it is
    * longer; bytes that are not UTF-8 read as U+FFFD.
    */
  private def tail(out: Path): String = {
    val channel = Files.newByteChannel(out)
    try {
      val size = channel.size()
      val start = (size - Workspace.MaxFileBytes).max(0L)
      val buffer = java.nio.ByteBuffer.allocate((size - start).toInt)
      channel.position(start)
      while (buffer.hasRemaining && channel.read(buffer) >= 0) {}
      StandardCharsets.UTF_8.decode(buffer.flip()).toString
    } finally channel.close()
  }

  /** Kills `process` and its descendants, and waits for it to end. */
  private def kill(process: Process): Unit = {
    val descendants = process.descendants().toList
    descendants.forEach(p => { val _ = p.destroyForcibly() })
    val _ = process.destroyForcibly()
    val _ = process.waitFor()
  }

  private val hint: Option[Kept] -> Option[String] = {
    case Some(Kept(_, total, true)) =>
      Some(
        s"Showing the last ${Clipped.MaxBytes / 1024} KB of line $total, the output's last. " +
          "Narrow the command's output to see the rest."
      )
    case Some(k) =>
      Some(
        s"Showing the last ${k.lines} of ${k.total} lines. Narrow the command's output, " +
          "for example with grep, head or tail, to see the rest."
      )
    case None => None
  }
}
