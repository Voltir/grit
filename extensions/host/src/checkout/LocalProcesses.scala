package grit.host.checkout

import java.io.{InputStream, OutputStream}
import java.lang.ProcessBuilder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

import grit.core.host.{Argv, Clipped, HostError, Kept, Processes, Ran, RelPath, Started, Workspace}
import grit.host.checkout.Checkout.attempt

/** [[Processes]] under `root`, passing only the variables of `environment` that
  * [[LocalShell.passed]] keeps. [[run]]'s output goes to a temporary file, not a pipe, so a
  * program that writes more than a pipe holds never blocks on it.
  */
final class LocalProcesses(root: Path, environment: Map[String, String]) extends Processes {

  private val checkout = new Checkout(root)

  private val passed = LocalShell.passed(environment)

  def run(argv: Argv, dir: RelPath, timeout: FiniteDuration): Either[HostError, Ran] =
    ran(argv, dir, timeout, _ => None)

  /** As [[run]], its output clipped with `hint`'s words for what was left out. */
  private[checkout] def ran(
      argv: Argv,
      dir: RelPath,
      timeout: FiniteDuration,
      hint: Option[Kept] -> Option[String]
  ): Either[HostError, Ran] =
    place(dir).flatMap { at =>
      attempt(Files.createTempFile("grit-run-", ".out")).flatMap { out =>
        try {
          start(argv, at, Map.empty)(_.redirectErrorStream(true).redirectOutput(out.toFile))
            .flatMap { process =>
              process.getOutputStream.close()
              val finished = LocalProcesses.awaited(process, timeout)
              attempt(LocalProcesses.tail(out)).flatMap { text =>
                val output = Clipped.tail(text, hint)
                if (finished) Right(Ran(process.exitValue(), output))
                else Left(HostError.TimedOut(timeout, output))
              }
            }
        } finally {
          val _ = attempt(Files.deleteIfExists(out))
        }
      }
    }

  def started[A](argv: Argv, dir: RelPath, added: Map[String, String])(
      body: Started^ => A
  ): Either[HostError, A] = startedWith(argv, dir, added)(body)

  /** As [[started]], with `added` set in the program's environment: [[LocalHanding]]'s start. */
  private[checkout] def startedWith[A](argv: Argv, dir: RelPath, added: Map[String, String])(
      body: Started^ => A
  ): Either[HostError, A] =
    place(dir).flatMap { at =>
      attempt(Files.createTempFile("grit-errors-", ".out")).flatMap { errors =>
        try {
          start(argv, at, added)(_.redirectError(errors.toFile)).map { process =>
            val handle = new LocalProcesses.Handle(process, errors)
            try body(handle)
            finally handle.kill()
          }
        } finally {
          val _ = attempt(Files.deleteIfExists(errors))
        }
      }
    }

  /** Where `dir` is: `NotFound` when nothing is there, `NotADirectory` when a file is. */
  private def place(dir: RelPath): Either[HostError, Path] =
    checkout.find(dir).flatMap { at =>
      if (!Files.exists(at)) Left(HostError.NotFound(dir))
      else if (!Files.isDirectory(at)) Left(HostError.NotADirectory(dir))
      else Right(at)
    }

  /** `argv` started in `at` with only the passed variables and `added`, its streams as `redirect`
    * sets them; `Failed` when it cannot start.
    */
  private def start(argv: Argv, at: Path, added: Map[String, String])(
      redirect: ProcessBuilder => ProcessBuilder
  ): Either[HostError, Process] =
    attempt {
      val command = new java.util.ArrayList[String]()
      command.add(argv.program)
      argv.args.foreach(a => { val _ = command.add(a) })
      val builder = redirect(new ProcessBuilder(command).directory(at.toFile))
      val env = builder.environment()
      env.clear()
      (passed ++ added).foreach((k, v) => { val _ = env.put(k, v) })
      builder.start()
    }
}

private[checkout] object LocalProcesses {

  /** A program [[LocalProcesses.started]] started, its errors going to the file `errors`. */
  final class Handle(process: Process, errors: Path) extends Started {

    def input: OutputStream^{this} = process.getOutputStream

    def output: InputStream^{this} = process.getInputStream

    def await(timeout: FiniteDuration): Either[HostError, Ran] = {
      val finished = awaited(process, timeout)
      attempt(tail(errors)).flatMap { text =>
        val shown = Clipped.tail(text, _ => None)
        if (finished) Right(Ran(process.exitValue(), shown))
        else Left(HostError.TimedOut(timeout, shown))
      }
    }

    def kill(): Unit = if (process.isAlive) LocalProcesses.kill(process)
  }

  /** Whether `process` ended within `timeout`; killed, with its descendants, when it did not. */
  def awaited(process: Process, timeout: FiniteDuration): Boolean = {
    val finished =
      try process.waitFor(timeout.toMillis, TimeUnit.MILLISECONDS)
      catch {
        case e: InterruptedException =>
          kill(process)
          throw e
      }
    if (!finished) kill(process)
    finished
  }

  /** The text of the file `out`, or of its last [[Workspace.MaxFileBytes]] when it is longer;
    * bytes that are not UTF-8 read as U+FFFD.
    */
  def tail(out: Path): String = {
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

  /** Kills `process` and its descendants, and waits for it to end, and for each descendant up to
    * [[DescendantWait]].
    */
  def kill(process: Process): Unit = {
    // The parent first, then what it started (listed before, while the parent still holds
    // them): a shell outliving its child reports the kill ("Killed") into the output.
    val descendants = process.descendants().toList
    val _ = process.destroyForcibly()
    descendants.forEach(p => { val _ = p.destroyForcibly() })
    val _ = process.waitFor()
    // A killed descendant is gone only once reaped; until then it still reads as alive.
    descendants.forEach { p =>
      try { val _ = p.onExit().get(DescendantWait.toMillis, TimeUnit.MILLISECONDS) }
      catch { case _: java.util.concurrent.TimeoutException => () }
    }
  }

  /** 1 second: how long a kill waits for each killed descendant to be gone. */
  val DescendantWait: FiniteDuration = FiniteDuration(1, TimeUnit.SECONDS)
}
