package grit.host.checkout

import java.io.{BufferedReader, InputStreamReader, OutputStream}
import java.lang.ProcessBuilder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

import scala.concurrent.duration.FiniteDuration

import grit.core.host.{Argv, Clipped, HostError, Kept, Processes, Ran, RelPath, Started, Workspace}
import grit.host.checkout.Checkout.attempt

/** [[Processes]] under `root`, passing only the variables of `environment` that
  * [[LocalShell.passed]] keeps; what a program shows, its output and its errors, is cleared by
  * `redaction`. [[run]]'s output goes to a temporary file, not a pipe, so a program that writes
  * more than a pipe holds never blocks on it.
  */
final class LocalProcesses(root: Path, environment: Map[String, String], redaction: Redaction)
    extends Processes {

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
    attempt(Files.createTempFile("grit-run-", ".out")).flatMap { out =>
      try {
        writing(argv, dir, out).flatMap(LocalProcesses.ended(_, timeout, out, redaction, hint))
      } finally {
        val _ = attempt(Files.deleteIfExists(out))
      }
    }

  /** `argv` started in `dir` with nothing on its input, its output and errors written to the
    * file `out` as they come, not a pipe, so it never blocks on them. `Left` as [[started]].
    */
  private[checkout] def writing(argv: Argv, dir: RelPath, out: Path): Either[HostError, Process] =
    place(dir).flatMap { at =>
      start(argv, at, Map.empty)(_.redirectErrorStream(true).redirectOutput(out.toFile))
        .flatMap(process => attempt(process.getOutputStream.close()).map(_ => process))
    }

  def started[A](argv: Argv, dir: RelPath)(body: Started^ => A): Either[HostError, A] =
    startedWith(argv, dir, Map.empty)(body)

  /** As [[started]], with `added` set in the program's environment: [[LocalHanding]]'s start. */
  private[checkout] def startedWith[A](argv: Argv, dir: RelPath, added: Map[String, String])(
      body: Started^ => A
  ): Either[HostError, A] =
    place(dir).flatMap { at =>
      attempt(Files.createTempFile("grit-errors-", ".out")).flatMap { errors =>
        try {
          start(argv, at, added)(_.redirectError(errors.toFile)).map { process =>
            val handle = new LocalProcesses.Handle(process, errors, redaction)
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

  /** A program [[LocalProcesses.started]] started, its errors going to the file `errors`, what
    * it shows cleared by `redaction`.
    */
  final class Handle(process: Process, errors: Path, redaction: Redaction) extends Started {

    def input: OutputStream^{this} = process.getOutputStream

    // The program's lines as one thread reads them, `None` once its output ends; bounded, so a
    // program whose output nobody reads still blocks. Filled by the reading thread and drained
    // by the callers of `lines` and `next`, each through the queue's own locking; it holds
    // strings alone, so no capability flows through it.
    @caps.unsafe.untrackedCaptures
    private val read = new ArrayBlockingQueue[Option[String]](LocalProcesses.Buffered)

    // Whether the reading thread has started, and whether the program was killed: each set once,
    // by its own atomic operation; booleans, so no capability flows through them.
    @caps.unsafe.untrackedCaptures
    private val reading = new AtomicBoolean(false)
    @caps.unsafe.untrackedCaptures
    private val killed = new AtomicBoolean(false)

    def lines: Iterator[String]^{this} =
      Iterator
        .continually(taken(None).flatten)
        .takeWhile(_.isDefined)
        .collect { case Some(line) => line }

    def next(within: FiniteDuration): Started.Next =
      taken(Some(within)) match {
        case Some(Some(line)) => Started.Next.Line(line)
        case Some(None) => Started.Next.Ended
        case None => Started.Next.Quiet
      }

    /** The next of [[read]], waiting for it up to `within` or without end: `Some(None)` at the
      * end, which stays there for every later reader; `None` when none came in time.
      */
    private def taken(within: Option[FiniteDuration]): Option[Option[String]] = {
      if (reading.compareAndSet(false, true)) {
        val reader =
          new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
        val _ = Thread.ofVirtual().name("grit-program-lines").start { () =>
          try
            Iterator
              .continually(reader.readLine())
              .takeWhile(_ != null)
              .foreach(line => queued(Some(redaction(line))))
          catch { case _: java.io.IOException => () }
          finally queued(None)
        }
      }
      val got = within match {
        case None => Some(read.take())
        case Some(w) => Option(read.poll(w.toMillis, TimeUnit.MILLISECONDS))
      }
      // The end is put back, so each later read finds it too.
      got.foreach(line => if (line.isEmpty) { val _ = read.offer(None) })
      got
    }

    def await(timeout: FiniteDuration): Either[HostError, Ran] =
      ended(process, timeout, errors, redaction, _ => None)

    /** `line` queued once there is room, or dropped once the program was killed: nothing reads
      * it then, and the reading thread must not wait on a queue nobody drains.
      */
    private def queued(line: Option[String]): Unit =
      while (
        !killed.get() && !read.offer(line, LocalProcesses.Offer.toMillis, TimeUnit.MILLISECONDS)
      ) ()

    def kill(): Unit = {
      killed.set(true)
      if (process.isAlive) LocalProcesses.kill(process)
      read.clear()
      val _ = read.offer(None)
    }
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

  /** `process` [[awaited]] up to `timeout`, then the file `out` [[shown]]: `Ran` with its exit
    * code when it ended in time, else `TimedOut` with what it showed.
    */
  def ended(
      process: Process,
      timeout: FiniteDuration,
      out: Path,
      redaction: Redaction,
      hint: Option[Kept] -> Option[String]
  ): Either[HostError, Ran] = {
    val finished = awaited(process, timeout)
    shown(out, redaction, hint).flatMap { output =>
      if (finished) Right(Ran(process.exitValue(), output))
      else Left(HostError.TimedOut(timeout, output))
    }
  }

  /** The file `out`'s [[tail]], cleared by `redaction` and clipped from its end with `hint`'s
    * words.
    */
  def shown(
      out: Path,
      redaction: Redaction,
      hint: Option[Kept] -> Option[String]
  ): Either[HostError, Clipped] =
    attempt(tail(out)).map(text => Clipped.tail(redaction(text), hint))

  /** The words for what an output clipped from its end left out: the lines kept of how many,
    * then `cut`; or, when only the end of its last line is kept, that it is the last
    * [[Clipped.MaxBytes]] of that line, then `partial`.
    */
  def tailHint(cut: String, partial: String): Option[Kept] -> Option[String] = {
    case Some(Kept(_, total, true)) =>
      Some(
        s"Showing the last ${Clipped.MaxBytes / 1024} KB of line $total, the output's last. " +
          partial
      )
    case Some(k) => Some(s"Showing the last ${k.lines} of ${k.total} lines. $cut")
    case None => None
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
    killTree(process.toHandle)
    // The handle's onExit can complete before the Process records its exit, so isAlive may
    // still be true just after; waitFor returns only once it has.
    val _ = process.waitFor()
  }

  /** As [[kill]], for a process this one may not have started: nothing is waited for when it
    * cannot be killed, as one another user runs.
    */
  def killTree(process: ProcessHandle): Unit = {
    // The parent first, then what it started (listed before, while the parent still holds
    // them): a shell outliving its child reports the kill ("Killed") into the output.
    val descendants = process.descendants().toList
    val destroyed = process.destroyForcibly()
    descendants.forEach(p => { val _ = p.destroyForcibly() })
    if (destroyed) { val _ = process.onExit().get() }
    // A killed descendant is gone only once reaped; until then it still reads as alive.
    descendants.forEach { p =>
      try { val _ = p.onExit().get(DescendantWait.toMillis, TimeUnit.MILLISECONDS) }
      catch { case _: java.util.concurrent.TimeoutException => () }
    }
  }

  /** 256: the most lines of a started program's output read ahead of its reader; past them, the
    * program blocks until one is read.
    */
  val Buffered: Int = 256

  /** 100 ms: how often the reading thread, waiting for room, looks whether its program was killed. */
  private val Offer: FiniteDuration = FiniteDuration(100, TimeUnit.MILLISECONDS)

  /** 1 second: how long a kill waits for each killed descendant to be gone. */
  val DescendantWait: FiniteDuration = FiniteDuration(1, TimeUnit.SECONDS)
}
