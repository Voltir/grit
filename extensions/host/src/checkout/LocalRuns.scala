package grit.host.checkout

import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import grit.core.clock.{Clock, Fresh}
import grit.core.host.{
  Argv,
  Clipped,
  HostError,
  Kept,
  Ran,
  RelPath,
  RunId,
  RunStatus,
  Runs,
  Workspace
}
import grit.host.checkout.Checkout.attempt

/** [[Runs]] under `root` on this machine, each command started as [[LocalProcesses]] starts one
  * (the passed environment, `redaction` over what it shows), its output and errors in a file
  * under `.grit/runs` in the checkout, which git ignores, removed when the run is forgotten or
  * this is closed. Its first start in the checkout kills, with what it started, each run a
  * process that ended without [[close]] left going, and removes its files.
  */
final class LocalRuns(
    root: Path,
    environment: Map[String, String],
    redaction: Redaction,
    clock: Clock^,
    fresh: Fresh^
) extends Runs
    with AutoCloseable {
  import LocalRuns.*

  private val processes = new LocalProcesses(root, environment, redaction)
  private val runs = root.resolve(Dir)

  /** This process's [[ownPrefix]] then a fresh id: what its directory and lock under [[Dir]]
    * are named.
    */
  private val stem = s"$ownPrefix${RunId.of(fresh.nonce()).written}"
  private val own = runs.resolve(stem)
  private val lock = runs.resolve(s"$stem.lock")

  /** Its runs by id, the one started first first. Only ever read or changed under this
    * object's lock.
    */
  private val kept = new java.util.LinkedHashMap[RunId, Entry]()

  /** The lock file's channel, holding its lock, once the first start has opened it. */
  private val held = new AtomicReference[Option[FileChannel]](None)

  def start(
      argv: Argv,
      timeout: FiniteDuration,
      wait: FiniteDuration
  ): Either[HostError, (id: RunId, status: RunStatus)] =
    begun(argv, timeout.min(Runs.Longest)).flatMap { (id, entry) =>
      val _ = waited(entry, wait)
      val status = statusOf(id, entry)
      forgetting()
      status.map(s => (id = id, status = s))
    }

  def tail(id: RunId, wait: FiniteDuration): Either[HostError, (argv: Argv, status: RunStatus)] =
    found(id).flatMap { entry =>
      val _ = waited(entry, wait)
      statusOf(id, entry).map(s => (argv = entry.argv, status = s))
    }

  def stop(id: RunId): Either[HostError, (argv: Argv, status: RunStatus)] =
    found(id).flatMap { entry =>
      if (entry.process.isAlive) {
        entry.stopped.compareAndSet(-1L, clock.millis() - entry.from)
        LocalProcesses.kill(entry.process)
      }
      statusOf(id, entry).map(s => (argv = entry.argv, status = s))
    }

  /** Kills every run still going, removes every run's files, and lets go of the lock. */
  def close(): Unit = {
    val entries = synchronized {
      val all = kept.values.asScala.toVector
      kept.clear()
      all
    }
    entries.foreach(e => if (e.process.isAlive) LocalProcesses.kill(e.process))
    held.getAndSet(None).foreach { channel =>
      val _ = LocalTree.removed(own)
      channel.close()
      val _ = attempt(Files.deleteIfExists(lock))
    }
  }

  /** `argv` started and kept under a fresh id, its watcher started; `Busy` when [[Runs.Most]]
    * are going.
    */
  private def begun(argv: Argv, timeout: FiniteDuration): Either[HostError, (RunId, Entry)] =
    synchronized {
      opened().flatMap { _ =>
        if (kept.values.asScala.count(_.process.isAlive) >= Runs.Most)
          Left(HostError.Busy(Runs.Most))
        else {
          val id = unused()
          val out = own.resolve(s"${id.written}.out")
          val pid = own.resolve(s"${id.written}.pid")
          processes.writing(argv, RelPath.Root, out).flatMap { process =>
            attempt(Files.writeString(pid, recorded(process.toHandle))) match {
              case Left(failed) =>
                LocalProcesses.kill(process)
                Left(failed)
              case Right(_) =>
                val entry = Entry(argv, process, clock.millis(), timeout, out, pid)
                val _ = kept.put(id, entry)
                watch(entry)
                Right((id, entry))
            }
          }
        }
      }
    }

  /** The lock taken, when this has none yet: `.grit/runs` made, with a `.gitignore` of `*`,
    * what earlier processes left swept, and this one's directory made.
    */
  private def opened(): Either[HostError, Unit] =
    if (held.get.nonEmpty) Right(())
    else
      attempt {
        Files.createDirectories(runs)
        val ignore = runs.resolve(".gitignore")
        if (!Files.exists(ignore)) { val _ = Files.writeString(ignore, "*\n") }
        swept()
        val channel = FileChannel.open(lock, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        Option(channel.tryLock()) match {
          case None =>
            channel.close()
            throw new java.io.IOException(s"$lock is held by another process")
          case Some(_) =>
            Files.createDirectories(own)
            held.set(Some(channel))
        }
      }

  /** Every other process's lock under [[Dir]] that no process holds, freed, the runs it recorded
    * killed first; and every directory without a lock but this process's own, removed.
    */
  private def swept(): Unit = {
    val mine = ownPrefix
    val entries = {
      val listing = Files.list(runs)
      try listing.iterator.asScala.toVector
      finally listing.close()
    }
    val others = entries.filterNot(_.getFileName.toString.startsWith(mine))
    others
      .filter(_.getFileName.toString.endsWith(".lock"))
      .foreach(l => { val _ = LocalTree.free(l)(killedRecorded) })
    others
      .filter(d =>
        Files.isDirectory(d) && !Files.exists(d.resolveSibling(s"${d.getFileName}.lock"))
      )
      .foreach(d => { val _ = LocalTree.removed(d) })
  }

  /** The run `id`, or `Gone`. */
  private def found(id: RunId): Either[HostError, Entry] =
    synchronized(Option(kept.get(id))).toRight(HostError.Gone(id))

  /** How the run `entry`, kept as `id`, stands; `TooLarge`, and it forgotten, when its output
    * passed [[Workspace.MaxFileBytes]].
    */
  private def statusOf(id: RunId, entry: Entry): Either[HostError, RunStatus] =
    attempt(Files.size(entry.out)).flatMap { bytes =>
      if (entry.overflowed.get || bytes > Workspace.MaxFileBytes) {
        forget(id, entry)
        written(entry.out).flatMap(at =>
          Left(HostError.TooLarge(at, bytes, Workspace.MaxFileBytes))
        )
      } else
        output(entry).map { out =>
          val alive = entry.process.isAlive
          val stoppedAfter = entry.stopped.get
          if (alive) RunStatus.Going((clock.millis() - entry.from).millis, entry.timeout, out)
          else if (stoppedAfter >= 0) RunStatus.Stopped(stoppedAfter.millis, out)
          else if (entry.timedOut.get) RunStatus.TimedOut(entry.timeout, out)
          else RunStatus.Ended(Ran(entry.process.exitValue(), out))
        }
    }

  /** `entry`'s output so far, its last lines, the hint naming its file. */
  private def output(entry: Entry): Either[HostError, Clipped] =
    written(entry.out).flatMap { at =>
      attempt(LocalProcesses.tail(entry.out)).map(text => Clipped.tail(redaction(text), hint(at)))
    }

  /** `file` as a path in the checkout. */
  private def written(file: Path): Either[HostError, RelPath] =
    RelPath.of(root.relativize(file).toString).left.map(e => HostError.Failed(e.message))

  /** The ended runs past [[Runs.Kept]] forgotten, the one started first first. */
  private def forgetting(): Unit = {
    val past = synchronized {
      val ended = kept.asScala.toVector.filterNot((_, e) => e.process.isAlive)
      ended.take((ended.size - Runs.Kept).max(0))
    }
    past.foreach((id, e) => forget(id, e))
  }

  /** `entry` no longer kept as `id`: killed when it is going, its files removed. */
  private def forget(id: RunId, entry: Entry): Unit = {
    val _ = synchronized(kept.remove(id))
    if (entry.process.isAlive) LocalProcesses.kill(entry.process)
    val _ = attempt(Files.deleteIfExists(entry.out))
    val _ = attempt(Files.deleteIfExists(entry.pid))
  }

  @tailrec private def unused(): RunId = {
    val id = RunId.of(fresh.nonce())
    if (kept.containsKey(id)) unused() else id
  }
}

object LocalRuns {

  /** What every directory and lock this process makes under [[Dir]] starts with, and the sweep
    * skips by name before opening anything, since closing a lock file this process holds lets go
    * of its lock: `<pid>-<start in epoch milliseconds>-`. A dead process that had the same pid
    * (pid 1 in a container, or a pid reused) started at another instant, so its files are swept.
    * Where the system gives no start instant it is `x`, and a dead predecessor of the same pid
    * and no start instant is skipped too: its files stay until a process of another pid opens
    * the checkout.
    */
  private val ownPrefix: String = {
    val self = ProcessHandle.current()
    s"${self.pid()}-${self.info().startInstant().toScala.fold("x")(_.toEpochMilli.toString)}-"
  }

  /** `.grit/runs`: where a checkout's runs keep their files. */
  val Dir: String = ".grit/runs"

  /** 250 ms: how often a run's watcher looks at the size of its output. */
  val Poll: FiniteDuration = 250.millis

  /** A run kept: what it runs, its process, when it started by the runs' clock, its timeout,
    * its files, and how it was killed: by [[LocalRuns.stop]] (after how many milliseconds, -1
    * until then), at its timeout, or for its output's size.
    */
  private final case class Entry(
      argv: Argv,
      process: Process,
      from: Long,
      timeout: FiniteDuration,
      out: Path,
      pid: Path
  ) {
    val stopped = new AtomicLong(-1L)
    val timedOut = new AtomicBoolean(false)
    val overflowed = new AtomicBoolean(false)
  }

  /** Waits up to `wait` for `entry` to end; whether it has. */
  private def waited(entry: Entry, wait: FiniteDuration): Boolean =
    entry.process.waitFor(wait.toMillis, TimeUnit.MILLISECONDS)

  /** A thread killing `entry`, with what it started, at its timeout or once its output passes
    * [[Workspace.MaxFileBytes]], marking which.
    */
  private def watch(entry: Entry): Unit = {
    val _ = Thread.ofVirtual().start { () =>
      var left = entry.timeout.toMillis
      var done = false
      while (!done) {
        val slice = Poll.toMillis.min(left).max(0L)
        done =
          try entry.process.waitFor(slice, TimeUnit.MILLISECONDS)
          catch { case _: InterruptedException => true }
        if (!done) {
          left -= slice
          val bytes = attempt(Files.size(entry.out)).getOrElse(0L)
          if (bytes > Workspace.MaxFileBytes) {
            entry.overflowed.set(true)
            LocalProcesses.kill(entry.process)
            done = true
          } else if (left <= 0) {
            entry.timedOut.set(true)
            LocalProcesses.kill(entry.process)
            done = true
          }
        }
      }
    }
  }

  /** What a run's `.pid` file holds: its pid and its start in epoch milliseconds, `-` when the
    * system does not say.
    */
  private def recorded(process: ProcessHandle): String =
    s"${process.pid()} ${process.info().startInstant().toScala.fold("-")(_.toEpochMilli.toString)}"

  /** Each run a `.pid` file in `dir` records killed with what it started, when a process of that
    * pid still runs and started when it says; one whose start is not recorded is left.
    */
  private def killedRecorded(dir: Path): Unit =
    attempt {
      val listing = Files.list(dir)
      try listing.iterator.asScala.toVector.filter(_.getFileName.toString.endsWith(".pid"))
      finally listing.close()
    }.getOrElse(Vector.empty).foreach { file =>
      attempt(Files.readString(file, StandardCharsets.UTF_8).trim.split(" ").toList)
        .getOrElse(Nil) match {
        case pid :: start :: Nil =>
          for {
            p <- pid.toLongOption
            s <- start.toLongOption
            h <- ProcessHandle.of(p).toScala
            if h.info().startInstant().toScala.exists(_.toEpochMilli == s)
          } LocalProcesses.killTree(h)
        case _ => ()
      }
    }

  /** The words for what a run's clipped output left out, naming `at`, its whole output. */
  private def hint(at: RelPath): Option[Kept] -> Option[String] = {
    case Some(Kept(_, total, true)) =>
      Some(
        s"Showing the last ${Clipped.MaxBytes / 1024} KB of line $total, the output's last. " +
          s"The whole output is in `${RelPath.value(at)}`, for `read`."
      )
    case Some(k) =>
      Some(
        s"Showing the last ${k.lines} of ${k.total} lines. The whole output is in " +
          s"`${RelPath.value(at)}`, for `read`."
      )
    case None => None
  }
}
