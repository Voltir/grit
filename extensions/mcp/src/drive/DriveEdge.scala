package grit.mcp.drive

import java.io.IOException
import java.net.URI
import java.nio.channels.{FileChannel, OverlappingFileLockException}
import java.nio.file.{Files, Path, StandardOpenOption}

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.files.Formats
import grit.core.id.{EdgeName, PrincipalId}
import grit.core.prompt.Fragment
import grit.core.store.StoreError
import grit.edge.{PlaceFragments, Server}
import grit.mcp.client.{Credential, KeyFiles}

/** An edge serving Drive's files at its service's place (ADR 0017). */
object DriveEdge {

  /** What the model is told of Drive at its place: the Place layer [[serving]] advertises by
    * default.
    */
  val Instructions: String =
    "drive_list lists the shared drives, or what is directly in one folder or shared drive; " +
      "drive_search finds Drive files by the words in them; and drive_read reads one by its id: " +
      "a Doc, a Sheet or a text file whole, and a PDF, a Word file or an Excel file a part at a " +
      "time, starting with its outline. " +
      "Files in folders that no one has labelled yet are not shown, so a folder may look " +
      "empty when it isn't. If someone asks about such a folder, tell them an administrator " +
      "can open it to you with the label command, typing " +
      s"${DriveItems.Opening}: the link Drive shows for the folder, or " +
      "the folder's id from a list or a search."

  /** The edge named for `reading`'s service, serving [[DriveTools.List]], [[DriveTools.Search]]
    * and [[DriveTools.Read]] there over Drive's API at `api` with `credential`'s tokens, read as
    * `reading` says, with `instructions` as the place's Place layer; or why not:
    * `instructions` blank, or `api` not an absolute URL. Opened, it registers the service's
    * place for grit itself ([[PrincipalId.Grit]]), advertises the three there, logs that it
    * serves them, and serves the requests addressed there with [[DriveTools]], reading PDFs,
    * Word and Excel files with `formats` in a directory of this machine's temporary files that
    * closing it removes, and that a Drive edge's next opening on this machine removes when this
    * process was killed first, logging what they say of each run. It needs the
    * credential's variables, is never asked, and delivers no replies. Opening it is refused,
    * naming the credential's variables, when the credential does not open (its key read from
    * this machine's files, [[KeyFiles.Local]]) or Drive does not
    * take its token; when Drive cannot be reached, is busy or answers what cannot be read, saying
    * its cause ([[DriveError.logged]]); or when the desk cannot be registered or advertise, or
    * that directory cannot be made.
    */
  def serving(
      credential: Credential,
      reading: DriveReading,
      formats: Formats,
      instructions: String = Instructions,
      api: String = DriveRest.Api
  ): Either[String, ServedEdge] =
    for {
      at <- scala.util
        .Try(URI.create(api))
        .toOption
        .filter(_.isAbsolute)
        .toRight(s"Drive's API $api is not an absolute URL")
      declared <- PlaceFragments.declared(reading.service.place, instructions)
    } yield new Edge(credential, reading, formats, at, declared)

  /** A new directory in `root` for the copies of files this edge reads, held while it is open by
    * a lock on its `.lock` file beside it, which the machine lets go when the process ends; or
    * why not, for the operator. Each `grit-drive-*` directory in `root` whose lock no process
    * holds is removed first, with the copies a read killed partway left in it; one whose lock is
    * held (another edge, here or in another deployment), and any other directory, are left as
    * they are.
    */
  private[drive] def scratch(root: Path): Either[EdgeRefusal, Scratch] =
    try {
      // One opening at a time in this JVM, so a sweep never meets a lock file another edge
      // here has made and not yet entered in Held.
      val (lock, channel) = this.synchronized {
        swept(root, Held.contains)
        locked(root)
      }
      val dir = Files.createDirectory(root.resolve(lock.getFileName.toString.stripSuffix(".lock")))
      Right(new Scratch(dir, lock, channel))
    } catch {
      case e: IOException =>
        Left(EdgeRefusal.Failed(s"no directory to read files in: ${e.getClass.getSimpleName}"))
    }

  /** A scratch directory, `dir`, and the lock that holds it. */
  private[drive] final class Scratch(val dir: Path, lock: Path, channel: FileChannel) {

    /** `dir` removed with what is in it, and its lock let go and removed. */
    def close(): Unit = {
      cleared(dir)
      // The file goes while its lock is held, so no sweep takes it between; then the lock.
      try { val _ = Files.deleteIfExists(lock) }
      catch { case _: IOException => () }
      try channel.close()
      catch { case _: IOException => () }
      val _ = Held.remove(lock)
    }
  }

  // The lock files this JVM holds, which a sweep must not open: closing any channel on a file
  // lets go every lock the process holds on it (POSIX locks; FileChannel says so), so a sweep
  // opening one would free another edge's lock. Process-wide state, an exception to STYLE rule
  // 2, because the locks it mirrors are the process's, whichever edge took them. Each holds an
  // absolute path, added by `locked` and removed by `Scratch.close`.
  private val Held: java.util.Set[Path] = java.util.concurrent.ConcurrentHashMap.newKeySet()

  /** A new `grit-drive-*.lock` in `root`, entered in [[Held]], and its channel holding its lock. */
  private def locked(root: Path): (Path, FileChannel) = {
    val lock = Files.createTempFile(root, "grit-drive-", ".lock").toAbsolutePath.normalize
    val channel = FileChannel.open(lock, StandardOpenOption.WRITE)
    val _ = channel.lock()
    // A sweep may have taken the new file's lock before this did, and removed it: a sweep
    // removes a file only while holding its lock, so one still there is this edge's.
    // Entered in Held only once it is this edge's, so a retry leaves no entry behind.
    if (Files.exists(lock)) {
      val _ = Held.add(lock)
      (lock, channel)
    } else {
      channel.close()
      locked(root)
    }
  }

  /** Every `grit-drive-*.lock` in `root` no process holds, removed with its directory while
    * its lock is held here; one `held` names (by its absolute path) is left unopened.
    */
  private[drive] def swept(root: Path, held: Path => Boolean): Unit = {
    val listed = Files.list(root)
    try
      listed.forEach { lock =>
        val name = lock.getFileName.toString
        if (
          name.startsWith("grit-drive-") && name.endsWith(".lock") &&
          !held(lock.toAbsolutePath.normalize)
        )
          try {
            val channel = FileChannel.open(lock, StandardOpenOption.WRITE)
            try {
              val elsewhere =
                try Option(channel.tryLock()).isEmpty
                catch { case _: OverlappingFileLockException => true }
              if (!elsewhere) {
                cleared(root.resolve(name.stripSuffix(".lock")))
                val _ = Files.deleteIfExists(lock)
              }
            } finally channel.close()
          } catch { case _: IOException => () }
      }
    finally listed.close()
  }

  /** `scratch` removed with any copy a read left in it, as far as this machine allows. */
  private def cleared(scratch: Path): Unit =
    try {
      if (Files.isDirectory(scratch)) {
        val left = Files.list(scratch)
        try left.forEach(p => { val _ = Files.deleteIfExists(p) })
        finally left.close()
        val _ = Files.deleteIfExists(scratch)
      }
    } catch { case _: IOException => () }

  /** The edge [[serving]] describes; `declared`, the Place layer it advertises. */
  private final class Edge(
      credential: Credential,
      reading: DriveReading,
      formats: Formats,
      api: URI,
      declared: Fragment
  ) extends ServedEdge {

    def name: EdgeName = EdgeName(reading.service.name)

    def needs: Vector[Variable] = credential.needs

    def asks: ServedEdge.Asks = ServedEdge.Asks.Never

    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] = {
      val place = reading.service.place
      val check = s"check ${credential.needs.map(Variable.value).mkString(", ")}"
      Credential.open(credential, env, KeyFiles.Local).flatMap { authorizer =>
        val rest: DriveRest^{clock} = new DriveRest(api, authorizer, clock)
        rest.reachable() match {
          case Left(DriveError.NoToken(why)) =>
            Left(EdgeRefusal.Refused(s"Drive gave no access ($why); $check"))
          // The operator reads a refusal: its cause, and the model's words only where it has none.
          case Left(other) =>
            Left(
              EdgeRefusal.Refused(
                other.logged.getOrElse(s"Drive could not be read: ${other.message}")
              )
            )
          case Right(()) =>
            stores.desks.register(PrincipalId.Grit, Set(place)) match {
              case Left(e) =>
                Left(EdgeRefusal.Failed(s"its desk could not be registered: ${e.why}"))
              case Right(desk) =>
                desk.advertise(place, DriveTools.Offered, Vector(declared)) match {
                  case Left(e) =>
                    Left(EdgeRefusal.Failed(s"its tools could not be advertised: ${e.why}"))
                  case Right(()) =>
                    DriveEdge.scratch(Path.of(System.getProperty("java.io.tmpdir"))).map {
                      scratch =>
                        log(s"${place.written}: serving drive_list, drive_search, drive_read")
                        val server = new Server(
                          desk,
                          new DriveTools(clock, rest, reading, formats, scratch.dir),
                          run => { val _ = Thread.ofVirtual().start(() => run()) },
                          said => log(s"${place.written}: $said")
                        )
                        server.serve()
                        new ServedEdge.Open {
                          def deliver(): Either[StoreError, Int] = Right(0)
                          def close(): Unit = {
                            server.close()
                            scratch.close()
                          }
                        }
                    }
                }
            }
        }
      }
    }
  }
}
