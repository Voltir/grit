package grit.core.files

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.Files as JFiles
import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.SetClock
import grit.core.host.TempBlobs
import grit.core.id.{EdgeName, FileHandle, Sha256}
import grit.core.identity.Email
import grit.core.place.Place
import grit.core.retention.Target
import grit.core.store.{Jot, Tombstones, Tx}
import grit.core.visibility.{Integrity, Provenance, Readers}

import utest.*

/** The contract the file area keeps over every [[FileCatalog]]: run over the in-memory catalog
  * in core and the SQL one in grit.dbos.
  */
abstract class FilesContract extends TestSuite {

  /** The catalog under test, emptied by [[emptied]]. */
  protected def catalog: FileCatalog

  /** Short transactions writing [[catalog]]. */
  protected def jot: Jot

  /** `body` in a transaction that reads an item kept at any place by whoever reads that place,
    * and none its originator alone reads.
    */
  protected def reading[A](body: (Tx^) ?=> A): A

  /** Where the sweep marks files for deletion. */
  protected def tombstones: Tombstones

  /** [[catalog]] with no files, and [[tombstones]] with none of a file. */
  protected def emptied(): Unit

  private val Start = Instant.parse("2026-10-22T09:00:00Z")
  private val Small = FileArea.Bounds(10.minutes, maxFile = 10, maxTotal = 12)

  private val drive =
    Place.read("service:drive/folder").fold(e => throw new java.lang.AssertionError(e), identity)
  private val shared = FileItem(Provenance(drive, Integrity.Member), Readers.AtPlace)
  private val anasOwn = FileItem(
    Provenance(drive, Integrity.Member),
    Readers.Originator(
      Email.of("ana@example.com").fold(e => throw new java.lang.AssertionError(e), identity)
    )
  )

  private def origin(ref: String) = FileOrigin(EdgeName("drive"), ref, Some("7"), Start)

  /** Bytes a source gives, counting how often it is asked. */
  private final class Source(text: String, stated: Option[Long] = None) extends Incoming {
    // Moved only by `read`, on the test's one thread.
    @caps.unsafe.untrackedCaptures
    var asked = 0
    def declared: Option[Long] = stated
    def read[A](body: InputStream^ => A): Either[String, A] = {
      asked += 1
      Right(body(new ByteArrayInputStream(text.getBytes("UTF-8"))))
    }
  }

  /** `body` over an area of `bounds` on an empty [[catalog]], dated by a clock at [[Start]]. */
  private def area[A](bounds: FileArea.Bounds = Small)(
      body: (FileArea, SetClock, TempBlobs) => A
  ): A = {
    emptied()
    TempBlobs.around { blobs =>
      val clock = new SetClock(Start)
      body(new FileArea(catalog, blobs, jot, clock, PagedText.formats, bounds), clock, blobs)
    }
  }

  private def key(n: Int): PutKey = PutKey.read(s"tool:test:0:$n")

  /** A put of `s` under `k`, from `from` (by default a ref of `k`'s own, so no two keys' puts
    * are one snapshot), kept at `item`.
    */
  private def put(
      a: FileArea,
      k: PutKey,
      s: Source,
      item: FileItem = shared,
      from: Option[FileOrigin] = None
  ) =
    a.put(k, "notes.txt", Some("text/plain"), from.getOrElse(origin(PutKey.value(k))), item, s)(f =>
      (f.name, f.bytes)
    )

  /** `handle`'s text, as admitted by [[reading]] and opened. */
  private def text(a: FileArea, handle: FileHandle): Either[FileError, String] =
    reading(a.admit(handle)).flatMap(a.open(_)(f => JFiles.readString(f.path)))

  private def sha(s: String): Sha256 = Sha256.digest(s.getBytes("UTF-8").toVector)

  val tests = Tests {
    test("a put keeps the bytes under a new handle, runs its body over them, and opens them") {
      area() { (a, _, _) =>
        put(a, key(1), new Source("hello")).map { (record, read) =>
          (read, record.bytes, record.sha256, record.expires, text(a, record.handle))
        } ==> Right((("notes.txt", 5L), 5L, sha("hello"), Start.plusSeconds(600), Right("hello")))
      }
    }
    test(
      "a put under a key whose file is kept answers that file, fetching nothing, running its body"
    ) {
      area() { (a, clock, _) =>
        val first = put(a, key(1), new Source("hello"))
        clock.at = Start.plusSeconds(60)
        val again = new Source("hello")
        (first.isRight, put(a, key(1), again), again.asked) ==> (true, first, 0)
      }
    }
    test(
      "a put of a snapshot kept, from the same edge's ref at the same revision and kept at the same item, answers that file, fetching nothing; another revision or item, or none, puts afresh"
    ) {
      area(Small.copy(maxTotal = 100)) { (a, clock, _) =>
        val at7 = origin("d1")
        def handle(k: Int, from: FileOrigin, item: FileItem = shared): (Option[FileHandle], Int) = {
          val s = new Source("hello")
          (put(a, key(k), s, item, Some(from)).toOption.map(_._1.handle), s.asked)
        }
        val first = handle(1, at7)
        clock.at = Start.plusSeconds(12)
        (
          handle(2, at7.copy(fetched = Start.plusSeconds(12))),
          handle(3, at7.copy(revision = Some("8")))._2,
          handle(4, at7, anasOwn)._2,
          handle(5, at7.copy(revision = None))._2,
          handle(6, at7.copy(ref = "d2"))._2
        ) ==> ((first._1, 0), 1, 1, 1, 1)
      }
    }
    test("a put under a key whose file expired puts afresh, under a new handle") {
      area() { (a, clock, _) =>
        val first = put(a, key(1), new Source("hello")).map(_._1.handle)
        clock.at = Start.plusSeconds(601)
        val again = new Source("hello")
        val second = put(a, key(1), again).map(_._1.handle)
        (again.asked, second.isRight, second != first, second.flatMap(text(a, _))) ==>
          (1, true, true, Right("hello"))
      }
    }
    test(
      "a file past the most one may have is refused, keeping nothing; unread when its size is stated"
    ) {
      area() { (a, _, blobs) =>
        val stated = new Source("eleven byte", Some(11))
        val unstated = new Source("eleven byte")
        (
          put(a, key(1), stated),
          stated.asked,
          put(a, key(2), unstated),
          blobs.kept(),
          reading(catalog.all())
        ) ==> (
          Left(FileError.TooLarge(Some(11), 10)),
          0,
          Left(FileError.TooLarge(None, 10)),
          Right(Vector.empty),
          Right(Vector.empty)
        )
      }
    }
    test(
      "a put past the most the files may have together is refused, evicting nothing; equal bytes count once"
    ) {
      area() { (a, _, _) =>
        val first = put(a, key(1), new Source("ten bytes!")).map(_._1.handle)
        val full = put(a, key(2), new Source("three"))
        val same = put(a, key(3), new Source("ten bytes!")).map(_._1.sha256)
        val fits = put(a, key(4), new Source("hi")).map(_._1.bytes)
        (full, first.flatMap(text(a, _)), same, fits) ==>
          (Left(FileError.Full), Right("ten bytes!"), Right(sha("ten bytes!")), Right(2L))
      }
    }
    test("a handle this transaction does not read is gone as an expired or unknown one is") {
      area() { (a, clock, _) =>
        val own = put(a, key(1), new Source("mine"), anasOwn).map(_._1.handle)
        val kept = put(a, key(2), new Source("ours")).map(_._1.handle)
        val unknown = FileHandle.of(999_999).getOrElse(throw new java.lang.AssertionError("handle"))
        val gone = (h: FileHandle) => Left(FileError.Gone(h, 10.minutes))
        val unread = own.map(h => reading(a.admit(h)) == gone(h))
        val live = kept.map(h => reading(a.admit(h)).isRight)
        clock.at = Start.plusSeconds(601)
        val expired = kept.map(h => reading(a.admit(h)) == gone(h))
        (unread, live, expired, reading(a.admit(unknown))) ==>
          (Right(true), Right(true), Right(true), gone(unknown))
      }
    }
    test("a file whose bytes are lost is gone when opened") {
      area() { (a, _, blobs) =>
        val handle = put(a, key(1), new Source("lost")).map(_._1.handle)
        val admitted = handle.flatMap(h => reading(a.admit(h)))
        val _ = blobs.remove(sha("lost"))
        (admitted.isRight, admitted.flatMap(a.open(_)(_.bytes))) ==>
          (true, handle.flatMap(h => Left(FileError.Gone(h, 10.minutes))))
      }
    }
    test(
      "a sweep marks each file expired or whose bytes are lost, and removes each blob no file names"
    ) {
      area() { (a, clock, blobs) =>
        val expired = put(a, key(1), new Source("old")).map(_._1.handle)
        val lost = put(a, key(2), new Source("lost")).map(_._1.handle)
        clock.at = Start.plusSeconds(300)
        val live = put(a, key(3), new Source("new")).map(_._1.handle)
        val _ = blobs.remove(sha("lost"))
        val _ = blobs.receive(new ByteArrayInputStream("stray".getBytes("UTF-8")), 10)(_.keep())
        clock.at = Start.plusSeconds(660)
        val first = a.sweep(tombstones)
        val marked = reading(tombstones.due(Target.Kind.File, clock.at.plusSeconds(1), 10))
          .map(_.map(_.target))
        // The collector forgets the expired file; the next sweep removes its blob.
        expired.foreach(h => reading(catalog.forget(h)))
        val second = a.sweep(tombstones)
        (
          live.isRight,
          first,
          marked,
          second.map(s => (s.marked, s.removed)),
          blobs.kept().map(_.toSet)
        ) ==> (
          true,
          Right(
            FileArea.Swept(Vector(expired, lost).flatMap(_.toOption), removed = 1, cleared = 0)
          ),
          Right(Vector(expired, lost).flatMap(_.toOption).map(Target.File(_))),
          Right((Vector.empty, 1)),
          Right(Set(sha("new")))
        )
      }
    }
  }
}
