package grit.eval.harness.log

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scala.util.control.NonFatal

import grit.core.message.Usage
import grit.eval.harness.capture.Fields

/** An answer as the cache keeps it: what came back, what the call consumed, the model that
  * `reported` answering, and how long the original call took.
  */
final case class Cached[A](answer: A, usage: Usage, reported: String, latency: FiniteDuration)

/** Answers kept as files under a directory, one per key, at `<dir>/<the key's first two hex
  * digits>/<key>.json`; or, made [[Cache.off]], none. Reads and writes the file system.
  */
final class Cache[A] private (dir: Option[Path], codec: Codec[A]) extends caps.SharedCapability {

  /** The answer kept under `key`; `None` when none is. `Left` when one is kept but cannot be
    * read, naming the key and why (never the file's contents).
    */
  def get(key: CacheKey): Either[String, Option[Cached[A]]] =
    path(key).filter(Files.isRegularFile(_)).fold(Right(None)) { file =>
      Try(ujson.read(Files.readString(file, StandardCharsets.UTF_8))).toOption
        .toRight("not JSON")
        .flatMap { v =>
          val f = Fields("cached", v)
          for {
            answer <- f.field("answer").flatMap(codec.read)
            usage <- f.obj("usage").flatMap(LogJson.readUsage)
            reported <- f.str("reported")
            latency <- f.millis("latency_ms")
          } yield Some(Cached(answer, usage, reported, latency))
        }
        .left
        .map(why => s"cache ${key.hex}: $why")
    }

  /** Keeps `c` under `key`, replacing what was kept; written whole, so a reader never sees part
    * of it. `Left` naming the key when it cannot be written.
    */
  def put(key: CacheKey, c: Cached[A]): Either[String, Unit] = path(key).fold(Right(())) { file =>
    val text = ujson
      .Obj(
        "answer" -> codec.write(c.answer),
        "usage" -> LogJson.writeUsage(c.usage),
        "reported" -> c.reported,
        "latency_ms" -> c.latency.toMillis.toDouble
      )
      .render()
    try {
      Files.createDirectories(file.getParent)
      val partial = file.resolveSibling(file.getFileName.toString + ".partial")
      Files.writeString(partial, text, StandardCharsets.UTF_8)
      Files.move(partial, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      Right(())
    } catch { case NonFatal(e) => Left(s"cache ${key.hex} not written: ${e.getClass.getName}") }
  }

  private def path(key: CacheKey): Option[Path] =
    dir.map(_.resolve(key.hex.take(2)).resolve(s"${key.hex}.json"))
}

object Cache {
  def at[A](dir: Path)(using codec: Codec[A]): Cache[A] = new Cache(Some(dir), codec)

  /** A cache that holds nothing and keeps nothing: every call is asked. */
  def off[A](using codec: Codec[A]): Cache[A] = new Cache(None, codec)
}
