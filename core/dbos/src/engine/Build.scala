package grit.dbos.engine

import java.time.Instant

import scala.io.Source
import scala.util.Using
import scala.util.control.NonFatal

/** The grit build this process runs: the git `commit` it was built from and whether the tree
  * was `dirty`, from the resource Mill writes into `grit.dbos`'s jar. `Unknown` when the
  * resource is missing (a test classpath, or a build outside git).
  */
enum Build {
  case Known(commit: String, dirty: Boolean)
  case Unknown
}

object Build {

  /** This process's build, read once. Never throws. */
  lazy val current: Build =
    try
      Option(getClass.getResourceAsStream(Resource)) match {
        case Some(in) => Using.resource(in)(i => of(Source.fromInputStream(i, "UTF-8").mkString))
        case None => Unknown
      }
    catch { case NonFatal(_) => Unknown }

  /** Where Mill writes it (`build.mill`'s `buildInfo`). */
  private val Resource = "/grit/build.properties"

  /** The build `properties` describe, in the resource's form: `commit=<sha>` and
    * `dirty=true|false`, a line each. `Unknown` for `commit=unknown`, or anything else.
    */
  private[engine] def of(properties: String): Build = {
    val fields = properties.linesIterator
      .map(_.trim)
      .flatMap { line =>
        line.split("=", 2) match {
          case Array(k, v) => Some(k.trim -> v.trim)
          case _ => None
        }
      }
      .toMap
    (fields.get("commit"), fields.get("dirty").flatMap(_.toBooleanOption)) match {
      case (Some(commit), Some(dirty)) if commit.matches("[0-9a-f]{40}") => Known(commit, dirty)
      case _ => Unknown
    }
  }

  /** One engine start, as `grit.engine_starts` keeps it. */
  final case class Started(at: Instant, machine: String, pid: Long, epoch: String, build: Build)
}
