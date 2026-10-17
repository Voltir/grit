package grit.knowledge.github

import grit.knowledge.card.{UnitKind, UnitVersion}

/** One entry of a directory's listing: its path from the repository's root, its kind, and its
  * version (a file's blob SHA, a directory's tree SHA).
  */
final case class Entry(path: String, kind: UnitKind, version: UnitVersion) extends caps.Pure

/** A directory's listing as `get_file_contents` answers it. Entries that are neither a file
  * nor a directory (a symlink, a submodule) are left out of both cases.
  */
enum Listing extends caps.Pure {

  /** Every entry of the directory. */
  case Complete(entries: Vector[Entry])

  /** Some entries, or none, and not known to be all: the answer was cut to its leading entries
    * (`shown`), or named no directory but the paths in the tree that end with the one asked for
    * (none shown).
    */
  case Partial(shown: Vector[Entry])
}

object Listing {

  /** The listing `answer`, the text of a `get_file_contents` call of a directory, holds: a JSON
    * array of entries is [[Listing.Complete]]; one on its first line with more after it (the
    * answer cut to fit) or an answer naming possible path matches is [[Listing.Partial]].
    * Otherwise why not: no such answer (a file's, or an error's), or an entry without a string
    * `type`, `path` or `sha`, or whose `sha` is no git SHA.
    */
  def of(answer: String): Either[String, Listing] =
    if (matched(answer)) Right(Listing.Partial(Vector.empty))
    else
      json(answer) match {
        case Some(whole) => entries(whole).map(Listing.Complete(_))
        case None =>
          answer.split("\n", 2).toVector match {
            case Vector(first, _) =>
              json(first).toRight(NoListing).flatMap(entries).map(Listing.Partial(_))
            case _ => Left(NoListing)
          }
      }

  private val NoListing = "not a directory's listing"

  private def json(text: String): Option[ujson.Value] = scala.util.Try(ujson.read(text)).toOption

  private val Matches = "Resolved potential matches in the repository tree"

  /** Whether `answer` names possible path matches: its text as it is, or wrapped in a JSON
    * object whose `content` holds it as a text block, as GitHub's server sends it.
    */
  private def matched(answer: String): Boolean =
    answer.startsWith(Matches) ||
      json(answer)
        .flatMap(_.objOpt)
        .flatMap(_.get("content"))
        .flatMap(_.arrOpt)
        .exists(
          _.exists(_.objOpt.flatMap(_.get("text")).flatMap(_.strOpt).exists(_.startsWith(Matches)))
        )

  private def entries(answer: ujson.Value): Either[String, Vector[Entry]] =
    answer.arrOpt match {
      case None => Left(NoListing)
      case Some(items) =>
        items.toVector.foldLeft[Either[String, Vector[Entry]]](Right(Vector.empty)) { (acc, item) =>
          acc.flatMap(kept => entry(item).map(kept ++ _))
        }
    }

  /** `item` as an entry, `None` for one neither a file nor a directory. */
  private def entry(item: ujson.Value): Either[String, Option[Entry]] = {
    def str(key: String): Either[String, String] =
      item.objOpt
        .flatMap(_.get(key))
        .flatMap(_.strOpt)
        .toRight(s"a listing's entry has no string $key: ${ujson.write(item)}")
    for {
      kind <- str("type")
      path <- str("path")
      sha <- str("sha")
      version <- UnitVersion.sha(sha)
    } yield kind match {
      case "file" => Some(Entry(path, UnitKind.File, version))
      case "dir" => Some(Entry(path, UnitKind.Directory, version))
      case _ => None
    }
  }
}
