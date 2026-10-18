package grit.knowledge.ref

import grit.core.document.{DocRef, Pointer}
import grit.core.id.{ConversationId, DocKey}
import grit.core.tool.ToolName

/** What a knowledge document refers to, kept as a [[grit.core.document.DocRef]]
  * ([[Ref.written]], [[Ref.read]]).
  */
enum Ref extends caps.Pure {

  /** A strand, by its root. */
  case Strand(root: ConversationId)

  /** A topic, by its key. */
  case Topic(key: TopicKey)

  /** A Drive file, by its id. */
  case Drive(file: String)

  /** A GitHub file or directory at a repository's path, `/` for its root. */
  case GitHub(owner: String, repo: String, path: String)

  /** Any other link, as [[Ref.ofLink]] normalises it. */
  case Url(url: String)
}

object Ref {

  /** `r` as a reference of kind `strand`, `topic`, `drive`, `github` (`{owner}/{repo}/{path}`)
    * or `url`, or why not: what it holds over [[grit.core.document.DocRef]]'s 512 characters,
    * or holding a NUL.
    */
  def written(r: Ref): Either[String, DocRef] = r match {
    case Strand(root) => DocRef.of("strand", ConversationId.value(root))
    case Topic(key) => DocRef.of("topic", TopicKey.value(key))
    case Drive(file) => DocRef.of("drive", file)
    case GitHub(owner, repo, path) => DocRef.of("github", s"$owner/$repo/$path")
    case Url(url) => DocRef.of("url", url)
  }

  /** What `ref` names, or `None` for a kind this module does not keep, or one it does that
    * `ref` does not hold as [[written]] writes it.
    */
  def read(ref: DocRef): Option[Ref] = ref.kind match {
    case "strand" => Some(Strand(ConversationId(ref.ref)))
    case "topic" => DocKey.of(ref.ref).toOption.flatMap(TopicKey.read).map(Topic(_))
    case "drive" => Some(Drive(ref.ref))
    case "github" =>
      ref.ref.split("/", 3) match {
        case Array(owner, repo, path) if owner.nonEmpty && repo.nonEmpty && path.nonEmpty =>
          Some(GitHub(owner, repo, path))
        case _ => None
      }
    case "url" => Some(Url(ref.ref))
    case _ => None
  }

  /** The unit a link names, or `None` for text that is not `{scheme}://{host}…`. A Docs
    * document's, sheet's or deck's link, or a Drive file's (`/file/d/{id}`, `/open?id={id}`),
    * is its [[Drive]] file; a GitHub link to a repository, or to a path in it (`blob` or
    * `tree`, then a ref of one segment, which is dropped), is its [[GitHub]] path; any other,
    * a Drive folder's included, is its [[Url]], scheme and host lower-cased and fragment
    * dropped.
    */
  def ofLink(link: String): Option[Ref] = link match {
    case Link(scheme, authority, rest) =>
      val at = authority.lastIndexOf('@') + 1
      val host = authority.drop(at).toLowerCase
      val normal = s"${scheme.toLowerCase}://${authority.take(at)}$host$rest"
      Some(host match {
        case "docs.google.com" =>
          rest match {
            case DocsFile(id) => Drive(id)
            case _ => Url(normal)
          }
        case "drive.google.com" =>
          rest match {
            case DriveFile(id) => Drive(id)
            case DriveOpen(id) => Drive(id)
            case _ => Url(normal)
          }
        case "github.com" | "www.github.com" =>
          rest match {
            case GitHubRepository(owner, repo) => GitHub(owner, repo, "/")
            case GitHubPath(owner, repo, path) =>
              val inside = path.stripPrefix("/").stripSuffix("/")
              GitHub(owner, repo, if (inside.isEmpty) "/" else inside)
            case _ => Url(normal)
          }
        case _ => Url(normal)
      })
    case _ => None
  }

  /** The unit a card's pointer reads: [[Drive]] for `drive_read` with its `file`, [[GitHub]]
    * for `{server}_get_file_contents` with its `owner`, `repo` and `path`; else `None`.
    */
  def ofPointer(p: Pointer): Option[Ref] = {
    val tool = ToolName.value(p.tool)
    val args = p.arguments.value
    def text(name: String): Option[String] = args.get(name).flatMap(_.strOpt)
    if (tool == "drive_read") text("file").map(Drive(_))
    else if (tool.endsWith("_get_file_contents"))
      for {
        owner <- text("owner")
        repo <- text("repo")
        path <- text("path")
      } yield GitHub(owner, repo, path)
    else None
  }

  private val Link = "([A-Za-z][A-Za-z0-9+.-]*)://([^/?#]+)([^#]*)(?:#.*)?".r
  private val Id = "([A-Za-z0-9_-]+)"
  private val DocsFile =
    s"/(?:document|spreadsheets|presentation)(?:/u/[0-9]+)?/d/$Id(?:[/?].*)?".r
  private val DriveFile = s"/file(?:/u/[0-9]+)?/d/$Id(?:[/?].*)?".r
  private val DriveOpen = s"/open\\?(?:.*&)?id=$Id(?:&.*)?".r
  private val GitHubRepository = "/([^/?]+)/([^/?]+)/?(?:\\?.*)?".r
  private val GitHubPath = "/([^/?]+)/([^/?]+)/(?:blob|tree)/[^/?]+(/[^?]*|)(?:\\?.*)?".r
}
