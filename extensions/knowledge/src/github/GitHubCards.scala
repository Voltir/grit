package grit.knowledge.github

import java.time.Instant

import grit.core.act.Answer
import grit.core.document.Pointer
import grit.core.id.{CorpusName, ScheduleKey}
import grit.core.place.{Place, Service}
import grit.core.tool.ToolName
import grit.core.triage.Corpus
import grit.core.visibility.Label
import grit.knowledge.card.Scope
import grit.knowledge.sync.{Fetched, Held, Source, Step, SyncSchedule}

/** A repository whose units become cards. `corpus` names it and `service` is the one its corpus
  * supplies, whose place its MCP server serves; `owner`/`repo` at `ref` are read with
  * `readTool`, `{server}_get_file_contents`, `server` being the name the deployment gave that
  * MCP server. Of the repository, the paths under `include` (each a path prefix, or a file's
  * path, with no leading or trailing `/`; `""` for every path) and, of files, those whose
  * extension is in `extensions` (each with no dot; every one when empty), which [[scope]]
  * digests; at most `maxUnits` cards.
  *
  * `label` is what the repository contains, `None` for its service place's label: its sync
  * schedules run at that clearance, and every card is kept at no less. It may be above the
  * service's (a confidential repository behind an internal service), never below. The head of
  * each file summarised (8 KB) goes to the catalog's summary model, a third party, whatever
  * `label` is: declaring the source consents to that.
  *
  * Windows draw on its cards where a conversation's scope holds `at` (ADR 0028); whether a turn
  * reads one is its label's business alone. A repository outside the server's own scope is
  * refused by its edge: every call fails.
  */
final case class GitHubCards private (
    corpus: CorpusName,
    service: Service,
    readTool: ToolName,
    owner: String,
    repo: String,
    ref: String,
    include: Vector[String],
    extensions: Set[String],
    maxUnits: Int,
    label: Option[Label],
    at: Place,
    syncs: Vector[SyncSchedule]
) extends Source[Listing] {

  /** What of the repository becomes cards, and what a directory's card says of it: [[include]],
    * [[extensions]] and the form directories' cards are written in, as a digest.
    */
  def scope: Scope =
    Scope.of(
      include.map(i => s"include $i").toSet ++ extensions.map(e => s"extension $e") +
        s"directory form ${GitHubCards.DirectoryForm}"
    )

  /** The arguments reading the unit at `path` at `ref`: `owner`, `repo`, `path` and `ref`. */
  def reading(path: String): ujson.Obj =
    ujson.Obj("owner" -> owner, "repo" -> repo, "path" -> path, "ref" -> ref)

  /** `readTool` with [[reading]] of the step's path: GitHub lists a directory as it reads a file. */
  def call(step: Step.Fetching): (ToolName, ujson.Obj) = (readTool, reading(step.path))

  /** [[Listing.of]] `answer`'s text: GitHub's text is its data (its answers carry no facts). */
  def listing(answer: Answer): Either[String, Listing] = Listing.of(answer.text)

  /** [[Contents.of]] `answer`'s text. */
  def fetched(answer: Answer): Fetched = Contents.of(answer.text)

  /** Its entries' paths, one a line: each entry's kind and SHA are read by code. */
  def checked(listing: Listing): String = {
    val entries = listing match {
      case Listing.Complete(entries) => entries
      case Listing.Partial(shown) => shown
    }
    entries.map(_.path).mkString("\n")
  }

  /** What the summary model is told the file at `path` is ([[Step.Summarise]]). */
  def subject(path: String): String = s"A file in the GitHub repository $owner/$repo, at `$path`"

  /** `perRun` (the most files a run reads), plus [[maxUnits]] (the most directories in scope it
    * lists), plus the root and each directory on the way to an included path, at most
    * `Int.MaxValue`.
    */
  def calls(perRun: Int): Int = {
    val passages = include.flatMap(i => i.indices.filter(i(_) == '/').map(i.take)).distinct
    (perRun.toLong + maxUnits + 1 + passages.size).min(Int.MaxValue.toLong).toInt
  }

  /** [[RepositoryWalk.start]]'s: a repository's walk goes by what it holds, whenever it is due. */
  def walk(perRun: Int, floor: Label, held: Vector[Held], due: Instant): RepositoryWalk =
    RepositoryWalk.start(this, perRun, floor, held)

  /** The pointer of the unit at `path`: `readTool` with `owner`, `repo` and `path`, and no
    * `ref`, so a read of it at any ref follows; or why not: those over
    * [[grit.core.document.Pointer.of]]'s 1000 characters of JSON (a path near that long).
    */
  def pointer(path: String): Either[String, Pointer] =
    Pointer.of(
      readTool,
      "owner" -> ujson.Str(owner),
      "path" -> ujson.Str(path),
      "repo" -> ujson.Str(repo)
    )
}

object GitHubCards {

  // Raised whenever which directories get a card, or what one says, changes: a card held under
  // another form is made under another scope, so the next run lists its directory again and
  // writes it anew (2: a directory with nothing in scope beneath it gets none).
  private val DirectoryForm = 2

  /** These, `include`'s paths without a leading or trailing `/` and `extensions` without a
    * leading dot, or why not: `corpus` supplies no service; `server` makes no tool name
    * (`grit.core.tool.ToolName.of` of `{server}_get_file_contents`); `owner`, `repo` or `ref`
    * blank; `maxUnits` under 1; no `syncs`, or two of one key.
    */
  def of(
      corpus: Corpus,
      server: String,
      owner: String,
      repo: String,
      ref: String,
      include: Vector[String],
      extensions: Set[String],
      maxUnits: Int,
      label: Option[Label],
      at: Place,
      syncs: Vector[SyncSchedule]
  ): Either[String, GitHubCards] =
    for {
      service <- corpus.supplies.toRight(
        s"the corpus ${CorpusName.value(corpus.name)} supplies no service"
      )
      tool <- ToolName.of(s"${server}_get_file_contents")
      _ <- Either.cond(!owner.isBlank, (), "a repository's owner is not blank")
      _ <- Either.cond(!repo.isBlank, (), "a repository's name is not blank")
      _ <- Either.cond(!ref.isBlank, (), "a repository's ref is not blank")
      _ <- Either.cond(maxUnits >= 1, (), s"maxUnits is at least 1, not $maxUnits")
      _ <- Either.cond(syncs.nonEmpty, (), "a source has at least one sync schedule")
      _ <- syncs
        .map(_.key)
        .diff(syncs.map(_.key).distinct)
        .headOption
        .fold(Right(()))(key =>
          Left(s"a source's sync schedules have one key each: ${ScheduleKey.value(key)} is two")
        )
    } yield new GitHubCards(
      corpus.name,
      service,
      tool,
      owner,
      repo,
      ref,
      include.map(_.stripPrefix("/").stripSuffix("/")),
      extensions.map(_.stripPrefix(".")),
      maxUnits,
      label,
      at,
      syncs
    )
}
