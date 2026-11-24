package grit.projects.plugin

import java.time.{Instant, LocalDate}

import grit.core.document.DocRef
import grit.core.id.{ClaimHolder, DocumentVersionId, SessionId}
import grit.core.tool.{Args, ArgsError, Field}
import grit.projects.artifact.Body
import grit.projects.name.{
  ArtifactKey,
  ArtifactRef,
  Handle,
  KindName,
  MethodName,
  ProjectKey,
  RoleName,
  StateName
}
import grit.projects.render.AnswerText
import grit.std.either.Traversal

import ProjectSteps.*

private[plugin] object ProjectArgs {

  /** `project_new`'s arguments. */
  final case class Starting(project: ProjectKey, method: Option[MethodName]) extends caps.Pure

  /** `project_keep`'s arguments: with no `key`, a new artifact of `project`, its id minted; else a
    * new version of the artifact `key` names, of `project`. A field left unchanged is none on a
    * new artifact; `version`, given only with a
    * `key`, is the one its writer read, and the keep is refused unless it is the artifact's
    * current one.
    */
  final case class Keeping private (
      project: ProjectKey,
      key: Option[ArtifactRef],
      handle: Change[Option[Handle]],
      kind: Option[KindName],
      title: String,
      body: String,
      parent: Change[Option[ArtifactRef]],
      cites: Change[Vector[ArtifactRef]],
      version: Option[DocumentVersionId]
  ) extends caps.Pure

  object Keeping {

    /** How a command line shows it: its key, or the project a new one is of. */
    def shown(k: Keeping): String =
      k.key.fold(s"a new artifact of ${ProjectKey.value(k.project)}")(_.value)

    /** A new version of artifact `key`, its title, body and version as given and the rest as
      * they are.
      */
    def again(key: ArtifactKey, title: String, body: String, version: DocumentVersionId): Keeping =
      new Keeping(
        key.project,
        Some(ArtifactRef.Id(key)),
        Change.Unchanged,
        None,
        title,
        body,
        Change.Unchanged,
        Change.Unchanged,
        Some(version)
      )

    /** A new artifact of `kind` under `parent`, of `title` and `body`, with no handle or cites. */
    def part(parent: ArtifactKey, kind: KindName, title: String, body: String): Keeping =
      new Keeping(
        parent.project,
        None,
        Change.Unchanged,
        Some(kind),
        title,
        body,
        Change.To(Some(ArtifactRef.Id(parent))),
        Change.Unchanged,
        None
      )

    /** `project_keep`'s arguments as sent, of `project`, or the argument error naming the field
      * that does not read: a key, parent or cite that is no reference ([[ArtifactRef.read]]) or
      * one of another project, a handle [[Handle.of]] refuses, or a version with no key.
      */
    def read(
        project: ProjectKey,
        key: Option[String],
        handle: Option[String],
        kind: Option[String],
        title: String,
        body: String,
        parent: Option[String],
        cites: Option[List[String]],
        version: Option[Long]
    ): Either[ArgsError, Keeping] =
      for {
        key <- key.fold(Right(None))(k => referred("key", k, project).map(Some(_)))
        handle <- handle match {
          case None => Right(Change.Unchanged)
          case Some("") => Right(Change.To(None))
          case Some(h) => parsed("handle", h, Handle.of).map(h => Change.To(Some(h)))
        }
        kind <- kind.fold(Right(None))(k => parsed("kind", k, KindName.of).map(Some(_)))
        parent <- parent.fold(Right(Change.Unchanged)) { p =>
          if (p.isEmpty) Right(Change.To(None))
          else referred("parent", p, project).map(k => Change.To(Some(k)))
        }
        cites <- cites
          .fold(Right(Change.Unchanged))(cs =>
            Traversal.traverse(cs.toVector)(referred("cites", _, project)).map(Change.To(_))
          )
        version <- (version, key) match {
          case (None, _) => Right(None)
          case (Some(_), None) =>
            Left(
              ArgsError.Refused(
                "version",
                "A new artifact has no version; give it only with the key of one kept already."
              )
            )
          case (Some(v), Some(_)) => versionOf(v).map(Some(_))
        }
      } yield new Keeping(project, key, handle, kind, title, body, parent, cites, version)
  }

  /** What a keep does to a field of an artifact's head: leaves it, or makes it `to`. */
  enum Change[+A] extends caps.Pure {
    case Unchanged
    case To(to: A)

    /** The field as this leaves it, `was` before. */
    def over[B >: A](was: B): B = this match {
      case Unchanged => was
      case To(to) => to
    }
  }

  /** `value` as a reference to an artifact of `project`, or the argument error for `field`: no
    * reference ([[ArtifactRef.read]]), or one of another project.
    */
  private[plugin] def referred(
      field: String,
      value: String,
      project: ProjectKey
  ): Either[ArgsError, ArtifactRef] =
    parsed(field, value, ArtifactRef.read).flatMap(r =>
      Either.cond(
        r.project == project,
        r,
        ArgsError.Refused(
          field,
          s"The $field ${r.value} is in project ${ProjectKey.value(r.project)}, " +
            s"not ${ProjectKey.value(project)}."
        )
      )
    )

  /** `version` as a version a keep or an edit was read at, or why not: it is below 1. */
  private[plugin] def versionOf(version: Long): Either[ArgsError, DocumentVersionId] =
    DocumentVersionId.of(version).toRight(ArgsError.Refused("version", "A version is at least 1."))

  /** `project_edit`'s arguments: `edit` made to `key`'s body, read at `version`. */
  final case class Editing(key: ArtifactRef, version: DocumentVersionId, edit: Body.Edit)
      extends caps.Pure

  /** `project_batch`'s arguments. */
  final case class Batching private (project: ProjectKey, steps: Vector[Step], dry: Boolean)
      extends caps.Pure

  object Batching {

    /** These as `project_batch`'s arguments, or why not: no step, more than [[BatchMost]], or a
      * step's key outside `project`.
      */
    def of(project: ProjectKey, steps: Vector[Step], dry: Boolean): Either[String, Batching] = {
      val outside = steps.zipWithIndex
        .map {
          case (Step.Keep(k, _), i) => (k.project, Keeping.shown(k), i)
          case (Step.Edit(e), i) => (e.key.project, e.key.value, i)
          case (Step.Move(m), i) => (m.key.project, m.key.value, i)
          case (Step.Reorder(r), i) => (r.artifact.project, r.artifact.value, i)
          case (Step.Split(s), i) => (s.artifact.project, s.artifact.value, i)
        }
        .filterNot(_._1 == project)
      if (steps.isEmpty) Left("A batch takes at least one step.")
      else if (steps.size > BatchMost)
        Left(s"At most $BatchMost steps; ${steps.size} were given.")
      else
        outside.headOption.fold(Right(new Batching(project, steps, dry))) { (p, k, i) =>
          Left(
            s"Step ${i + 1}'s key $k is in project ${ProjectKey.value(p)}, " +
              s"not ${ProjectKey.value(project)}."
          )
        }
    }
  }

  /** 50: the most steps one `project_batch` takes, so one transaction keeps at most 100
    * artifacts (a move may merge into a second).
    */
  val BatchMost: Int = 50

  // Its type spelled out: inferred, twelve fields fail Batch's Args.of (docs/capture-checking.md).
  /** A step of `project_batch`, as sent. */
  private[plugin] val BatchStep: Args[
    (
        command: String,
        key: Option[String],
        handle: Option[String],
        state: Option[String],
        kind: Option[String],
        title: Option[String],
        body: Option[String],
        parent: Option[String],
        cites: Option[List[String]],
        version: Option[Long],
        old: Option[String],
        `new`: Option[String],
        section: Option[String]
    )
  ] = Args.of(
    (
      command = Field.oneOf(
        "What the step does: keep, as project_keep; edit, as project_edit; or move, as " +
          "project_move.",
        "keep",
        "edit",
        "move"
      ),
      key = Field
        .text(s"For an edit or a move, needed: the artifact's key. For a keep: ${KeepMeaning.key}")
        .optional,
      handle = Field.text(s"For a keep: ${KeepMeaning.handle}").optional,
      state = Field.text("For a move, the state to move it to.").optional,
      kind = Field.text(s"For a keep: ${KeepMeaning.kind}").optional,
      title = Field.text(s"For a keep: ${KeepMeaning.title}").optional,
      body = Field
        .text(s"For a keep: ${KeepMeaning.body} For an edit of a section: ${EditMeaning.body}")
        .optional,
      parent = Field.text(s"For a keep: ${KeepMeaning.parent}").optional,
      cites = Field.texts(s"For a keep: ${KeepMeaning.cites}", 0, DocRef.Most).optional,
      version = Field
        .integer(
          s"For a keep: ${KeepMeaning.version} For an edit, needed: ${EditMeaning.version}",
          1,
          Long.MaxValue
        )
        .optional,
      old = Field.text(s"For an edit: ${EditMeaning.old}").optional,
      `new` = Field.text(s"For an edit: ${EditMeaning.replaced}").optional,
      section = Field.text(s"For an edit: ${EditMeaning.section}").optional
    )
  )

  /** What `project_keep`'s fields mean, said once for it and for a batch's keeps. */
  private[plugin] object KeepMeaning {
    val key: String =
      "The key of the artifact to keep a new version of: `<project>/<n>`, or `<project>/<handle>`. " +
        "Leave it out to keep a new artifact."
    val handle: String =
      "A name for it, unique in its project, beside its number: lower case, led by a letter, " +
        "words joined by `-`. A batch's later steps may name a new artifact by it. Left as it is " +
        "if omitted."
    val kind: String = "Its kind, for a new artifact."
    val title: String = "Its title."
    val body: String = s"Its body, as markdown. $size"

    /** The size a body is refused past, said before it is sent. */
    val size: String =
      f"A body is refused when its stored form passes ${Body.StoredMost}%,d bytes, which " +
        "takes up to about one and a half times its markdown's size, most for tables."
    val parent: String =
      "The key of its parent artifact, or \"\" for none; left as it is if omitted."
    val cites: String = "The keys of the artifacts it cites; left as they are if omitted."
    val version: String =
      "The version it was read at, as project_show or project_history shows it; refused " +
        "when the artifact has been kept since. Left out, it is kept whatever its version."
  }

  /** What `project_edit`'s fields mean, said once for it and for a batch's edits. */
  private[plugin] object EditMeaning {
    val version: String = "The version project_show gave; refused when it has been kept since."
    val old: String = "The exact text to replace, as project_show gives it, held once."
    val replaced: String = "What replaces old."
    val section: String = "The `##` heading whose section to replace, as written after the `## `."
    val body: String =
      "The section's new text as markdown, under its heading, which is kept; it may hold no " +
        "`#` or `##` heading. Blank leaves the heading alone."
  }

  /** The change `project_edit`'s `old` and `new`, or `section` and `body`, ask for, or why they
    * ask for none: neither pair, or parts of both.
    */
  private[plugin] def editOf(
      old: Option[String],
      replaced: Option[String],
      section: Option[String],
      body: Option[String]
  ): Either[ArgsError, Body.Edit] = (old, replaced, section, body) match {
    case (Some(o), Some(n), None, None) => Right(Body.Edit.Text(o, n))
    case (None, None, Some(s), Some(b)) => Right(Body.Edit.Section(s, b))
    case (None, None, None, None) =>
      Left(ArgsError.Missing("old", "old and new, or section and body"))
    case _ =>
      Left(
        ArgsError.Refused(
          "old",
          "Give old and new together, or section and body together, not both."
        )
      )
  }

  /** `project_move`'s arguments. */
  final case class Moving(key: ArtifactRef, to: StateName) extends caps.Pure

  /** `project_reorder`'s arguments: `artifact`, to sit right beside a sibling. */
  final case class Reordering(artifact: ArtifactRef, beside: Beside) extends caps.Pure

  /** `project_split`'s arguments: `artifact`, read at `version`. */
  final case class Splitting(artifact: ArtifactRef, version: DocumentVersionId) extends caps.Pure

  /** Which side of `sibling` an artifact is to sit. */
  enum Beside extends caps.Pure {
    def sibling: ArtifactRef
    case After(sibling: ArtifactRef)
    case Before(sibling: ArtifactRef)
  }

  /** `project_show`'s arguments: one key, refused when not kept, or several, each not kept or
    * refused its part said in its place; as of `at`, or current.
    */
  final case class Showing(keys: ShowKeys, at: Option[Instant], part: ShowPart) extends caps.Pure

  /** What of each artifact `project_show` shows: all of it, its head, or its head and the `##`
    * section titled `title` ([[ProjectMarkdown.section]]).
    */
  enum ShowPart extends caps.Pure {
    case Whole, Head
    case Section(title: String)
  }

  /** Which artifacts `project_show` shows. */
  enum ShowKeys extends caps.Pure {
    case One(key: ArtifactRef)
    case Several(keys: Vector[ArtifactRef])
  }

  /** 20: the most keys one `project_show` names. */
  val ShowMost: Int = 20

  /** `project_history`'s arguments: the two version numbers to diff, from and to, if any. */
  final case class Recalling(key: ArtifactRef, diff: Option[(Long, Long)]) extends caps.Pure

  /** 50: the most versions `project_history` lists, and reads to find the two it diffs. */
  private[plugin] val HistoryMost: Int = 50

  /** `text` as an ISO-8601 instant, or why not. */
  private[plugin] def instant(text: String): Either[String, Instant] =
    scala.util
      .Try(Instant.parse(text))
      .toOption
      .toRight(s"`$text` is not an ISO-8601 instant such as 2026-10-27T12:00:00Z")

  /** `project_list`'s arguments; `under`, the artifact of `project` listed beneath, and how deep;
    * `skip`, the lines passed over before those listed.
    */
  final case class Listing(
      project: ProjectKey,
      kind: Option[KindName],
      state: Option[StateName],
      under: Option[Under],
      ended: Boolean,
      opening: Boolean,
      skip: Int
  ) extends caps.Pure

  /** The `skip` argument of `project_list` and `project_sweep`: 0 to [[Projects.Terms]]' bound,
    * the most current documents, so the most lines either answers.
    */
  private[plugin] val skipField: Field[Option[Int]] =
    Field
      .count(
        s"How many lines to pass over, to read past the first $ListMost: the last line of a " +
          "cut answer gives the number; 0 when not given.",
        0,
        Projects.Terms.bound
      )
      .optional

  /** An artifact `parent`, as named, and the levels below it to list. */
  final case class Under private (parent: ArtifactRef, depth: Int) extends caps.Pure

  object Under {

    /** `parent` listed `depth` levels down, or why not: `depth` outside 1 to [[DepthMost]]. */
    def of(parent: ArtifactRef, depth: Int): Either[String, Under] =
      if (depth < 1 || depth > DepthMost) Left(s"depth is $depth, outside 1 to $DepthMost")
      else Right(new Under(parent, depth))
  }

  /** 40: the most artifacts one `project_list` answer lists, and one `project_sweep` answer. */
  val ListMost: Int = 40

  /** 3: the most levels under a parent one `project_list` lists. */
  val DepthMost: Int = 3

  /** What an ended artifact is, in the words `project_list` and `project_find` describe it. */
  private[plugin] val EndedWords: String =
    "in an end state of their kind, such as done, dropped or closed"

  /** The `ended` argument of `project_list` and `project_find`, whose answer `verb` says. */
  private[plugin] def endedField(verb: String): Field[Option[Boolean]] =
    Field
      .flag(
        s"Whether to $verb ended artifacts too: true when asking what was tried, " +
          "rejected or decided, and why; false when not given."
      )
      .optional

  /** `project_find`'s arguments. */
  final case class Finding(query: String, project: Option[ProjectKey], ended: Boolean)
      extends caps.Pure

  /** `project_export`'s arguments. */
  final case class Exporting(project: ProjectKey, after: Option[String]) extends caps.Pure

  /** `project_rekey`'s arguments: a survey of every project left to re-key, keeping nothing; or
    * one project re-keyed, or with `dry` only counted.
    */
  enum Rekeying extends caps.Pure {
    case Survey
    case One(project: ProjectKey, dry: Boolean)
  }

  /** `project_claim`'s arguments. */
  final case class Claiming(target: Claiming.Target, role: RoleName, holder: ClaimHolder)
      extends caps.Pure

  object Claiming {

    /** Which artifact a claim is of, and the session it is for. */
    enum Target extends caps.Pure {

      /** The artifact `key` names, worked by `session` (none: its holder's next session to
        * start).
        */
      case Key(key: ArtifactRef, session: Option[SessionId])

      /** Of `project`'s artifacts of `kind` in `state` that no live claim holds, nor, for a
        * covering role, another holder's covers, the one that entered `state` first, ties by
        * number; worked by `session` as [[Key]]'s is.
        */
      case Oldest(
          override val project: ProjectKey,
          kind: KindName,
          state: StateName,
          session: Option[SessionId]
      )

      /** `key`'s claim moved to `to` and to the claim's holder, whoever works it, by the person
        * who made it; claimed for them when none holds it.
        */
      case TakeoverTo(key: ArtifactRef, to: SessionId)

      /** The project the claim is made in. */
      def project: ProjectKey = this match {
        case Key(key, _) => key.project
        case Oldest(project, _, _, _) => project
        case TakeoverTo(key, _) => key.project
      }
    }
  }

  /** `project_release`'s arguments. */
  final case class Releasing(key: ArtifactRef) extends caps.Pure

  /** `project_withdraw`'s arguments. */
  final case class Withdrawing(key: ArtifactRef, cascade: Boolean) extends caps.Pure

  /** `project_sweep`'s arguments: withdraw what is of `kinds`, ended, and last kept or moved
    * before `before` began, UTC; when `dry`, answer only, changing nothing; list the lines of
    * `show` alone when given, past the first `skip`.
    */
  final case class Sweeping private (
      project: ProjectKey,
      kinds: Set[KindName],
      before: LocalDate,
      dry: Boolean,
      show: Option[AnswerText.SweptGroup],
      skip: Int
  ) extends caps.Pure

  object Sweeping {

    /** These arguments, or why not, refusing the argument at fault: `kinds` when there is none,
      * `skip` when under 0.
      */
    def of(
        project: ProjectKey,
        kinds: Set[KindName],
        before: LocalDate,
        dry: Boolean,
        show: Option[AnswerText.SweptGroup],
        skip: Int
    ): Either[ArgsError, Sweeping] =
      if (kinds.isEmpty) Left(ArgsError.Refused("kinds", "A sweep takes at least one kind."))
      else if (skip < 0) Left(ArgsError.Refused("skip", s"skip is $skip, under 0"))
      else Right(new Sweeping(project, kinds, before, dry, show, skip))
  }

  /** 20: the most kinds one `project_sweep` names. */
  private[plugin] val SweepKinds: Int = 20

  /** `text` as a day, YYYY-MM-DD, or why not. */
  private[plugin] def day(text: String): Either[String, LocalDate] =
    scala.util
      .Try(LocalDate.parse(text))
      .toOption
      .toRight(s"`$text` is not a day such as 2026-10-27")

  /** `project_migrate`'s arguments: the version of its method to move `project` to, and the
    * kinds, states and artifacts it renames, as `project_migrate` gives them.
    */
  final case class Migrating(
      project: ProjectKey,
      version: Int,
      kinds: Map[KindName, KindName],
      states: Map[(KindName, StateName), StateName],
      artifacts: Map[ArtifactRef, KindName]
  ) extends caps.Pure

  /** `pairs` as a map, or a refusal of `field` naming a key given twice. */
  private[plugin] def once[K, V](
      field: String,
      pairs: Vector[(K, V)],
      named: K => String
  ): Either[ArgsError, Map[K, V]] =
    pairs.groupBy(_._1).collect { case (k, vs) if vs.size > 1 => named(k) }.toVector.sorted match {
      case Vector() => Right(pairs.toMap)
      case twice => Left(ArgsError.Refused(field, s"${twice.mkString(", ")} given twice"))
    }

  /** `project_stale`'s arguments: artifacts untouched for more than `weeks` weeks. */
  final case class Staling private (project: ProjectKey, weeks: Int) extends caps.Pure

  object Staling {

    /** These arguments, or why not: `weeks` under 1. */
    def of(project: ProjectKey, weeks: Int): Either[String, Staling] =
      if (weeks < 1) Left(s"weeks is $weeks, under 1") else Right(new Staling(project, weeks))
  }

  /** `project_claims`'s arguments. */
  final case class Surveying(project: Option[ProjectKey]) extends caps.Pure

  /** `project_brief`'s arguments. */
  final case class RoleBriefing(key: ArtifactRef, role: RoleName, chars: Int) extends caps.Pure

  /** 100,000: the most characters `project_brief` is asked to give. */
  private[plugin] val BriefMost: Int = 100_000

  private[plugin] def parsed[A](
      field: String,
      value: String,
      read: String => Either[String, A]
  ): Either[ArgsError, A] =
    read(value).left.map(ArgsError.Refused(field, _))
}
