package grit.projects.plugin

import java.time.Instant

import grit.core.claim.{ClaimRole, Claimed, Claims}
import grit.core.document.{
  DocRef,
  DocText,
  Document,
  DocumentKeeper,
  DocumentShelf,
  DocumentVersion
}
import grit.core.either.Traversal
import grit.core.id.{ClaimHolder, ConversationId, DocKey, DocumentVersionId, PrincipalId, TurnSeq}
import grit.core.identity.{Account, Principal}
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{CommandBy, CommandOutcome, KeepingCommand, PluginCommand, ReadingCommand}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Args, ArgsError, Field, ToolName, ToolSpec}
import grit.core.visibility.{Integrity, Label}
import grit.projects.artifact.{Artifact, ArtifactHead, ArtifactJson, Body, Project}
import grit.projects.method.{ArtifactKind, Method, MoveRefusal, Remap, RoleView}
import grit.projects.name.{
  ArtifactKey,
  CheckName,
  KindName,
  MethodName,
  Pin,
  ProjectKey,
  RoleName,
  StateName
}
import grit.projects.render.{AnswerText, Brief, BriefRefusal, ProjectMarkdown}

/** The projects plugin's commands over `methods`, new projects under the latest version of
  * `default`.
  */
private[plugin] final class ProjectCommands(methods: Vector[Method], default: MethodName)
    extends caps.Pure {

  import ProjectCommands.*

  /** Every command, in the order a tool list shows them. */
  def all: Vector[PluginCommand[?]] =
    Vector(
      New,
      Keep,
      Move,
      Show,
      History,
      List,
      Find,
      Export,
      ProjectClaim,
      ProjectRelease,
      ProjectClaims,
      ProjectBrief,
      Withdraw,
      Migrate,
      Stale
    )

  /** `project_new`. */
  object New extends KeepingCommand[Starting] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Starting): String = ProjectKey.value(args.project)

    val spec: ToolSpec[Starting] = ToolSpec(
      ToolName("project_new"),
      "Starts a project, holding to the latest version of the method named, or of this " +
        "deployment's own when none is. Refused for a key a project has already.",
      Args
        .of(
          (
            project = Field.text("The new project's key."),
            method = Field.text("The method it holds to.").optional
          )
        )
        .refine(a =>
          for {
            p <- parsed("project", a.project, ProjectKey.of)
            m <- a.method.fold(Right(None))(m => parsed("method", m, MethodName.of).map(Some(_)))
          } yield Starting(p, m)
        )
    )
    def run(
        args: Starting,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      val p = ProjectKey.value(args.project)
      val named = args.method.getOrElse(default)
      for {
        method <- latest(named).toRight(
          Stop.Refused(
            s"no method is named ${MethodName.value(named)}; the methods here: " +
              methods.map(m => MethodName.value(m.name)).distinct.sorted.mkString(", ")
          )
        )
        indexing <- refuse(ArtifactJson.indexing(args.project))
        indexes <- stored(own.referring(indexing, 1))
        _ <- Either.cond(indexes.isEmpty, (), Stop.Refused(s"project $p exists already"))
        pin <- refuse(Pin.of(method.name, method.version, method.digest))
        index <- refuse(ArtifactJson.index(args.project, pin, author(by)))
        (key, text, data, refs) = index
        _ <- written(
          own.write(
            key,
            Label.Public,
            Integrity.Authority,
            Place.Everywhere,
            text,
            data,
            None,
            at,
            refs
          )
        )
      } yield s"Started project $p under method ${MethodName.value(method.name)} version ${method.version}."
    }
  }

  /** `project_keep`. */
  object Keep extends KeepingCommand[Keeping] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Keeping): String = args.key.value

    val spec: ToolSpec[Keeping] = ToolSpec(
      ToolName("project_keep"),
      "Keeps an artifact of a project: a new one, of the kind given, in its kind's first " +
        "state; or a new version of one kept already, its kind and state unchanged (only " +
        "project_move changes a state). Its body is markdown; a link to `artifact:<key>` cites " +
        "that artifact. Keeping one that exists, leave out parent or cites to keep them as " +
        "they are; give cites as [] to clear them, or parent as \"\" to remove it. Refused " +
        "for a parent or a cite the project lacks.",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            kind = Field.text("Its kind, for a new artifact.").optional,
            title = Field.text("Its title."),
            body = Field.text("Its body, as markdown."),
            parent = Field
              .text("The key of its parent artifact, or \"\" for none; left as it is if omitted.")
              .optional,
            cites = Field
              .texts(
                "The keys of the artifacts it cites; left as they are if omitted.",
                0,
                DocRef.Most
              )
              .optional
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            kind <- a.kind.fold(Right(None))(k => parsed("kind", k, KindName.of).map(Some(_)))
            parent <- a.parent.fold(Right(None)) { p =>
              if (p.isEmpty) Right(Some(None))
              else parsed("parent", p, ArtifactKey.read).map(k => Some(Some(k)))
            }
            cites <- a.cites
              .fold(Right(None))(cs => each("cites", cs.toVector, ArtifactKey.read).map(Some(_)))
          } yield Keeping(key, kind, a.title, a.body, parent, cites)
        )
    )
    def run(
        args: Keeping,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own, args.key.project)
        method <- refuse(declared(project.pin))
        existing = project.documents.find(_._2.head.key == args.key)
        placed <- (existing, args.kind) match {
          case (Some((_, a)), Some(k)) if k != a.head.kind =>
            Left(
              Stop.Refused(
                s"${headed(a.head)} keeps its kind; it cannot become ${KindName.value(k)}"
              )
            )
          case (Some((_, a)), _) => Right((a.head.kind, a.head.state))
          case (None, Some(k)) =>
            method.kinds
              .find(_.name == k)
              .map(kind => (kind.name, kind.first))
              .toRight(
                Stop.Refused(
                  s"${KindName.value(k)} is no kind of method ${MethodName.value(method.name)}; its kinds: " +
                    method.kinds.map(k => KindName.value(k.name)).mkString(", ")
                )
              )
          case (None, None) =>
            Left(Stop.Refused(s"${args.key.value} is new, so give its kind"))
        }
        (kind, state) = placed
        body <- refuse(Body.read(args.body))
        artifact = Artifact(
          ArtifactHead(
            args.key,
            kind,
            state,
            args.title,
            args.parent.getOrElse(existing.flatMap(_._2.head.parent)),
            args.cites.getOrElse(existing.fold(Vector.empty)(_._2.head.cites)),
            Some(author(by))
          ),
          body
        )
        _ <- refuse(
          Project.of(
            args.key.project,
            project.pin,
            project.artifacts.filterNot(_.head.key == args.key) :+ artifact
          )
        )
        _ <- atTheirLabels(existing.toVector.map((d, a) => d -> a.head.key.value), "keep")
        _ <- kept(own, artifact, existing.map(_._1.label), at)
      } yield s"Kept ${headed(artifact.head)}."
    }
  }

  /** `project_move`. */
  object Move extends KeepingCommand[Moving] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Moving): String = s"${args.key.value} to ${StateName.value(args.to)}"

    val spec: ToolSpec[Moving] = ToolSpec(
      ToolName("project_move"),
      "Moves an artifact to another state, as its project's method allows, when every check " +
        "that move names holds. Refused naming each check that failed, or the states it may " +
        "move to.",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            state = Field.text("The state to move it to.")
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            to <- parsed("state", a.state, StateName.of)
          } yield Moving(key, to)
        )
    )
    def run(
        args: Moving,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own, args.key.project)
        method <- refuse(declared(project.pin))
        _ <- Either.cond(
          method.digest == project.pin.digest,
          (),
          Stop.Refused(Method.changed(args.key.project, method))
        )
        found <- artifact(project, args.key)
        (document, _) = found
        done <- method
          .move(project.artifacts, args.key, args.to)
          .left
          .map(r => Stop.Refused(refusal(r)))
        into <- done.changed.fold(Right(None))(c => artifact(project, c.head.key).map(Some(_)))
        _ <- atTheirLabels(
          (found +: into.toVector).map((d, a) => d -> a.head.key.value),
          "move"
        )
        writer = Some(author(by))
        moved = Artifact(done.artifact.head.copy(by = writer), done.artifact.body)
        _ <- kept(own, moved, Some(document.label), at)
        _ <- done.changed.zip(into).fold(Right(())) { (c, held) =>
          kept(own, Artifact(c.head.copy(by = writer), c.body), Some(held._1.label), at)
        }
      } yield s"Moved ${headed(moved.head)}." +
        done.changed.fold("")(c => s" Merged its deltas into ${headed(c.head)}.")
    }
  }

  /** `project_show`. */
  object Show extends ReadingCommand[Showing] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Showing): String = args.keys match {
      case ShowKeys.One(key) => key.value
      case ShowKeys.Several(keys) => keys.map(_.value).mkString(", ")
    }

    val spec: ToolSpec[Showing] = ToolSpec(
      ToolName("project_show"),
      "Shows artifacts by key: each one's key, kind, state, title, parent, cites and author, " +
        "then its text as markdown. With head, only those lines, its opening paragraph and " +
        "the names of its sections: enough to tell what it is without reading it all.",
      Args
        .of(
          (
            key = Field.text("One artifact's key, `<project>/<artifact>`.").optional,
            keys = Field
              .texts(
                s"Several artifacts' keys, up to $ShowMost, shown in this order. Give key or keys.",
                1,
                ShowMost
              )
              .optional,
            head = Field
              .flag("Show each one's head and opening paragraph only, not its whole text.")
              .optional,
            at = Field
              .text(
                "An instant, ISO-8601 in UTC (`2026-10-27T12:00:00Z`): shows the version " +
                  "current then instead of the current one."
              )
              .optional
          )
        )
        .refine(a =>
          for {
            keys <- (a.key, a.keys) match {
              case (Some(k), None) => parsed("key", k, ArtifactKey.read).map(ShowKeys.One(_))
              case (None, Some(ks)) =>
                each("keys", ks.toVector, ArtifactKey.read).map(ShowKeys.Several(_))
              case (Some(_), Some(_)) =>
                Left(ArgsError.Refused("keys", "Give key or keys, not both."))
              case (None, None) => Left(ArgsError.Missing("key", "an artifact's key, or give keys"))
            }
            when <- a.at.fold(Right(None))(t => parsed("at", t, instant).map(Some(_)))
          } yield Showing(keys, when, a.head.getOrElse(false))
        )
    )
    def run(args: Showing, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      def version(project: Loaded, key: ArtifactKey): Either[Stop, Artifact] =
        args.at match {
          case None => artifact(project, key).map(_._2)
          case Some(when) =>
            for {
              where <- recorded(own.documents, project, key)
              (doc, label) = where
              found <- stored(own.documents.asOf(doc, label, when))
              document <- found.toRight(Stop.Refused(s"${key.value} had no version at $when"))
              version <- refuse(readable(document))
            } yield version
        }
      def rendered(a: Artifact): String =
        if (args.head) ProjectMarkdown.head(a) else ProjectMarkdown.file(a)._2
      args.keys match {
        case ShowKeys.One(key) =>
          loaded(own.documents, key.project).flatMap(version(_, key)).map(rendered)
        case ShowKeys.Several(keys) =>
          // Each project read once, however many of its keys are asked for.
          val projects = keys.map(_.project).distinct.map(p => p -> loaded(own.documents, p)).toMap
          keys
            .foldLeft[Either[Stop, Vector[String]]](Right(Vector.empty)) { (acc, key) =>
              acc.flatMap { done =>
                projects
                  .getOrElse(
                    key.project,
                    Left(Stop.Refused(s"no project ${ProjectKey.value(key.project)}"))
                  )
                  .flatMap(version(_, key))
                  .map(rendered) match {
                  case Right(text) => Right(done :+ text)
                  case Left(Stop.Refused(why)) => Right(done :+ s"$why\n")
                  case Left(failed) => Left(failed)
                }
              }
            }
            .map(_.mkString)
      }
    }
  }

  /** `project_history`. */
  object History extends ReadingCommand[Recalling] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Recalling): String = args.key.value

    val spec: ToolSpec[Recalling] = ToolSpec(
      ToolName("project_history"),
      s"Lists an artifact's kept versions, newest first, at most $HistoryMost, one line each " +
        "(its version number, when it was written, by whom, its state and title); given " +
        "`from_version` and `to_version`, two of those version numbers, answers instead a line " +
        "diff of their export files, `- ` for a line only `from_version` holds and `+ ` for one " +
        "only `to_version` holds.",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            from_version = Field.integer("The version to diff from.", 1, Long.MaxValue).optional,
            to_version = Field.integer("The version to diff to.", 1, Long.MaxValue).optional
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            pair <- (a.from_version, a.to_version) match {
              case (Some(f), Some(t)) => Right(Some((f, t)))
              case (None, None) => Right(None)
              case (Some(_), None) =>
                Left(
                  ArgsError.Refused(
                    "to_version",
                    "`to_version` is missing: give both versions to compare them, or neither for the whole history"
                  )
                )
              case (None, Some(_)) =>
                Left(
                  ArgsError.Refused(
                    "from_version",
                    "`from_version` is missing: give both versions to compare them, or neither for the whole history"
                  )
                )
            }
          } yield Recalling(key, pair)
        )
    )
    def run(args: Recalling, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own.documents, args.key.project)
        where <- recorded(own.documents, project, args.key)
        kept <- stored(own.documents.versions(where._1, where._2, HistoryMost))
        text <- args.diff match {
          case None =>
            refuse(every(kept.map(listed))).map(AnswerText.versions(_, "No version is kept."))
          case Some((from, to)) =>
            def file(n: Long): Either[Stop, String] =
              kept
                .collectFirst {
                  case DocumentVersion.Holding(d) if DocumentVersionId.value(d.version) == n => d
                }
                .toRight(
                  Stop.Refused(
                    s"${args.key.value} has no version $n among its $HistoryMost newest kept"
                  )
                )
                .flatMap(d => refuse(readable(d)))
                .map(a => ProjectMarkdown.file(a)._2)
            for {
              older <- file(from)
              newer <- file(to)
            } yield AnswerText.diff(
              s"${args.key.value} version $from",
              older,
              s"${args.key.value} version $to",
              newer
            )
        }
      } yield text
    }
  }

  /** `project_list`. */
  object List extends ReadingCommand[Listing] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Listing): String = ProjectKey.value(args.project)

    val spec: ToolSpec[Listing] = ToolSpec(
      ToolName("project_list"),
      "Lists a project's artifacts, one line each: key, kind, state, title, the artifact it " +
        "sits under, and the opening words of its text. Only those of a kind, or in a state, " +
        "when given.",
      Args
        .of(
          (
            project = Field.text("The project's key."),
            kind = Field.text("Only artifacts of this kind.").optional,
            state = Field.text("Only artifacts in this state.").optional
          )
        )
        .refine(a =>
          for {
            p <- parsed("project", a.project, ProjectKey.of)
            kind <- a.kind.fold(Right(None))(k => parsed("kind", k, KindName.of).map(Some(_)))
            state <- a.state.fold(Right(None))(s => parsed("state", s, StateName.of).map(Some(_)))
          } yield Listing(p, kind, state)
        )
    )
    def run(args: Listing, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      loaded(own.documents, args.project).map { project =>
        val listed = project.artifacts
          .filter(a => args.kind.forall(_ == a.head.kind) && args.state.forall(_ == a.head.state))
          .sortBy(_.head.key.value)
        AnswerText.listed(listed, "No artifact matches.")
      }
    }
  }

  /** `project_find`. */
  object Find extends ReadingCommand[Finding] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Finding): String = args.query

    val spec: ToolSpec[Finding] = ToolSpec(
      ToolName("project_find"),
      s"Finds the artifacts whose titles and bodies best match some words, best first, at " +
        s"most $Found, one line each: only one project's, when given. Leaves out ended " +
        "artifacts (in an end state of their kind, such as done, dropped or closed) unless " +
        "`ended` is true. Pass `ended: true` when asking what was tried before, what was " +
        "decided against, or why something was decided: dropped notes and ended milestones " +
        "hold the alternatives that were rejected.",
      Args
        .of(
          (
            query = Field.text("The words to find."),
            project = Field.text("Only artifacts of this project.").optional,
            ended = Field
              .flag(
                "Whether to find ended artifacts too: true when asking what was tried, " +
                  "rejected or decided, and why; false when not given."
              )
              .optional
          )
        )
        .refine(a =>
          a.project
            .fold(Right(None))(p => parsed("project", p, ProjectKey.of).map(Some(_)))
            .map(Finding(a.query, _, a.ended.getOrElse(false)))
        )
    )
    def run(args: Finding, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        hits <- stored(own.documents.search(args.query, FoundAmong, at))
        // A project's index is no artifact: its key has no `/`.
        documents = hits.map(_.document).filter(d => ArtifactKey.read(DocKey.value(d.key)).isRight)
        artifacts <- refuse(every(documents.map(readable)))
        heads = artifacts
          .map(_.head)
          .filter(h => args.project.forall(_ == h.key.project))
        // An artifact of a project whose method is not declared here is never left out.
        ends <-
          if (args.ended) Right(Map.empty[ProjectKey, ArtifactHead -> Boolean])
          else
            heads
              .map(_.key.project)
              .distinct
              .foldLeft[Either[Stop, Map[ProjectKey, ArtifactHead -> Boolean]]](Right(Map.empty)) {
                (so, p) =>
                  so.flatMap(m =>
                    pinned(own.documents, p)
                      .map(pin => ending(pin).toOption.fold(m)(m.updated(p, _)))
                  )
              }
      } yield AnswerText.heads(
        heads.filterNot(h => ends.get(h.key.project).exists(_(h))).take(Found),
        "Nothing matches."
      )
    }
  }

  /** `project_export`. */
  object Export extends ReadingCommand[Exporting] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Exporting): String = ProjectKey.value(args.project)

    val spec: ToolSpec[Exporting] = ToolSpec(
      ToolName("project_export"),
      s"Exports a project as files, at most $ExportPage a page: a JSON object whose `files` " +
        "each hold a `path` and its `text`, and whose `next`, unless null, is the `after` " +
        "asking for the page after.",
      Args
        .of(
          (
            project = Field.text("The project's key."),
            after = Field.text("The `next` of the page before.").optional
          )
        )
        .refine(a => parsed("project", a.project, ProjectKey.of).map(Exporting(_, a.after)))
    )
    def run(args: Exporting, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own.documents, args.project)
        whole <- refuse(Project.of(args.project, project.pin, project.artifacts))
        rest = ProjectMarkdown
          .render(whole)
          .sortBy(_._1)
          .filter((path, _) => args.after.forall(path > _))
        page = rest.take(ExportPage)
      } yield AnswerText.page(page, page.lastOption.map(_._1).filter(_ => rest.size > page.size))
    }
  }

  /** `project_claim`. */
  object ProjectClaim extends KeepingCommand[Claiming] {
    val spec: ToolSpec[Claiming] = ToolSpec(
      ToolName("project_claim"),
      "Claims an artifact for a session or agent to work in a role, until project_release " +
        "frees it; the artifact itself is not changed. Refused, naming who holds it, when it " +
        "is claimed already; refused, naming the artifact, when what it is claimed for works " +
        "another; and refused for a role the project's method lacks. Claims made at the same " +
        "moment are not refused: of two for one artifact, the later fails with a store error " +
        "(asked again, it is refused naming the first), and two for one session or agent on " +
        "different artifacts may both be kept. A claim kept where the asker cannot read it " +
        "(at another label) is not seen, so it refuses nothing.",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            role = Field.text("The role working on it, one its project's method declares."),
            `for` = Field.text(
              "What it is claimed for: the session, or the agent type in it, whose brief is " +
                s"written; parts joined by `/`, at most ${ClaimHolder.Longest} characters."
            )
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            role <- parsed("role", a.role, RoleName.of)
            holder <- parsed("for", a.`for`, ClaimHolder.of)
          } yield Claiming(key, role, holder)
        )
    )
    def run(
        args: Claiming,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      val slot = ClaimHolder.value(args.holder)
      val key = args.key.value
      for {
        project <- loaded(own, args.key.project)
        method <- refuse(declared(project.pin))
        view <- viewOf(method, args.role)
        found <- artifact(project, args.key)
        (document, a) = found
        whole <- refuse(Project.of(args.key.project, project.pin, project.artifacts))
        claimed <- stored(
          claims.claim(document.key, args.holder, args.role, by.person, document.label, at)
        )
        _ <- claimed match {
          case Claimed.Made(_) => Right(())
          case Claimed.Taken(first) =>
            Left(
              Stop.Refused(
                first.fold(s"$key is claimed already; project_release frees it")(c =>
                  s"$key is claimed already, as ${ClaimRole.value(c.role)} for " +
                    s"${ClaimHolder.value(c.holder)} by ${accounts(c.by)} since ${c.made}; " +
                    s"it lapses at ${c.lapses} unless they keep working; project_release frees it"
                )
              )
            )
          case Claimed.Holding(other) =>
            Left(
              Stop.Refused(
                other.fold(
                  s"$slot works another artifact already; release it with project_release first"
                )(c =>
                  s"$slot works ${DocKey.value(c.subject)} already, claimed since ${c.made}; " +
                    s"it lapses at ${c.lapses} unless they keep working; " +
                    "release it with project_release first"
                )
              )
            )
        }
        _ <- briefed(own, Brief.of(whole, args.key, view, Projects.BriefChars), args, document, at)
      } yield s"Claimed ${headed(a.head)} as ${RoleName.value(args.role)} for $slot."
    }
  }

  /** `project_release`. */
  object ProjectRelease extends KeepingCommand[Releasing] {
    val spec: ToolSpec[Releasing] = ToolSpec(
      ToolName("project_release"),
      "Frees an artifact's claim, so it may be claimed again, answering a JSON object: " +
        "`released`, the artifact's key, and `for`, the slot it was claimed for. Refused when " +
        "it is not claimed.",
      Args
        .of((key = Field.text("The artifact's key, `<project>/<artifact>`.")))
        .refine(a => parsed("key", a.key, ArtifactKey.read).map(Releasing(_)))
    )
    def run(
        args: Releasing,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own, args.key.project)
        found <- artifact(project, args.key)
        (document, _) = found
        freed <- stored(claims.release(document.key, document.label))
        // The brief goes even when its claim is gone, as a swept lapsed claim is.
        briefFor <- unbriefed(own, args.key, at)
        holder <- freed
          .map(_.holder)
          .orElse(briefFor)
          .toRight(Stop.Refused(s"${args.key.value} is not claimed"))
      } yield AnswerText.released(args.key, holder)
    }
  }

  /** `project_stale`. */
  object Stale extends ReadingCommand[Staling] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Staling): String = ProjectKey.value(args.project)

    val spec: ToolSpec[Staling] = ToolSpec(
      ToolName("project_stale"),
      "Lists a project's artifacts not ended (not in an end state of their kind) that were " +
        "last kept or moved more than `weeks` weeks ago, and that no artifact not ended has " +
        "as parent or cites, oldest first, one line each (key, kind, state and title): " +
        "candidates to end. One an ended artifact still cites is listed until that " +
        "citation is dropped.",
      Args
        .of(
          (
            project = Field.text("The project's key."),
            weeks = Field.integer("How many weeks untouched, at least 1.", 1, 10_000)
          )
        )
        .refine(a =>
          for {
            p <- parsed("project", a.project, ProjectKey.of)
            staling <- Staling.of(p, a.weeks.toInt).left.map(ArgsError.Refused("weeks", _))
          } yield staling
        )
    )

    def run(args: Staling, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own.documents, args.project)
        ended <- ending(project.pin)
        open = project.documents.filterNot((_, a) => ended(a.head))
        named = Project.named(open.map(_._2))
        before = at.minus(java.time.Duration.ofDays(7L * args.weeks))
        stale = open
          .filter((d, a) => d.written.isBefore(before) && !named(a.head.key))
          .sortBy((d, a) => (d.written.toEpochMilli, a.head.key.value))
      } yield AnswerText.heads(stale.map(_._2.head), "Nothing is stale.")
    }
  }

  /** `project_withdraw`. */
  object Withdraw extends KeepingCommand[Withdrawing] {
    val spec: ToolSpec[Withdrawing] = ToolSpec(
      ToolName("project_withdraw"),
      "Withdraws an artifact: it leaves the project and its export, and project_show with an " +
        s"`at` before now, and project_history, still read it for ${Projects.Terms.retention.toDays} " +
        "days. Refused, withdrawing nothing, while another artifact has it as parent or cites " +
        "it, naming them. With cascade, also withdraws every artifact under it by parent, and " +
        "removes their citations from ended artifacts that cite them (a new version of each, " +
        "written by you); refused, naming them, while an artifact under it has not ended or " +
        "an artifact not ended cites one of them. Refused while any artifact it would change " +
        "is claimed. Answers every artifact withdrawn and every one rewritten.",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            cascade = Field
              .flag(
                "Whether to withdraw everything under it too, dropping ended artifacts' " +
                  "citations of them; false when not given."
              )
              .optional
          )
        )
        .refine(a =>
          parsed("key", a.key, ArtifactKey.read).map(Withdrawing(_, a.cascade.getOrElse(false)))
        )
    )

    def run(
        args: Withdrawing,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own, args.key.project)
        whole <- refuse(Project.of(args.key.project, project.pin, project.artifacts))
        ended <-
          if (args.cascade) ending(project.pin)
          else Right[Stop, ArtifactHead -> Boolean](_ => false)
        // What is left is a Project, so its export reads back (ADR 0049).
        done <- refuse(Project.withdrawn(whole, args.key, args.cascade, ended))
        held = project.documents.map((d, a) => a.head.key -> (d, a)).toMap
        gone = done.gone.flatMap(held.get)
        uncited = done.uncited.flatMap(a => held.get(a.head.key).map((d, _) => d -> a))
        _ <- unclaimed(claims, gone ++ done.uncited.flatMap(a => held.get(a.head.key)), at)
        _ <- atTheirLabels(
          uncited.map((d, a) => d -> s"${a.head.key.value} cites what would be withdrawn, but"),
          "withdraw"
        )
        writer = Some(author(by))
        _ <- uncited.foldLeft[Either[Stop, Unit]](Right(())) { case (so, (d, a)) =>
          so.flatMap(_ => kept(own, Artifact(a.head.copy(by = writer), a.body), Some(d.label), at))
        }
        _ <- gone.foldLeft[Either[Stop, Unit]](Right(())) { case (so, (d, a)) =>
          so.flatMap(_ =>
            stored(own.withdraw(d.key, d.label, at)).flatMap(
              _.toRight(Stop.Refused(s"${a.head.key.value} was withdrawn meanwhile")).map(_ => ())
            )
          )
        }
      } yield AnswerText.withdrawn(
        done.gone,
        done.uncited.map(_.head.key),
        Projects.Terms.retention
      )
    }
  }

  /** `project_migrate`. */
  object Migrate extends KeepingCommand[Migrating] {

    // Each item a value of its own: built inline inside the outer `Args.of`, its tuple gets a
    // capture-set variable and `Args.Values` does not reduce.
    private val renamedKind =
      Args.of((from = Field.text("The kind's name now."), to = Field.text("Its new name.")))
    private val renamedState = Args.of(
      (
        kind = Field.text("The kind's name now."),
        from = Field.text("The state's name now."),
        to = Field.text("Its new name.")
      )
    )
    private val rekinded =
      Args.of((key = Field.text("The artifact's key."), kind = Field.text("Its new kind.")))

    val spec: ToolSpec[Migrating] = ToolSpec(
      ToolName("project_migrate"),
      "Moves a project to another version of its method: each artifact is kept again (a new " +
        "version, written by you; its history still reads), its kind and state renamed as " +
        "given, and the project then holds to that version. A kind or state not renamed " +
        "keeps its name. Refused, changing nothing, naming each cause: the version is not " +
        "declared; an artifact would be of a kind the version lacks, or in a state its kind " +
        "lacks; a renamed artifact is not in the project; it is the project's own version, " +
        "whose rules have changed (declare a new version instead); an artifact is claimed; " +
        "or an artifact or the project is kept at a label below what this room writes.",
      Args
        .of(
          (
            project = Field.text("The project's key."),
            version =
              Field.integer("The version of the project's method to move it to.", 1, Int.MaxValue),
            kinds = Field
              .each(
                "Kinds renamed: each from its name in the project to its name in the version.",
                renamedKind,
                0
              )
              .optional,
            states = Field
              .each(
                "States renamed: a state of the kind (by its name now) to its new name.",
                renamedState,
                0
              )
              .optional,
            artifacts = Field
              .each(
                "Artifacts given a kind of their own, over the kinds renamed: each by key.",
                rekinded,
                0
              )
              .optional
          )
        )
        .refine(a =>
          for {
            p <- parsed("project", a.project, ProjectKey.of)
            kindPairs <- Traversal.traverse(a.kinds.getOrElse(Nil).toVector)(k =>
              for {
                from <- parsed("kinds", k.from, KindName.of)
                to <- parsed("kinds", k.to, KindName.of)
              } yield from -> to
            )
            statePairs <- Traversal.traverse(a.states.getOrElse(Nil).toVector)(s =>
              for {
                kind <- parsed("states", s.kind, KindName.of)
                from <- parsed("states", s.from, StateName.of)
                to <- parsed("states", s.to, StateName.of)
              } yield (kind, from) -> to
            )
            artifactPairs <- Traversal.traverse(a.artifacts.getOrElse(Nil).toVector)(x =>
              for {
                key <- parsed("artifacts", x.key, ArtifactKey.read)
                kind <- parsed("artifacts", x.kind, KindName.of)
              } yield key -> kind
            )
            kinds <- once("kinds", kindPairs, KindName.value)
            states <- once(
              "states",
              statePairs,
              (ks: (KindName, StateName)) => s"${KindName.value(ks._1)} ${StateName.value(ks._2)}"
            )
            artifacts <- once("artifacts", artifactPairs, (k: ArtifactKey) => k.value)
          } yield Migrating(p, a.version.toInt, Remap(kinds, states, artifacts))
        )
    )

    def run(
        args: Migrating,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      val p = ProjectKey.value(args.project)
      for {
        project <- loaded(own, args.project)
        whole <- refuse(Project.of(args.project, project.pin, project.artifacts))
        target <- methods
          .find(m => m.name == project.pin.method && m.version == args.version)
          .toRight(
            Stop.Refused(
              s"the method ${MethodName.value(project.pin.method)} version ${args.version} is " +
                "not declared here; its versions here: " +
                methods.filter(_.name == project.pin.method).map(_.version).sorted.mkString(", ")
            )
          )
        done <- refuse(target.migrated(whole, args.remap))
        index <- indexed(own, args.project)
        _ <- unclaimed(claims, project.documents, at)
        _ <- atTheirLabels(
          (index._1 -> s"project $p") +: project.documents.map((d, a) => d -> a.head.key.value),
          "migrate"
        )
        writer = Some(author(by))
        labels = project.documents.map((d, a) => a.head.key -> d.label).toMap
        _ <- done.project.artifacts.foldLeft[Either[Stop, Unit]](Right(())) { (so, a) =>
          so.flatMap(_ =>
            kept(own, Artifact(a.head.copy(by = writer), a.body), labels.get(a.head.key), at)
          )
        }
        rewritten <- refuse(ArtifactJson.index(args.project, done.project.pin, author(by)))
        (key, text, data, refs) = rewritten
        _ <- written(
          own.write(
            key,
            index._1.label,
            Integrity.Authority,
            Place.Everywhere,
            text,
            data,
            None,
            at,
            refs
          )
        )
      } yield {
        val n = done.project.artifacts.size
        s"Moved project $p to method ${MethodName.value(target.name)} version " +
          s"${target.version}, $n artifact${if (n == 1) "" else "s"} kept again; renamed: " +
          (if (done.renamed.isEmpty) "none." else s"${done.renamed.map(_.value).mkString(", ")}.")
      }
    }
  }

  /** `project_claims`. A keeping command only for its plugin's claims, which a reading command is
    * not handed; it keeps nothing.
    */
  object ProjectClaims extends KeepingCommand[Surveying] {
    val spec: ToolSpec[Surveying] = ToolSpec(
      ToolName("project_claims"),
      "Lists the artifacts claimed now, only one project's when given, answering a JSON " +
        "object: `claims`, each its artifact's `key`, its `role`, `for`, the slot it is " +
        "claimed for, `by`, who claimed it, `since`, when, and `lapses`, when it stops " +
        "holding unless whoever works it keeps working. Ordered by key.",
      Args
        .of((project = Field.text("Only claims on this project's artifacts.").optional))
        .refine(a =>
          a.project
            .fold(Right(None))(p => parsed("project", p, ProjectKey.of).map(Some(_)))
            .map(Surveying(_))
        )
    )

    def run(
        args: Surveying,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      stored(claims.live(at)).map { live =>
        val held = live.flatMap(c =>
          ArtifactKey
            .read(DocKey.value(c.subject))
            .toOption
            .filter(k => args.project.forall(_ == k.project))
            .map(k => (k, c, accounts(c.by)))
        )
        AnswerText.claims(held)
      }
    }
  }

  /** `project_brief`. */
  object ProjectBrief extends ReadingCommand[Briefing] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Briefing): String =
      s"${args.key.value} as ${RoleName.value(args.role)}"

    val spec: ToolSpec[Briefing] = ToolSpec(
      ToolName("project_brief"),
      "Gives the brief for working on an artifact in a role: a header line, its body whole, " +
        "then what the role is shown of what it cites, nearest first, as pointer lines once " +
        "past `chars` characters. Refused for a role the project's method lacks, or an " +
        "artifact that alone does not fit.",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            role = Field.text("The role working on it."),
            chars = Field.count("The most characters the brief holds.", 1, BriefMost)
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            role <- parsed("role", a.role, RoleName.of)
          } yield Briefing(key, role, a.chars)
        )
    )
    def run(args: Briefing, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own.documents, args.key.project)
        method <- refuse(declared(project.pin))
        view <- viewOf(method, args.role)
        whole <- refuse(Project.of(args.key.project, project.pin, project.artifacts))
        brief <- Brief.of(whole, args.key, view, args.chars).left.map {
          case BriefRefusal.Unknown(k) =>
            Stop.Refused(s"project ${ProjectKey.value(k.project)} has no artifact ${k.value}")
          case BriefRefusal.Over(size) =>
            Stop.Refused(
              s"${args.key.value} with what its role always carries is $size characters, " +
                s"over ${args.chars}"
            )
        }
      } yield brief.text
    }
  }

  /** The latest version of the method `name`, if one is declared. */
  private def latest(name: MethodName): Option[Method] =
    methods.filter(_.name == name).maxByOption(_.version)

  /** The method version `pin` names, or why not. */
  private def declared(pin: Pin): Either[String, Method] =
    methods
      .find(m => m.name == pin.method && m.version == pin.version)
      .toRight(
        s"the method ${MethodName.value(pin.method)} version ${pin.version} is not declared here"
      )

  /** Whether an artifact of a project holding to `pin` is in an end state of its kind, or why
    * not: `pin`'s method is not declared here.
    */
  private def ending(pin: Pin): Either[Stop, ArtifactHead -> Boolean] =
    refuse(declared(pin)).map[ArtifactHead -> Boolean](m => h => ArtifactKind.ends(m.kinds, h))
}

private[plugin] object ProjectCommands {

  /** `project_new`'s arguments. */
  final case class Starting(project: ProjectKey, method: Option[MethodName]) extends caps.Pure

  /** `project_keep`'s arguments; a `parent` or `cites` of `None` leaves an existing artifact's
    * as it was (a new one's, none).
    */
  final case class Keeping(
      key: ArtifactKey,
      kind: Option[KindName],
      title: String,
      body: String,
      parent: Option[Option[ArtifactKey]],
      cites: Option[Vector[ArtifactKey]]
  ) extends caps.Pure

  /** `project_move`'s arguments. */
  final case class Moving(key: ArtifactKey, to: StateName) extends caps.Pure

  /** `project_show`'s arguments: one key, refused when not kept, or several, each not kept said
    * in its place; whole, or heads when `head`; as of `at`, or current.
    */
  final case class Showing(keys: ShowKeys, at: Option[Instant], head: Boolean) extends caps.Pure

  /** Which artifacts `project_show` shows. */
  enum ShowKeys extends caps.Pure {
    case One(key: ArtifactKey)
    case Several(keys: Vector[ArtifactKey])
  }

  /** 20: the most keys one `project_show` names. */
  val ShowMost: Int = 20

  /** `project_history`'s arguments: the two version numbers to diff, from and to, if any. */
  final case class Recalling(key: ArtifactKey, diff: Option[(Long, Long)]) extends caps.Pure

  /** 50: the most versions `project_history` lists, and reads to find the two it diffs. */
  private val HistoryMost: Int = 50

  /** `version` as [[AnswerText.versions]] lists it, or why it does not read. */
  private def listed(
      version: DocumentVersion
  ): Either[String, (version: Long, written: Instant, head: Option[ArtifactHead])] =
    version match {
      case DocumentVersion.Holding(d) =>
        readable(d).map(a =>
          (version = DocumentVersionId.value(d.version), written = d.written, head = Some(a.head))
        )
      case DocumentVersion.Withdrawal(v, written) =>
        Right((version = DocumentVersionId.value(v), written = written, head = None))
    }

  /** `text` as an ISO-8601 instant, or why not. */
  private def instant(text: String): Either[String, Instant] =
    scala.util
      .Try(Instant.parse(text))
      .toOption
      .toRight(s"`$text` is not an ISO-8601 instant such as 2026-10-27T12:00:00Z")

  /** `project_list`'s arguments. */
  final case class Listing(project: ProjectKey, kind: Option[KindName], state: Option[StateName])
      extends caps.Pure

  /** `project_find`'s arguments. */
  final case class Finding(query: String, project: Option[ProjectKey], ended: Boolean)
      extends caps.Pure

  /** `project_export`'s arguments. */
  final case class Exporting(project: ProjectKey, after: Option[String]) extends caps.Pure

  /** `project_claim`'s arguments. */
  final case class Claiming(key: ArtifactKey, role: RoleName, holder: ClaimHolder) extends caps.Pure

  /** `project_release`'s arguments. */
  final case class Releasing(key: ArtifactKey) extends caps.Pure

  /** `project_withdraw`'s arguments. */
  final case class Withdrawing(key: ArtifactKey, cascade: Boolean) extends caps.Pure

  /** `project_migrate`'s arguments: the version of its method to move `project` to. */
  final case class Migrating(project: ProjectKey, version: Int, remap: Remap) extends caps.Pure

  /** `pairs` as a map, or a refusal of `field` naming a key given twice. */
  private def once[K, V](
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
  final case class Briefing(key: ArtifactKey, role: RoleName, chars: Int) extends caps.Pure

  /** 100,000: the most characters `project_brief` is asked to give. */
  private val BriefMost: Int = 100_000

  /** Why a command stopped: its store failed, or it refused, in words its asker reads. */
  private enum Stop extends caps.Pure {
    case Store(error: StoreError)
    case Refused(why: String)
  }

  /** A project as its documents hold it: its pin, and each artifact with its document. */
  private final case class Loaded(pin: Pin, documents: Vector[(Document, Artifact)]) {
    def artifacts: Vector[Artifact] = documents.map(_._2)
  }

  private def answered(step: Either[Stop, String]): Either[StoreError, CommandOutcome] =
    step match {
      case Right(text) => Right(CommandOutcome.Done(text))
      case Left(Stop.Refused(why)) => Right(CommandOutcome.Refused(why))
      case Left(Stop.Store(e)) => Left(e)
    }

  private def refuse[A](e: Either[String, A]): Either[Stop, A] = e.left.map(Stop.Refused(_))

  private def stored[A](e: Either[StoreError, A]): Either[Stop, A] = e.left.map(Stop.Store(_))

  /** As [[stored]], a write refused past the bound refused in words. */
  private def written[A](e: Either[StoreError, A]): Either[Stop, A] = e.left.map {
    case StoreError.Full(bound) => Stop.Refused(Projects.full(bound))
    case other => Stop.Store(other)
  }

  /** `does`, then what an artifact here is. */
  /** 20: the most files one page of `project_export` holds. */
  private val ExportPage: Int = 20

  /** 20: the most artifacts `project_find` answers. */
  private val Found: Int = 20

  /** 100: how many best matches `project_find` reads, before leaving out another project's and
    * ended ones, to answer [[Found]] of.
    */
  private val FoundAmong: Int = 100

  /** `project` and its artifacts as `own` holds them, or why not. */
  private def loaded(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Loaded] = {
    for {
      pin <- pinned(own, project)
      holding <- refuse(ArtifactJson.holding(project))
      documents <- stored(own.referring(holding, Projects.Terms.bound))
      artifacts <- refuse(every(documents.map(d => readable(d).map(d -> _))))
    } yield Loaded(pin, artifacts)
  }

  /** What `role` is shown under `method`, or a refusal naming the roles it declares. */
  private def viewOf(method: Method, role: RoleName): Either[Stop, RoleView] =
    method.views
      .find(_.role == role)
      .toRight(
        Stop.Refused(
          s"method ${MethodName.value(method.name)} version ${method.version} shows no role " +
            s"${RoleName.value(role)}; its roles: " +
            method.views.map(v => RoleName.value(v.role)).sorted.mkString(", ")
        )
      )

  /** `document` as an artifact, or why not, naming it. */
  private def readable(document: Document): Either[String, Artifact] =
    Artifact.of(document).left.map(why => s"${DocKey.value(document.key)} cannot be read: $why")

  /** The pin `project`'s index holds as `own` holds it, or why not. */
  private def pinned(own: DocumentShelf, project: ProjectKey)(using Tx^): Either[Stop, Pin] =
    indexed(own, project).map(_._2)

  /** `project`'s index document as `own` holds it, and the pin it holds, or why not. */
  private def indexed(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, (Document, Pin)] = {
    val p = ProjectKey.value(project)
    for {
      indexing <- refuse(ArtifactJson.indexing(project))
      indexes <- stored(own.referring(indexing, 1))
      index <- indexes.headOption.toRight(
        Stop.Refused(s"there is no project $p; start it with project_new")
      )
      pin <- refuse(ArtifactJson.pinOf(index).left.map(why => s"project $p cannot be read: $why"))
    } yield (index, pin)
  }

  /** The artifact `key` of `project`, with its document, or why not. */
  private def artifact(project: Loaded, key: ArtifactKey): Either[Stop, (Document, Artifact)] =
    project.documents
      .find(_._2.head.key == key)
      .toRight(
        Stop.Refused(s"project ${ProjectKey.value(key.project)} has no artifact ${key.value}")
      )

  /** Nothing, or a refusal naming each of `changed` that a claim live at `at` holds, at any
    * label, and the slot it is claimed for.
    */
  private def unclaimed(claims: Claims, changed: Vector[(Document, Artifact)], at: Instant)(using
      Tx^
  ): Either[Stop, Unit] =
    stored(claims.live(at)).flatMap { live =>
      changed.flatMap((document, artifact) =>
        live
          .find(_.subject == document.key)
          .map(c => s"${artifact.head.key.value} is claimed for ${ClaimHolder.value(c.holder)}")
      ) match {
        case Vector() => Right(())
        case Vector(one) => Left(Stop.Refused(s"$one: release it first"))
        case many => Left(Stop.Refused(s"${many.mkString("; ")}: release them first"))
      }
    }

  /** The key and label `key`'s versions are kept under: its current document's, or for an
    * artifact of `project` no longer current, the label its newest version `own` reads is at; or
    * why not, as [[artifact]] refuses, when `own` reads no version of it.
    */
  private def recorded(own: DocumentShelf, project: Loaded, key: ArtifactKey)(using
      Tx^
  ): Either[Stop, (DocKey, Label)] =
    artifact(project, key) match {
      case Right((document, _)) => Right((document.key, document.label))
      case Left(missing) =>
        for {
          docKey <- refuse(DocKey.of(key.value))
          labels <- stored(own.labels(docKey))
          label <- labels.headOption.toRight(missing)
        } yield (docKey, label)
    }

  /** Nothing, or a refusal naming each of `writing` kept at a label below what this room writes,
    * as `<subject> is kept <label>, below what this room writes (<clearance>): <verb> from a room
    * at its label`, joined by `; `: written from here, it would gain a second current copy at the
    * room's label.
    */
  private def atTheirLabels(writing: Vector[(Document, String)], verb: String)(using
      tx: Tx^
  ): Either[Stop, Unit] =
    writing.filterNot((d, _) => d.label.dominates(Tx.floor(tx))) match {
      case Vector() => Right(())
      case below =>
        Left(
          Stop.Refused(
            below
              .map((d, subject) =>
                s"$subject is kept ${Label.described(d.label)}, below what this room writes " +
                  s"(${Label.clearance(Tx.floor(tx))}): $verb from a room at its label"
              )
              .mkString("; ")
          )
        )
    }

  /** The key `artifact`'s brief is kept under: its own key and `#brief`. */
  private def briefKey(artifact: ArtifactKey): Either[String, DocKey] =
    DocKey.of(s"${artifact.value}#brief")

  /** Keeps `rendered`, the claimed artifact's brief, through `own` beside `claimed` (at its label
    * and place), as the brief of `args.holder` ([[DocRef.brief]]), its data naming the holder:
    * whether one was kept. None is when it is over [[Projects.BriefChars]] or the plugin is
    * full; then any brief kept before for the artifact is withdrawn.
    */
  private def briefed(
      own: DocumentKeeper,
      rendered: Either[BriefRefusal, Brief],
      args: Claiming,
      claimed: Document,
      at: Instant
  )(using Tx^): Either[Stop, Boolean] = {
    val written = for {
      brief <- rendered.toOption
      key <- briefKey(args.key).toOption
      text <- DocText.of(brief.text).toOption
    } yield own.write(
      key,
      claimed.label,
      Integrity.Authority,
      claimed.place,
      text,
      ujson.Obj(BriefHolder -> ClaimHolder.value(args.holder)),
      None,
      at,
      Set(DocRef.brief(args.holder))
    )
    written match {
      case Some(Right(_)) => Right(true)
      case Some(Left(StoreError.Full(_))) | None => unbriefed(own, args.key, at).map(_ => false)
      case Some(Left(other)) => Left(Stop.Store(other))
    }
  }

  /** Withdraws `artifact`'s brief at every label it is current at that the transaction reads:
    * the holder the newest of them was for, `None` when there was none.
    */
  private def unbriefed(own: DocumentKeeper, artifact: ArtifactKey, at: Instant)(using
      Tx^
  ): Either[Stop, Option[ClaimHolder]] =
    briefKey(artifact).fold(
      _ => Right(None),
      key =>
        for {
          labels <- stored(own.labels(key))
          current <- stored(Traversal.traverse(labels)(own.current(key, _)).map(_.flatten))
          holder = current
            .sortBy(_.written)
            .lastOption
            .flatMap(_.data.objOpt.flatMap(_.get(BriefHolder)).flatMap(_.strOpt))
            .flatMap(ClaimHolder.of(_).toOption)
          withdrawn: Vector[Option[DocumentVersionId]] <- stored(
            Traversal.traverse(current)(d => own.withdraw(key, d.label, at))
          )
        } yield holder.filter(_ => withdrawn.nonEmpty)
    )

  /** `holder`: the key a brief's data names its holder under. */
  private val BriefHolder: String = "holder"

  /** Keeps `artifact` through `own` at `label`, or at public when new. */
  private def kept(own: DocumentKeeper, artifact: Artifact, label: Option[Label], at: Instant)(using
      Tx^
  ): Either[Stop, Unit] =
    for {
      document <- refuse(
        ArtifactJson
          .write(artifact)
          .left
          .map(why => s"${artifact.head.key.value} cannot be kept: $why")
      )
      (text, data, refs) = document
      key <- refuse(DocKey.of(artifact.head.key.value))
      _ <- written(
        own.write(
          key,
          label.getOrElse(Label.Public),
          Integrity.Authority,
          Place.Everywhere,
          text,
          data,
          None,
          at,
          refs
        )
      )
    } yield ()

  /** Who `by` is, as an artifact records its writer: the person's [[accounts]], and the turn
    * that wrote for them when one did.
    */
  private def author(by: CommandBy): String = by match {
    case CommandBy.Direct(person) => accounts(person)
    case CommandBy.Turn(person, turn) =>
      s"${accounts(person)}, through grit (turn ${TurnSeq.value(turn.turnSeq)} of conversation " +
        s"${ConversationId.value(turn.conversationId)})"
  }

  /** Who `person` is, as a claim or a writer names them: each of their accounts, sorted. */
  private def accounts(person: Principal.Person): String = {
    val held = person.accounts.toVector.map(h => Account.written(h.account)).sorted
    if (held.isEmpty) PrincipalId.value(person.id) else held.mkString(", ")
  }

  private def headed(h: ArtifactHead): String =
    s"${h.key.value} (${KindName.value(h.kind)}, ${StateName.value(h.state)})"

  /** Why a move was refused, in words its mover can act on. */
  private def refusal(r: MoveRefusal): String = r match {
    case MoveRefusal.Missing(key) =>
      s"project ${ProjectKey.value(key.project)} has no artifact ${key.value}"
    case MoveRefusal.NoTransition(head, to, may) =>
      s"${headed(head)} cannot move to ${StateName.value(to)}; " +
        (if (may.isEmpty) "nothing moves it on"
         else s"it may move to ${may.map(StateName.value).mkString(", ")}")
    case MoveRefusal.Unheld(head, to, failed) =>
      s"${head.key.value} cannot move to ${StateName.value(to)}: " +
        failed.map((name, why) => s"${CheckName.value(name)}: $why").mkString("; ")
    case MoveRefusal.NoInto(head, to, wanted) =>
      s"${head.key.value} cannot move to ${StateName.value(to)}: that merges its deltas into the " +
        s"one ${KindName.value(wanted)} it cites, and it cites none; cite the one to merge them into"
    case MoveRefusal.IntoSeveral(head, to, wanted, cited) =>
      s"${head.key.value} cannot move to ${StateName.value(to)}: that merges its deltas into the " +
        s"one ${KindName.value(wanted)} it cites, and it cites ${cited.map(_.value).mkString(", ")}; " +
        "cite only the one to merge them into"
    case MoveRefusal.Unmerged(head, to, into, why) =>
      s"${head.key.value} cannot move to ${StateName.value(to)}: its deltas merged into " +
        s"${into.key.value} would be too long ($why)"
  }

  private def parsed[A](
      field: String,
      value: String,
      read: String => Either[String, A]
  ): Either[ArgsError, A] =
    read(value).left.map(ArgsError.Refused(field, _))

  private def each[A](
      field: String,
      values: Vector[String],
      read: String => Either[String, A]
  ): Either[ArgsError, Vector[A]] =
    values.foldLeft[Either[ArgsError, Vector[A]]](Right(Vector.empty)) { (acc, v) =>
      acc.flatMap(as => parsed(field, v, read).map(as :+ _))
    }

  private def every[A](each: Vector[Either[String, A]]): Either[String, Vector[A]] =
    each.foldLeft[Either[String, Vector[A]]](Right(Vector.empty)) { (acc, e) =>
      acc.flatMap(as => e.map(as :+ _))
    }
}
