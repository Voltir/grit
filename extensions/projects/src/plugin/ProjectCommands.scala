package grit.projects.plugin

import java.time.{Instant, LocalDate, ZoneOffset}

import grit.core.claim.{Claim, ClaimRole, Claimed, Claims}
import grit.core.document.{
  DocRef,
  DocText,
  Document,
  DocumentKeeper,
  DocumentShelf,
  DocumentVersion,
  Written
}
import grit.core.id.{ClaimHolder, ConversationId, DocKey, DocumentVersionId, PrincipalId, TurnSeq}
import grit.core.identity.{Account, Principal}
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{CommandBy, CommandOutcome, KeepingCommand, PluginCommand, ReadingCommand}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Args, ArgsError, Field, ToolName, ToolSpec}
import grit.core.visibility.{Integrity, Label}
import grit.projects.artifact.{Artifact, ArtifactHead, ArtifactJson, Body, Project, Rekeyed}
import grit.projects.method.{
  ArtifactKind,
  Method,
  MoveRefusal,
  Mover,
  Remap,
  RoleView,
  TransitionCheck
}
import grit.projects.name.{
  ArtifactKey,
  ArtifactNumber,
  ArtifactRef,
  CheckName,
  Handle,
  KindName,
  MethodName,
  Pin,
  ProjectKey,
  RoleName,
  StateName
}
import grit.projects.render.{AnswerText, Brief, BriefRefusal, ProjectMarkdown}
import grit.std.either.Traversal

/** The projects plugin's commands over the methods declared in its store, their checks found
  * among `checks`, new projects under the latest version of `default`, offered to turns as
  * `turns` says.
  */
private[plugin] final class ProjectCommands(
    checks: Vector[TransitionCheck],
    default: MethodName,
    turns: TurnCommands,
    methodsAt: Label
) extends caps.Pure {

  private val declaring = new MethodCommands(checks, methodsAt)

  import ProjectCommands.*

  /** Whether turns are offered the shipped commands, not `project_propose` alone. */
  private val shipped: Boolean = turns match {
    case TurnCommands.Shipped => true
    case TurnCommands.Proposing(_) => false
  }

  /** Every command, in the order a tool list shows them, `project_propose` last when proposing. */
  def all: Vector[PluginCommand[?]] = {
    val proposing = turns match {
      case TurnCommands.Proposing(kind) => Vector(new Propose(kind))
      case TurnCommands.Shipped => Vector.empty
    }
    Vector(
      New,
      Keep,
      Edit,
      Move,
      Batch,
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
      Sweep,
      Migrate,
      Rekey,
      Stale,
      declaring.Declare,
      declaring.Methods
    ) ++ proposing
  }

  /** `project_new`. */
  object New extends KeepingCommand[Starting] {
    override def offeredToTurns: Boolean = shipped
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
      locked(claims, args.project).flatMap { lock =>
        given Locked = lock
        for {
          method <- declaring.latest(own, named)
          was <- indexOf(own, args.project)
          _ <- Either.cond(was.isEmpty, (), Stop.Refused(s"project $p exists already"))
          pin <- refuse(Pin.of(method.name, method.version, method.digest))
          index <- refuse(
            ArtifactJson.index(args.project, pin, author(by), ArtifactNumber.First)
          )
          (key, text, data, refs) = index
          _ <- keptEverywhere(own, key, Label.Public, Tx.top, text, data, refs, at)
        } yield s"Started project $p under method ${MethodName.value(method.name)} version ${method.version}."
      }
    }
  }

  /** `project_keep`. */
  object Keep extends KeepingCommand[Keeping] {
    override def offeredToTurns: Boolean = shipped
    override def shown(args: Keeping): String = Keeping.shown(args)

    val spec: ToolSpec[Keeping] = ToolSpec(
      ToolName("project_keep"),
      "Keeps an artifact of a project: with no key, a new one of the kind given, in its kind's " +
        "first state, numbered by the project (`<project>/<n>`); with a key, a new version of " +
        "that one, its kind and state unchanged (only project_move changes a state). Its body " +
        "is markdown; a link to `artifact:<key>` cites that artifact. Keeping one that exists, " +
        "leave out handle, parent or cites to keep them as they are; give cites as [] to clear " +
        "them, or handle or parent as \"\" to remove it; give version, as project_show shows " +
        "it, to keep it only if no one has kept it since. Answers with the artifact's key and " +
        "the version it is kept at. Refused, keeping nothing: for a key, parent or cite the " +
        "project lacks, naming the nearest it has; for a version that is not the artifact's " +
        "current one; for a handle another artifact holds, or a change of one a role's brief " +
        f"carries by name; and when the brief of an artifact claimed in the project would pass ${DocRef.BriefChars}%,d " +
        "characters, naming the characters to cut.",
      Args
        .of(
          (
            key = Field.text(KeepMeaning.key).optional,
            project = Field
              .text(
                "The project's key, needed for a new artifact; refused when it is not the key's own."
              )
              .optional,
            handle = Field.text(KeepMeaning.handle).optional,
            kind = Field.text(KeepMeaning.kind).optional,
            title = Field.text(KeepMeaning.title),
            body = Field.text(KeepMeaning.body),
            parent = Field.text(KeepMeaning.parent).optional,
            cites = Field.texts(KeepMeaning.cites, 0, DocRef.Most).optional,
            version = Field.integer(KeepMeaning.version, 1, Long.MaxValue).optional
          )
        )
        .refine(a =>
          for {
            project <- (a.key, a.project) match {
              case (_, Some(p)) => parsed("project", p, ProjectKey.of)
              case (Some(k), None) => parsed("key", k, ArtifactRef.read).map(_.project)
              case (None, None) =>
                Left(ArgsError.Refused("project", "A new artifact needs its project's key."))
            }
            keeping <- Keeping.read(
              project,
              a.key,
              a.handle,
              a.kind,
              a.title,
              a.body,
              a.parent,
              a.cites,
              a.version
            )
          } yield keeping
        )
    )
    def run(
        args: Keeping,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] =
      answered(
        alone(own, claims, args.project, by, at, "keep")(_ => Right(Step.Keep(args)))
          // A keep changes one artifact, or none when it changes nothing.
          .map(done =>
            done.versions.values.headOption.fold(done.said)(AnswerText.nowAt(done.said, _))
          )
      )
  }

  /** `project_edit`: part of an artifact's body changed, kept as `project_keep` keeps it. */
  object Edit extends KeepingCommand[Editing] {
    override def offeredToTurns: Boolean = shipped
    override def shown(args: Editing): String = args.key.value

    val spec: ToolSpec[Editing] = ToolSpec(
      ToolName("project_edit"),
      "Changes part of an artifact's body and keeps it as project_keep does, its title, kind, " +
        "state, parent and cites as they are. Give old and new to replace one exact text, " +
        "copied from project_show, which the body must hold exactly once; or section and body " +
        "to replace what follows one `##` heading, up to the next, keeping the heading. Give " +
        "the version project_show shows. Answers with the version it is kept at. Refused, " +
        "keeping nothing: for a version that is not the artifact's current one, naming the one " +
        "it is at; for old text held twice or not at all; for a section no heading names, or " +
        s"a heading several sections share; and as project_keep refuses. ${KeepMeaning.size}",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            version = Field.integer(EditMeaning.version, 1, Long.MaxValue),
            old = Field.text(EditMeaning.old).optional,
            `new` = Field.text(EditMeaning.replaced).optional,
            section = Field.text(EditMeaning.section).optional,
            body = Field.text(EditMeaning.body).optional
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactRef.read)
            version <- versionOf(a.version)
            edit <- editOf(a.old, a.`new`, a.section, a.body)
          } yield Editing(key, version, edit)
        )
    )
    def run(
        args: Editing,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      val key = args.key
      for {
        done <- alone(own, claims, key.project, by, at, "edit")(_ => Right(Step.Edit(args)))
        // An edit that changes nothing writes nothing, and the version given is current.
        now = done.versions.values.headOption.getOrElse(args.version)
      } yield AnswerText.nowAt(done.said, now)
    }
  }

  /** `project_move`. */
  object Move extends KeepingCommand[Moving] {
    override def offeredToTurns: Boolean = shipped
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
            key <- parsed("key", a.key, ArtifactRef.read)
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
    )(using Tx^): Either[StoreError, CommandOutcome] =
      answered(
        alone(own, claims, args.key.project, by, at, "move")(_ => Right(Step.Move(args)))
          .map(_.said)
      )
  }

  /** `project_propose`: a new artifact of `kind`, kept as `project_keep` keeps one. */
  final class Propose(kind: KindName) extends KeepingCommand[Keeping] {
    override def offeredToTurns: Boolean = true
    override def shown(args: Keeping): String = Keeping.shown(args)

    val spec: ToolSpec[Keeping] = ToolSpec(
      ToolName("project_propose"),
      s"Proposes a new ${KindName.value(kind)}: keeps it in its project in its first state, " +
        "numbered by the project, for a person to judge. It never changes anything kept " +
        "already. Its body is markdown; a link to `artifact:<key>` cites that artifact. " +
        "Answers with its key. Refused, keeping nothing: for a handle another artifact of the " +
        "project has; and for a parent or a cite the project lacks, naming the nearest it has.",
      Args
        .of(
          (
            project = Field.text("The project's key."),
            handle = Field.text(KeepMeaning.handle).optional,
            title = Field.text(KeepMeaning.title),
            body = Field.text(KeepMeaning.body),
            parent = Field.text("The key of its parent artifact; none if omitted.").optional,
            cites = Field
              .texts("The keys of the artifacts it cites; none if omitted.", 0, DocRef.Most)
              .optional
          )
        )
        .refine(a =>
          for {
            project <- parsed("project", a.project, ProjectKey.of)
            keeping <- Keeping.read(
              project,
              None,
              a.handle,
              Some(KindName.value(kind)),
              a.title,
              a.body,
              a.parent,
              a.cites,
              None
            )
          } yield keeping
        )
    )
    def run(
        args: Keeping,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] =
      answered(
        alone(own, claims, args.project, by, at, "propose")(_ => Right(Step.Keep(args)))
          .map(_.said)
      )
  }

  /** `project_batch`. */
  object Batch extends KeepingCommand[Batching] {

    override def shown(args: Batching): String =
      s"${ProjectKey.value(args.project)}, ${args.steps.size} steps" +
        (if (args.dry) ", dry" else "")

    val spec: ToolSpec[Batching] = ToolSpec(
      ToolName("project_batch"),
      "Keeps, edits and moves artifacts of one project, in the order given, each step as " +
        "project_keep, project_edit or project_move would and seeing what the steps before it " +
        "did; all of them are kept, or none when any is refused. A keep's or an edit's version " +
        "is checked against the artifact as it was before the batch, an edit changes the body " +
        "the steps before it left, and each artifact changed is kept once, as the " +
        "last step left it. With dry, keeps nothing and answers what each step would do. " +
        "Refused, naming the step and why, as its command would refuse it; and when a claimed " +
        f"artifact's brief would pass ${DocRef.BriefChars}%,d characters once every step is done.",
      Args
        .of(
          (
            project = Field.text("The project's key; every step's key is in it."),
            steps = Field.each(
              s"The steps, 1 to $BatchMost, in order: each a keep, an edit or a move.",
              BatchStep
            ),
            dry = Field.flag("Answer what each step would do, keeping nothing.").optional
          )
        )
        .refine(a =>
          for {
            project <- parsed("project", a.project, ProjectKey.of)
            steps <- Traversal.traverse(a.steps.toVector.zipWithIndex)((s, i) =>
              for {
                named = (what: String) =>
                  s.key
                    .toRight(
                      ArgsError.Refused("steps", s"$what needs its key; step ${i + 1} gives none.")
                    )
                    .flatMap(referred("key", _, project))
                gives = Vector(
                  "handle" -> s.handle.isDefined,
                  "state" -> s.state.isDefined,
                  "kind" -> s.kind.isDefined,
                  "title" -> s.title.isDefined,
                  "body" -> s.body.isDefined,
                  "parent" -> s.parent.isDefined,
                  "cites" -> s.cites.isDefined,
                  "version" -> s.version.isDefined,
                  "old" -> s.old.isDefined,
                  "new" -> s.`new`.isDefined,
                  "section" -> s.section.isDefined
                )
                beyond = (takes: Set[String]) =>
                  gives.collect { case (name, true) if !takes(name) => name }
                step <- s.command match {
                  case "move" =>
                    val keeps = beyond(Set("state"))
                    if (keeps.nonEmpty)
                      Left(
                        ArgsError.Refused(
                          "steps",
                          s"A move takes key and state alone; step ${i + 1} gives " +
                            s"${keeps.mkString(", ")}."
                        )
                      )
                    else
                      s.state
                        .toRight(
                          ArgsError
                            .Refused("steps", s"A move needs its state; step ${i + 1} gives none.")
                        )
                        .flatMap(st => parsed("state", st, StateName.of))
                        .flatMap(to => named("A move").map(key => Step.Move(Moving(key, to))))
                  case "edit" =>
                    val keeps = beyond(Set("version", "old", "new", "section", "body"))
                    if (keeps.nonEmpty)
                      Left(
                        ArgsError.Refused(
                          "steps",
                          "An edit takes key, version, and old and new or section and body; " +
                            s"step ${i + 1} gives ${keeps.mkString(", ")}."
                        )
                      )
                    else
                      for {
                        read <- s.version.toRight(
                          ArgsError
                            .Refused(
                              "steps",
                              s"An edit needs its version; step ${i + 1} gives none."
                            )
                        )
                        version <- versionOf(read)
                        edit <- editOf(s.old, s.`new`, s.section, s.body).left
                          .map(e => ArgsError.Refused("steps", s"Step ${i + 1}: ${e.message}"))
                        key <- named("An edit")
                      } yield Step.Edit(Editing(key, version, edit))
                  case _ =>
                    val edits =
                      beyond(
                        Set(
                          "handle",
                          "state",
                          "kind",
                          "title",
                          "body",
                          "parent",
                          "cites",
                          "version"
                        )
                      )
                    (s.state, s.title, s.body) match {
                      case _ if edits.nonEmpty =>
                        Left(
                          ArgsError.Refused(
                            "steps",
                            s"A keep takes no old, new or section; step ${i + 1} gives " +
                              s"${edits.mkString(", ")}: make it an edit step."
                          )
                        )
                      case (Some(_), _, _) =>
                        Left(
                          ArgsError.Refused(
                            "steps",
                            s"A keep takes no state; step ${i + 1} gives one: move it with a " +
                              "move step."
                          )
                        )
                      case (None, Some(title), Some(body)) =>
                        Keeping
                          .read(
                            project,
                            s.key,
                            s.handle,
                            s.kind,
                            title,
                            body,
                            s.parent,
                            s.cites,
                            s.version
                          )
                          .map(Step.Keep(_))
                      case (None, _, _) =>
                        Left(
                          ArgsError.Refused(
                            "steps",
                            s"A keep needs its title and body; step ${i + 1} lacks " +
                              Vector("title" -> s.title, "body" -> s.body)
                                .collect { case (name, None) => name }
                                .mkString(" and ") +
                              "."
                          )
                        )
                    }
                }
              } yield step
            )
            batching <- Batching
              .of(project, steps, a.dry.getOrElse(false))
              .left
              .map(ArgsError.Refused("steps", _))
          } yield batching
        )
    )

    def run(
        args: Batching,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      val n = args.steps.size
      locked(claims, args.project).flatMap { lock =>
        given Locked = lock
        for {
          project <- loaded(own, args.project)
          held <- if (args.dry) Right(Vector.empty) else holding(claims, project, at)
          method <- declared(own, project.pin)
          plan = planned(project, method, args.steps, author(by), moverOf(by), onward = args.dry)
          (said, steps) = plan
          answers <- Traversal.traverse(said.zipWithIndex) {
            case (Right(answer), i) => Right(s"${i + 1}. $answer")
            case (Left(Stop.Refused(why)), i) => Right(s"${i + 1}. Refused: $why")
            case (Left(failed), _) => Left(failed)
          }
          lines = answers.mkString("\n")
          wouldRefuse =
            s"Dry run, nothing kept; the batch would be refused. Each step would answer:\n$lines"
          refused = said.zipWithIndex.collectFirst { case (Left(Stop.Refused(why)), i) => (i, why) }
          done <- (refused, args.dry) match {
            case (Some((i, why)), false) =>
              Left(
                Stop.Refused(
                  s"Nothing was kept: step ${i + 1} of $n (${described(args.steps, i)}) was " +
                    s"refused: $why"
                )
              )
            case (Some(_), true) => Left(Stop.Refused(wouldRefuse))
            case (None, false) =>
              settled(claims, project, method, args.project, steps, at, "change it").left
                .map {
                  case Stop.Refused(why) => Stop.Refused(s"Nothing was kept: $why")
                  case failed => failed
                }
                .flatMap(_ => wrote(own, project, steps, author(by), at))
                .flatMap(_ => rebriefed(own, held, args.project, at))
                .map(_ => s"Every step is done:\n$lines")
            case (None, true) =>
              settled(claims, project, method, args.project, steps, at, "change it").left
                .map {
                  case Stop.Refused(why) =>
                    Stop.Refused(s"$wouldRefuse\nOnce every step is done, refused: $why")
                  case failed => failed
                }
                .map(_ => s"Dry run, nothing kept. Each step would answer:\n$lines")
          }
        } yield done
      }
    }

    /** Step `i` of `steps` in a few words: `keep <key>`, `edit <key>`, or `move <key> to <state>`.
      */
    private def described(steps: Vector[Step], i: Int): String =
      steps.lift(i).fold("") {
        case Step.Keep(k, _) => s"keep ${Keeping.shown(k)}"
        case Step.Edit(e) => s"edit ${e.key.value}"
        case Step.Move(m) => s"move ${m.key.value} to ${StateName.value(m.to)}"
      }
  }

  /** `project_show`. */
  object Show extends ReadingCommand[Showing] {
    override def offeredToTurns: Boolean = shipped
    override def shown(args: Showing): String = args.keys match {
      case ShowKeys.One(key) => key.value
      case ShowKeys.Several(keys) => keys.map(_.value).mkString(", ")
    }

    val spec: ToolSpec[Showing] = ToolSpec(
      ToolName("project_show"),
      "Shows artifacts by key: each one's key, kind, state, title, parent, cites, author and " +
        "version (what project_keep's and project_edit's version takes), then its text as " +
        "markdown. With head, only those lines, its opening paragraph and the names of its " +
        "sections: enough to tell what it is without reading it all. With section, those lines " +
        "and that one section alone; refused when no section has that name, or several do.",
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
            section = Field
              .text(
                "Show only the section of each one under this `##` heading, its text as " +
                  "written after the `## `."
              )
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
              case (Some(k), None) => parsed("key", k, ArtifactRef.read).map(ShowKeys.One(_))
              case (None, Some(ks)) =>
                Traversal
                  .traverse(ks.toVector)(parsed("keys", _, ArtifactRef.read))
                  .map(ShowKeys.Several(_))
              case (Some(_), Some(_)) =>
                Left(ArgsError.Refused("keys", "Give key or keys, not both."))
              case (None, None) => Left(ArgsError.Missing("key", "an artifact's key, or give keys"))
            }
            when <- a.at.fold(Right(None))(t => parsed("at", t, instant).map(Some(_)))
            part <- (a.head.getOrElse(false), a.section) match {
              case (true, Some(_)) =>
                Left(ArgsError.Refused("section", "Give head or section, not both."))
              case (true, None) => Right(ShowPart.Head)
              case (false, Some(title)) => Right(ShowPart.Section(title))
              case (false, None) => Right(ShowPart.Whole)
            }
          } yield Showing(keys, when, part)
        )
    )
    def run(args: Showing, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      def version(project: Loaded, key: ArtifactRef): Either[Stop, (Document, Artifact)] =
        args.at match {
          case None => resolved(project, key).flatMap(artifact(project, _))
          case Some(when) =>
            for {
              where <- recorded(own.documents, project, key)
              (doc, label) = where
              found <- stored(own.documents.asOf(doc, label, when))
              document <- found.toRight(Stop.Refused(s"${key.value} had no version at $when"))
              version <- refuse(readable(document))
            } yield (document, version)
        }
      def rendered(key: ArtifactRef)(kept: (Document, Artifact)): Either[Stop, String] =
        args.part match {
          case ShowPart.Whole => Right(ProjectMarkdown.shown(kept._2, kept._1.version))
          case ShowPart.Head => Right(ProjectMarkdown.head(kept._2, kept._1.version))
          case ShowPart.Section(title) =>
            ProjectMarkdown
              .section(kept._2, kept._1.version, title)
              .left
              .map(why => Stop.Refused(s"${key.value}: $why"))
        }
      args.keys match {
        case ShowKeys.One(key) =>
          loaded(own.documents, key.project).flatMap(version(_, key)).flatMap(rendered(key))
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
                  .flatMap(rendered(key)) match {
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
    override def offeredToTurns: Boolean = shipped
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
            key <- parsed("key", a.key, ArtifactRef.read)
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
            refuse(Traversal.sequence(kept.map(row)))
              .map(AnswerText.versions(_, "No version is kept."))
          case Some((from, to)) =>
            // Its id, as its newest version holds it: the key it is stored under is never shown.
            val id = kept
              .collectFirst { case DocumentVersion.Holding(d) => readable(d).toOption }
              .flatten
              .fold(args.key.value)(_.head.key.value)
            def file(n: Long): Either[Stop, String] =
              kept
                .collectFirst {
                  case DocumentVersion.Holding(d) if DocumentVersionId.value(d.version) == n => d
                }
                .toRight(
                  Stop.Refused(
                    s"$id has no version $n among its $HistoryMost newest kept"
                  )
                )
                .flatMap(d => refuse(readable(d)))
                .map(a => ProjectMarkdown.file(a)._2)
            for {
              older <- file(from)
              newer <- file(to)
            } yield AnswerText.diff(
              s"$id version $from",
              older,
              s"$id version $to",
              newer
            )
        }
      } yield text
    }
  }

  /** `project_list`. */
  object List extends ReadingCommand[Listing] {
    override def offeredToTurns: Boolean = shipped
    override def shown(args: Listing): String = ProjectKey.value(args.project)

    val spec: ToolSpec[Listing] = ToolSpec(
      ToolName("project_list"),
      s"Lists a project's artifacts, one line each, most recently changed first, at most " +
        s"$ListMost. Leaves out ended ones ($EndedWords) unless `ended` is true or `state` " +
        "names an end state. A first line counts all that match; when more than " +
        s"$ListMost match, a last line says how many more, the `skip` that shows the next " +
        s"$ListMost, and which arguments narrow the list. Refused for a parent the project " +
        "lacks, naming the nearest keys it has.",
      Args
        .of(
          (
            project = Field.text("The project's key."),
            kind = Field.text("Only artifacts of this kind.").optional,
            state = Field
              .text("Only artifacts in this state; an end state, such as done, needs no `ended`.")
              .optional,
            parent = Field
              .text(
                "Only the artifacts under this one, `<project>/<artifact>`: those directly " +
                  "under it, or down to `depth` levels."
              )
              .optional,
            depth = Field
              .integer(
                s"With `parent`, how many levels under it to list, from 1 to $DepthMost; 1, " +
                  "directly under it, when not given.",
                1,
                DepthMost
              )
              .optional,
            ended = endedField("list"),
            opening = Field
              .flag(
                "Whether each line ends with the opening words of the artifact's text; false " +
                  "when not given."
              )
              .optional,
            skip = skipField
          )
        )
        .refine(a =>
          for {
            p <- parsed("project", a.project, ProjectKey.of)
            kind <- a.kind.fold(Right(None))(k => parsed("kind", k, KindName.of).map(Some(_)))
            state <- a.state.fold(Right(None))(s => parsed("state", s, StateName.of).map(Some(_)))
            parent <- a.parent.fold(Right(None)) { k =>
              parsed("parent", k, ArtifactRef.read).flatMap(key =>
                Either.cond(
                  key.project == p,
                  Some(key),
                  ArgsError.Refused(
                    "parent",
                    s"The parent ${key.value} is not in project ${ProjectKey.value(p)}."
                  )
                )
              )
            }
            under <- (parent, a.depth) match {
              case (None, None) => Right(None)
              case (None, Some(_)) =>
                Left(
                  ArgsError
                    .Refused("depth", "depth counts levels under a parent: give parent too.")
                )
              case (Some(local), depth) =>
                Under
                  .of(local, depth.fold(1)(_.toInt))
                  .left
                  .map(ArgsError.Refused("depth", _))
                  .map(Some(_))
            }
          } yield Listing(
            p,
            kind,
            state,
            under,
            a.ended.getOrElse(false),
            a.opening.getOrElse(false),
            a.skip.getOrElse(0)
          )
        )
    )
    def run(args: Listing, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own.documents, args.project)
        parent <- args.under.fold(Right(None)) { u =>
          resolved(project, u.parent).map(Some(_))
        }
        ended <- endedOf(own.documents, project.pin)
      } yield {
        // What a line's `<n> under it` counts: the lines `parent: <it>` would list under the
        // same `ended`, of any kind; a `state` naming an end state does not bring ended ones in.
        val counted = (h: ArtifactHead) => args.ended || !ended(h)
        val beneath = project.artifacts
          .flatMap(a => a.head.parent.filter(_ => counted(a.head)))
          .groupMapReduce(identity)(_ => 1)(_ + _)
        def newest(among: Vector[(Document, Artifact)]): Vector[(Document, Artifact)] =
          among.sortBy((d: Document, a: Artifact) => (-d.written.toEpochMilli, a.head.key.number))
        val children: Map[ArtifactKey, Vector[(Document, Artifact)]] = project.documents
          .flatMap((d: Document, a: Artifact) => a.head.parent.map(_ -> (d, a)))
          .groupMap(_._1)(_._2)
        // What sits `levels` levels under `from`, each followed by what sits under it.
        def walk(from: ArtifactKey, levels: Int): Vector[(Document, Artifact)] =
          if (levels < 1) Vector.empty
          else
            newest(children.getOrElse(from, Vector.empty)).flatMap((d: Document, a: Artifact) =>
              (d, a) +: walk(a.head.key, levels - 1)
            )
        val depth = args.under.fold(1)(_.depth)
        val reached = parent.fold(newest(project.documents))(walk(_, depth))
        val matching = reached.filter((_, a) =>
          args.kind.forall(_ == a.head.kind) && args.state.forall(_ == a.head.state)
        )
        val (listed, left) =
          matching.partition((_, a) => args.state.isDefined || counted(a.head))
        val rows = listed
          .map((d, a) =>
            (artifact = a, written = d.written, beneath = beneath.getOrElse(a.head.key, 0))
          )
        AnswerText.listed(
          rows,
          AnswerText.LineParts(args.kind.isEmpty, args.state.isEmpty, parent, args.opening),
          args.skip,
          ListMost,
          left.size,
          // Below the first level, a cut drops whole later subtrees, not the stalest.
          if (depth > 1) "use a smaller depth or a parent further down"
          else "narrow with kind, state or parent"
        )
      }
    }
  }

  /** `project_find`. */
  object Find extends ReadingCommand[Finding] {
    override def offeredToTurns: Boolean = shipped
    override def shown(args: Finding): String = args.query

    val spec: ToolSpec[Finding] = ToolSpec(
      ToolName("project_find"),
      s"Finds the artifacts whose titles and bodies best match some words, best first, at " +
        s"most $Found, one line each: only one project's, when given. Leaves out ended " +
        s"artifacts ($EndedWords) unless " +
        "`ended` is true. Pass `ended: true` when asking what was tried before, what was " +
        "decided against, or why something was decided: dropped notes and ended milestones " +
        "hold the alternatives that were rejected.",
      Args
        .of(
          (
            query = Field.text("The words to find."),
            project = Field.text("Only artifacts of this project.").optional,
            ended = endedField("find")
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
        documents = hits.map(_.document).filter(ArtifactJson.isArtifact)
        artifacts <- refuse(Traversal.sequence(documents.map(readable)))
        heads = artifacts
          .map(_.head)
          .filter(h => args.project.forall(_ == h.key.project))
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
                      .flatMap(endedOf(own.documents, _))
                      .map(ended => m.updated(p, ended))
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
    override def offeredToTurns: Boolean = shipped
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
        files <- exported(own.documents, args.project)
        rest = files
          .sortBy(_._1)
          .filter((path, _) => args.after.forall(path > _))
        page = rest.take(ExportPage)
      } yield AnswerText.page(page, page.lastOption.map(_._1).filter(_ => rest.size > page.size))
    }
  }

  /** `project_rekey`: not built yet, so a project of the old form stays in it. */
  object Rekey extends KeepingCommand[Rekeying] {
    val spec: ToolSpec[Rekeying] = ToolSpec(
      ToolName("project_rekey"),
      "Gives each artifact of a project of the old form, its artifacts keyed by name, a " +
        "number, keeping its name as its handle. Not built yet: always refused.",
      Args
        .of((project = Field.text("The project's key.")))
        .refine(a => parsed("project", a.project, ProjectKey.of).map(Rekeying(_)))
    )
    def run(
        args: Rekeying,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] =
      Right(
        CommandOutcome.Refused(
          s"project_rekey is not built yet: project ${ProjectKey.value(args.project)} is left " +
            "as it is"
        )
      )
  }

  /** `project` as files ([[ProjectMarkdown.render]]), or, in the old form, as it was exported
    * then ([[ProjectMarkdown.oldForm]]); or why not.
    */
  private def exported(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Vector[(String, String)]] =
    for {
      found <- indexed(own, project)
      (index, pin) = found
      next <- refuse(ArtifactJson.nextOf(index))
      files <- next match {
        case Some(_) =>
          loaded(own, project)
            .flatMap(p => refuse(Project.of(project, p.pin, p.artifacts)))
            .map(ProjectMarkdown.render)
        case None =>
          for {
            holding <- refuse(ArtifactJson.holding(project))
            documents <- stored(own.referring(holding, Projects.Terms.bound))
            files <- refuse(
              ProjectMarkdown.oldForm(
                project,
                pin,
                documents.map(d => DocKey.value(d.key) -> d.data)
              )
            )
          } yield files
      }
    } yield files

  /** `project_claim`. */
  object ProjectClaim extends KeepingCommand[Claiming] {
    val spec: ToolSpec[Claiming] = ToolSpec(
      ToolName("project_claim"),
      "Claims an artifact for a session or agent to work in a role, until project_release " +
        "frees it; the artifact itself is not changed. Keeps the artifact's brief for what it " +
        "is claimed for, which that session is shown when its view is next rebuilt. Answers a " +
        "JSON object: `claimed`, a sentence saying what was claimed; `brief`, `kept` or " +
        "`none`; and with `none`, `note`, saying why. Refused, naming who holds it, when it " +
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
            holder = Field.text(
              "Who works it: the session, or the agent type in it, whose brief is written, " +
                s"as its edge names it; one line of at most ${ClaimHolder.Longest} characters."
            )
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactRef.read)
            role <- parsed("role", a.role, RoleName.of)
            holder <- parsed("holder", a.holder, ClaimHolder.of)
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
      val holder = ClaimHolder.value(args.holder)
      locked(claims, args.key.project).flatMap { lock =>
        given Locked = lock
        for {
          project <- loaded(own, args.key.project)
          method <- declared(own, project.pin)
          _ <- viewOf(method, args.role)
          key <- resolved(project, args.key)
          found <- artifact(project, key)
          (document, a) = found
          claimed <- claimedFor(claims, document, args, by.person, at)
          made <- claimed match {
            case Claimed.Made(c) => Right(c)
            case Claimed.Taken(first) =>
              Left(
                Stop.Refused(
                  first.fold(s"${a.head.called} is claimed already; project_release frees it")(c =>
                    s"${a.head.called} is claimed already, as ${ClaimRole.value(c.role)} for " +
                      s"${ClaimHolder.value(c.holder)} by ${accounts(c.by)} since ${c.made}; " +
                      s"it lapses at ${c.lapses} unless they keep working; project_release frees it"
                  )
                )
              )
            case Claimed.Holding(other) =>
              Left(
                Stop.Refused(
                  other.fold(
                    s"$holder works another artifact already; release it with project_release first"
                  )(c =>
                    s"$holder works ${subjectOf(project, c)} already, claimed since ${c.made}; " +
                      s"it lapses at ${c.lapses} unless they keep working; " +
                      "release it with project_release first"
                  )
                )
              )
          }
          whole <- refuse(Project.of(args.key.project, project.pin, project.artifacts))
          // The role has a view and the artifact is the project's, so the claim has its brief.
          unkept <- rendered(method, whole, ids(project), Vector(made)).headOption
            .toRight(Stop.Refused(s"${a.head.called} has no brief as ${RoleName.value(args.role)}"))
            .flatMap(briefed(own, _, document.place, project, at))
        } yield AnswerText.claimed(
          s"Claimed ${AnswerText.named(a.head)} as ${RoleName.value(args.role)} for $holder.",
          unkept
        )
      }
    }
  }

  /** `project_release`. */
  object ProjectRelease extends KeepingCommand[Releasing] {
    val spec: ToolSpec[Releasing] = ToolSpec(
      ToolName("project_release"),
      "Frees an artifact's claim, so it may be claimed again, and removes the brief kept with " +
        "it, answering a JSON object: `released`, the artifact's key, and `holder`, who it was " +
        "claimed for. Refused when it is not claimed and has no brief.",
      Args
        .of((key = Field.text("The artifact's key, `<project>/<artifact>`.")))
        .refine(a => parsed("key", a.key, ArtifactRef.read).map(Releasing(_)))
    )
    def run(
        args: Releasing,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      locked(claims, args.key.project).flatMap { lock =>
        given Locked = lock
        for {
          project <- loaded(own, args.key.project)
          key <- resolved(project, args.key)
          found <- artifact(project, key)
          (document, _) = found
          freed <- stored(claims.release(document.key, document.label))
          // The brief goes even when its claim is gone, as a swept lapsed claim is.
          briefFor <- unbriefed(own, document.key, at)
          holder <- freed
            .map(_.holder)
            .orElse(briefFor)
            .toRight(Stop.Refused(s"${args.key.value} is not claimed"))
        } yield AnswerText.released(key, holder)
      }
    }
  }

  /** `project_stale`. */
  object Stale extends ReadingCommand[Staling] {
    override def offeredToTurns: Boolean = shipped
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
        ended <- ending(own.documents, project.pin)
        open = project.documents.filterNot((_, a) => ended(a.head))
        named = Project.named(open.map(_._2))
        before = at.minus(java.time.Duration.ofDays(7L * args.weeks))
        stale = open
          .filter((d, a) => d.written.isBefore(before) && !named(a.head.key))
          .sortBy((d, a) => (d.written.toEpochMilli, a.head.key.number))
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
          parsed("key", a.key, ArtifactRef.read).map(Withdrawing(_, a.cascade.getOrElse(false)))
        )
    )

    def run(
        args: Withdrawing,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      locked(claims, args.key.project).flatMap { lock =>
        given Locked = lock
        for {
          project <- loaded(own, args.key.project)
          held <- holding(claims, project, at)
          whole <- refuse(Project.of(args.key.project, project.pin, project.artifacts))
          ended <-
            if (args.cascade) ending(own, project.pin)
            else Right[Stop, ArtifactHead -> Boolean](_ => false)
          // What is left is a Project, so its export reads back (ADR 0049).
          key <- resolved(project, args.key)
          done <- refuse(Project.withdrawn(whole, key, args.cascade, ended))
          docs = documented(project, done)
          _ <- unclaimed(claims, docs.gone ++ docs.uncited, at)
          _ <- atTheirLabels(citing(docs), "withdraw")
          _ <- carried(own, docs, by, at)
          _ <- rebriefed(own, held, args.key.project, at)
        } yield AnswerText.withdrawn(
          done.gone.map(_.head.key),
          done.uncited.map(_.head.key),
          Projects.Terms.retention
        )
      }
    }
  }

  /** `project_sweep`. */
  object Sweep extends KeepingCommand[Sweeping] {

    override def shown(args: Sweeping): String = ProjectKey.value(args.project)

    val spec: ToolSpec[Sweeping] = ToolSpec(
      ToolName("project_sweep"),
      "Withdraws a project's old ended artifacts in one call. An artifact goes when all of " +
        "these hold: its kind is one of `kinds`; it is in an end state of its kind; it was last " +
        "kept or moved before the day `before` began (UTC; the `changed` day project_list " +
        "shows); it is not claimed; no artifact that is claimed or has not ended cites it; and " +
        "everything under it by parent goes too. Each ended artifact left that cites one that " +
        "goes is kept again without those citations (a new version, written by you, so its " +
        "changed day becomes today; the words of each link stay in its text). What goes leaves " +
        "the project and its export; project_show with an `at` before now, and project_history, " +
        s"still read it for ${Projects.Terms.retention.toDays} days. Answers how many went, by " +
        "kind and state; how many lost citations; how many old enough stayed, by reason; then " +
        s"one line each, at most $ListMost, those that stayed first; `show` lists one group " +
        s"alone and `skip` reads past the first $ListMost. Call it with dry first: it " +
        "changes nothing and answers what would go. Refused, changing nothing: `before` is " +
        "later than today; the project's method is not declared here; a kind is not one of the " +
        "method's kinds; or an artifact it would withdraw or rewrite is kept at a label below " +
        "what this room writes.",
      Args
        .of(
          (
            project = Field.text("The project's key."),
            kinds = Field.texts(
              s"The kinds it may withdraw, such as note or checkpoint: 1 to $SweepKinds of the " +
                "method's kinds.",
              1,
              SweepKinds
            ),
            before = Field.text(
              "A day, YYYY-MM-DD, today (UTC) or earlier: an artifact last kept or moved on or " +
                "after it stays."
            ),
            dry = Field
              .flag(
                "Whether to change nothing and only answer what would go and what would stay; " +
                  "false when not given."
              )
              .optional,
            show = Field
              .oneOf(
                "Which lines to list: stayed, rewritten or withdrawn; all three, in that order, " +
                  "when not given. The counts above them are of everything either way.",
                AnswerText.SweptGroup.Stayed.word,
                AnswerText.SweptGroup.values.toVector.map(_.word)*
              )
              .optional,
            skip = skipField
          )
        )
        .refine(a =>
          for {
            project <- parsed("project", a.project, ProjectKey.of)
            kinds <- Traversal.traverse(a.kinds.toVector)(parsed("kinds", _, KindName.of))
            before <- parsed("before", a.before, day)
            // `show` reads only as one of the three `oneOf` names, each a group's.
            show = a.show
              .flatMap(s => AnswerText.SweptGroup.values.find(_.word == s))
            sweeping <- Sweeping
              .of(project, kinds.toSet, before, a.dry.getOrElse(false), show, a.skip.getOrElse(0))
          } yield sweeping
        )
    )

    def run(
        args: Sweeping,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      val today = at.atZone(ZoneOffset.UTC).toLocalDate
      locked(claims, args.project).flatMap { lock =>
        given Locked = lock
        for {
          _ <-
            if (args.before.isAfter(today))
              Left(Stop.Refused(s"before is ${args.before}, later than today, $today (UTC)"))
            else Right(())
          project <- loaded(own, args.project)
          held <- if (args.dry) Right(Vector.empty) else holding(claims, project, at)
          method <- declared(own, project.pin)
          _ <- args.kinds.filterNot(k => method.kinds.exists(_.name == k)).toVector match {
            case Vector() => Right(())
            case unknown =>
              Left(
                Stop.Refused(
                  s"${unknown.map(KindName.value).sorted.mkString(", ")}: not among the kinds of " +
                    s"${MethodName.value(method.name)} version ${method.version}, which are " +
                    s"${method.kinds.map(k => KindName.value(k.name)).sorted.mkString(", ")}"
                )
              )
          }
          whole <- refuse(Project.of(args.project, project.pin, project.artifacts))
          live <- stored(claims.live(at))
          began = args.before.atStartOfDay(ZoneOffset.UTC).toInstant
          due = project.documents.collect {
            case (d, a) if args.kinds(a.head.kind) && d.written.isBefore(began) => a.head.key
          }.toSet
          claimed = claimedOf(live, project.documents).map((a, h) => a.head.key -> h).toMap
          // What is left is a Project, so its export reads back (ADR 0049).
          swept = Project.swept(whole, due, endsUnder(method), claimed)
          docs = documented(project, swept.withdrawn)
          _ <- atTheirLabels(
            docs.gone.map((d, a) => d -> s"${a.head.key.value} would be withdrawn, but") ++
              citing(docs),
            "withdraw"
          )
          _ <-
            if (args.dry) Right(())
            else carried(own, docs, by, at).flatMap(_ => rebriefed(own, held, args.project, at))
        } yield AnswerText.swept(
          swept,
          args.dry,
          args.show,
          args.skip,
          ListMost,
          Projects.Terms.retention
        )
      }
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
      "Moves a project to another version of its method, each artifact's kind and state " +
        "renamed as given: an artifact whose kind or state is renamed is kept again (a new " +
        "version, written by you; its history still reads); every other artifact is left as " +
        "it is. The project then holds to that version. A kind or state not renamed keeps " +
        "its name. Refused, changing nothing, naming each cause: the version is not " +
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
                key <- parsed("artifacts", x.key, ArtifactRef.read)
                kind <- parsed("artifacts", x.kind, KindName.of)
              } yield key -> kind
            )
            kinds <- once("kinds", kindPairs, KindName.value)
            states <- once(
              "states",
              statePairs,
              (ks: (KindName, StateName)) => s"${KindName.value(ks._1)} ${StateName.value(ks._2)}"
            )
            artifacts <- once("artifacts", artifactPairs, (k: ArtifactRef) => k.value)
          } yield Migrating(p, a.version.toInt, kinds, states, artifacts)
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
      locked(claims, args.project).flatMap { lock =>
        given Locked = lock
        for {
          project <- loaded(own, args.project)
          whole <- refuse(Project.of(args.project, project.pin, project.artifacts))
          target <- declaring.version(own, project.pin.method, args.version)
          artifacts <- Traversal.traverse(args.artifacts.toVector)((r, k) =>
            resolved(project, r).map(_ -> k)
          )
          done <- refuse(
            target.migrated(whole, Remap(args.kinds, args.states, artifacts.toMap))
          )
          index <- indexed(own, args.project)
          _ <- unclaimed(claims, project.documents, at)
          renamed = done.renamed.toSet
          _ <- atTheirLabels(
            (index._1 -> s"project $p") +: project.documents.collect {
              case (d, a) if renamed(a.head.key) => d -> a.head.key.value
            },
            "migrate"
          )
          writer = Some(author(by))
          before = project.documents.map((d, a) => a.head.key -> d).toMap
          _ <- done.project.artifacts
            .filter(a => renamed(a.head.key))
            .foldLeft[Either[Stop, Unit]](Right(())) { (so, a) =>
              so.flatMap(_ =>
                kept(
                  own,
                  Artifact(a.head.copy(by = writer), a.body),
                  before.get(a.head.key),
                  Whose.Prior,
                  at
                ).map(_ => ())
              )
            }
          rewritten <- refuse(
            ArtifactJson.index(args.project, done.project.pin, author(by), project.next)
          )
          (key, text, data, refs) = rewritten
          _ <- keptEverywhere(own, key, index._1.label, Tx.top, text, data, refs, at)
        } yield AnswerText.migrated(args.project, done.project.pin, done.renamed, ListMost)
      }
    }
  }

  /** `project_claims`. A keeping command only for its plugin's claims, which a reading command is
    * not handed; it keeps nothing.
    */
  object ProjectClaims extends KeepingCommand[Surveying] {
    override def onlyReads: Boolean = true

    val spec: ToolSpec[Surveying] = ToolSpec(
      ToolName("project_claims"),
      "Lists the artifacts claimed now, only one project's when given, answering a JSON " +
        "object: `claims`, each its artifact's `key`, its `role`, `holder`, who it is " +
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
      for {
        live <- stored(claims.live(at))
        // A claim's subject is the key its artifact is kept under; its id is in the head.
        documents <- stored(Traversal.traverse(live)(c => own.current(c.subject, c.label)))
      } yield AnswerText.claims(
        live.zip(documents).flatMap { (c, d) =>
          d.flatMap(Artifact.of(_).toOption)
            .map(_.head.key)
            .orElse(ArtifactKey.read(DocKey.value(c.subject)).toOption)
            .filter(k => args.project.forall(_ == k.project))
            .map(k => (k, c, accounts(c.by)))
        }
      )
    }
  }

  /** `project_brief`. */
  object ProjectBrief extends ReadingCommand[Briefing] {
    override def offeredToTurns: Boolean = shipped
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
            key <- parsed("key", a.key, ArtifactRef.read)
            role <- parsed("role", a.role, RoleName.of)
          } yield Briefing(key, role, a.chars)
        )
    )
    def run(args: Briefing, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own.documents, args.key.project)
        method <- declared(own.documents, project.pin)
        view <- viewOf(method, args.role)
        whole <- refuse(Project.of(args.key.project, project.pin, project.artifacts))
        key <- resolved(project, args.key)
        brief <- Brief.of(whole, key, view, args.chars).left.map {
          case BriefRefusal.Unknown(k) => lacking(k)
          case BriefRefusal.Over(size) =>
            Stop.Refused(
              s"${args.key.value} with what its role always carries is $size characters, " +
                s"over ${args.chars}"
            )
        }
      } yield brief.text
    }
  }

  /** The step `stepping` makes of project `key` as loaded, taken alone, as `project_keep` or
    * `project_move` takes it, saying `verb` of an artifact kept below this room: its answer and
    * the version each artifact it wrote is now at, or why it was refused.
    */
  private def alone(
      own: DocumentKeeper,
      claims: Claims,
      key: ProjectKey,
      by: CommandBy,
      at: Instant,
      verb: String
  )(stepping: Loaded -> Either[Stop, Step])(using
      Tx^
  ): Either[Stop, (said: String, versions: Map[ArtifactKey, DocumentVersionId])] =
    locked(claims, key).flatMap { lock =>
      given Locked = lock
      for {
        project <- loaded(own, key)
        held <- holding(claims, project, at)
        step <- stepping(project)
        method <- declared(own, project.pin)
        plan = planned(project, method, Vector(step), author(by), moverOf(by), onward = false)
        (said, steps) = plan
        answer <- said.headOption.toRight(Stop.Refused("nothing was asked")).flatten
        _ <- settled(claims, project, method, key, steps, at, verb)
        versions <- wrote(own, project, steps, author(by), at)
        _ <- rebriefed(own, held, key, at)
      } yield (said = answer, versions = versions)
    }

  /** Each claim live at `at` on an artifact of `project`, matched by the key its document is
    * kept under, and kept at this transaction's floor, as held ([[Claims.held]]) until it ends, in the order [[Claims.live]] gives, so that
    * the session sweep, which takes no project's lock, waits for this transaction before it
    * deletes one. A claim gone or made anew since it was read is left out. A claim kept below
    * the floor is passed over: a command changes only artifacts kept at its floor
    * ([[atTheirLabels]]), so nothing that brief shows changes.
    */
  private def holding(claims: Claims, project: Loaded, at: Instant)(using
      tx: Tx^
  )(using Locked): Either[Stop, Vector[Claim]] =
    for {
      live <- stored(claims.live(at))
      keys = ids(project)
      here = live.filter(c => c.label == Tx.floor(tx) && keys.contains(c.subject))
      held <- Traversal.traverse(here)(c => stored(claims.held(c)))
    } yield held.flatten

  /** Keeps again ([[briefed]]) the brief of each of `held` ([[holding]]), claims on artifacts of
    * project `key`, as the project now stands. Nothing when the project's method is not declared
    * here; refused when it is and does not read. A brief already over its budget before the
    * command is withdrawn ([[briefed]]); one the command would have put over it, [[settled]] has
    * refused, `held` taken first.
    */
  private def rebriefed(own: DocumentKeeper, held: Vector[Claim], key: ProjectKey, at: Instant)(
      using Tx^
  )(using Locked): Either[Stop, Unit] =
    for {
      project <- if (held.isEmpty) Right(None) else loaded(own, key).map(Some(_))
      method <- project.fold(Right(None))(p =>
        declaring.ifDeclared(own, p.pin.method, p.pin.version)
      )
      briefs = (for {
        p <- project
        m <- method
        whole <- Project.of(key, p.pin, p.artifacts).toOption
      } yield rendered(m, whole, ids(p), held).flatMap(r =>
        p.documents.find(_._2.head.key == r.key).map(d => (r, d._1.place, p))
      )).getOrElse(Vector.empty)
      _ <- Traversal
        .traverse(briefs)((r, place, p) => briefed(own, r, place, p, at))
        .map((_: Vector[Option[String]]) => ())
    } yield ()

  /** The method version `pin` names, as `own` holds it, or why not. */
  private def declared(own: DocumentShelf, pin: Pin)(using Tx^): Either[Stop, Method] =
    declaring.version(own, pin.method, pin.version)

  /** Whether an artifact of a project holding to `pin` is in an end state of its kind, or why
    * not: `pin`'s method is not declared here.
    */
  private def ending(own: DocumentShelf, pin: Pin)(using
      Tx^
  ): Either[Stop, ArtifactHead -> Boolean] =
    declared(own, pin).map(endsUnder)

  /** Whether `project_list` and `project_find` leave an artifact of a project holding to `pin`
    * out as ended: as [[ending]] says, and never when `pin`'s method is not declared here; or why
    * not: it is declared and does not read.
    */
  private def endedOf(own: DocumentShelf, pin: Pin)(using
      Tx^
  ): Either[Stop, ArtifactHead -> Boolean] =
    declaring
      .ifDeclared(own, pin.method, pin.version)
      .map(_.fold[ArtifactHead -> Boolean](_ => false)(endsUnder))

  /** Whether an artifact is in an end state of its kind under `method`. */
  private def endsUnder(method: Method): ArtifactHead -> Boolean =
    h => ArtifactKind.ends(method.kinds, h)
}

private[plugin] object ProjectCommands {

  /** `project_new`'s arguments. */
  final case class Starting(project: ProjectKey, method: Option[MethodName]) extends caps.Pure

  /** `project_keep`'s arguments: with no `key`, a new artifact of `project`, its id minted; else a
    * new version of the artifact `key` names, of `project`. A `parent` or `cites` of `None`
    * leaves an existing artifact's as it was (a new one's, none); `version`, given only with a
    * `key`, is the one its writer read, and the keep is refused unless it is the artifact's
    * current one.
    */
  final case class Keeping private (
      project: ProjectKey,
      key: Option[ArtifactRef],
      handle: HandleChange,
      kind: Option[KindName],
      title: String,
      body: String,
      parent: Option[Option[ArtifactRef]],
      cites: Option[Vector[ArtifactRef]],
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
        HandleChange.Unchanged,
        None,
        title,
        body,
        None,
        None,
        Some(version)
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
          case None => Right(HandleChange.Unchanged)
          case Some("") => Right(HandleChange.Removed)
          case Some(h) => parsed("handle", h, Handle.of).map(HandleChange.To(_))
        }
        kind <- kind.fold(Right(None))(k => parsed("kind", k, KindName.of).map(Some(_)))
        parent <- parent.fold(Right(None)) { p =>
          if (p.isEmpty) Right(Some(None))
          else referred("parent", p, project).map(k => Some(Some(k)))
        }
        cites <- cites
          .fold(Right(None))(cs =>
            Traversal.traverse(cs.toVector)(referred("cites", _, project)).map(Some(_))
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

  /** What a keep does to an artifact's handle: leaves it, removes it, or sets it. */
  enum HandleChange extends caps.Pure {
    case Unchanged
    case Removed
    case To(handle: Handle)
  }

  /** `value` as a reference to an artifact of `project`, or the argument error for `field`: no
    * reference ([[ArtifactRef.read]]), or one of another project.
    */
  private def referred(
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
  private def versionOf(version: Long): Either[ArgsError, DocumentVersionId] =
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
  private val BatchStep: Args[
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

  /** One step of `project_batch`, read as its command reads its arguments. */
  enum Step extends caps.Pure {
    case Keep(args: Keeping, whose: Whose = Whose.Caller)
    case Edit(args: Editing)
    case Move(args: Moving)
  }

  /** Whose words a kept body is: wholly its caller's, as `project_keep` gives it; or the version
    * before's, carried forward with a part its caller changed, as `project_edit` makes it.
    */
  enum Whose extends caps.Pure {
    case Caller, Prior
  }

  /** What `project_keep`'s fields mean, said once for it and for a batch's keeps. */
  private object KeepMeaning {
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
  private object EditMeaning {
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
  private def editOf(
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
  private val HistoryMost: Int = 50

  /** `version` as [[AnswerText.versions]] lists it, or why it does not read. */
  private def row(
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
  private val skipField: Field[Option[Int]] =
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
  private val EndedWords: String =
    "in an end state of their kind, such as done, dropped or closed"

  /** The `ended` argument of `project_list` and `project_find`, whose answer `verb` says. */
  private def endedField(verb: String): Field[Option[Boolean]] =
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

  /** `project_rekey`'s arguments: the project to re-key. */
  final case class Rekeying(project: ProjectKey) extends caps.Pure

  /** `project_claim`'s arguments. */
  final case class Claiming(key: ArtifactRef, role: RoleName, holder: ClaimHolder) extends caps.Pure

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
  private val SweepKinds: Int = 20

  /** `text` as a day, YYYY-MM-DD, or why not. */
  private def day(text: String): Either[String, LocalDate] =
    scala.util
      .Try(LocalDate.parse(text))
      .toOption
      .toRight(s"`$text` is not a day such as 2026-10-27")

  /** `project_migrate`'s arguments: the version of its method to move `project` to. */
  final case class Migrating(
      project: ProjectKey,
      version: Int,
      kinds: Map[KindName, KindName],
      states: Map[(KindName, StateName), StateName],
      artifacts: Map[ArtifactRef, KindName]
  ) extends caps.Pure

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
  final case class Briefing(key: ArtifactRef, role: RoleName, chars: Int) extends caps.Pure

  /** 100,000: the most characters `project_brief` is asked to give. */
  private val BriefMost: Int = 100_000

  /** Why project `project`, in the old form (ADR 0063), is refused a command. */
  private def OldForm(project: ProjectKey): String =
    s"project ${ProjectKey.value(project)} is in the old form, its artifacts keyed by name: " +
      "re-key it with project_rekey first"

  /** Why a command stopped: its store failed, or it refused, in words its asker reads. */
  private[plugin] enum Stop extends caps.Pure {
    case Store(error: StoreError)
    case Refused(why: String)
  }

  /** A project as its documents hold it: its key, its index, its pin, the number its next new
    * artifact takes, and each artifact with its document.
    */
  private final case class Loaded(
      key: ProjectKey,
      index: Document,
      pin: Pin,
      next: ArtifactNumber,
      documents: Vector[(Document, Artifact)]
  ) {
    def artifacts: Vector[Artifact] = documents.map(_._2)
  }

  /** That this transaction holds a project's lock: what each helper that keeps, withdraws or
    * claims for a project asks for. [[locked]] alone makes one.
    */
  private final class Locked private[ProjectCommands] () extends caps.Pure

  /** Holds `project`'s lock ([[Claims.lock]] of `<project>#lock`) until this transaction ends:
    * of two commands on one project, from rooms at any labels, the second waits here for the
    * first to end, then reads what it kept.
    */
  private def locked(claims: Claims, project: ProjectKey)(using Tx^): Either[Stop, Locked] =
    for {
      key <- refuse(DocKey.of(s"${ProjectKey.value(project)}#lock"))
      _ <- stored(claims.lock(key))
    } yield new Locked

  /** [[Claims.claim]] of `document`, the artifact `args` names, as `args` asks, by `by`, at its
    * label.
    */
  private def claimedFor(
      claims: Claims,
      document: Document,
      args: Claiming,
      by: Principal.Person,
      at: Instant
  )(using Tx^)(using Locked): Either[Stop, Claimed] =
    stored(claims.claim(document.key, args.holder, args.role, by, document.label, at))

  private[plugin] def answered(step: Either[Stop, String]): Either[StoreError, CommandOutcome] =
    step match {
      case Right(text) => Right(CommandOutcome.Done(text))
      case Left(Stop.Refused(why)) => Right(CommandOutcome.Refused(why))
      case Left(Stop.Store(e)) => Left(e)
    }

  private[plugin] def refuse[A](e: Either[String, A]): Either[Stop, A] = e.left.map(Stop.Refused(_))

  private[plugin] def stored[A](e: Either[StoreError, A]): Either[Stop, A] =
    e.left.map(Stop.Store(_))

  /** As [[stored]], a write refused past the bound refused in words. */
  private[plugin] def written[A](e: Either[StoreError, A]): Either[Stop, A] = e.left.map {
    case StoreError.Full(bound) => Stop.Refused(Projects.full(bound))
    case other => Stop.Store(other)
  }

  /** 20: the most files one page of `project_export` holds. */
  private val ExportPage: Int = 20

  /** 20: the most artifacts `project_find` answers. */
  private val Found: Int = 20

  /** 100: how many best matches `project_find` reads, before leaving out another project's and
    * ended ones, to answer [[Found]] of.
    */
  private val FoundAmong: Int = 100

  /** `project` and its artifacts as `own` holds them, or why not: among them, that it is in the
    * old form ([[OldForm]]).
    */
  private def loaded(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Loaded] = {
    for {
      found <- indexed(own, project)
      (index, pin) = found
      read <- refuse(
        ArtifactJson
          .nextOf(index)
          .left
          .map(why => s"project ${ProjectKey.value(project)} cannot be read: $why")
      )
      next <- read.toRight(Stop.Refused(OldForm(project)))
      holding <- refuse(ArtifactJson.holding(project))
      documents <- stored(own.referring(holding, Projects.Terms.bound))
      artifacts <- refuse(Traversal.sequence(documents.map(d => readable(d).map(d -> _))))
    } yield Loaded(project, index, pin, next, artifacts)
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

  /** `project`'s index document as `own` holds it, if it is started. */
  private def indexOf(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Option[Document]] =
    for {
      indexing <- refuse(ArtifactJson.indexing(project))
      indexes <- stored(own.referring(indexing, 1))
    } yield indexes.headOption

  /** As [[written]], `text`, `data` and `refs` kept through `own` under `key` at `label`, at
    * every place and with no pointer, believed at `believed` as `own` believes it.
    */
  private def keptEverywhere(
      own: DocumentKeeper,
      key: DocKey,
      label: Label,
      believed: Integrity,
      text: DocText,
      data: ujson.Value,
      refs: Set[DocRef],
      at: Instant
  )(using Tx^)(using Locked): Either[Stop, Written] =
    written(own.write(key, label, believed, Place.Everywhere, text, data, None, at, refs))

  /** `project`'s index document as `own` holds it, and the pin it holds, or why not. */
  private def indexed(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, (Document, Pin)] = {
    val p = ProjectKey.value(project)
    for {
      was <- indexOf(own, project)
      index <- was.toRight(
        Stop.Refused(s"there is no project $p; start it with project_new")
      )
      pin <- refuse(ArtifactJson.pinOf(index).left.map(why => s"project $p cannot be read: $why"))
    } yield (index, pin)
  }

  /** The artifact `key` of `project`, with its document, or why not. */
  private def artifact(project: Loaded, key: ArtifactKey): Either[Stop, (Document, Artifact)] =
    project.documents
      .find(_._2.head.key == key)
      .toRight(lacking(key))

  /** `ref`'s id among `artifacts`, `project`'s as steps have left them: by id or current handle
    * ([[Project.resolve]]), else as the store key of one of `project`'s documents whose artifact
    * is among them; or why not, as [[Project.resolve]] refuses.
    */
  private def resolver(
      project: Loaded,
      artifacts: Vector[Artifact]
  ): ArtifactRef -> Either[String, ArtifactKey] = ref =>
    Project.resolving(project.key, artifacts, ref).left.flatMap { why =>
      project.documents
        .collectFirst {
          case (d, a)
              if DocKey.value(d.key) == ref.value && artifacts.exists(_.head.key == a.head.key) =>
            a.head.key
        }
        .toRight(why)
    }

  /** [[resolver]] of `ref` over `project` as loaded. */
  private def resolved(project: Loaded, ref: ArtifactRef): Either[Stop, ArtifactKey] =
    refuse(resolver(project, project.artifacts)(ref))

  /** A live claim on artifact `key` of a project, and the brief [[Brief.of]] gives it within
    * [[DocRef.BriefChars]] under the view its role is shown, or how long it would be.
    */
  private final case class Rendered(
      claim: Claim,
      key: ArtifactKey,
      view: RoleView,
      brief: Either[BriefRefusal.Over, Brief]
  )

  /** Each claim of `live` on an artifact of `project` whose role `method` shows a view of, in the
    * order given, with its brief: its artifact the one `keys` gives its subject.
    */
  private def rendered(
      method: Method,
      project: Project,
      keys: Map[DocKey, ArtifactKey],
      live: Vector[Claim]
  ): Vector[Rendered] =
    live.flatMap { c =>
      for {
        key <- keys.get(c.subject)
        if key.project == project.key
        view <- method.views.find(_.role == c.role)
        brief <- Brief.of(project, key, view, DocRef.BriefChars) match {
          case Right(b) => Some(Right(b))
          case Left(over: BriefRefusal.Over) => Some(Left(over))
          case Left(BriefRefusal.Unknown(_)) => None
        }
      } yield Rendered(c, key, view, brief)
    }

  /** Nothing, or a refusal naming each of `after` over its budget when, rendered over `before`
    * under `method`, each claim's artifact the one `keys` gives its subject, it was not or was shorter (`before` none: as if it was not), and the
    * characters to cut from those of `changed` the brief holds whole (the claimed artifact, or one
    * its role carries).
    */
  private def within(
      method: Method,
      before: Option[Project],
      keys: Map[DocKey, ArtifactKey],
      after: Vector[Rendered],
      changed: Vector[ArtifactHead]
  ): Either[Stop, Unit] = {
    lazy val was =
      before.fold(Vector.empty[Rendered])(rendered(method, _, keys, after.map(_.claim)))
    after.flatMap { r =>
      for {
        size <- r.brief.left.toOption.map(_.size)
        if was.find(_.claim == r.claim).flatMap(_.brief.left.toOption).forall(_.size < size)
      } yield {
        val (c, key, view) = (r.claim, r.key, r.view)
        val held = changed.filter(h => h.key == key || h.handle.exists(view.carried.contains))
        val cut = size - DocRef.BriefChars
        f"the brief of ${key.value}, claimed as ${ClaimRole.value(c.role)} for " +
          f"${ClaimHolder.value(c.holder)}, would be $size%,d characters, $cut%,d over the " +
          f"${DocRef.BriefChars}%,d a brief holds (the artifact with what its role always " +
          f"carries): cut at least $cut%,d character${if (cut == 1) "" else "s"} from " +
          (if (held.isEmpty) changed else held).map(_.key.value).mkString(", ")
      }
    } match {
      case Vector() => Right(())
      case refusals => Left(Stop.Refused(refusals.mkString("; ")))
    }
  }

  /** What steps have made of a loaded project so far: its artifacts, the keys of those they
    * changed, in the order first changed, and the keys of those whose every word their caller
    * wrote ([[Whose.Caller]]).
    */
  private final case class Stepping(
      artifacts: Vector[Artifact],
      changed: Vector[ArtifactKey],
      callers: Set[ArtifactKey],
      next: ArtifactNumber
  ) extends caps.Pure

  /** `steps` over `project`, each as its command would take it and seeing what those before it
    * did, written as `writer` and moved by `mover`: each step's answer or refusal, in order, and
    * what they made of it. A refused step changes nothing; past one, the rest are taken only when
    * `onward`.
    */
  private def planned(
      project: Loaded,
      method: Method,
      steps: Vector[Step],
      writer: String,
      mover: Mover,
      onward: Boolean
  ): (Vector[Either[Stop, String]], Stepping) =
    steps.foldLeft(
      (
        Vector.empty[Either[Stop, String]],
        Stepping(project.artifacts, Vector.empty, Set.empty, project.next)
      )
    ) { (sofar: (Vector[Either[Stop, String]], Stepping), step: Step) =>
      val (said, now) = sofar
      if (!onward && said.exists(_.isLeft)) sofar
      else
        stepped(project, method, now, step, writer, mover) match {
          case Right((next, answer)) => (said :+ Right(answer), next)
          case Left(why) => (said :+ Left(why), now)
        }
    }

  /** The handle `change` leaves the artifact `key` (`existing` as it is now in `now`), or why not:
    * another artifact holds it, or it renames or removes a handle a role of `method` carries.
    */
  private def handled(
      method: Method,
      now: Stepping,
      key: ArtifactKey,
      existing: Option[Artifact],
      change: HandleChange
  ): Either[Stop, Option[Handle]] = {
    val was = existing.flatMap(_.head.handle)
    val called = existing.fold(key.value)(_.head.called)
    val becomes = change match {
      case HandleChange.Unchanged => was
      case HandleChange.Removed => None
      case HandleChange.To(h) => Some(h)
    }
    val holder =
      becomes.flatMap(h => now.artifacts.find(a => a.head.key != key && a.head.handle.contains(h)))
    val carrying = was
      .filterNot(becomes.contains)
      .map(h => method.views.filter(_.carried.contains(h)).map(_.role))
      .getOrElse(Vector.empty)
    (holder, becomes, carrying, was) match {
      case (Some(other), Some(h), _, _) =>
        Left(
          Stop.Refused(
            s"$called cannot take the handle ${Handle.value(h)}: ${other.head.called} holds it"
          )
        )
      case (_, _, roles @ (_ +: _), Some(h)) =>
        Left(
          Stop.Refused(
            s"$called keeps its handle: the brief of role " +
              s"${roles.map(RoleName.value).mkString(", ")} carries the artifact named ${Handle.value(h)}"
          )
        )
      case _ => Right(becomes)
    }
  }

  /** `step` taken over `now`, from `project` as loaded, written as `writer`, `mover` as
    * [[planned]] takes it: what it makes of `now` and its answer, or why it is refused, as
    * `project_keep`, `project_edit` or `project_move` refuses.
    */
  private def stepped(
      project: Loaded,
      method: Method,
      now: Stepping,
      step: Step,
      writer: String,
      mover: Mover
  ): Either[Stop, (Stepping, String)] = {
    def changing(changed: Vector[Artifact], wrote: Set[ArtifactKey] = Set.empty): Stepping = {
      val keys = changed.map(_.head.key)
      Stepping(
        now.artifacts.map(a => changed.find(_.head.key == a.head.key).getOrElse(a)) ++
          changed.filterNot(c => now.artifacts.exists(_.head.key == c.head.key)),
        now.changed ++ keys.filterNot(now.changed.contains),
        now.callers ++ wrote,
        now.next
      )
    }
    val resolve = resolver(project, now.artifacts)
    step match {
      case Step.Keep(args, whose) =>
        for {
          named <- args.key.fold(Right(None))(r => refuse(resolve(r)).map(Some(_)))
          existing = named.flatMap(k => now.artifacts.find(_.head.key == k))
          key = named.getOrElse(ArtifactKey.of(project.key, now.next))
          _ <- unchanged(key, args.version, project.documents.find(_._2.head.key == key))
          placed <- (existing, args.kind) match {
            case (Some(a), Some(k)) if k != a.head.kind =>
              Left(
                Stop.Refused(
                  s"${AnswerText.named(a.head)} keeps its kind; it cannot become ${KindName.value(k)}"
                )
              )
            case (Some(a), _) => Right((a.head.kind, a.head.state))
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
              Left(Stop.Refused("a new artifact needs its kind"))
          }
          (kind, state) = placed
          parent <- args.parent.fold(Right(existing.flatMap(_.head.parent)))(
            _.fold(Right(None))(r => refuse(resolve(r)).map(Some(_)))
          )
          cites <- args.cites.fold(Right(existing.fold(Vector.empty)(_.head.cites)))(cs =>
            refuse(Traversal.traverse(cs)(resolve))
          )
          body <- refuse(Body.read(args.body, resolve))
          handle <- handled(method, now, key, existing, args.handle)
          artifact = Artifact(
            ArtifactHead(
              key,
              handle,
              kind,
              state,
              args.title,
              parent,
              cites,
              Some(writer)
            ),
            body
          )
          changed = changing(
            Vector(artifact),
            whose match {
              case Whose.Caller => Set(key)
              case Whose.Prior => Set.empty
            }
          )
          next =
            if (named.isDefined) changed
            else changed.copy(next = ArtifactNumber.after(key.number))
          _ <- refuse(Project.of(project.key, project.pin, next.artifacts))
        } yield (next, s"Kept ${AnswerText.named(artifact.head)}.")
      case Step.Edit(args) =>
        for {
          key <- refuse(resolve(args.key))
          current <- now.artifacts
            .find(_.head.key == key)
            .toRight(Stop.Refused(s"project ${ProjectKey.value(project.key)} has no ${key.value}"))
          _ <- unchanged(key, Some(args.version), project.documents.find(_._2.head.key == key))
          body <- Body
            .edited(current.body, args.edit, resolve)
            .left
            .map(why => Stop.Refused(s"${current.head.called} cannot be edited: $why"))
          kept <- stepped(
            project,
            method,
            now,
            Step.Keep(Keeping.again(key, current.head.title, body, args.version), Whose.Prior),
            writer,
            mover
          )
        } yield kept
      case Step.Move(args) =>
        for {
          _ <- Either.cond(
            method.digest == project.pin.digest,
            (),
            Stop.Refused(Method.changed(project.key, method))
          )
          key <- refuse(resolve(args.key))
          done <- method
            .move(now.artifacts, key, args.to, mover)
            .left
            .map(r => Stop.Refused(refusal(r)))
          moved = Artifact(done.artifact.head.copy(by = Some(writer)), done.artifact.body)
          merged = done.changed.map(c => Artifact(c.head.copy(by = Some(writer)), c.body))
        } yield (
          changing(moved +: merged.toVector),
          s"Moved ${AnswerText.named(moved.head)}." +
            merged.fold("")(c => s" Merged its deltas into ${AnswerText.named(c.head)}.")
        )
    }
  }

  /** Nothing, or why what `steps` made of `project` may not be kept: a brief over its budget
    * ([[within]]), or an artifact changed, or the index when a step minted an id, kept below what
    * this room writes ([[atTheirLabels]], saying `verb`).
    */
  private def settled(
      claims: Claims,
      project: Loaded,
      method: Method,
      key: ProjectKey,
      steps: Stepping,
      at: Instant,
      verb: String
  )(using Tx^): Either[Stop, Unit] =
    for {
      after <- refuse(Project.of(key, project.pin, steps.artifacts))
      live <- stored(claims.live(at))
      _ <- within(
        method,
        Project.of(key, project.pin, project.artifacts).toOption,
        ids(project),
        rendered(method, after, ids(project), live),
        steps.changed.flatMap(k => steps.artifacts.find(_.head.key == k)).map(_.head)
      )
      _ <- atTheirLabels(
        Option
          .when(steps.next != project.next)(project.index -> s"project ${ProjectKey.value(key)}")
          .toVector ++
          project.documents
            .filter((_, a) => steps.changed.contains(a.head.key))
            .map((d, a) => d -> a.head.key.value),
        verb
      )
    } yield ()

  /** Keeps each artifact `steps` changed through `own`, once, as they left it ([[kept]]): wholly
    * its caller's when a step wrote every word of it, else carrying its document's; and the
    * project's index, when a step minted an id, its `next` moved on and who started it kept
    * (`by`, when it names no one), believed at no more than its version before, nor its caller.
    */
  private def wrote(
      own: DocumentKeeper,
      project: Loaded,
      steps: Stepping,
      by: String,
      at: Instant
  )(using
      Tx^
  )(using Locked): Either[Stop, Map[ArtifactKey, DocumentVersionId]] =
    for {
      versions <- Traversal
        .traverse(steps.changed.flatMap(k => steps.artifacts.find(_.head.key == k)))(a =>
          kept(
            own,
            a,
            project.documents.find(_._2.head.key == a.head.key).map(_._1),
            if (steps.callers(a.head.key)) Whose.Caller else Whose.Prior,
            at
          ).map(a.head.key -> _)
        )
      _ <-
        if (steps.next == project.next) Right(())
        else
          for {
            index <- refuse(
              ArtifactJson.index(
                project.key,
                project.pin,
                ArtifactJson.startedBy(project.index).getOrElse(by),
                steps.next
              )
            )
            (key, text, data, refs) = index
            _ <- keptEverywhere(
              own,
              key,
              project.index.label,
              DocumentKeeper.replacing(Some(project.index)),
              text,
              data,
              refs,
              at
            )
          } yield ()
    } yield versions.toMap

  /** Nothing, or a refusal when `read`, the version a keep of `key` was read at, is not that of
    * `current`, its document: the version it is at, and who kept it when; or when `key` has no
    * document, made by a step before it.
    */
  private def unchanged(
      key: ArtifactKey,
      read: Option[DocumentVersionId],
      current: Option[(Document, Artifact)]
  ): Either[Stop, Unit] = (read, current) match {
    case (None, _) => Right(())
    case (Some(_), None) =>
      Left(Stop.Refused(s"${key.value} is new in this batch, so it has no version to give"))
    case (Some(v), Some((d, _))) if d.version == v => Right(())
    case (Some(v), Some((d, a))) =>
      Left(
        Stop.Refused(
          s"${a.head.called} is at version ${DocumentVersionId.value(d.version)}, not " +
            s"${DocumentVersionId.value(v)} as given: kept since" +
            a.head.by.fold("")(b => s" by $b") +
            s" at ${d.written}. Show it again and keep from what it holds now."
        )
      )
  }

  /** What claim `c` holds, named as [[ArtifactHead.called]] names it when it is an artifact of
    * `project`, matched by its document's key; else its subject as stored.
    */
  private def subjectOf(project: Loaded, c: Claim): String =
    project.documents
      .collectFirst { case (d, a) if d.key == c.subject => a.head.called }
      .getOrElse(DocKey.value(c.subject))

  /** That `key`'s project has no artifact `key`. */
  private def lacking(key: ArtifactKey): Stop =
    Stop.Refused(s"project ${ProjectKey.value(key.project)} has no artifact ${key.value}")

  /** Nothing, or a refusal naming each of `changed` that a claim live at `at` holds, at any
    * label, and the holder it is claimed for.
    */
  private def unclaimed(claims: Claims, changed: Vector[(Document, Artifact)], at: Instant)(using
      Tx^
  ): Either[Stop, Unit] =
    stored(claims.live(at)).flatMap { live =>
      claimedOf(live, changed).map((artifact, holder) =>
        s"${artifact.head.called} is claimed for ${ClaimHolder.value(holder)}"
      ) match {
        case Vector() => Right(())
        case Vector(one) => Left(Stop.Refused(s"$one: release it first"))
        case many => Left(Stop.Refused(s"${many.mkString("; ")}: release them first"))
      }
    }

  /** Each of `documents` that a claim of `live` holds, in the order given, with the holder of the
    * first such claim.
    */
  private def claimedOf(
      live: Vector[Claim],
      documents: Vector[(Document, Artifact)]
  ): Vector[(Artifact, ClaimHolder)] =
    documents.flatMap((d, a) => live.find(_.subject == d.key).map(c => a -> c.holder))

  /** What a withdrawal changes, each artifact with its document as loaded: those withdrawn, as
    * they were, and those rewritten, as rewritten.
    */
  private final case class Documented(
      gone: Vector[(Document, Artifact)],
      uncited: Vector[(Document, Artifact)]
  ) extends caps.Pure

  /** The documents of what `done` withdraws and rewrites, as `project` loaded them. */
  private def documented(project: Loaded, done: Project.Withdrawn): Documented = {
    val held = project.documents.map((d, a) => a.head.key -> d).toMap
    Documented(
      done.gone.flatMap(a => held.get(a.head.key).map(_ -> a)),
      done.uncited.flatMap(a => held.get(a.head.key).map(_ -> a))
    )
  }

  /** Each rewritten document of `docs`, with the subject [[atTheirLabels]] names it by. */
  private def citing(docs: Documented): Vector[(Document, String)] =
    docs.uncited.map((d, a) => d -> s"${a.head.key.value} cites what would be withdrawn, but")

  /** Keeps each rewrite of `docs`, written by `by`, at its document's label, then withdraws each
    * document of what goes, at `at`; refused when one was withdrawn meanwhile.
    */
  private def carried(own: DocumentKeeper, docs: Documented, by: CommandBy, at: Instant)(using
      Tx^
  )(using Locked): Either[Stop, Unit] = {
    val writer = Some(author(by))
    for {
      _ <- docs.uncited.foldLeft[Either[Stop, Unit]](Right(())) { case (so, (d, a)) =>
        so.flatMap(_ =>
          kept(own, Artifact(a.head.copy(by = writer), a.body), Some(d), Whose.Prior, at)
            .map(_ => ())
        )
      }
      _ <- docs.gone.foldLeft[Either[Stop, Unit]](Right(())) { case (so, (d, a)) =>
        so.flatMap(_ =>
          stored(own.withdraw(d.key, d.label, at)).flatMap(
            _.toRight(Stop.Refused(s"${a.head.key.value} was withdrawn meanwhile")).map(_ => ())
          )
        )
      }
    } yield ()
  }

  /** The key and label the versions of the artifact `key` names are kept under: its current
    * document's, as [[resolved]] finds it; or, for an artifact of `project` no longer current,
    * the key it is kept under (for an id, the one [[Rekeyed]] gives, else the id; for a handle,
    * the key it writes) and the label its newest version `own` reads is at; or why not, as
    * [[resolved]] refuses, when `own` reads no version of it, or, for an id, the newest holds
    * another id.
    */
  private def recorded(own: DocumentShelf, project: Loaded, key: ArtifactRef)(using
      Tx^
  ): Either[Stop, (DocKey, Label)] =
    resolved(project, key).flatMap(artifact(project, _)) match {
      case Right((document, _)) => Right((document.key, document.label))
      case Left(missing) =>
        for {
          docKey <- key match {
            case ArtifactRef.Id(id) =>
              rekeyed(own, id).flatMap(_.fold(refuse(DocKey.of(id.value)))(Right(_)))
            case named => refuse(DocKey.of(named.value))
          }
          labels <- stored(own.labels(docKey))
          label <- labels.headOption.toRight(missing)
          newest <- stored(own.versions(docKey, label, HistoryMost))
          _ <- key match {
            case ArtifactRef.Id(id)
                if !newest
                  .collectFirst { case DocumentVersion.Holding(d) => readable(d).toOption }
                  .flatten
                  .forall(_.head.key == id) =>
              Left(missing)
            case _ => Right(())
          }
        } yield (docKey, label)
    }

  /** The key [[Rekeyed]] says `id` is kept under, its documents read at every label `own` reads,
    * or why one cannot be read.
    */
  private def rekeyed(own: DocumentShelf, id: ArtifactKey)(using
      Tx^
  ): Either[Stop, Option[DocKey]] =
    for {
      key <- refuse(Rekeyed.key(id))
      labels <- stored(own.labels(key))
      documents <- stored(Traversal.traverse(labels)(own.current(key, _)))
      under <- refuse(Rekeyed.storeKey(id, documents.flatten))
    } yield under

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

  /** The key the brief of the artifact kept under `stored` is kept under: that key and `#brief`. */
  private def briefKey(stored: DocKey): Either[String, DocKey] =
    DocKey.of(s"${DocKey.value(stored)}#brief")

  /** The id of each artifact of `project`, by the key its document is kept under. */
  private def ids(project: Loaded): Map[DocKey, ArtifactKey] =
    project.documents.map((d, a) => d.key -> a.head.key).toMap

  /** Keeps `r`'s brief through `own`, at its claim's label and at `place`, as the brief of the
    * claim's holder ([[DocRef.brief]]), its data naming the holder, believed at no more than the
    * least of what it shows as `project` holds them ([[Brief.shows]]), unless the one current
    * there holds that text for that holder, believed no higher than what it shows, and a write
    * through `own` would not believe it higher: `None` when it is current, else
    * why none is, the one current there withdrawn: it is over [[DocRef.BriefChars]], or the
    * plugin is full.
    */
  private def briefed(own: DocumentKeeper, r: Rendered, place: Place, project: Loaded, at: Instant)(
      using Tx^
  )(using Locked): Either[Stop, Option[String]] = {
    val over =
      f"it is over ${DocRef.BriefChars}%,d characters; shorten the artifact or what its role carries"
    val (label, holder) = (r.claim.label, r.claim.holder)
    val data = ujson.Obj(BriefHolder -> ClaimHolder.value(holder))
    // A shown artifact `project` holds no document of is believed at nothing, so a brief
    // rendered from another project's value fails closed.
    val whole = Tx.top
    val believed = r.brief.fold(
      _ => whole,
      b =>
        Integrity
          .meetAll(
            b.shows.map(k =>
              project.documents.find(_._2.head.key == k).fold(Integrity.Untrusted)(_._1.integrity)
            )
          )
          .getOrElse(whole)
    )
    // What a write through `own` would believe it at: no rewrite only lowers it to its caller.
    val target = own.believing(believed)
    def raises(was: Integrity): Boolean = target.dominates(was) && target != was
    briefKey(r.claim.subject).fold(
      _ => Right(Some(s"No brief was kept: $over.")),
      key =>
        stored(own.current(key, label)).flatMap { was =>
          def none(why: String): Either[Stop, Option[String]] =
            stored(
              was.fold[Either[StoreError, Option[DocumentVersionId]]](Right(None))(_ =>
                own.withdraw(key, label, at)
              )
            ).map(_ => Some(s"No brief was kept: $why."))
          r.brief.left.map(_ => over).flatMap(b => DocText.of(b.text).left.map(_ => over)) match {
            case Left(why) => none(why)
            case Right(text)
                if was.exists(d =>
                  d.text == text && d.data == data && believed.dominates(d.integrity) &&
                    !raises(d.integrity)
                ) =>
              Right(None)
            case Right(text) =>
              own.write(
                key,
                label,
                believed,
                place,
                text,
                data,
                None,
                at,
                Set(DocRef.brief(holder))
              ) match {
                case Right(_) => Right(None)
                case Left(StoreError.Full(bound)) => none(Projects.full(bound))
                case Left(other) => Left(Stop.Store(other))
              }
          }
        }
    )
  }

  /** Withdraws `artifact`'s brief at every label it is current at that the transaction reads:
    * the holder the newest of them was for, `None` when there was none.
    */
  private def unbriefed(own: DocumentKeeper, artifact: DocKey, at: Instant)(using
      Tx^
  )(using Locked): Either[Stop, Option[ClaimHolder]] =
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

  /** Keeps `artifact` through `own` under the key and at the label of `before`, its document, or
    * when new under its id and at public: the version it is now at. Believed at what `own` believes its caller at, met with what
    * `before` was believed at when `whose` is [[Whose.Prior]] and there is one.
    */
  private def kept(
      own: DocumentKeeper,
      artifact: Artifact,
      before: Option[Document],
      whose: Whose,
      at: Instant
  )(using Tx^)(using Locked): Either[Stop, DocumentVersionId] =
    for {
      document <- refuse(
        ArtifactJson
          .write(artifact)
          .left
          .map(why => s"${artifact.head.called} cannot be kept: $why")
      )
      (text, data, refs) = document
      key <- before.fold(refuse(DocKey.of(artifact.head.key.value)))(d => Right(d.key))
      write <- keptEverywhere(
        own,
        key,
        before.fold(Label.Public)(_.label),
        whose match {
          case Whose.Caller => Tx.top
          case Whose.Prior => DocumentKeeper.replacing(before)
        },
        text,
        data,
        refs,
        at
      )
    } yield write match {
      case Written.Unchanged(current, _) => current
      case Written.Versioned(version, _, _) => version
    }

  /** Who `by` is, as an artifact records its writer: the person's [[accounts]], and the turn
    * that wrote for them when one did.
    */
  private def author(by: CommandBy): String = by match {
    case CommandBy.Direct(person, _) => accounts(person)
    case CommandBy.Turn(person, turn) =>
      s"${accounts(person)}, through grit (turn ${TurnSeq.value(turn.turnSeq)} of conversation " +
        s"${ConversationId.value(turn.conversationId)})"
  }

  /** Who `by` moves as ([[Mover]]): a person when a direct caller believed at `Authority` or
    * more, else an agent (a turn, or a caller believed lower).
    */
  private[plugin] def moverOf(by: CommandBy): Mover = by match {
    case CommandBy.Direct(_, believed) if believed.dominates(Integrity.Authority) => Mover.Person
    case _ => Mover.Agent
  }

  /** Who [[moverOf]] moves as a person, in the words of every refusal that names the rule. */
  private[plugin] val PersonOnlyBy: String =
    "a caller believed at Authority giving the command directly, never through a turn or by a " +
      "session believed lower"

  /** Who `person` is, as a claim or a writer names them: each of their accounts, sorted. */
  private def accounts(person: Principal.Person): String = {
    val held = person.accounts.toVector.map(h => Account.written(h.account)).sorted
    if (held.isEmpty) PrincipalId.value(person.id) else held.mkString(", ")
  }

  /** Why a move was refused, in words its mover can act on. */
  private[plugin] def refusal(r: MoveRefusal): String = r match {
    case MoveRefusal.Missing(key) =>
      s"project ${ProjectKey.value(key.project)} has no artifact ${key.value}"
    case MoveRefusal.NoTransition(head, to, may) =>
      s"${AnswerText.named(head)} cannot move to ${StateName.value(to)}; " +
        (if (may.isEmpty) "nothing moves it on"
         else s"it may move to ${may.map(StateName.value).mkString(", ")}")
    case MoveRefusal.Unheld(head, to, failed) =>
      s"${head.called} cannot move to ${StateName.value(to)}: " +
        failed.map((name, why) => s"${CheckName.value(name)}: $why").mkString("; ")
    case MoveRefusal.NoInto(head, to, wanted) =>
      s"${head.called} cannot move to ${StateName.value(to)}: that merges its deltas into the " +
        s"one ${KindName.value(wanted)} it cites, and it cites none; cite the one to merge them into"
    case MoveRefusal.IntoSeveral(head, to, wanted, cited) =>
      s"${head.called} cannot move to ${StateName.value(to)}: that merges its deltas into the " +
        s"one ${KindName.value(wanted)} it cites, and it cites ${cited.map(_.value).mkString(", ")}; " +
        "cite only the one to merge them into"
    case MoveRefusal.Unmerged(head, to, into, why) =>
      s"${head.called} cannot move to ${StateName.value(to)}: its deltas merged into " +
        s"${into.key.value} would be too long ($why)"
    case MoveRefusal.PersonOnly(head, to) =>
      s"${head.called} cannot move to ${StateName.value(to)} here: that move is made only by " +
        s"$PersonOnlyBy; ask the person to make it"
  }

  private[plugin] def parsed[A](
      field: String,
      value: String,
      read: String => Either[String, A]
  ): Either[ArgsError, A] =
    read(value).left.map(ArgsError.Refused(field, _))
}
