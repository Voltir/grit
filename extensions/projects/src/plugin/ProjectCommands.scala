package grit.projects.plugin

import java.time.{Instant, ZoneOffset}

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
import grit.projects.artifact.{Artifact, ArtifactHead, ArtifactJson, Body, Pin, Project}
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
  KindName,
  MethodName,
  ProjectKey,
  RoleName,
  StateName
}
import grit.projects.render.{AnswerText, Brief, BriefRefusal, ProjectMarkdown}
import grit.prose.words.Words
import grit.std.either.Traversal

/** The projects plugin's commands over the methods declared in its store, their checks found
  * among `checks`, new projects under the latest version of `default`, offered to turns as
  * `turns` says.
  */
private[plugin] final class ProjectCommands(
    checks: Vector[TransitionCheck],
    default: MethodName,
    turns: TurnCommands,
    methodsAt: Label,
    commandLine: Option[String]
) extends caps.Pure {

  private val declaring = new MethodCommands(checks, methodsAt)

  /** What a person runs to give `project_claim` themselves, as a brief names it. */
  private def claimCommand: String = {
    val name = ToolName.value(ProjectClaim.spec.name)
    commandLine.fold(name)(c => s"$c $name")
  }

  import ProjectArgs.*
  import ProjectBriefs.*
  import ProjectCovers.*
  import ProjectRekey.*
  import ProjectCommands.*
  import ProjectSteps.*
  import Stop.{answered, refuse, stored}

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
      Reorder,
      Split,
      Batch,
      Show,
      Stored,
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
        alone(own, claims, args.project, by, at, "keep")(Step.Keep(args))
          .map(done => done.version.fold(done.said)(AnswerText.nowAt(done.said, _)))
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
        done <- alone(own, claims, key.project, by, at, "edit")(Step.Edit(args))
        // An edit that changes nothing writes nothing, and the version given is current.
        now = done.version.getOrElse(args.version)
      } yield AnswerText.nowAt(done.said, now)
    }
  }

  /** `project_reorder`. */
  object Reorder extends KeepingCommand[Reordering] {
    override def offeredToTurns: Boolean = shipped
    override def shown(args: Reordering): String = args.artifact.value

    val spec: ToolSpec[Reordering] = ToolSpec(
      ToolName("project_reorder"),
      "Moves an artifact among its siblings, the artifacts under the same parent (or, with " +
        "none, those under none), in the order project_list with a parent lists them; ended " +
        "siblings count too, though project_list leaves them out. Give after or before, not " +
        "both: the sibling it is to sit right after, or right before. Nothing else about it " +
        "changes. Answers where it now sits; when no room is left there, its siblings are " +
        "re-ranked too, one version each, and the answer says how many. Refused for a key the " +
        "project lacks, naming the nearest it has, and for a sibling that is the artifact " +
        "itself or sits under another parent.",
      Args
        .of(
          (
            artifact = Field.text("The artifact's key, `<project>/<artifact>`."),
            after = Field.text("The sibling it is to sit right after.").optional,
            before = Field.text("The sibling it is to sit right before.").optional
          )
        )
        .refine(a =>
          for {
            artifact <- parsed("artifact", a.artifact, ArtifactRef.read)
            beside <- (a.after, a.before) match {
              case (Some(s), None) => parsed("after", s, ArtifactRef.read).map(Beside.After(_))
              case (None, Some(s)) => parsed("before", s, ArtifactRef.read).map(Beside.Before(_))
              case (Some(_), Some(_)) =>
                Left(ArgsError.Refused("before", "Give after or before, not both."))
              case (None, None) =>
                Left(
                  ArgsError
                    .Refused("after", "Give after or before: the sibling it is to sit beside.")
                )
            }
          } yield Reordering(artifact, beside)
        )
    )

    def run(
        args: Reordering,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] =
      answered(
        alone(own, claims, args.artifact.project, by, at, "reorder")(Step.Reorder(args))
          .map(_.said)
      )
  }

  /** `project_split`: an artifact's `##` sections made artifacts of their own under it. */
  object Split extends KeepingCommand[Splitting] {
    override def offeredToTurns: Boolean = shipped
    override def shown(args: Splitting): String = args.artifact.value

    val spec: ToolSpec[Splitting] = ToolSpec(
      ToolName("project_split"),
      "Makes each `##` section of an artifact an artifact of its own under it, of kind " +
        "section, in the order of their headings: its title the heading, its body what follows " +
        "the heading, any `###` headings as written. The artifact keeps what sits above its " +
        "first `##` heading, then a list linking each new artifact by its heading, then the " +
        "sections that stay: those its kind must hold, and each `Delta: ` section. Use it when " +
        "an artifact is too large to keep whole. Give the version project_show shows. Answers " +
        "with each new artifact's key and the version the artifact is kept at. Refused, keeping " +
        "nothing: for a version that is not the artifact's current one, naming the one it is " +
        "at; for a body with no section to move, or with a `#` heading below its first `##` " +
        "one; for two sections sharing a heading, a blank heading, or a section holding " +
        "nothing; for a kind whose sections another kind's deltas are merged into; for a " +
        "method with no kind section, or one whose sections cannot sit under the artifact; and " +
        "as project_keep refuses.",
      Args
        .of(
          (
            artifact = Field.text("The artifact's key, `<project>/<artifact>`."),
            version = Field.integer(EditMeaning.version, 1, Long.MaxValue)
          )
        )
        .refine(a =>
          for {
            artifact <- parsed("artifact", a.artifact, ArtifactRef.read)
            version <- versionOf(a.version)
          } yield Splitting(artifact, version)
        )
    )

    def run(
        args: Splitting,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered(
      alone(own, claims, args.artifact.project, by, at, "split")(Step.Split(args))
        .map(done => done.version.fold(done.said)(AnswerText.nowAt(done.said, _)))
    )
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
        alone(own, claims, args.key.project, by, at, "move")(Step.Move(args))
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
        alone(own, claims, args.project, by, at, "propose")(Step.Keep(args))
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
      changed(own, claims, args.project, by, at, "change it", args.steps, args.dry).flatMap {
        done =>
          def each(answers: Vector[Either[String, String]]): String =
            answers.zipWithIndex
              .map {
                case (Right(answer), i) => s"${i + 1}. $answer"
                case (Left(why), i) => s"${i + 1}. Refused: $why"
              }
              .mkString("\n")
          def wouldRefuse(lines: String): String =
            s"Dry run, nothing kept; the batch would be refused. Each step would answer:\n$lines"
          done match {
            case Changed.Refused(answers) if args.dry =>
              Left(Stop.Refused(wouldRefuse(each(answers.map(_.map(_.said))))))
            case Changed.Refused(answers) =>
              Left(
                Stop.Refused(
                  answers.zipWithIndex
                    .collectFirst { case (Left(why), i) =>
                      s"Nothing was kept: step ${i + 1} of ${args.steps.size} " +
                        s"(${described(args.steps, i)}) was refused: $why"
                    }
                    .getOrElse("Nothing was kept.")
                )
              )
            case Changed.Unsettled(answers, why) if args.dry =>
              Left(
                Stop.Refused(
                  s"${wouldRefuse(each(answers.map(a => Right(a.said))))}\n" +
                    s"Once every step is done, refused: $why"
                )
              )
            case Changed.Unsettled(_, why) => Left(Stop.Refused(s"Nothing was kept: $why"))
            case Changed.Done(answers, _) if args.dry =>
              Right(
                s"Dry run, nothing kept. Each step would answer:\n" +
                  each(answers.map(a => Right(a.said)))
              )
            case Changed.Done(answers, _) =>
              Right(s"Every step is done:\n${each(answers.map(a => Right(a.said)))}")
          }
      }
    }

    /** Step `i` of `steps` in a few words: `keep <key>`, `edit <key>`, `move <key> to <state>`,
      * `reorder <key>`, or `split <key>`.
      */
    private def described(steps: Vector[Step], i: Int): String =
      steps.lift(i).fold("") {
        case Step.Keep(k, _) => s"keep ${Keeping.shown(k)}"
        case Step.Edit(e) => s"edit ${e.key.value}"
        case Step.Move(m) => s"move ${m.key.value} to ${StateName.value(m.to)}"
        case Step.Reorder(r) => s"reorder ${r.artifact.value}"
        case Step.Split(s) => s"split ${s.artifact.value}"
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
              newest <- stored(own.documents.current(doc, label))
              readable <- versionsRead(own.documents, project, (newest.toVector :+ document))
              version <- refuse(readable(document))
            } yield (document, version)
        }
      def shown(key: ArtifactRef)(kept: (Document, Artifact)): Either[Stop, String] =
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
          loaded(own.documents, key.project).flatMap(version(_, key)).flatMap(shown(key))
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
                  .flatMap(shown(key)) match {
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

  /** `project_stored`. */
  object Stored extends ReadingCommand[Releasing] {
    override def shown(args: Releasing): String = args.key.value

    val spec: ToolSpec[Releasing] = ToolSpec(
      ToolName("project_stored"),
      "Answers the key an artifact is stored under, which a wait on it watches: a re-keyed " +
        "artifact's key from before its id, any other's id. Keeps nothing. Refused as " +
        "project_show refuses the key.",
      Args
        .of((key = Field.text("The artifact's id or handle, `<project>/<id or handle>`.")))
        .refine(a => parsed("key", a.key, ArtifactRef.read).map(Releasing(_)))
    )

    def run(args: Releasing, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      for {
        project <- loaded(own.documents, args.key.project)
        where <- recorded(own.documents, project, args.key)
      } yield DocKey.value(where._1)
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
        text <- keptVersions(own.documents, project, where._1, where._2).flatMap {
          (kept: Vector[DocumentVersion], readable: Document -> Either[String, Artifact]) =>
            args.diff match {
              case None =>
                refuse(Traversal.sequence(kept.map(row(readable))))
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
                      case DocumentVersion.Holding(d) if DocumentVersionId.value(d.version) == n =>
                        d
                    }
                    .toRight(
                      Stop.Refused(
                        s"$id has no version $n among its $HistoryMost newest kept"
                      )
                    )
                    .flatMap(d => refuse(readable(d)))
                    .map(ProjectMarkdown.text)
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
      s"Lists a project's artifacts, one line each, at most $ListMost: most recently changed " +
        "first, or, with a parent, in the order project_reorder sets among siblings, ties " +
        "oldest first. Leaves out ended ones ($EndedWords) unless `ended` is true or `state` " +
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
        // What sits `levels` levels under `from`, in rank order, each followed by what sits
        // under it.
        def walk(from: ArtifactKey, levels: Int): Vector[(Document, Artifact)] =
          if (levels < 1) Vector.empty
          else
            children
              .getOrElse(from, Vector.empty)
              .sortBy(_._2.head)(using ArtifactHead.ranked)
              .flatMap((d: Document, a: Artifact) => (d, a) +: walk(a.head.key, levels - 1))
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
        s"most $Found, one line each: only one project's, when given. A line ends with " +
        "`§ <heading>`, naming the artifact's `##` section that holds the most of the words, " +
        "spelled as given, when it holds more than what sits above its first `##` heading: " +
        "project_show gives that section alone. Leaves out ended " +
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
        _ <- documents
          .find(ArtifactJson.isOldForm)
          .flatMap(d => ProjectKey.of(DocKey.value(d.key).takeWhile(_ != '/')).toOption)
          .fold(Right(()))(p =>
            indexOf(own.documents, p).flatMap(index =>
              Left(
                Stop.Refused(
                  index
                    .filter(ArtifactJson.nextOf(_).exists(_.isDefined))
                    .fold(OldForm(p))(i =>
                      LeftOver(
                        p,
                        i,
                        documents.filter(d =>
                          ArtifactJson.isOldForm(d) &&
                            DocKey.value(d.key).startsWith(s"${ProjectKey.value(p)}/")
                        )
                      )
                    )
                )
              )
            )
          )
        artifacts <- refuse(Traversal.sequence(documents.map(readable)))
        among = artifacts.filter(a => args.project.forall(_ == a.head.key.project))
        heads = among.map(_.head)
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
        "Nothing matches.",
        h => among.find(_.head.key == h.key).flatMap(a => Body.matching(a.body, args.query))
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

  /** `project_rekey`. */
  object Rekey extends KeepingCommand[Rekeying] {
    val spec: ToolSpec[Rekeying] = ToolSpec(
      ToolName("project_rekey"),
      "Gives each artifact of a project of the old form, whose artifacts are keyed by name, " +
        "an id, numbered in the order the artifacts were made; its name becomes its handle " +
        s"(`${grit.projects.rekey.Rekey.HandlePrefix}<name>` for a name starting with a digit, " +
        "which its old key then no longer names), and every reference to it now names its id. " +
        "One transaction: refused, it keeps nothing, naming each artifact in the way, such as " +
        "one citing an artifact you cannot read. With `dry`, it counts what it would do and " +
        "keeps nothing; with `dry` and no project, it lists every project left to re-key. " +
        s"Only for $PersonOnlyBy.",
      Args
        .of(
          (
            project = Field.text("The project's key.").optional,
            dry = Field.flag("Count what it would do, keeping nothing.").optional
          )
        )
        .refine(a =>
          (a.project, a.dry.getOrElse(false)) match {
            case (Some(p), dry) => parsed("project", p, ProjectKey.of).map(Rekeying.One(_, dry))
            case (None, true) => Right(Rekeying.Survey)
            case (None, false) =>
              Left(
                ArgsError.Refused(
                  "project",
                  "Name the project to re-key, or give dry: true to list every project left " +
                    "to re-key."
                )
              )
          }
        )
    )
    def run(
        args: Rekeying,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      moverOf(by) match {
        case Mover.Other =>
          Left(Stop.Refused(s"A project is re-keyed only by $PersonOnlyBy."))
        case Mover.Person =>
          args match {
            case Rekeying.Survey => surveyed(own)
            case Rekeying.One(project, true) =>
              rekeyPlanned(own, project).map(rekeyAnswer(project, _, dry = true))
            case Rekeying.One(project, false) =>
              locked(claims, project).flatMap { lock =>
                given Locked = lock
                for {
                  found <- indexed(own, project)
                  _ <- atTheirLabels(
                    Vector(found._1 -> s"project ${ProjectKey.value(project)}"),
                    "re-key"
                  )
                  planned <- rekeyPlanned(own, project)
                  answer <- rekeyed(own, project, planned, by, at)
                } yield answer
              }
          }
      }
    }
  }

  /** `project` as files ([[ProjectMarkdown.render]]), or, in the old form, as it was exported
    * then ([[grit.projects.rekey.OldForm.exported]]); or why not.
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
            documents <- projectDocuments(own, project)
            files <- refuse(
              grit.projects.rekey.OldForm.exported(
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

    /** The refusal of a claim given both `key` and `oldest`, or neither, as `said`. */
    private def bothOrNeither(said: String): String =
      s"$said: give one; `key` claims that artifact, `oldest` (`project`, `kind`, `state`) the " +
        "unclaimed one of that kind in that state that entered it first"

    /** `oldest`'s fields. */
    private val oldestArgs = Args.of(
      (
        project = Field.text("The project's key."),
        kind = Field.text("A kind its project's method declares."),
        state = Field.text("A state of that kind.")
      )
    )

    val spec: ToolSpec[Claiming] = ToolSpec(
      ToolName("project_claim"),
      "Claims an artifact for a session or agent to work in a role, until project_release " +
        "frees it; the artifact itself is not changed. Give `key`, or `oldest` (`project`, " +
        "`kind`, `state`): of that kind's artifacts in that state that no one holds, the one " +
        "that entered it first; its answer names the artifact claimed. " +
        "Keeps the artifact's brief for what it " +
        "is claimed for, which that session is shown when its view is next rebuilt. Answers a " +
        "JSON object: `claimed`, a sentence saying what was claimed; `brief`, `kept` or " +
        "`none`; and with `none`, `note`, saying why. Refused, naming who holds it, when it " +
        "is claimed already; refused, naming the artifact, when what it is claimed for works " +
        "another; refused for a role, a kind or a state the project's method lacks; refused " +
        "for both or neither of `key` and `oldest`; and refused, saying there is nothing to " +
        "take, when `oldest` finds none. A role the method says " +
        "covers claims everything under the artifact too: such a claim is refused, naming " +
        "the other, when another session or agent holds one at, above or under it in that " +
        "role or another that covers. A claim kept where the asker cannot read it " +
        "(at another label) is not seen, so it refuses nothing.",
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`.").optional,
            oldest = Field
              .one(
                "In place of `key`: of the artifacts of `kind` in `state` that no one holds, " +
                  "the one that entered it first.",
                oldestArgs
              )
              .optional,
            role = Field.text("The role working on it, one its project's method declares."),
            holder = Field.text(
              "Who works it: the session, or the agent type in it, whose brief is written, " +
                s"as its edge names it; one line of at most ${ClaimHolder.Longest} characters."
            )
          )
        )
        .refine(a =>
          for {
            target <- (a.key, a.oldest) match {
              case (Some(key), None) =>
                parsed("key", key, ArtifactRef.read).map(Claiming.Target.Key(_))
              case (None, Some(o)) =>
                for {
                  project <- parsed("oldest.project", o.project, ProjectKey.of)
                  kind <- parsed("oldest.kind", o.kind, KindName.of)
                  state <- parsed("oldest.state", o.state, StateName.of)
                } yield Claiming.Target.Oldest(project, kind, state)
              case (Some(_), Some(_)) =>
                Left(ArgsError.Refused("key", bothOrNeither("`key` and `oldest` are both given")))
              case (None, None) =>
                Left(
                  ArgsError.Refused("key", bothOrNeither("neither `key` nor `oldest` is given"))
                )
            }
            role <- parsed("role", a.role, RoleName.of)
            holder <- parsed("holder", a.holder, ClaimHolder.of)
          } yield Claiming(target, role, holder)
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
      locked(claims, args.target.project).flatMap { lock =>
        given Locked = lock
        for {
          project <- loaded(own, args.target.project)
          method <- declared(own, project.pin)
          _ <- viewOf(method, args.role)
          whole <- refuse(Project.of(args.target.project, project.pin, project.artifacts))
          live <- stored(claims.live(at))
          key <- args.target match {
            case Claiming.Target.Key(ref) => resolved(project, ref)
            case Claiming.Target.Oldest(p, kind, state) =>
              for {
                _ <- stateOf(method, kind, state)
                dated <- Traversal.traverse(
                  project.documents.filter((_, a) => a.head.kind == kind && a.head.state == state)
                ) { (d, a) =>
                  keptVersions(own, project, d.key, d.label).map(
                    (kept: Vector[DocumentVersion], read: Document -> Either[String, Artifact]) =>
                      a.head.key -> entered(kept, read, state).getOrElse(d.written)
                  )
                }
                oldest <- oldestFirst(
                  method,
                  whole,
                  ids(project),
                  live,
                  args.role,
                  args.holder,
                  dated
                ).headOption
                  .toRight(
                    Stop.Refused(
                      s"nothing to take: no ${KindName.value(kind)} in ${StateName.value(state)} " +
                        s"of ${ProjectKey.value(p)} is unclaimed"
                    )
                  )
              } yield oldest
          }
          found <- artifact(project, key)
          (document, a) = found
          _ <- uncovered(method, whole, ids(project), live, key, args.role, args.holder)
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
                    s"$holder works another artifact already; ${releaseFirst("it")}"
                  )(c =>
                    s"$holder works ${subjectOf(project, c)} already, claimed since ${c.made}; " +
                      s"it lapses at ${c.lapses} unless they keep working; ${releaseFirst("it")}"
                  )
                )
              )
            case Claimed.HeldBy(c) =>
              Left(
                Stop.Refused(
                  s"${a.head.called} is claimed already as ${ClaimRole.value(c.role)} for " +
                    ClaimHolder.value(c.holder) +
                    c.session.fold("")(s => s", ${Claims.working(c, s)}") +
                    ". Nothing was changed."
                )
              )
            case Claimed.Elsewhere(s, other) =>
              Left(
                Stop.Refused(
                  s"session ${s.written} works as ${ClaimHolder.value(other)}, not $holder"
                )
              )
          }
          // The role has a view and the artifact is the project's, so the claim has its brief.
          unkept <- rendered(method, whole, ids(project), Vector(made)).headOption
            .toRight(Stop.Refused(s"${a.head.called} has no brief as ${RoleName.value(args.role)}"))
            .flatMap(briefed(own, _, document.place, project, at, claimCommand))
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
          held <- heldClaims(claims, project, at)
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
          held <- if (args.dry) Right(Vector.empty) else heldClaims(claims, project, at)
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
          _ <- unclaimed(claims, project.documents, at)
          renamed = done.renamed.toSet
          _ <- atTheirLabels(
            (project.index -> s"project $p") +: project.documents.collect {
              case (d, a) if renamed(a.head.key) => d -> a.head.key.value
            },
            "migrate"
          )
          writer = Some(author(by))
          before = project.documents.map((d, a) => a.head.key -> d).toMap
          _ <- Traversal
            .traverse(done.project.artifacts.filter(a => renamed(a.head.key)))(a =>
              kept(
                own,
                Artifact(a.head.copy(by = writer), a.body),
                before.get(a.head.key),
                before.get(a.head.key),
                at
              )
            )
            .map(_ => ())
          _ <- rewroteIndex(
            own,
            args.project,
            project.index,
            done.project.pin,
            project.next,
            author(by),
            at
          )
        } yield AnswerText.migrated(args.project, done.project.pin, done.renamed, ListMost)
      }
    }
  }

  /** `project_claims`: the live claims, one project's when given. */
  // A keeping command only for its plugin's claims, which a reading command is not handed; it
  // keeps nothing.
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

  /** `step` taken alone over project `key`, as `project_keep` or `project_move` takes it, saying
    * `verb` of an artifact kept below this room: its answer and the version the artifact it names
    * is now at, none when it wrote that one no new version; or why it was refused.
    */
  private def alone(
      own: DocumentKeeper,
      claims: Claims,
      key: ProjectKey,
      by: CommandBy,
      at: Instant,
      verb: String
  )(step: Step)(using
      Tx^
  ): Either[Stop, (said: String, version: Option[DocumentVersionId])] =
    changed(own, claims, key, by, at, verb, Vector(step), dry = false).flatMap {
      case Changed.Refused(answers) =>
        Left(
          Stop.Refused(
            answers.collectFirst { case Left(why) => why }.getOrElse("nothing was asked")
          )
        )
      case Changed.Unsettled(_, why) => Left(Stop.Refused(why))
      case Changed.Done(answers, versions) =>
        answers.headOption
          .map(a => (said = a.said, version = versions.get(a.artifact)))
          .toRight(Stop.Refused("nothing was asked"))
    }

  /** `steps` taken over project `key` as loaded, each seeing what those before it did, written
    * and moved as `by`, saying `verb` of an artifact kept below this room ([[settled]]); kept
    * unless `dry`, or when a step or the settling refuses. Refused for what the store refuses.
    */
  private def changed(
      own: DocumentKeeper,
      claims: Claims,
      key: ProjectKey,
      by: CommandBy,
      at: Instant,
      verb: String,
      steps: Vector[Step],
      dry: Boolean
  )(using Tx^): Either[Stop, Changed] =
    locked(claims, key).flatMap { lock =>
      given Locked = lock
      for {
        project <- loaded(own, key)
        held <- if (dry) Right(Vector.empty) else heldClaims(claims, project, at)
        method <- declared(own, project.pin)
        plan = planned(project, method, steps, author(by), moverOf(by), onward = dry)
        (said, stepping) = plan
        answers <- Traversal.traverse(said) {
          case Right(answer) => Right(Right(answer))
          case Left(Stop.Refused(why)) => Right(Left(why))
          case Left(failed) => Left(failed)
        }
        done <-
          if (answers.exists(_.isLeft)) Right(Changed.Refused(answers))
          else {
            val all = answers.collect { case Right(a) => a }
            settled(claims, project, method, key, stepping, at, verb) match {
              case Left(Stop.Refused(why)) => Right(Changed.Unsettled(all, why))
              case Left(failed) => Left(failed)
              case Right(_) if dry => Right(Changed.Done(all, Map.empty))
              case Right(_) =>
                for {
                  versions <- wrote(own, project, stepping, author(by), at)
                  _ <- rebriefed(own, held, key, at)
                } yield Changed.Done(all, versions)
            }
          }
      } yield done
    }

  /** Each claim live at `at` on an artifact of `project`, matched by the key its document is
    * kept under, and kept at this transaction's floor, as held ([[Claims.held]]) until it ends,
    * in the order [[Claims.live]] gives. A claim gone or made anew since it was read is left out.
    * A claim kept below the floor is passed over: a command changes only artifacts kept at its
    * floor ([[atTheirLabels]]), so nothing that brief shows changes.
    */
  private def heldClaims(claims: Claims, project: Loaded, at: Instant)(using
      tx: Tx^
  )(using Locked): Either[Stop, Vector[Claim]] =
    for {
      live <- stored(claims.live(at))
      keys = ids(project)
      here = live.filter(c => c.label == Tx.floor(tx) && keys.contains(c.subject))
      // Held, so the session sweep, which takes no project's lock, waits for this transaction
      // before it deletes one.
      held <- Traversal.traverse(here)(c => stored(claims.held(c)))
    } yield held.flatten

  /** Keeps again ([[briefed]]) the brief of each of `held` ([[heldClaims]]), claims on artifacts of
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
        .traverse(briefs)((r, place, p) => briefed(own, r, place, p, at, claimCommand))
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
  import ProjectArgs.*
  import ProjectBriefs.*
  import ProjectCovers.*
  import ProjectRekey.*
  import ProjectSteps.*
  import Stop.{refuse, stored}

  /** `version` as [[AnswerText.versions]] lists it, read by `readable`, or why it does not read. */
  private[plugin] def row(readable: Document -> Either[String, Artifact])(
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

  /** 5: the most artifacts [[LeftOver]] names. */
  private[plugin] val LeftOverMost: Int = 5

  /** Why project `project`, re-keyed, is refused a command to a reader of `left`, artifacts of
    * the old form labelled above its `index`, which no re-key can number without writing down:
    * the first [[LeftOverMost]] of them named with their labels, and the room that works.
    */
  private[plugin] def LeftOver(
      project: ProjectKey,
      index: Document,
      left: Vector[Document]
  ): String = {
    val named = left
      .take(LeftOverMost)
      .map(d => s"${DocKey.value(d.key)} (kept ${Label.shown(d.label)})")
      .mkString(", ")
    val more = if (left.size > LeftOverMost) s" and ${left.size - LeftOverMost} more" else ""
    val it = if (left.size == 1) "it" else "them"
    s"project ${ProjectKey.value(project)} was re-keyed, but " +
      s"${Words.counted(left.size, "artifact")} of the old form " +
      s"${if (left.size == 1) "is" else "are"} kept above its index, where the re-key could " +
      s"not read $it: $named$more. No command can number $it, since that would write the " +
      s"index from above it. Use the project from a room at its index's label " +
      s"(${Label.shown(index.label)}), which does not read $it."
  }

  /** Why project `project`, in the old form (ADR 0063), is refused a command. */
  private[plugin] def OldForm(project: ProjectKey): String =
    s"project ${ProjectKey.value(project)} is in the old form, its artifacts keyed by name: " +
      "re-key it with project_rekey first"

  /** A project as its documents hold it: its key, its index, its pin, the number its next new
    * artifact takes, and each artifact with its document.
    */
  private[plugin] final case class Loaded(
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
  private[plugin] final class Locked private[ProjectCommands] () extends caps.Pure

  /** Holds `project`'s lock ([[Claims.lock]] of `<project>#lock`) until this transaction ends:
    * of two commands on one project, from rooms at any labels, the second waits here for the
    * first to end, then reads what it kept.
    */
  private[plugin] def locked(claims: Claims, project: ProjectKey)(using Tx^): Either[Stop, Locked] =
    for {
      key <- refuse(DocKey.of(s"${ProjectKey.value(project)}#lock"))
      _ <- stored(claims.lock(key))
    } yield new Locked

  /** As [[stored]], a write refused past the bound refused in words. */
  private[plugin] def written[A](e: Either[StoreError, A]): Either[Stop, A] = e.left.map {
    case StoreError.Full(bound) => Stop.Refused(Projects.full(bound))
    case other => Stop.Store(other)
  }

  /** 20: the most files one page of `project_export` holds. */
  private[plugin] val ExportPage: Int = 20

  /** 20: the most artifacts `project_find` answers. */
  private[plugin] val Found: Int = 20

  /** 100: how many best matches `project_find` reads, before leaving out another project's and
    * ended ones, to answer [[Found]] of.
    */
  private[plugin] val FoundAmong: Int = 100

  /** `project` and its artifacts as `own` holds them, or why not: among them, that it is in the
    * old form ([[OldForm]]).
    */
  private[plugin] def loaded(own: DocumentShelf, project: ProjectKey)(using
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
      documents <- projectDocuments(own, project)
      _ <-
        if (documents.exists(ArtifactJson.isOldForm))
          Left(Stop.Refused(LeftOver(project, index, documents.filter(ArtifactJson.isOldForm))))
        else Right(())
      artifacts <- refuse(Traversal.sequence(documents.map(d => readable(d).map(d -> _))))
    } yield Loaded(project, index, pin, next, artifacts)
  }

  /** What `role` is shown under `method`, or a refusal naming the roles it declares. */
  private[plugin] def viewOf(method: Method, role: RoleName): Either[Stop, RoleView] =
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
  private[plugin] def readable(document: Document): Either[String, Artifact] =
    Artifact.of(document).left.map(why => s"${DocKey.value(document.key)} cannot be read: $why")

  /** The pin `project`'s index holds as `own` holds it, or why not. */
  private[plugin] def pinned(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Pin] =
    indexed(own, project).map(_._2)

  /** `project`'s index document as `own` holds it, if it is started. */
  private[plugin] def indexOf(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Option[Document]] =
    for {
      indexing <- refuse(ArtifactJson.indexing(project))
      indexes <- stored(own.referring(indexing, 1))
    } yield indexes.headOption

  /** As [[written]], `text`, `data` and `refs` kept through `own` under `key` at `label`, at
    * every place and with no pointer, believed at `believed` as `own` believes it.
    */
  private[plugin] def keptEverywhere(
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

  /** Project `project`'s index, `index`, kept again holding `pin` and `next`: who started the
    * project kept (`by`, when it names no one), believed at no more than `index` was.
    */
  private[plugin] def rewroteIndex(
      own: DocumentKeeper,
      project: ProjectKey,
      index: Document,
      pin: Pin,
      next: ArtifactNumber,
      by: String,
      at: Instant
  )(using Tx^)(using Locked): Either[Stop, Unit] =
    for {
      rewritten <- refuse(
        ArtifactJson.index(project, pin, ArtifactJson.startedBy(index).getOrElse(by), next)
      )
      (key, text, data, refs) = rewritten
      _ <- keptEverywhere(
        own,
        key,
        index.label,
        DocumentKeeper.replacing(Some(index)),
        text,
        data,
        refs,
        at
      )
    } yield ()

  /** The documents `own` reads that hold an artifact of `project`, of either form, at most
    * [[Projects.Terms.bound]] of them; or why not.
    */
  private[plugin] def projectDocuments(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Vector[Document]] =
    refuse(ArtifactJson.holding(project)).flatMap(holding =>
      stored(own.referring(holding, Projects.Terms.bound))
    )

  /** `project`'s index document as `own` holds it, and the pin it holds, or why not. */
  private[plugin] def indexed(own: DocumentShelf, project: ProjectKey)(using
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
  private[plugin] def artifact(
      project: Loaded,
      key: ArtifactKey
  ): Either[Stop, (Document, Artifact)] =
    project.documents
      .find(_._2.head.key == key)
      .toRight(lacking(key))

  /** [[resolver]] of `ref` over `project` as loaded. */
  private[plugin] def resolved(project: Loaded, ref: ArtifactRef): Either[Stop, ArtifactKey] =
    refuse(resolver(project, project.artifacts)(ref))

  /** What [[changed]] made of its steps, each answered with the artifact it names. */
  private[plugin] enum Changed extends caps.Pure {

    /** A step was refused, each step's answer or refusal given; nothing was kept. */
    case Refused(answers: Vector[Either[String, (said: String, artifact: ArtifactKey)]])

    /** Every step was taken, and what they made together was refused, why given; nothing was
      * kept.
      */
    case Unsettled(answers: Vector[(said: String, artifact: ArtifactKey)], why: String)

    /** Every step was taken, and kept at `versions` (none when dry). */
    case Done(
        answers: Vector[(said: String, artifact: ArtifactKey)],
        versions: Map[ArtifactKey, DocumentVersionId]
    )
  }

  /** Nothing, or why what `steps` made of `project` may not be kept: a brief over its budget
    * ([[within]]), or an artifact changed, or the index when a step minted an id, kept below what
    * this room writes ([[atTheirLabels]], saying `verb`).
    */
  private[plugin] def settled(
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
      before = Project.of(key, project.pin, project.artifacts).toOption
      _ <- stillUncovered(method, before, after, ids(project), live)
      _ <- within(
        method,
        before,
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

  /** Keeps each artifact `steps` changed through `own`, once, as they left it ([[kept]]), carrying
    * the words [[Stepping.whose]] says it holds; and the project's index, when a step minted an
    * id, its `next` moved on ([[rewroteIndex]]).
    */
  private[plugin] def wrote(
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
        .traverse(steps.changed.flatMap(k => steps.artifacts.find(_.head.key == k))) { a =>
          val before = project.documents.find(_._2.head.key == a.head.key).map(_._1)
          kept(
            own,
            a,
            before,
            steps.whose.get(a.head.key) match {
              case Some(Whose.Caller) => None
              case Some(Whose.Part(of)) => project.documents.find(_._2.head.key == of).map(_._1)
              case Some(Whose.Prior) | None => before
            },
            at
          ).map(a.head.key -> _)
        }
      _ <-
        if (steps.next == project.next) Right(())
        else
          rewroteIndex(own, project.key, project.index, project.pin, steps.next, by, at)
    } yield versions.toMap

  /** What a withdrawal changes, each artifact with its document as loaded: those withdrawn, as
    * they were, and those rewritten, as rewritten.
    */
  private[plugin] final case class Documented(
      gone: Vector[(Document, Artifact)],
      uncited: Vector[(Document, Artifact)]
  ) extends caps.Pure

  /** The documents of what `done` withdraws and rewrites, as `project` loaded them. */
  private[plugin] def documented(project: Loaded, done: Project.Withdrawn): Documented = {
    val held = project.documents.map((d, a) => a.head.key -> d).toMap
    Documented(
      done.gone.flatMap(a => held.get(a.head.key).map(_ -> a)),
      done.uncited.flatMap(a => held.get(a.head.key).map(_ -> a))
    )
  }

  /** Each rewritten document of `docs`, with the subject [[atTheirLabels]] names it by. */
  private[plugin] def citing(docs: Documented): Vector[(Document, String)] =
    docs.uncited.map((d, a) => d -> s"${a.head.key.value} cites what would be withdrawn, but")

  /** Keeps each rewrite of `docs`, written by `by`, at its document's label, then withdraws each
    * document of what goes, at `at`; refused when one was withdrawn meanwhile.
    */
  private[plugin] def carried(own: DocumentKeeper, docs: Documented, by: CommandBy, at: Instant)(
      using Tx^
  )(using Locked): Either[Stop, Unit] = {
    val writer = Some(author(by))
    for {
      _ <- Traversal
        .traverse(docs.uncited)((d, a) =>
          kept(own, Artifact(a.head.copy(by = writer), a.body), Some(d), Some(d), at)
        )
        .map(_ => ())
      _ <- Traversal
        .traverse(docs.gone)((d, a) =>
          stored(own.withdraw(d.key, d.label, at)).flatMap(
            _.toRight(Stop.Refused(s"${a.head.key.value} was withdrawn meanwhile"))
          )
        )
        .map(_ => ())
    } yield ()
  }

  /** The key and label the versions of the artifact `key` names are kept under: its current
    * document's, as [[resolved]] finds it; or, for an artifact of `project` no longer current,
    * the key it is kept under and the label its newest version `own` reads is at; or why not, as
    * [[resolved]] refuses, when `own` reads no version of it, or, for an id, the newest holds
    * another id.
    */
  private[plugin] def recorded(own: DocumentShelf, project: Loaded, key: ArtifactRef)(using
      Tx^
  ): Either[Stop, (DocKey, Label)] =
    resolved(project, key).flatMap(artifact(project, _)) match {
      case Right((document, _)) => Right((document.key, document.label))
      case Left(missing) =>
        for {
          docKey <- key match {
            case ArtifactRef.Id(id) =>
              rekeyedKey(own, id).flatMap(_.fold(refuse(DocKey.of(id.value)))(Right(_)))
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

  /** Nothing, or a refusal naming each of `writing` kept at a label below what this room writes,
    * as `<subject> is kept <label>, below what this room writes (<clearance>): <verb> from a room
    * at its label`, joined by `; `: written from here, it would gain a second current copy at the
    * room's label.
    */
  private[plugin] def atTheirLabels(writing: Vector[(Document, String)], verb: String)(using
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

  /** The versions of an artifact of `project` kept under `key` at `label` that `own` reads, newest
    * first, at most [[HistoryMost]], with how each reads ([[versionsRead]]).
    */
  private[plugin] def keptVersions(own: DocumentShelf, project: Loaded, key: DocKey, label: Label)(
      using Tx^
  ): Either[Stop, (Vector[DocumentVersion], Document -> Either[String, Artifact])] =
    for {
      kept <- stored(own.versions(key, label, HistoryMost))
      read <- versionsRead(own, project, kept.collect { case DocumentVersion.Holding(d) => d })
    } yield (kept, read)

  /** `state`, when `kind` is one of `method`'s and has it; else a refusal naming theirs. */
  private[plugin] def stateOf(
      method: Method,
      kind: KindName,
      state: StateName
  ): Either[Stop, StateName] =
    method.kinds
      .find(_.name == kind)
      .toRight(
        Stop.Refused(
          s"${KindName.value(kind)} is no kind of method ${MethodName.value(method.name)}; " +
            "its kinds: " + method.kinds.map(k => KindName.value(k.name)).mkString(", ")
        )
      )
      .flatMap(k =>
        k.all
          .find(_ == state)
          .toRight(
            Stop.Refused(
              s"${KindName.value(kind)} has no state ${StateName.value(state)}; its states: " +
                k.all.map(StateName.value).mkString(", ")
            )
          )
      )

  /** The id of each artifact of `project`, by the key its document is kept under. */
  private[plugin] def ids(project: Loaded): Map[DocKey, ArtifactKey] =
    project.documents.map((d, a) => d.key -> a.head.key).toMap

  /** Keeps `artifact` through `own` under the key and at the label of `before`, its document, or
    * when new under its id and at public: the version it is now at. Believed at no more than
    * `carried`, the document whose words it carries, or none when every word is its caller's.
    */
  private[plugin] def kept(
      own: DocumentKeeper,
      artifact: Artifact,
      before: Option[Document],
      carried: Option[Document],
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
        DocumentKeeper.replacing(carried),
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
  private[plugin] def author(by: CommandBy): String = by match {
    case CommandBy.Direct(person, _) => accounts(person)
    case CommandBy.Turn(person, turn) =>
      s"${accounts(person)}, through grit (turn ${TurnSeq.value(turn.turnSeq)} of conversation " +
        s"${ConversationId.value(turn.conversationId)})"
  }

  /** Who `by` moves as ([[Mover]]): a person when a direct caller believed at `Authority` or
    * more, else another (a turn, or a caller believed lower).
    */
  private[plugin] def moverOf(by: CommandBy): Mover = by match {
    case CommandBy.Direct(_, believed) if believed.dominates(Integrity.Authority) => Mover.Person
    case _ => Mover.Other
  }

  /** Who [[moverOf]] moves as a person, in the words of every refusal that names the rule. */
  private[plugin] val PersonOnlyBy: String =
    "a person running the command themselves, from a command line this deployment trusts to " +
      "speak for them, never a turn or a session working for them"

  /** Who `person` is, as a claim or a writer names them: each of their accounts, sorted. */
  private[plugin] def accounts(person: Principal.Person): String = {
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
}
