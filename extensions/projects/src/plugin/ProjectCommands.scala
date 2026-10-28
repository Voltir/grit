package grit.projects.plugin

import java.time.Instant

import grit.core.document.{DocRef, Document, DocumentKeeper, DocumentShelf}
import grit.core.id.{DocKey, PrincipalId}
import grit.core.identity.{Account, Principal}
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{Answered, KeepingCommand, PluginCommand, ReadingCommand}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Args, ArgsError, Field, ToolName, ToolSpec}
import grit.core.visibility.{Integrity, Label}
import grit.projects.artifact.{Artifact, ArtifactHead, ArtifactJson, Body, Project}
import grit.projects.method.{Method, RoleView, TransitionAction}
import grit.projects.name.{
  ArtifactKey,
  BriefSlot,
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
    Vector(New, Keep, Move, Show, List, Find, Export, ProjectClaim, ProjectRelease, ProjectBrief)

  /** `project_new`. */
  object New extends KeepingCommand[Starting] {
    val spec: ToolSpec[Starting] = ToolSpec(
      ToolName("project_new"),
      said(
        "Starts a project, holding to the latest version of the method named, or of this " +
          "deployment's own when none is. Refused for a key a project has already."
      ),
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
    def run(args: Starting, by: Principal.Person, own: DocumentKeeper, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
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
    val spec: ToolSpec[Keeping] = ToolSpec(
      ToolName("project_keep"),
      said(
        "Keeps an artifact of a project: a new one, of the kind given, in its kind's first " +
          "state; or a new version of one kept already, its kind and state unchanged (only " +
          "project_move changes a state). Its body is markdown; a link to `artifact:<key>` cites " +
          "that artifact. Refused for a parent or a cite the project lacks."
      ),
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            kind = Field.text("Its kind, for a new artifact.").optional,
            title = Field.text("Its title."),
            body = Field.text("Its body, as markdown."),
            parent = Field.text("The key of its parent artifact.").optional,
            cites = Field.texts("The keys of the artifacts it cites.", 0, DocRef.Most).optional
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            kind <- a.kind.fold(Right(None))(k => parsed("kind", k, KindName.of).map(Some(_)))
            parent <- a.parent
              .fold(Right(None))(p => parsed("parent", p, ArtifactKey.read).map(Some(_)))
            cites <- each("cites", a.cites.getOrElse(Nil).toVector, ArtifactKey.read)
          } yield Keeping(key, kind, a.title, a.body, parent, cites)
        )
    )
    def run(args: Keeping, by: Principal.Person, own: DocumentKeeper, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      for {
        project <- loaded(own, args.key.project)
        method <- refuse(declared(project.pin))
        existing = project.documents.find(_._2.head.key == args.key)
        placed <- (existing, args.kind) match {
          case (Some((_, a)), Some(k)) if k != a.head.kind =>
            Left(
              Stop.Refused(
                s"${shown(a.head)} keeps its kind; it cannot become ${KindName.value(k)}"
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
            args.parent,
            args.cites,
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
        _ <- kept(own, artifact, existing.map(_._1.label), at)
      } yield s"Kept ${shown(artifact.head)}."
    }
  }

  /** `project_move`. */
  object Move extends KeepingCommand[Moving] {
    val spec: ToolSpec[Moving] = ToolSpec(
      ToolName("project_move"),
      said(
        "Moves an artifact to another state, as its project's method allows, when every check " +
          "that move names holds. Refused naming each check that failed, or the states it may " +
          "move to."
      ),
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            to = Field.text("The state to move it to.")
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            to <- parsed("to", a.to, StateName.of)
          } yield Moving(key, to)
        )
    )
    def run(args: Moving, by: Principal.Person, own: DocumentKeeper, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      for {
        project <- loaded(own, args.key.project)
        method <- refuse(declared(project.pin))
        _ <- Either.cond(
          method.digest == project.pin.digest,
          (),
          Stop.Refused(
            s"project ${ProjectKey.value(args.key.project)} holds to method ${MethodName.value(method.name)} " +
              s"version ${method.version}, whose rules have changed since the project started; " +
              "declare a new version of the method rather than changing this one"
          )
        )
        found <- artifact(project, args.key)
        (document, a) = found
        from = method.from(a.head.kind, a.head.state)
        transition <- from
          .find(_.to == args.to)
          .toRight(
            Stop.Refused(
              s"${shown(a.head)} cannot move to ${StateName.value(args.to)}; " +
                (if (from.isEmpty) "nothing moves it on"
                 else
                   s"it may move to ${from.map(t => StateName.value(t.to)).distinct.mkString(", ")}")
            )
          )
        failed = transition.checks.flatMap { name =>
          method.checks.find(_.name == name) match {
            case Some(check) =>
              check
                .holds(a, project.artifacts.map(_.head), method.kinds)
                .left
                .toOption
                .map(why => s"${CheckName.value(name)}: $why")
            case None => Some(s"${CheckName.value(name)} is not compiled in")
          }
        }
        _ <- Either.cond(
          failed.isEmpty,
          (),
          Stop.Refused(
            s"${a.head.key.value} cannot move to ${StateName.value(args.to)}: ${failed.mkString("; ")}"
          )
        )
        _ <- transition.action match {
          case TransitionAction.Nothing => Right(())
          case TransitionAction.MergeDeltas =>
            Left(
              Stop.Refused(
                s"moving to ${StateName.value(args.to)} merges deltas, which this version of grit cannot do yet"
              )
            )
        }
        moved = Artifact(a.head.copy(state = args.to, by = Some(author(by))), a.body)
        _ <- kept(own, moved, Some(document.label), at)
      } yield s"Moved ${shown(moved.head)}."
    }
  }

  /** `project_show`. */
  object Show extends ReadingCommand[Showing] {
    val spec: ToolSpec[Showing] = ToolSpec(
      ToolName("project_show"),
      said(
        "Shows one artifact: its key, kind, state, title, parent, cites and author, then its " +
          "body as markdown."
      ),
      Args
        .of((key = Field.text("The artifact's key, `<project>/<artifact>`.")))
        .refine(a => parsed("key", a.key, ArtifactKey.read).map(Showing(_)))
    )
    def run(args: Showing, by: Principal.Person, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      for {
        project <- loaded(own.documents, args.key.project)
        found <- artifact(project, args.key)
      } yield ProjectMarkdown.file(found._2)._2
    }
  }

  /** `project_list`. */
  object List extends ReadingCommand[Listing] {
    val spec: ToolSpec[Listing] = ToolSpec(
      ToolName("project_list"),
      said(
        "Lists a project's artifacts by key, one line each (key, kind, state and title): only " +
          "those of a kind, or in a state, when given."
      ),
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
    def run(args: Listing, by: Principal.Person, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      loaded(own.documents, args.project).map { project =>
        val heads = project.artifacts
          .map(_.head)
          .filter(h => args.kind.forall(_ == h.kind) && args.state.forall(_ == h.state))
          .sortBy(_.key.value)
        AnswerText.heads(heads, "No artifact matches.")
      }
    }
  }

  /** `project_find`. */
  object Find extends ReadingCommand[Finding] {
    val spec: ToolSpec[Finding] = ToolSpec(
      ToolName("project_find"),
      said(
        s"Finds the artifacts whose titles and bodies best match some words, best first, at " +
          s"most $Found, one line each: only one project's, when given."
      ),
      Args
        .of(
          (
            query = Field.text("The words to find."),
            project = Field.text("Only artifacts of this project.").optional
          )
        )
        .refine(a =>
          a.project
            .fold(Right(None))(p => parsed("project", p, ProjectKey.of).map(Some(_)))
            .map(Finding(a.query, _))
        )
    )
    def run(args: Finding, by: Principal.Person, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      for {
        hits <- stored(own.documents.search(args.query, Found))
        // A project's index is no artifact: its key has no `/`.
        documents = hits.map(_.document).filter(d => ArtifactKey.read(DocKey.value(d.key)).isRight)
        artifacts <- refuse(every(documents.map(readable)))
        heads = artifacts
          .map(_.head)
          .filter(h => args.project.forall(_ == h.key.project))
      } yield AnswerText.heads(heads, "Nothing matches.")
    }
  }

  /** `project_export`. */
  object Export extends ReadingCommand[Exporting] {
    val spec: ToolSpec[Exporting] = ToolSpec(
      ToolName("project_export"),
      said(
        s"Exports a project as files, at most $ExportPage a page: a JSON object whose `files` " +
          "each hold a `path` and its `text`, and whose `next`, unless null, is the `after` " +
          "asking for the page after."
      ),
      Args
        .of(
          (
            project = Field.text("The project's key."),
            after = Field.text("The `next` of the page before.").optional
          )
        )
        .refine(a => parsed("project", a.project, ProjectKey.of).map(Exporting(_, a.after)))
    )
    def run(args: Exporting, by: Principal.Person, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
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
      said(
        "Claims an artifact for a session or agent to work in a role, until project_release " +
          "frees it; the artifact itself is not changed. Refused, naming who holds it, when it " +
          "is claimed already; refused, naming the artifact, when what it is claimed for works " +
          "another; and refused for a role the project's method lacks. Claims made at the same " +
          "moment are not refused: of two for one artifact, the later fails with a store error " +
          "(asked again, it is refused naming the first), and two for one session or agent on " +
          "different artifacts may both be kept. A claim kept where the asker cannot read it " +
          "(at another label) is not seen, so it refuses nothing."
      ),
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            role = Field.text("The role working on it, one its project's method declares."),
            `for` = Field.text(
              "What it is claimed for: the session, or the agent type in it, whose brief is " +
                s"written; parts joined by `/`, at most ${BriefSlot.Longest} characters."
            )
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            role <- parsed("role", a.role, RoleName.of)
            slot <- parsed("for", a.`for`, BriefSlot.of)
          } yield Claiming(key, role, slot)
        )
    )
    def run(args: Claiming, by: Principal.Person, own: DocumentKeeper, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      val slot = BriefSlot.value(args.slot)
      for {
        project <- loaded(own, args.key.project)
        method <- refuse(declared(project.pin))
        _ <- viewOf(method, args.role)
        found <- artifact(project, args.key)
        (document, a) = found
        onArtifact <- claimsHolding(own, ArtifactJson.claiming(args.key))
        _ <- onArtifact.headOption match {
          case Some(first) =>
            refuse(ArtifactJson.claimed(first)).flatMap(c =>
              Left(
                Stop.Refused(
                  s"${args.key.value} is claimed already, as ${RoleName.value(c.role)} for " +
                    s"${BriefSlot.value(c.slot)} by ${c.by} since ${first.written}; " +
                    "project_release frees it"
                )
              )
            )
          case None => Right(())
        }
        forSlot <- claimsHolding(own, ArtifactJson.claimedFor(args.slot))
        _ <- forSlot.headOption match {
          case Some(first) =>
            refuse(ArtifactJson.claimed(first)).flatMap(c =>
              Left(
                Stop.Refused(
                  s"$slot works ${c.artifact.value} already, claimed since ${first.written}; " +
                    "release it with project_release first"
                )
              )
            )
          case None => Right(())
        }
        claim <- refuse(ArtifactJson.claim(args.key, author(by), args.slot, args.role))
        (key, text, data, refs) = claim
        _ <- written(
          own.write(
            key,
            document.label,
            Integrity.Authority,
            Place.Everywhere,
            text,
            data,
            None,
            at,
            refs
          )
        )
      } yield s"Claimed ${shown(a.head)} as ${RoleName.value(args.role)} for $slot."
    }
  }

  /** `project_release`. */
  object ProjectRelease extends KeepingCommand[Releasing] {
    val spec: ToolSpec[Releasing] = ToolSpec(
      ToolName("project_release"),
      said(
        "Frees an artifact's claim, so it may be claimed again. Refused when it is not claimed."
      ),
      Args
        .of((key = Field.text("The artifact's key, `<project>/<artifact>`.")))
        .refine(a => parsed("key", a.key, ArtifactKey.read).map(Releasing(_)))
    )
    def run(args: Releasing, by: Principal.Person, own: DocumentKeeper, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      for {
        claims <- claimsHolding(own, ArtifactJson.claiming(args.key))
        claim <- claims.headOption.toRight(Stop.Refused(s"${args.key.value} is not claimed"))
        _ <- stored(own.withdraw(claim.key, claim.label, at))
      } yield s"Released ${args.key.value}."
    }
  }

  /** `project_brief`. */
  object ProjectBrief extends ReadingCommand[Briefing] {
    val spec: ToolSpec[Briefing] = ToolSpec(
      ToolName("project_brief"),
      said(
        "Gives the brief for working on an artifact in a role: a header line, its body whole, " +
          "then what the role is shown of what it cites, nearest first, as pointer lines once " +
          s"past `chars` characters (${Brief.HookChars} when not given). Refused for a role the " +
          "project's method lacks, or an artifact that alone does not fit."
      ),
      Args
        .of(
          (
            key = Field.text("The artifact's key, `<project>/<artifact>`."),
            role = Field.text("The role working on it."),
            chars = Field.count("The most characters the brief holds.", 1, BriefMost).optional
          )
        )
        .refine(a =>
          for {
            key <- parsed("key", a.key, ArtifactKey.read)
            role <- parsed("role", a.role, RoleName.of)
          } yield Briefing(key, role, a.chars.getOrElse(Brief.HookChars))
        )
    )
    def run(args: Briefing, by: Principal.Person, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = answered {
      for {
        project <- loaded(own.documents, args.key.project)
        method <- refuse(declared(project.pin))
        view <- viewOf(method, args.role)
        whole <- refuse(Project.of(args.key.project, project.pin, project.artifacts))
        brief <- Brief.of(whole, args.key, view, args.chars).left.map {
          case BriefRefusal.Unknown(k) =>
            Stop.Refused(s"project ${ProjectKey.value(k.project)} has no artifact ${k.value}")
          case BriefRefusal.Over(size) =>
            Stop.Refused(s"${args.key.value} alone is $size characters, over ${args.chars}")
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
}

private[plugin] object ProjectCommands {

  /** `project_new`'s arguments. */
  final case class Starting(project: ProjectKey, method: Option[MethodName]) extends caps.Pure

  /** `project_keep`'s arguments. */
  final case class Keeping(
      key: ArtifactKey,
      kind: Option[KindName],
      title: String,
      body: String,
      parent: Option[ArtifactKey],
      cites: Vector[ArtifactKey]
  ) extends caps.Pure

  /** `project_move`'s arguments. */
  final case class Moving(key: ArtifactKey, to: StateName) extends caps.Pure

  /** `project_show`'s arguments. */
  final case class Showing(key: ArtifactKey) extends caps.Pure

  /** `project_list`'s arguments. */
  final case class Listing(project: ProjectKey, kind: Option[KindName], state: Option[StateName])
      extends caps.Pure

  /** `project_find`'s arguments. */
  final case class Finding(query: String, project: Option[ProjectKey]) extends caps.Pure

  /** `project_export`'s arguments. */
  final case class Exporting(project: ProjectKey, after: Option[String]) extends caps.Pure

  /** `project_claim`'s arguments. */
  final case class Claiming(key: ArtifactKey, role: RoleName, slot: BriefSlot) extends caps.Pure

  /** `project_release`'s arguments. */
  final case class Releasing(key: ArtifactKey) extends caps.Pure

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

  private def answered(step: Either[Stop, String]): Either[StoreError, Answered] = step match {
    case Right(text) => Right(Answered.Done(text))
    case Left(Stop.Refused(why)) => Right(Answered.Refused(why))
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
  private def said(does: String): String = s"$does ${Projects.NotClaudeAi}"

  /** 20: the most files one page of `project_export` holds. */
  private val ExportPage: Int = 20

  /** 20: the most artifacts `project_find` answers. */
  private val Found: Int = 20

  /** `project` and its artifacts as `own` holds them, or why not. */
  private def loaded(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, Loaded] = {
    val p = ProjectKey.value(project)
    for {
      indexing <- refuse(ArtifactJson.indexing(project))
      indexes <- stored(own.referring(indexing, 1))
      index <- indexes.headOption.toRight(
        Stop.Refused(s"there is no project $p; start it with project_new")
      )
      pin <- refuse(ArtifactJson.pinOf(index).left.map(why => s"project $p cannot be read: $why"))
      holding <- refuse(ArtifactJson.holding(project))
      documents <- stored(own.referring(holding, Projects.Terms.bound))
      artifacts <- refuse(every(documents.map(d => readable(d).map(d -> _))))
    } yield Loaded(pin, artifacts)
  }

  /** The newest live claim holding `ref` that `own` reads, if any, or why not. */
  private def claimsHolding(own: DocumentShelf, ref: Either[String, DocRef])(using
      Tx^
  ): Either[Stop, Vector[Document]] =
    refuse(ref).flatMap(r => stored(own.referring(r, 1)))

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

  /** The artifact `key` of `project`, with its document, or why not. */
  private def artifact(project: Loaded, key: ArtifactKey): Either[Stop, (Document, Artifact)] =
    project.documents
      .find(_._2.head.key == key)
      .toRight(
        Stop.Refused(s"project ${ProjectKey.value(key.project)} has no artifact ${key.value}")
      )

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

  /** Who `person` is, as an artifact records its writer: each of their accounts, sorted. */
  private def author(person: Principal.Person): String = {
    val accounts = person.accounts.toVector.map(h => Account.written(h.account)).sorted
    if (accounts.isEmpty) PrincipalId.value(person.id) else accounts.mkString(", ")
  }

  private def shown(h: ArtifactHead): String =
    s"${h.key.value} (${KindName.value(h.kind)}, ${StateName.value(h.state)})"

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
