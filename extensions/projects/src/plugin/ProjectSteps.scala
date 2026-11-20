package grit.projects.plugin

import grit.core.document.Document
import grit.core.id.{DocKey, DocumentVersionId}
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project, Rank}
import grit.projects.method.{Method, Mover, TransitionAction}
import grit.projects.name.{
  ArtifactKey,
  ArtifactNumber,
  ArtifactRef,
  Handle,
  KindName,
  MethodName,
  ProjectKey,
  RoleName
}
import grit.projects.render.AnswerText
import grit.prose.words.Words
import grit.std.either.Traversal

import ProjectArgs.*
import ProjectCommands.*
import Stop.refuse

/** What a keeping command's steps make of a loaded project, before anything is kept: each step
  * as its command takes it, over what the steps before it made. No step reads or writes the
  * store.
  */
private[plugin] object ProjectSteps {

  /** One step a keeping command takes over a loaded project, read as its command reads its
    * arguments: `project_batch` takes several.
    */
  enum Step extends caps.Pure {
    case Keep(args: Keeping, whose: Whose = Whose.Caller)
    case Edit(args: Editing)
    case Move(args: Moving)
    case Reorder(args: Reordering)
    case Split(args: Splitting)
  }

  /** Whose words a kept body is: wholly its caller's, as `project_keep` gives it; or the version
    * before's, carried forward with a part its caller changed, as `project_edit` makes it.
    */
  enum Whose extends caps.Pure {
    case Caller, Prior

    /** A new artifact whose every word was artifact `of`'s, as `project_split` cuts it out. */
    case Part(of: ArtifactKey)
  }

  /** `ref`'s id among `artifacts`, `project`'s as steps have left them: by id or current handle
    * ([[Project.resolve]]), else as the store key of one of `project`'s documents whose artifact
    * is among them; or why not, as [[Project.resolve]] refuses.
    */
  private[plugin] def resolver(
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

  /** What steps have made of a loaded project so far: its artifacts, the keys of those they
    * changed, in the order first changed, and whose words each changed one is, where not the
    * version before's ([[Whose.Prior]]).
    */
  private[plugin] final case class Stepping(
      artifacts: Vector[Artifact],
      changed: Vector[ArtifactKey],
      whose: Map[ArtifactKey, Whose],
      next: ArtifactNumber
  ) extends caps.Pure

  /** `steps` over `project`, each as its command would take it and seeing what those before it
    * did, written as `writer` and moved by `mover`: each step's answer or refusal, in order, and
    * what they made of it. A refused step changes nothing; past one, the rest are taken only when
    * `onward`.
    */
  private[plugin] def planned(
      project: Loaded,
      method: Method,
      steps: Vector[Step],
      writer: String,
      mover: Mover,
      onward: Boolean
  ): (Vector[Either[Stop, (said: String, artifact: ArtifactKey)]], Stepping) =
    steps.foldLeft(
      (
        Vector.empty[Either[Stop, (said: String, artifact: ArtifactKey)]],
        Stepping(project.artifacts, Vector.empty, Map.empty, project.next)
      )
    ) {
      (
          sofar: (Vector[Either[Stop, (said: String, artifact: ArtifactKey)]], Stepping),
          step: Step
      ) =>
        val (said, now) = sofar
        if (!onward && said.exists(_.isLeft)) sofar
        else
          stepped(project, method, now, step, writer, mover) match {
            case Right((next, answer, artifact)) =>
              (said :+ Right((said = answer, artifact = artifact)), next)
            case Left(why) => (said :+ Left(why), now)
          }
    }

  /** The handle `change` leaves the artifact `key` (`existing` as it is now in `now`), or why not:
    * another artifact holds it, or it renames or removes a handle a role of `method` carries.
    */
  private[plugin] def handled(
      method: Method,
      now: Stepping,
      key: ArtifactKey,
      existing: Option[Artifact],
      change: Change[Option[Handle]]
  ): Either[Stop, Option[Handle]] = {
    val was = existing.flatMap(_.head.handle)
    val called = existing.fold(key.value)(_.head.called)
    val becomes = change.over(was)
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
    * [[planned]] takes it: what it makes of `now`, its answer and the artifact it names (the one
    * kept, edited, moved, re-ordered or split), or why it is refused, as
    * `project_keep`, `project_edit`, `project_move` or `project_reorder` refuses.
    */
  private[plugin] def stepped(
      project: Loaded,
      method: Method,
      now: Stepping,
      step: Step,
      writer: String,
      mover: Mover
  ): Either[Stop, (Stepping, String, ArtifactKey)] = {
    def changing(
        changed: Vector[Artifact],
        wrote: Map[ArtifactKey, Whose] = Map.empty
    ): Stepping = {
      val keys = changed.map(_.head.key)
      Stepping(
        now.artifacts.map(a => changed.find(_.head.key == a.head.key).getOrElse(a)) ++
          changed.filterNot(c => now.artifacts.exists(_.head.key == c.head.key)),
        now.changed ++ keys.filterNot(now.changed.contains),
        now.whose ++ wrote,
        now.next
      )
    }
    val resolve = resolver(project, now.artifacts)
    def found(key: ArtifactKey): Either[Stop, Artifact] =
      now.artifacts
        .find(_.head.key == key)
        .toRight(Stop.Refused(s"project ${ProjectKey.value(project.key)} has no ${key.value}"))
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
          parent <- args.parent match {
            case Change.Unchanged => Right(existing.flatMap(_.head.parent))
            case Change.To(to) => to.fold(Right(None))(r => refuse(resolve(r)).map(Some(_)))
          }
          cites <- args.cites match {
            case Change.Unchanged => Right(existing.fold(Vector.empty)(_.head.cites))
            case Change.To(to) => refuse(Traversal.traverse(to)(resolve))
          }
          body <- Body.reading(args.body, resolve).left.map { why =>
            // Only what is kept already can be split.
            Stop.Refused(why match {
              case Body.Unread.Over(_) =>
                existing.fold(why.words)(a =>
                  s"${why.words}. Split ${a.head.called} with project_split, which makes each " +
                    "of its `##` sections an artifact of its own under it, then keep the part " +
                    "that grew."
                )
              case Body.Unread.Other(_) => why.words
            })
          }
          handle <- handled(method, now, key, existing, args.handle)
          // An artifact keeps its rank among the siblings it keeps; under a new parent, or new,
          // it ranks last.
          staying = existing.filter(_.head.parent == parent)
          artifact = Artifact(
            ArtifactHead(
              key,
              handle,
              kind,
              state,
              args.title,
              parent,
              staying.fold(Rank.Middle)(_.head.rank),
              cites,
              Some(writer)
            ),
            body
          )
          siblings = siblingsOf(now.artifacts, parent, key)
          ranked =
            if (staying.isDefined) Vector(artifact)
            else among(siblings, artifact, siblings.size, writer)
          changed = changing(
            ranked,
            whose match {
              case Whose.Prior => Map.empty
              case other => Map(key -> other)
            }
          )
          next =
            if (named.isDefined) changed
            else changed.copy(next = ArtifactNumber.after(key.number))
          whole <- refuse(Project.of(project.key, project.pin, next.artifacts))
          // Only a placing is judged: an artifact a restore left out of place can still be kept.
          _ <- (if (staying.isDefined) None else method.misplaced(whole, artifact))
            .map(Stop.Refused(_))
            .toLeft(())
        } yield (next, s"Kept ${AnswerText.named(artifact.head)}." + respread(ranked.size - 1), key)
      case Step.Edit(args) =>
        for {
          key <- refuse(resolve(args.key))
          current <- found(key)
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
      case Step.Reorder(args) =>
        def under(h: ArtifactHead): String =
          h.parent.fold("under no parent")(p => s"under ${p.value}")
        for {
          moving <- refuse(resolve(args.artifact)).flatMap(found)
          sibling <- refuse(resolve(args.beside.sibling)).flatMap(found)
          _ <- Either.cond(
            sibling.head.key != moving.head.key,
            (),
            Stop.Refused(s"${moving.head.called} cannot sit beside itself")
          )
          _ <- Either.cond(
            sibling.head.parent == moving.head.parent,
            (),
            Stop.Refused(
              s"${sibling.head.called} is no sibling of ${moving.head.called}: it sits " +
                s"${under(sibling.head)}, and ${moving.head.called} ${under(moving.head)}"
            )
          )
          siblings = siblingsOf(now.artifacts, moving.head.parent, moving.head.key)
          (side, offset) = args.beside match {
            case Beside.After(_) => ("after", 1)
            case Beside.Before(_) => ("before", 0)
          }
          to = siblings.indexWhere(_.head.key == sibling.head.key) + offset
          from = siblings.count(s => ArtifactHead.ranked.lt(s.head, moving.head))
          ranked =
            if (to == from) Vector.empty
            else
              among(
                siblings,
                Artifact(moving.head.copy(by = Some(writer)), moving.body),
                to,
                writer
              )
          where = s"$side ${AnswerText.named(sibling.head)}"
        } yield (
          changing(ranked),
          if (ranked.isEmpty) s"${AnswerText.named(moving.head)} already sits $where."
          else s"Placed ${AnswerText.named(moving.head)} $where." + respread(ranked.size - 1),
          moving.head.key
        )
      case Step.Split(args) =>
        for {
          key <- refuse(resolve(args.artifact))
          current <- found(key)
          // The version is checked where the artifact is kept, after its parts.
          cannot = (why: String) => Stop.Refused(s"${current.head.called} cannot be split: $why")
          into = method.transitions.collectFirst {
            case t if t.action == TransitionAction.MergeDeltas(current.head.kind) => t.kind
          }
          _ <- into
            .map(from =>
              cannot(
                s"a move of a ${KindName.value(from)} merges its deltas into the sections of a " +
                  KindName.value(current.head.kind)
              )
            )
            .toLeft(())
          required = method.kinds
            .find(_.name == current.head.kind)
            .fold(Set.empty)(_.sections.toSet)
          split <- Body.split(current.body, required).left.map(cannot)
          made <- cutOut(project, method, now, key, split.parts, writer, mover).left.map {
            case Stop.Refused(why) => cannot(why)
            case failed => failed
          }
          left <- split.leaving(made.links).left.map(cannot)
          kept <- stepped(
            project,
            method,
            made.stepping,
            Step.Keep(
              Keeping.again(key, current.head.title, Body.markdown(left), args.version),
              Whose.Prior
            ),
            writer,
            mover
          )
          (done, _, _) = kept
        } yield (
          done,
          s"Split ${AnswerText.named(current.head)}, each section moved to an artifact of its " +
            "own under it: " +
            made.links.map(l => s"${l.key.value} ${l.part.title}").mkString("; ") + ".",
          key
        )
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
            merged.fold("")(c => s" Merged its deltas into ${AnswerText.named(c.head)}."),
          key
        )
    }
  }

  /** What [[cutOut]] made: the steps' work so far, and each part with the key it is kept under. */
  private[plugin] final case class Parted(
      stepping: Stepping,
      links: Vector[(part: Body.Part, key: ArtifactKey)]
  ) extends caps.Pure

  /** Each of `parts` kept over `now` as a new artifact of kind section under `parent`, in order,
    * as [[stepped]] keeps one, written as `writer`; or the first refusal.
    */
  private[plugin] def cutOut(
      project: Loaded,
      method: Method,
      now: Stepping,
      parent: ArtifactKey,
      parts: Vector[Body.Part],
      writer: String,
      mover: Mover
  ): Either[Stop, Parted] =
    parts.foldLeft[Either[Stop, Parted]](Right(Parted(now, Vector.empty))) {
      (so: Either[Stop, Parted], part: Body.Part) =>
        so.flatMap { sofar =>
          val child = ArtifactKey.of(project.key, sofar.stepping.next)
          stepped(
            project,
            method,
            sofar.stepping,
            Step.Keep(
              Keeping.part(parent, KindName.Section, part.title, Body.markdown(part.body)),
              Whose.Part(parent)
            ),
            writer,
            mover
          ).map((next, _, _) => Parted(next, sofar.links.appended((part = part, key = child))))
        }
    }

  /** The artifacts of `artifacts` under `parent` (or, with none, those under none), but `besides`,
    * in [[ArtifactHead.ranked]] order.
    */
  private[plugin] def siblingsOf(
      artifacts: Vector[Artifact],
      parent: Option[ArtifactKey],
      besides: ArtifactKey
  ): Vector[Artifact] =
    artifacts
      .filter(a => a.head.parent == parent && a.head.key != besides)
      .sortBy(_.head)(using ArtifactHead.ranked)

  /** `artifact` placed among `siblings`, in their order, at `at` ([[Rank.among]]): it, then each
    * sibling whose rank that changes, with its new rank, written as `writer`.
    */
  private[plugin] def among(
      siblings: Vector[Artifact],
      artifact: Artifact,
      at: Int,
      writer: String
  ): Vector[Artifact] = {
    val ranks = Rank.among(siblings.map(_.head.rank), at)
    val order = siblings.patch(at.max(0), Vector(artifact), 0)
    val (moved, others) = order
      .zip(ranks)
      .map((a, r) => Artifact(a.head.copy(rank = r), a.body) -> (a.head.rank != r))
      .partition(_._1.head.key == artifact.head.key)
    moved.map(_._1) ++ others.collect { case (a, true) =>
      Artifact(a.head.copy(by = Some(writer)), a.body)
    }
  }

  /** What an answer adds when a step re-ranked `n` siblings to make room: nothing when none. */
  private[plugin] def respread(n: Int): String =
    if (n < 1) "" else s" Re-ranked ${Words.counted(n, "sibling")} to make room."

  /** Nothing, or a refusal when `read`, the version a keep of `key` was read at, is not that of
    * `current`, its document: the version it is at, and who kept it when; or when `key` has no
    * document, made by a step before it.
    */
  private[plugin] def unchanged(
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
}
