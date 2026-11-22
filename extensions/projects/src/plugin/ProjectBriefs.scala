package grit.projects.plugin

import java.time.Instant

import grit.core.claim.{Claim, ClaimRole}
import grit.core.document.{BriefClaim, DocRef, DocText, DocumentKeeper, Takeover}
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Integrity
import grit.projects.artifact.{ArtifactHead, Project}
import grit.projects.method.{Method, RoleView}
import grit.projects.name.{ArtifactKey, RoleName, StateName}
import grit.projects.render.{Brief, BriefRefusal}
import grit.prose.words.Words
import grit.std.either.Traversal

import ProjectCommands.*
import Stop.stored

private[plugin] object ProjectBriefs {

  /** A live claim on artifact `key` of a project, and the brief [[Brief.of]] gives it within
    * [[DocRef.BriefChars]] under the view its role is shown, or how long it would be.
    */
  private[plugin] final case class Rendered(
      claim: Claim,
      key: ArtifactKey,
      view: RoleView,
      brief: Either[BriefRefusal.Over, Brief]
  )

  /** Each claim of `live` on an artifact of `project` whose role `method` shows a view of, in the
    * order given, with its brief: its artifact the one `keys` gives its subject.
    */
  private[plugin] def rendered(
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

  /** Nothing, or a refusal naming each brief over its budget that `changed` made so: of `claimed`,
    * the live claims on `after`'s artifacts as rendered ([[rendered]]), each that was not over it,
    * or was shorter, rendered over `before` (none: as if it was not), each claim's artifact the
    * one `keys` gives its subject; and, of each artifact of `after` in a state a view of `method`
    * is claimed in ([[RoleView.claimedIn]]) whose brief for that view `changed` reaches (the
    * artifact, or one the view carries), each that entered that state or grew, unless a claim of
    * that view's role on it is named already. Each names the characters to cut from those of
    * `changed` the brief holds whole.
    */
  private[plugin] def within(
      method: Method,
      before: Option[Project],
      after: Project,
      keys: Map[DocKey, ArtifactKey],
      claimed: Vector[Rendered],
      changed: Vector[ArtifactHead]
  ): Either[Stop, Unit] = {
    def from(key: ArtifactKey, view: RoleView): String = {
      val held = changed.filter(h => h.key == key || h.handle.exists(view.carried.contains))
      (if (held.isEmpty) changed else held).map(_.key.value).mkString(", ")
    }
    def clause(key: ArtifactKey, as: String, size: Int, view: RoleView): String =
      s"the brief of ${key.value}$as ${overBy(size)} (the artifact with what its role always " +
        s"carries): ${cutting(size)} from ${from(key, view)}"
    lazy val was =
      before.fold(Vector.empty[Rendered])(rendered(method, _, keys, claimed.map(_.claim)))
    val ofClaims = claimed.flatMap { r =>
      for {
        size <- r.brief.left.toOption.map(_.size)
        if was.find(_.claim == r.claim).flatMap(_.brief.left.toOption).forall(_.size < size)
      } yield (
        key = r.key,
        role = r.view.role,
        said = clause(
          r.key,
          s", claimed as ${ClaimRole.value(r.claim.role)} for ${ClaimHolder.value(r.claim.holder)},",
          size,
          r.view
        )
      )
    }
    def claimedIn(view: RoleView, head: ArtifactHead): Boolean =
      view.claimedIn.get(head.kind).exists(_.contains(head.state))
    val unclaimed = for {
      view <- method.views.filter(_.claimedIn.nonEmpty).sortBy(v => RoleName.value(v.role))
      reached = changed.exists(_.handle.exists(view.carried.contains))
      a <- after.artifacts
      if claimedIn(view, a.head) && (reached || changed.exists(_.key == a.head.key))
      if !ofClaims.exists(c => c.key == a.head.key && c.role == view.role)
      size <- Brief.size(after, a.head.key, view).toOption
      if size > DocRef.BriefChars
      prior = before.flatMap(_.artifacts.find(_.head.key == a.head.key))
      if prior.forall(p =>
        !claimedIn(view, p.head) ||
          before.flatMap(Brief.size(_, a.head.key, view).toOption).forall(_ < size)
      )
    } yield clause(a.head.key, s" as ${RoleName.value(view.role)}", size, view)
    ofClaims.map(_.said) ++ unclaimed match {
      case Vector() => Right(())
      case refusals => Left(Stop.Refused(refusals.mkString("; ")))
    }
  }

  /** A brief of `size` characters against the budget: `would be <size> characters, <cut> over
    * the 9,500 a brief holds`.
    */
  private def overBy(size: Int): String =
    f"would be $size%,d characters, ${size - DocRef.BriefChars}%,d over the " +
      f"${DocRef.BriefChars}%,d a brief holds"

  /** What a brief of `size` characters is to lose: `cut at least <cut> characters`. */
  private def cutting(size: Int): String = {
    val cut = size - DocRef.BriefChars
    f"cut at least $cut%,d character${if (cut == 1) "" else "s"}"
  }

  /** Each view of `method` claimed in `head`'s kind in some state ([[RoleView.claimedIn]]) whose
    * brief of `head`'s artifact within `project` would pass [[DocRef.BriefChars]], in role order,
    * with the characters it would be ([[Brief.size]]); none, its briefs never measured, when no
    * view is claimed in the kind.
    */
  private[plugin] def overBudget(
      method: Method,
      project: Project,
      head: ArtifactHead
  ): Vector[(view: RoleView, size: Int)] =
    method.views
      .filter(_.claimedIn.contains(head.kind))
      .sortBy(v => RoleName.value(v.role))
      .flatMap(v =>
        Brief
          .size(project, head.key, v)
          .toOption
          .filter(_ > DocRef.BriefChars)
          .map(s => (view = v, size = s))
      )

  /** What a keep or an edit answers of `head` after its version, for each brief of
    * [[overBudget]]: `Its brief as <role> would be <size> characters, <cut> over the 9,500 a brief
    * holds: cut at least <cut> characters before it moves to <states>.`, the states it is claimed
    * in, in its kind's order; empty when there is none.
    */
  private[plugin] def overNote(method: Method, project: Project, head: ArtifactHead): String =
    overBudget(method, project, head).map { o =>
      val order = method.kinds.find(_.name == head.kind).fold(Vector.empty[StateName])(_.all)
      val states = o.view.claimedIn.getOrElse(head.kind, Set.empty)
      val named = order.filter(states.contains).map(StateName.value)
      s" Its brief as ${RoleName.value(o.view.role)} ${overBy(o.size)}: ${cutting(o.size)} " +
        s"before it moves to ${Words.alternatives(named)}."
    }.mkString

  /** The key the brief of the artifact kept under `stored` is kept under: that key and `#brief`. */
  private[plugin] def briefKey(stored: DocKey): Either[String, DocKey] =
    DocKey.of(s"${DocKey.value(stored)}#brief")

  /** Keeps `r`'s brief through `own`, at its claim's label and at `place`, as the brief of the
    * claim's holder ([[DocRef.brief]]), its data naming the holder and the claim, which a person
    * moves to another session with `claimCommand` ([[grit.core.document.BriefClaim]]), believed at no more than the
    * least of what it shows as `project` holds them ([[Brief.shows]]), unless the one current
    * there is that brief already: `None` when it is current, else why none is, the one current
    * there withdrawn: it is over [[DocRef.BriefChars]], or the plugin is full.
    */
  private[plugin] def briefed(
      own: DocumentKeeper,
      r: Rendered,
      place: Place,
      project: Loaded,
      at: Instant,
      claimCommand: String
  )(using
      Tx^
  )(using Locked): Either[Stop, Option[String]] = {
    val over =
      f"it is over ${DocRef.BriefChars}%,d characters; shorten the artifact or what its role carries"
    val (label, holder) = (r.claim.label, r.claim.holder)
    val data = BriefClaim.kept(
      ujson.Obj(BriefHolder -> ClaimHolder.value(holder)),
      BriefClaim(r.claim.subject, Some(Takeover(claimCommand, r.key.value)))
    )
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
  private[plugin] def unbriefed(own: DocumentKeeper, artifact: DocKey, at: Instant)(using
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
  private[plugin] val BriefHolder: String = "holder"
}
