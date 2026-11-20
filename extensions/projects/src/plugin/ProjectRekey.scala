package grit.projects.plugin

import java.time.Instant

import grit.core.document.{DocText, Document, DocumentKeeper, DocumentShelf, DocumentVersion}
import grit.core.id.DocKey
import grit.core.plugin.CommandBy
import grit.core.store.Tx
import grit.core.visibility.Label
import grit.projects.artifact.{Artifact, ArtifactJson, Rank, Rekeyed}
import grit.projects.name.{ArtifactKey, ProjectKey}
import grit.std.either.Traversal

import ProjectArgs.*
import ProjectCommands.*
import Stop.{refuse, stored}

private[plugin] object ProjectRekey {

  /** The key [[Rekeyed]] says `id` is kept under, its documents read at every label `own` reads,
    * or why one cannot be read.
    */
  private[plugin] def rekeyedKey(own: DocumentShelf, id: ArtifactKey)(using
      Tx^
  ): Either[Stop, Option[DocKey]] =
    for {
      key <- refuse(Rekeyed.key(id))
      labels <- stored(own.labels(key))
      documents <- stored(Traversal.traverse(labels)(own.current(key, _)))
      under <- refuse(Rekeyed.storeKey(id, documents.flatten))
    } yield under

  /** How `versions`, versions of one artifact of `project`, read, one kept before the project's
    * re-key among them; or why the table of where re-keyed artifacts are kept cannot be read.
    */
  private[plugin] def versionsRead(own: DocumentShelf, project: Loaded, versions: Vector[Document])(
      using Tx^
  ): Either[Stop, Document -> Either[String, Artifact]] =
    if (!versions.exists(ArtifactJson.isOldForm)) Right(readable)
    else {
      for {
        keys <- refuse(Rekeyed.keys(project.key, project.next))
        documents <- stored(Traversal.traverse(keys) { key =>
          own.labels(key).flatMap(labels => Traversal.traverse(labels)(own.current(key, _)))
        })
        keyed <- refuse(Rekeyed.ids(project.key, documents.flatten.flatten))
      } yield {
        val rank = versions.iterator
          .filterNot(ArtifactJson.isOldForm)
          .map(Artifact.of(_).toOption)
          .collectFirst { case Some(a) => a.head.rank }
          .getOrElse(Rank.Middle)
        d =>
          if (ArtifactJson.isOldForm(d))
            grit.projects.rekey.Rekey
              .before(d, keyed, rank)
              .left
              .map(why => s"${DocKey.value(d.key)} cannot be read: $why")
          else readable(d)
      }
    }

  /** Every project `own` reads that has anything left to re-key, a line each: its counts, or why
    * it cannot be re-keyed; or that none is.
    */
  private[plugin] def surveyed(own: DocumentShelf)(using Tx^): Either[Stop, String] =
    for {
      documents <- stored(own.newest(Projects.Terms.bound))
      indexes = documents.filter(d => !ArtifactJson.isArtifact(d) && ArtifactJson.pinOf(d).isRight)
      pending = indexes.flatMap { index =>
        ProjectKey.of(DocKey.value(index.key)).toOption.filter { p =>
          ArtifactJson.nextOf(index).fold(_ => true, _.isEmpty) ||
          documents.exists(d =>
            ArtifactJson.isOldForm(d) && DocKey.value(d.key).startsWith(s"${ProjectKey.value(p)}/")
          )
        }
      }
      lines <- Traversal.traverse(pending.sortBy(ProjectKey.value)) { p =>
        rekeyPlanned(own, p) match {
          case Right(planned) => Right(rekeyCounts(p, planned))
          case Left(Stop.Refused(why)) => Right(why)
          case Left(store) => Left(store)
        }
      }
    } yield if (lines.isEmpty) Projects.NothingToRekey else lines.mkString("\n")

  /** 5: the most reasons a refused re-key names. */
  private[plugin] val RekeyReasonsMost: Int = 5

  /** What a re-key of `project` would write, read from its documents as `own` holds them, or why
    * not: no such project, or what [[grit.projects.rekey.Rekey.plan]] refuses.
    */
  private[plugin] def rekeyPlanned(own: DocumentShelf, project: ProjectKey)(using
      Tx^
  ): Either[Stop, grit.projects.rekey.Rekey.Planned] = {
    val p = ProjectKey.value(project)
    for {
      found <- indexed(own, project)
      (index, pin) = found
      next <- refuse(ArtifactJson.nextOf(index).left.map(why => s"project $p cannot be read: $why"))
      documents <- projectDocuments(own, project)
      (legacy, now) = documents.partition(ArtifactJson.isOldForm)
      _ <- (next, legacy.isEmpty, now.isEmpty) match {
        case (Some(_), false, _) => Left(Stop.Refused(LeftOver(project, index, legacy)))
        case (None, _, false) =>
          Left(Stop.Refused(s"project $p is in the old form but holds artifacts with ids too"))
        case _ => Right(())
      }
      old <- Traversal.traverse(legacy) { d =>
        stored(own.versions(d.key, d.label, HistoryMost)).map { kept =>
          grit.projects.rekey.Rekey.Old(
            d,
            kept
              .collect { case DocumentVersion.Holding(v) => v.written }
              .lastOption
              .getOrElse(d.written)
          )
        }
      }
      planned <- next match {
        case Some(n) =>
          Right(grit.projects.rekey.Rekey.Planned(Vector.empty, Vector.empty, n, Vector.empty))
        case None =>
          grit.projects.rekey.Rekey
            .plan(project, pin, old)
            .left
            .map { reasons =>
              val more = reasons.size - RekeyReasonsMost
              Stop.Refused(
                s"project $p cannot be re-keyed, so nothing was kept: " +
                  reasons.take(RekeyReasonsMost).mkString("; ") +
                  (if (more > 0) s"; and $more more" else "")
              )
            }
      }
    } yield planned
  }

  /** `planned` kept: each artifact under its key and at its label, believed as it was; the
    * table's documents, refused where one is kept already; and the index, its `next` the
    * planned one, believed as it was: its answer, which says so when nothing is left to re-key.
    */
  private[plugin] def rekeyed(
      own: DocumentKeeper,
      project: ProjectKey,
      planned: grit.projects.rekey.Rekey.Planned,
      by: CommandBy,
      at: Instant
  )(using Tx^)(using Locked): Either[Stop, String] =
    if (planned.artifacts.isEmpty) Right(rekeyAnswer(project, planned, dry = false))
    else
      for {
        found <- indexed(own, project)
        (index, pin) = found
        _ <- atTheirLabels(
          (index -> s"project ${ProjectKey.value(project)}") +:
            planned.artifacts.map(a => a.before -> a.after.head.called),
          "re-key"
        )
        _ <- Traversal
          .traverse(planned.artifacts)(a => kept(own, a.after, Some(a.before), Some(a.before), at))
          .map(_ => ())
        _ <- Traversal
          .traverse(planned.table)(t =>
            for {
              there <- stored(own.current(t.key, t.label))
              _ <- there.fold(Right(()))(_ =>
                Left(
                  Stop.Refused(
                    s"${DocKey.value(t.key)} is kept already at that label: re-keying " +
                      s"project ${ProjectKey.value(project)} would replace it, so nothing was kept"
                  )
                )
              )
              text <- refuse(
                DocText.of(
                  s"where project ${ProjectKey.value(project)}'s re-keyed artifacts are kept"
                )
              )
              _ <- keptEverywhere(own, t.key, t.label, Tx.top, text, t.data, Set.empty, at)
            } yield ()
          )
          .map(_ => ())
        _ <- rewroteIndex(own, project, index, pin, planned.next, author(by), at)
      } yield rekeyAnswer(project, planned, dry = false)

  /** A re-key's counts: `<project>: <n> artifacts, <n> body links, <n> head cites, <n> handles
    * carried`.
    */
  private[plugin] def rekeyCounts(
      project: ProjectKey,
      planned: grit.projects.rekey.Rekey.Planned
  ): String =
    s"${ProjectKey.value(project)}: ${planned.artifacts.size} artifacts, ${planned.links} body " +
      s"links, ${planned.cites} head cites, ${planned.prefixed.size} handles carried"

  /** What a re-key of `project` answers: its counts, the ids it gave, the next new artifact's
    * number, and each carried handle; or that nothing is left to re-key.
    */
  private[plugin] def rekeyAnswer(
      project: ProjectKey,
      planned: grit.projects.rekey.Rekey.Planned,
      dry: Boolean
  ): String = {
    val p = ProjectKey.value(project)
    val numbered = planned.artifacts.map(_.after.head.key)
    (numbered.headOption, numbered.lastOption) match {
      case (Some(lo), Some(hi)) =>
        val verb = if (dry) "would give" else "gave"
        (Vector(
          rekeyCounts(project, planned),
          s"It $verb ids ${lo.value} to ${hi.value}; the next new artifact is " +
            s"${ArtifactKey.of(project, planned.next).value}." +
            (if (dry) " Nothing was kept." else "")
        ) ++ planned.artifacts.collect {
          case a if planned.prefixed.exists(_.was == a.before.key) =>
            s"${a.after.head.called} was ${DocKey.value(a.before.key)}, a name a handle cannot " +
              "be: that key no longer names it."
        }).mkString("\n")
      case _ => s"Project $p has nothing left to re-key."
    }
  }
}
