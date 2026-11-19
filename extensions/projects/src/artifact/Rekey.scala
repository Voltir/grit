package grit.projects.artifact

import java.time.Instant

import grit.core.document.Document
import grit.core.id.DocKey
import grit.core.visibility.Label
import grit.projects.name.{ArtifactKey, ArtifactNumber, Handle, Pin, ProjectKey}

/** The re-key (ADR 0063): a project's artifacts of the old form, keyed by name, given ids. */
object Rekey {

  /** An old-form document, and when the oldest of its versions still kept was written. */
  final case class Old(document: Document, first: Instant) extends caps.Pure

  /** What a re-key writes: each old-form document with its artifact as rewritten, which keeps
    * its key and label; the documents of [[Rekeyed]]'s table, each at the label of what it maps;
    * the number the index's next new artifact takes; and the handle each artifact whose name was
    * led by a digit takes, which its old key no longer reaches.
    */
  final case class Planned(
      artifacts: Vector[(before: Document, after: Artifact)],
      table: Vector[(key: DocKey, label: Label, data: ujson.Value)],
      next: ArtifactNumber,
      prefixed: Vector[(was: DocKey, handle: Handle)]
  ) extends caps.Pure {

    /** The references in their bodies, each body's distinct ones counted once. */
    def links: Int = artifacts.map(a => Body.cites(a.after.body).size).sum

    /** The cites in their heads. */
    def cites: Int = artifacts.map(_.after.head.cites.size).sum
  }

  /** `k-`: what a handle is led by in place of a name led by a digit, which no handle may be. */
  val HandlePrefix: String = "k-"

  /** The old-form documents of `project` this transaction reads, numbered from 1, handled,
    * ranked and re-cited, or every reason not. A key with a leading digit takes the handle
    * `k-<key>`.
    *
    * Numbered by when each oldest kept version was written, ties by key. Ranked among their
    * siblings: those titled `M<n>` first, by `n`, ties by number, then the rest by number. Each
    * reference, in a head or a body, names a store key of `legacy`. Refused, naming the document:
    * a key not of `project`; a name no handle may be; a reference to no document of `legacy` (one
    * this transaction does not read); data that is not an old-form artifact's; or what
    * [[Project.of]] refuses of the re-keyed, such as a handle taken twice.
    */
  def plan(project: ProjectKey, pin: Pin, legacy: Vector[Old]): Either[Vector[String], Planned] = {
    val p = ProjectKey.value(project)
    val ordered = legacy.sortWith((a, b) =>
      a.first.isBefore(b.first) ||
        (a.first == b.first && DocKey.value(a.document.key) < DocKey.value(b.document.key))
    )
    val numbers =
      Iterator.iterate(ArtifactNumber.First)(ArtifactNumber.after).take(legacy.size + 1).toVector
    val numbered = ordered.zip(numbers).map((o, n) => o.document -> ArtifactKey.of(project, n))
    val ids: Map[String, ArtifactKey] =
      numbered.map((d, k) => DocKey.value(d.key) -> k).toMap
    val reference: String -> Either[String, ArtifactKey] = s =>
      ids.get(s).toRight(s"it cites $s, which this transaction reads no artifact under")
    val read: Vector[Either[String, (Document, Artifact, Option[Handle])]] = numbered.map {
      (d, id) =>
        (for {
          named <- handleOf(project, d.key)
          artifact <- ArtifactJson.oldFormOf(id, Some(named.handle), Rank.Middle, d.data, reference)
        } yield (d, artifact, Option.when(named.prefixed)(named.handle))).left
          .map(why => s"${DocKey.value(d.key)} cannot be re-keyed: $why")
    }
    val (refused, artifacts) = read.partitionMap(identity)
    if (refused.nonEmpty) Left(refused)
    else {
      val after = artifacts.zip(ranks(artifacts.map(_._2))).map { case ((d, a, _), r) =>
        (before = d, after = Artifact(a.head.copy(rank = r), a.body))
      }
      for {
        _ <- Project
          .of(project, pin, after.map(_.after))
          .left
          .map(why => Vector(s"project $p cannot be re-keyed: $why"))
        table <- tableOf(project, after).left.map(Vector(_))
      } yield Planned(
        after,
        table,
        numbers.lastOption.getOrElse(ArtifactNumber.First),
        artifacts.collect { case (d, _, Some(h)) => (was = d.key, handle = h) }
      )
    }
  }

  /** `document`, a version kept before its project's re-key, read as the re-key wrote its
    * artifact, at `rank`: its id, parent and cites the ids `ids` gives the keys they name, its
    * handle its name (`k-<name>` for one led by a digit); or why not: a key `ids` holds no id
    * of, or data that is not an old-form artifact's.
    */
  private[projects] def before(
      document: Document,
      ids: Map[DocKey, ArtifactKey],
      rank: Rank
  ): Either[String, Artifact] = {
    val reference: String -> Either[String, ArtifactKey] = s =>
      DocKey.of(s).toOption.flatMap(ids.get).toRight(s"it cites $s, which the re-key gave no id")
    for {
      id <- ids.get(document.key).toRight("the re-key gave it no id")
      named <- handleOf(id.project, document.key)
      artifact <- ArtifactJson.oldFormOf(id, Some(named.handle), rank, document.data, reference)
    } yield artifact
  }

  /** The handle the re-key gives the artifact kept under `key`, and whether it is led by
    * [[HandlePrefix]]; or why not: a key not of `project`, or a name no handle may be.
    */
  private def handleOf(
      project: ProjectKey,
      key: DocKey
  ): Either[String, (handle: Handle, prefixed: Boolean)] = {
    val p = ProjectKey.value(project)
    val whole = DocKey.value(key)
    for {
      name <-
        if (whole.startsWith(s"$p/")) Right(whole.stripPrefix(s"$p/"))
        else Left(s"it is not of project $p")
      prefixed = name.headOption.exists(_.isDigit)
      handle <- Handle.of(if (prefixed) s"$HandlePrefix$name" else name)
    } yield (handle = handle, prefixed = prefixed)
  }

  /** A title's leading `M<n>`, as a milestone is titled. */
  private val Milestone = """^M(\d{1,9})\b""".r

  /** Each of `rekeyed`'s rank, in their order: among the children of one parent, those titled
    * `M<n>` first by `n`, then the rest by number, spread over the ranks.
    */
  private def ranks(rekeyed: Vector[Artifact]): Vector[Rank] = {
    val byKey: Map[ArtifactKey, Rank] =
      rekeyed
        .groupBy(_.head.parent)
        .toVector
        .flatMap { (_: Option[ArtifactKey], children: Vector[Artifact]) =>
          val order = children.sortBy { a =>
            val m = Milestone.findPrefixMatchOf(a.head.title).map(_.group(1).toInt)
            (m.isEmpty, m.getOrElse(0), ArtifactNumber.value(a.head.key.number))
          }
          order.map(_.head.key).zip(Rank.spread(order.size))
        }
        .toMap
    rekeyed.map(a => byKey.getOrElse(a.head.key, Rank.Middle))
  }

  /** [[Rekeyed]]'s documents for `after`: one per table document and label of what it maps. */
  private def tableOf(
      project: ProjectKey,
      after: Vector[(before: Document, after: Artifact)]
  ): Either[String, Vector[(key: DocKey, label: Label, data: ujson.Value)]] =
    grit.std.either.Traversal.traverse(
      after
        .groupBy(a => (a.before.label, Rekeyed.key(a.after.head.key)))
        .toVector
        .sortBy((k, _) => (k._2.fold(identity, DocKey.value), Label.written(k._1)))
    ) {
      (at: (Label, Either[String, DocKey]), mapped: Vector[(before: Document, after: Artifact)]) =>
        val (label, key) = at
        for {
          k <- key
          data <- Rekeyed.data(
            project,
            mapped.map(a => a.after.head.key.number -> a.before.key).sortBy(_._1)
          )
        } yield (key = k, label = label, data = data)
    }
}
