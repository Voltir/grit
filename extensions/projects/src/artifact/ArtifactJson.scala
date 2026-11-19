package grit.projects.artifact

import java.nio.charset.StandardCharsets

import grit.core.document.{DocRef, DocText, Document}
import grit.core.id.DocKey
import grit.projects.name.{
  ArtifactKey,
  ArtifactNumber,
  ArtifactRef,
  Handle,
  KindName,
  MethodName,
  Pin,
  ProjectKey,
  StateName
}
import grit.std.either.Proven
import grit.std.either.Traversal.sequence

/** An artifact as the projects plugin's document holds it. */
object ArtifactJson {

  /** 4,096: the most bytes a document's data holds besides its body's stored form, as compact
    * JSON: what [[Document.DataMost]] leaves beside [[Body.StoredMost]].
    */
  val HeadMost: Int = Document.DataMost - Body.StoredMost

  /** `artifact` as its document holds it, or why not: its search text (its title, then its
    * body's) for the text column; its head and its body's stored form as data; and refs naming
    * its project, kind, state, parent and each artifact it cites, in its head or its body.
    * Refused when its head takes more than [[HeadMost]] bytes of data, or it would hold more
    * than [[DocRef.Most]] refs.
    */
  def write(artifact: Artifact): Either[String, (DocText, ujson.Value, Set[DocRef])] = {
    val h = artifact.head
    val body = Body.stored(artifact.body)
    val data = dataOf(artifact)
    val headBytes = bytes(data) - bytes(body)
    val named = Vector(
      HoldingKind -> ProjectKey.value(h.key.project),
      "kind" -> KindName.value(h.kind),
      StateKind -> StateName.value(h.state)
    ) ++ h.parent.map("parent" -> _.value) ++
      artifact.cites.map("cites" -> _.value)
    if (headBytes > HeadMost)
      Left(s"its head is $headBytes bytes, over $HeadMost: shorten its title or cite fewer")
    else if (named.size > DocRef.Most)
      Left(s"it would hold ${named.size} references, over ${DocRef.Most}: cite fewer")
    else
      for {
        refs <- sequence(named.map(DocRef.of(_, _)))
        text <- DocText.of(s"${h.title}\n\n${Body.searchText(artifact.body)}")
      } yield (text, data, refs.toSet)
  }

  /** The ref every artifact of `project` holds, or why not (see [[DocRef.of]]). */
  def holding(project: ProjectKey): Either[String, DocRef] =
    DocRef.of(HoldingKind, ProjectKey.value(project))

  /** The ref `project`'s index holds, or why not (see [[DocRef.of]]). */
  def indexing(project: ProjectKey): Either[String, DocRef] =
    DocRef.of("pinned", ProjectKey.value(project))

  /** `project`'s index as its document holds it, or why not: under the project's key, which no
    * artifact's is; text naming the project and its method; its pin's method, version and
    * digest, `by`, and `next`, the number its next new artifact takes, as data; and the one ref
    * [[indexing]] gives. With no `next` it is in the old form, before the re-key (ADR 0063).
    */
  def index(
      project: ProjectKey,
      pin: Pin,
      by: String,
      next: Option[ArtifactNumber]
  ): Either[String, (DocKey, DocText, ujson.Value, Set[DocRef])] = {
    val method = MethodName.value(pin.method)
    for {
      key <- DocKey.of(ProjectKey.value(project))
      text <- DocText.of(
        s"project ${ProjectKey.value(project)}, method $method version ${pin.version}"
      )
      ref <- indexing(project)
    } yield (
      key,
      text,
      ujson.Obj.from(
        pinData(pin).value.toSeq ++ Seq("by" -> ujson.Str(by)) ++
          next.map(n => NextField -> ujson.Num(ArtifactNumber.value(n)))
      ),
      Set(ref)
    )
  }

  /** Who started the project of the index `document`, as [[index]] wrote it; none when it names
    * no one.
    */
  def startedBy(document: Document): Option[String] =
    document.data.objOpt.flatMap(_.get("by")).flatMap(_.strOpt)

  /** The number the next new artifact of the index `document` takes, as [[index]] wrote it:
    * `None` in the old form; or why not: a `next` that is no number.
    */
  def nextOf(document: Document): Either[String, Option[ArtifactNumber]] =
    document.data.objOpt.flatMap(_.get(NextField)) match {
      case None => Right(None)
      case Some(n) =>
        n.numOpt
          .filter(_.isValidInt)
          .toRight(s"its next is no number: $n")
          .flatMap(d => ArtifactNumber.of(d.toInt))
          .map(Some(_))
    }

  /** The pin of the index `document` holds, as [[index]] wrote it, or why not. */
  def pinOf(document: Document): Either[String, Pin] = pinFrom(document.data)

  /** `project` as JSON, apart from any document: its key, its pin as [[index]] writes it, and
    * each artifact's head and body's stored form as [[write]] writes them.
    */
  def projectData(project: Project): ujson.Obj =
    ujson.Obj(
      "key" -> ProjectKey.value(project.key),
      "pin" -> pinData(project.pin),
      "artifacts" -> ujson.Arr.from(project.artifacts.map(dataOf))
    )

  /** The project [[projectData]] wrote as `json`, or why not: a field missing or malformed, a
    * stored body [[Body.fromStored]] refuses, or what [[Project.of]] refuses. It reads too the
    * old form, each artifact beside its key `<project>/<name>` and its head without an id: each
    * takes `<name>` as its handle and its place in the list, from 1, as its number, and every
    * reference names a handle. Refused for artifacts of both forms.
    */
  def projectFrom(json: ujson.Value): Either[String, Project] =
    for {
      fields <- json.objOpt.toRight("it is no object")
      key <- fields.get("key").flatMap(_.strOpt).toRight("it holds no key").flatMap(ProjectKey.of)
      pin <- fields.get("pin").toRight("it holds no pin").flatMap(pinFrom)
      each <- fields.get("artifacts").flatMap(_.arrOpt).toRight("it holds no artifacts")
      heads <- sequence(each.toVector.map(headOf))
      artifacts <- heads.count(_.contains(IdField)) match {
        case n if n == heads.size => sequence(each.toVector.map(fromData))
        case 0 => oldForm(key, each.toVector)
        case _ => Left("its artifacts are of both forms, some with an id and some without")
      }
      project <- Project.of(key, pin, artifacts)
    } yield project

  /** `written`, artifacts of project `project` in the old form [[projectFrom]] reads, or why not:
    * a key that is not `<project>/<handle>` ([[ArtifactRef.read]]), or a reference no handle of
    * them is.
    */
  private def oldForm(
      project: ProjectKey,
      written: Vector[ujson.Value]
  ): Either[String, Vector[Artifact]] =
    for {
      named <- sequence(written.zipWithIndex.map { (a, i) =>
        for {
          ref <- a.objOpt
            .flatMap(_.get("key"))
            .flatMap(_.strOpt)
            .toRight("an artifact holds no key")
            .flatMap(ArtifactRef.read)
          handle <- ref match {
            case ArtifactRef.Named(`project`, h) => Right(h)
            case other => Left(s"an artifact's key is no name in project $project: ${other.value}")
          }
          number <- ArtifactNumber.of(i + 1)
        } yield handle -> ArtifactKey.of(project, number)
      })
      ids = named.toMap
      reference = (s: String) =>
        ArtifactRef.read(s).flatMap {
          case ArtifactRef.Named(`project`, h) => ids.get(h).toRight(s"no artifact is named $s")
          case other => Left(s"no artifact is named ${other.value}")
        }
      artifacts <- sequence(written.zip(named).map { case (a, (handle, key)) =>
        headOf(a).flatMap(fromHead(key, Some(handle), _, a, reference))
      })
    } yield artifacts

  /** `document` as [[write]] wrote it, as an artifact, or why not. */
  private[artifact] def read(document: Document): Either[String, Artifact] =
    fromData(document.data)

  private def dataOf(artifact: Artifact): ujson.Obj =
    ujson.Obj(HeadField -> head(artifact.head), BodyField -> Body.stored(artifact.body))

  private def pinData(pin: Pin): ujson.Obj =
    ujson.Obj(
      "method" -> MethodName.value(pin.method),
      "version" -> pin.version,
      "digest" -> pin.digest.hex
    )

  private def pinFrom(data: ujson.Value): Either[String, Pin] =
    data.objOpt
      .flatMap { d =>
        for {
          method <- d.get("method").flatMap(_.strOpt)
          version <- d.get("version").flatMap(_.numOpt).filter(_.isWhole)
          digest <- d.get("digest").flatMap(_.strOpt)
        } yield (method, version.toInt, digest)
      }
      .toRight("its pin holds no method, version and digest")
      .flatMap((m, v, d) => MethodName.of(m).flatMap(Pin.read(_, v, d)))

  /** The artifact whose head and stored body `data` holds, its id and handle as the head holds
    * them, or why not.
    */
  private def fromData(data: ujson.Value): Either[String, Artifact] =
    for {
      fields <- headOf(data)
      key <- text(fields, IdField).flatMap(
        ArtifactKey.read(_).left.map(why => s"its id is no artifact's: $why")
      )
      handle <- nullable(fields, "handle").flatMap {
        case Some(h) => Handle.of(h).map(Some(_))
        case None => Right(None)
      }
      artifact <- fromHead(key, handle, fields, data, ArtifactKey.read)
    } yield artifact

  /** The head of `data`, or why not. */
  private def headOf(data: ujson.Value): Either[String, collection.Map[String, ujson.Value]] =
    data.objOpt
      .flatMap(_.get(HeadField))
      .flatMap(_.objOpt)
      .toRight("its data holds no head")

  /** The artifact `key`, `handle`, of head `fields` and the stored body `data` holds, each
    * reference read by `reference`, or why not.
    */
  private def fromHead(
      key: ArtifactKey,
      handle: Option[Handle],
      fields: collection.Map[String, ujson.Value],
      data: ujson.Value,
      reference: String -> Either[String, ArtifactKey]
  ): Either[String, Artifact] =
    for {
      kind <- text(fields, "kind").flatMap(KindName.of)
      state <- text(fields, "state").flatMap(StateName.of)
      title <- text(fields, "title")
      parent <- nullable(fields, "parent").flatMap {
        case Some(p) => reference(p).map(Some(_))
        case None => Right(None)
      }
      cites <- fields
        .get("cites")
        .flatMap(_.arrOpt)
        .toRight("its head holds no cites")
        .flatMap(cs => sequence(cs.toVector.map(c => c.strOpt.toRight("a cite is not text"))))
        .flatMap(cs => sequence(cs.map(reference)))
      by <- nullable(fields, "by")
      stored <- data.objOpt.flatMap(_.get(BodyField)).toRight("its data holds no body")
      body <- Body.fromStored(stored, reference)
    } yield Artifact(ArtifactHead(key, handle, kind, state, title, parent, cites, by), body)

  /** The ref an artifact in `state` holds, as [[write]] names it. */
  def stateRef(state: StateName): DocRef =
    // A literal kind DocRef.of accepts, and a state's name, which is a slug.
    Proven.right(DocRef.of(StateKind, StateName.value(state)))

  /** The state `ref` names, when it is the ref [[stateRef]] gives. */
  def stateOf(ref: DocRef): Option[StateName] =
    if (ref.kind == StateKind) StateName.of(ref.ref).toOption else None

  private val StateKind: String = "state"

  private val HoldingKind: String = "project"
  private val HeadField: String = "head"
  private val IdField: String = "id"
  private val NextField: String = "next"
  private val BodyField: String = "body"

  private def head(h: ArtifactHead): ujson.Value =
    ujson.Obj(
      IdField -> h.key.value,
      "handle" -> h.handle.fold[ujson.Value](ujson.Null)(n => ujson.Str(Handle.value(n))),
      "kind" -> KindName.value(h.kind),
      "state" -> StateName.value(h.state),
      "title" -> h.title,
      "parent" -> h.parent.fold[ujson.Value](ujson.Null)(k => ujson.Str(k.value)),
      "cites" -> ujson.Arr.from(h.cites.map(k => ujson.Str(k.value))),
      "by" -> h.by.fold[ujson.Value](ujson.Null)(ujson.Str(_))
    )

  private def bytes(json: ujson.Value): Int =
    ujson.write(json).getBytes(StandardCharsets.UTF_8).length

  private def text(
      fields: collection.Map[String, ujson.Value],
      name: String
  ): Either[String, String] =
    fields.get(name).flatMap(_.strOpt).toRight(s"its head holds no $name")

  private def nullable(
      fields: collection.Map[String, ujson.Value],
      name: String
  ): Either[String, Option[String]] = fields.get(name) match {
    case Some(ujson.Null) => Right(None)
    case Some(ujson.Str(s)) => Right(Some(s))
    case _ => Left(s"its head holds no $name")
  }
}
