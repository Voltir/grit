package grit.projects.artifact

import java.nio.charset.StandardCharsets

import grit.core.document.{DocRef, DocText, Document}
import grit.core.id.DocKey
import grit.projects.name.{ArtifactKey, KindName, MethodName, Pin, ProjectKey, StateName}

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
        refs <- all(named.map(DocRef.of(_, _)))
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
    * digest, and `by`, as data; and the one ref [[indexing]] gives.
    */
  def index(
      project: ProjectKey,
      pin: Pin,
      by: String
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
      ujson.Obj.from(pinData(pin).value.toSeq :+ ("by" -> ujson.Str(by))),
      Set(ref)
    )
  }

  /** The pin of the index `document` holds, as [[index]] wrote it, or why not. */
  def pinOf(document: Document): Either[String, Pin] = pinFrom(document.data)

  /** `project` as a file holds it, apart from any document: its key, its pin as [[index]] writes
    * it, and each artifact's key beside its head and body's stored form as [[write]] writes them.
    */
  def projectData(project: Project): ujson.Obj =
    ujson.Obj(
      "key" -> ProjectKey.value(project.key),
      "pin" -> pinData(project.pin),
      "artifacts" -> ujson.Arr.from(
        project.artifacts.map(a =>
          ujson.Obj.from(("key" -> ujson.Str(a.head.key.value)) +: dataOf(a).value.toSeq)
        )
      )
    )

  /** The project [[projectData]] wrote as `json`, or why not: a field missing or malformed, a
    * stored body [[Body.fromStored]] refuses, or what [[Project.of]] refuses.
    */
  def projectFrom(json: ujson.Value): Either[String, Project] =
    for {
      fields <- json.objOpt.toRight("it is no object")
      key <- text(fields, "key").flatMap(ProjectKey.of)
      pin <- fields.get("pin").toRight("it holds no pin").flatMap(pinFrom)
      each <- fields.get("artifacts").flatMap(_.arrOpt).toRight("it holds no artifacts")
      artifacts <- all(each.toVector.map { a =>
        a.objOpt
          .flatMap(_.get("key"))
          .flatMap(_.strOpt)
          .toRight("an artifact holds no key")
          .flatMap(fromData(_, a))
      })
      project <- Project.of(key, pin, artifacts)
    } yield project

  /** `document` as [[write]] wrote it, as an artifact, or why not. */
  private[artifact] def read(document: Document): Either[String, Artifact] =
    fromData(DocKey.value(document.key), document.data)

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

  /** The artifact of key `written` whose head and stored body `data` holds, or why not. */
  private def fromData(written: String, data: ujson.Value): Either[String, Artifact] =
    for {
      key <- ArtifactKey
        .read(written)
        .left
        .map(why => s"its key is no artifact's: $why")
      fields <- data.objOpt
        .flatMap(_.get(HeadField))
        .flatMap(_.objOpt)
        .toRight("its data holds no head")
      kind <- text(fields, "kind").flatMap(KindName.of)
      state <- text(fields, "state").flatMap(StateName.of)
      title <- text(fields, "title")
      parent <- nullable(fields, "parent").flatMap {
        case Some(p) => ArtifactKey.read(p).map(Some(_))
        case None => Right(None)
      }
      cites <- fields
        .get("cites")
        .flatMap(_.arrOpt)
        .toRight("its head holds no cites")
        .flatMap(cs => all(cs.toVector.map(c => c.strOpt.toRight("a cite is not text"))))
        .flatMap(cs => all(cs.map(ArtifactKey.read)))
      by <- nullable(fields, "by")
      stored <- data.objOpt.flatMap(_.get(BodyField)).toRight("its data holds no body")
      body <- Body.fromStored(stored)
    } yield Artifact(ArtifactHead(key, kind, state, title, parent, cites, by), body)

  /** The ref an artifact in `state` holds, as [[write]] names it. */
  def stateRef(state: StateName): DocRef =
    // A literal kind DocRef.of accepts, and a state's name, which is a slug.
    DocRef.of(StateKind, StateName.value(state)).fold(e => sys.error(e), identity)

  /** The state `ref` names, when it is the ref [[stateRef]] gives. */
  def stateOf(ref: DocRef): Option[StateName] =
    if (ref.kind == StateKind) StateName.of(ref.ref).toOption else None

  private val StateKind: String = "state"

  private val HoldingKind: String = "project"
  private val HeadField: String = "head"
  private val BodyField: String = "body"

  private def head(h: ArtifactHead): ujson.Value =
    ujson.Obj(
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

  private def all[A](each: Vector[Either[String, A]]): Either[String, Vector[A]] =
    each.foldLeft[Either[String, Vector[A]]](Right(Vector.empty)) { (acc, e) =>
      acc.flatMap(as => e.map(as :+ _))
    }
}
