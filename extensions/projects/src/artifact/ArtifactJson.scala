package grit.projects.artifact

import java.nio.charset.StandardCharsets

import grit.core.document.{DocRef, DocText, Document}
import grit.core.id.DocKey
import grit.projects.name.{ArtifactKey, KindName, ProjectKey, StateName}

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
    val data = ujson.Obj(HeadField -> head(h), BodyField -> body)
    val headBytes = bytes(data) - bytes(body)
    val named = Vector(
      "project" -> ProjectKey.value(h.key.project),
      "kind" -> KindName.value(h.kind),
      "state" -> StateName.value(h.state)
    ) ++ h.parent.map("parent" -> _.value) ++
      (h.cites ++ Body.cites(artifact.body)).distinct.map("cites" -> _.value)
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

  /** `document` as [[write]] wrote it, as an artifact, or why not. */
  private[artifact] def read(document: Document): Either[String, Artifact] =
    for {
      key <- ArtifactKey
        .read(DocKey.value(document.key))
        .left
        .map(why => s"its key is no artifact's: $why")
      fields <- document.data.objOpt
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
      stored <- document.data.objOpt.flatMap(_.get(BodyField)).toRight("its data holds no body")
      body <- Body.fromStored(stored)
    } yield Artifact(ArtifactHead(key, kind, state, title, parent, cites, by), body)

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
