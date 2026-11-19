package grit.projects.artifact

import grit.core.document.Document
import grit.core.id.DocKey
import grit.projects.name.{ArtifactKey, ArtifactNumber, Handle, ProjectKey}

/** Where a re-key left each artifact it gave an id: the key its versions are kept under, the name
  * it had before. Kept in documents of [[PerDocument]] numbers each: numbers `256k + 1` to
  * `256k + 256` under `<project>#rekeyed-<k>`, each name as the part of its key after the
  * project's `/`, at most [[Handle.Longest]] characters, so that a full document is under 25 KB.
  */
object Rekeyed {

  /** How many numbers one document holds. */
  val PerDocument: Int = 256

  /** The key of the document that holds where `id` is kept. */
  def key(id: ArtifactKey): Either[String, DocKey] =
    DocKey.of(
      s"${ProjectKey.value(id.project)}#rekeyed-${(ArtifactNumber.value(id.number) - 1) / PerDocument}"
    )

  /** `kept`, each number with the key its artifact is kept under, as one document's data, or why
    * not: a key of another project than `project`'s, or whose name is over [[Handle.Longest]]
    * characters. Its caller gives numbers of one document ([[key]]).
    */
  def data(
      project: ProjectKey,
      kept: Vector[(ArtifactNumber, DocKey)]
  ): Either[String, ujson.Value] = {
    val prefix = s"${ProjectKey.value(project)}/"
    kept
      .foldLeft[Either[String, Vector[(String, ujson.Value)]]](Right(Vector.empty)) {
        case (so, (n, k)) =>
          so.flatMap { done =>
            val whole = DocKey.value(k)
            val name = whole.stripPrefix(prefix)
            if (!whole.startsWith(prefix))
              Left(s"$whole is not of project ${ProjectKey.value(project)}")
            else if (name.length > Handle.Longest)
              Left(s"$whole's name is ${name.length} characters, over ${Handle.Longest}")
            else Right(done :+ (ArtifactNumber.value(n).toString -> ujson.Str(name)))
          }
      }
      .map(entries => ujson.Obj(KeysField -> ujson.Obj.from(entries)))
  }

  /** The key `id` is kept under as `documents` hold it, each a version of [[key]]'s document at a
    * label, merged: `None` when none names it, or why one cannot be read.
    */
  def storeKey(id: ArtifactKey, documents: Vector[Document]): Either[String, Option[DocKey]] = {
    val number = ArtifactNumber.value(id.number).toString
    documents
      .foldLeft[Either[String, Option[String]]](Right(None)) { (so, d) =>
        so.flatMap(found =>
          d.data.objOpt.flatMap(_.get(KeysField)).flatMap(_.objOpt) match {
            case None => Left(s"${DocKey.value(d.key)} holds no $KeysField")
            case Some(keys) =>
              keys.get(number) match {
                case None => Right(found)
                case Some(ujson.Str(name)) => Right(found.orElse(Some(name)))
                case Some(other) =>
                  Left(s"${DocKey.value(d.key)} names $number as ${other.render()}, not a name")
              }
          }
        )
      }
      .flatMap(
        _.fold(Right(None))(name =>
          DocKey.of(s"${ProjectKey.value(id.project)}/$name").map(Some(_))
        )
      )
  }

  private val KeysField: String = "keys"
}
