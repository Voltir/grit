package grit.projects.artifact

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, Document, DocumentTerms, InMemoryDocuments}
import grit.core.id.{DocKey, PluginName}
import grit.core.place.Place
import grit.core.visibility.{Clearance, Integrity, Label}
import grit.dbos.sql.TestTx
import grit.projects.method.MethodFixtures.right

/** Artifacts of the old form (ADR 0063), as the store held them before ids, for the suites of
  * the re-key and of a project re-keyed.
  */
object OldFormFixtures {

  /** `data` kept by the projects plugin in `documents` under `under` at `label`, titled `text`,
    * believed at member and written `at`, read back as the plugin's shelf holds it.
    */
  def kept(
      documents: InMemoryDocuments,
      under: DocKey,
      label: Label,
      text: String,
      data: ujson.Value,
      at: Instant
  ): Document = {
    val plugin = right(PluginName.of("projects"))
    val terms = right(
      DocLabel.of("projects").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 100))
    )
    given grit.core.store.Tx = TestTx.fake(Clearance.of(label))
    right((for {
      _ <- documents
        .keeper(plugin, terms)
        .write(
          under,
          label,
          Integrity.Member,
          Place.Everywhere,
          right(DocText.of(text)),
          data,
          None,
          at,
          Set.empty
        )
      d <- documents.shelf(plugin).current(under, label)
    } yield d).left.map(_.toString).flatMap(_.toRight("nothing kept")))
  }

  /** The old form's data of a note, open, titled `title`, under `parent`, citing `cites` in its
    * head and linking each in its body, which reads `says` then the links then a full stop.
    */
  def data(
      title: String,
      parent: Option[String] = None,
      cites: Vector[String] = Vector.empty,
      says: String = "See "
  ): ujson.Value = {
    val spans = ujson.Obj("t" -> says) +: cites.zipWithIndex.map { (c, i) =>
      ujson.Obj("t" -> c.drop(c.indexOf('/') + 1), "n" -> ujson.Arr(i))
    } :+ ujson.Obj("t" -> ".")
    ujson.Obj(
      "head" -> ujson.Obj(
        "kind" -> "note",
        "state" -> "open",
        "title" -> title,
        "parent" -> parent.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
        "cites" -> ujson.Arr.from(cites.map(ujson.Str(_))),
        "by" -> "nick"
      ),
      "body" -> ujson.Obj(
        "v" -> 1,
        "notes" -> ujson.Arr.from(cites.map(ujson.Str(_))),
        "blocks" -> ujson.Arr(ujson.Obj("type" -> "para", "text" -> ujson.Arr.from(spans)))
      )
    )
  }
}
