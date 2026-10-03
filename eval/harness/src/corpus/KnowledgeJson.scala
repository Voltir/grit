package grit.eval.harness.corpus

import scala.util.Try

import grit.core.id.KnowledgeSourceName
import grit.core.place.{Place, Service}
import grit.core.triage.{KnowledgeSource, KnowledgeSources}

/** The knowledge sources the deployment that recorded a corpus declares, as its
  * `knowledge.json` holds them: grit's database keeps no declaration, so the file is written
  * beside the corpus from the deployment's.
  */
object KnowledgeJson {

  /** The sources `text` holds: `{"sources": [{"name", "line", "within", "supplies"}, …]}` in
    * the order declared, `within` a place as written ([[Place.read]]) and `supplies` a
    * service's name, `null` or absent for a source no service's tools reach. Why not, naming
    * the source and field, when it is not of that form or names a source twice.
    */
  def read(text: String): Either[String, KnowledgeSources] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("knowledge: not JSON")
      all <- Fields("knowledge", root).arr("sources")
      each <- Fields.each(all) { v =>
        val f = Fields("knowledge: a source", v)
        for {
          name <- f.str("name").flatMap(KnowledgeSourceName.of)
          what = s"knowledge: ${KnowledgeSourceName.value(name)}"
          line <- f.str("line")
          within <- f.str("within").flatMap(Place.read).left.map(w => s"$what: within: $w")
          supplies <- f
            .added("supplies")
            .flatMap(Fields.opt(_)(s => Fields.str(s"$what: supplies", s).flatMap(Service.of)))
        } yield KnowledgeSource(name, line, within, supplies)
      }
      sources <- KnowledgeSources
        .of(each)
        .left
        .map(n => s"knowledge: ${KnowledgeSourceName.value(n)} is declared twice")
    } yield sources
}
