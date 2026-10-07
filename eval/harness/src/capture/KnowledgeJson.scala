package grit.eval.harness.capture

import scala.util.Try

import grit.core.id.CorpusName
import grit.core.place.{Place, Service}
import grit.core.triage.{Corpora, Corpus}

/** The corpora the deployment that recorded a capture declares, as its
  * `knowledge.json` holds them: grit's database keeps no declaration, so the file is written
  * beside the capture from the deployment's.
  */
object KnowledgeJson {

  /** The sources `text` holds: `{"sources": [{"name", "line", "within", "supplies"}, …]}` in
    * the order declared, `within` a place as written ([[Place.read]]) and `supplies` a
    * service's name, `null` or absent for a source no service's tools reach. Why not, naming
    * the source and field, when it is not of that form or names a source twice.
    */
  def read(text: String): Either[String, Corpora] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("knowledge: not JSON")
      all <- Fields("knowledge", root).arr("sources")
      each <- Fields.each(all) { v =>
        val f = Fields("knowledge: a source", v)
        for {
          name <- f.str("name").flatMap(CorpusName.of)
          what = s"knowledge: ${CorpusName.value(name)}"
          line <- f.str("line")
          within <- f.str("within").flatMap(Place.read).left.map(w => s"$what: within: $w")
          supplies <- f
            .added("supplies")
            .flatMap(Fields.opt(_)(s => Fields.str(s"$what: supplies", s).flatMap(Service.of)))
        } yield Corpus(name, line, within, supplies)
      }
      sources <- Corpora
        .of(each)
        .left
        .map(n => s"knowledge: ${CorpusName.value(n)} is declared twice")
    } yield sources
}
