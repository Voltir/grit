package grit.models

import scala.io.Source
import scala.util.Using

import grit.core.model.{Catalog, CatalogJson}

/** The catalog checked in beside this module, `extensions/models/resources/catalog.json`: the
  * policy and the profiles every run starts from. It is part of the build, so reading it
  * gives the same catalog every time.
  */
object Seed {

  /** The seed's catalog, or why it cannot be read: missing from the classpath, not JSON, or
    * not a catalog ([[CatalogJson.read]] names the first thing wrong by its path).
    */
  def catalog: Either[String, Catalog] =
    for {
      text <- Option(getClass.getResourceAsStream("/catalog.json"))
        .toRight("catalog.json is not on the classpath (extensions/models/resources/)")
        .flatMap(in =>
          scala.util
            .Try(Using.resource(in)(Source.fromInputStream(_, "UTF-8").mkString))
            .toEither
            .left
            .map(e => s"catalog.json: ${e.getMessage}")
        )
      json <- scala.util
        .Try(ujson.read(text))
        .toEither
        .left
        .map(e => s"catalog.json is not JSON: ${e.getMessage}")
      catalog <- CatalogJson.read(json).left.map(why => s"catalog.json: $why")
    } yield catalog
}
