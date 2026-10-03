package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.id.KnowledgeSourceName
import grit.core.place.{Place, Service}

import utest.*

object KnowledgeSourcesTests extends TestSuite {

  private def source(
      name: String,
      within: String,
      supplies: Option[String] = None
  ): KnowledgeSource =
    (for {
      n <- KnowledgeSourceName.of(name)
      p <- Place.read(within)
      s <- supplies.fold[Either[String, Option[Service]]](Right(None))(Service.of(_).map(Some(_)))
    } yield KnowledgeSource(n, s"the $name", p, s))
      .getOrElse(throw new java.lang.AssertionError(name))

  val tests = Tests {
    test("at keeps the sources whose within holds the place, in the order declared") {
      // Declared out of alphabetical order, so an order the catalog did not declare shows.
      val github = source("github", "slack:")
      val conversations = source("conversations", "slack:acme")
      val notes = source("notes", "fs:/home/nick")
      val elsewhere = source("billing", "slack:other")
      val catalog = KnowledgeSources
        .of(Vector(github, notes, conversations, elsewhere))
        .getOrElse(throw new java.lang.AssertionError("catalog"))
      val thread =
        Place.read("slack:acme/#dev/1712.3").getOrElse(throw new java.lang.AssertionError("place"))
      catalog.at(thread).all ==> Vector(github, conversations)
    }

    test("of refuses two sources of one name, naming it") {
      KnowledgeSources.of(
        Vector(source("github", "slack:"), source("notes", "fs:/x"), source("github", "fs:/y"))
      ) ==> Left(
        KnowledgeSourceName.of("github").getOrElse(throw new java.lang.AssertionError("name"))
      )
    }

    test("supplied gives each service its supplying sources, in the order declared") {
      // linear's sources declared apart, docs between them, and notes supplying nothing.
      val catalog = KnowledgeSources
        .of(
          Vector(
            source("tickets", "slack:", Some("linear")),
            source("notes", "fs:/x"),
            source("handbook", "slack:", Some("docs")),
            source("roadmap", "slack:", Some("linear"))
          )
        )
        .getOrElse(throw new java.lang.AssertionError("catalog"))
      def name(n: String) =
        KnowledgeSourceName.of(n).getOrElse(throw new java.lang.AssertionError(n))
      def service(n: String) = Service.of(n).getOrElse(throw new java.lang.AssertionError(n))
      catalog.supplied.toVector ==> VectorMap(
        service("linear") -> Vector(name("tickets"), name("roadmap")),
        service("docs") -> Vector(name("handbook"))
      ).toVector
    }
  }
}
