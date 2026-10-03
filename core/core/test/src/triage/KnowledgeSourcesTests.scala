package grit.core.triage

import grit.core.id.KnowledgeSourceName
import grit.core.place.Place

import utest.*

object KnowledgeSourcesTests extends TestSuite {

  private def source(name: String, within: String): KnowledgeSource =
    (for {
      n <- KnowledgeSourceName.of(name)
      p <- Place.read(within)
    } yield KnowledgeSource(n, s"the $name", p)).getOrElse(throw new java.lang.AssertionError(name))

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
  }
}
