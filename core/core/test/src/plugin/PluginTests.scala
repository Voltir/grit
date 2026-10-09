package grit.core.plugin

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.id.{PluginName, WorkflowId}
import grit.core.period.CloseOrdinal
import grit.core.store.{ClosedPeriod, StoreError, Tx}

import utest.*

object PluginTests extends TestSuite {

  private def name(s: String): PluginName =
    PluginName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  /** A plugin keeping documents, posted closed periods when `posting` says so. */
  private final class Keeps(val name: PluginName, posts: Option[DocumentPosting]) extends Plugin {
    val version: Int = 1
    override val documents: Option[Documents] = Some(new Documents {
      val terms: DocumentTerms =
        DocLabel
          .of("notes")
          .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 10))
          .fold(why => throw new java.lang.AssertionError(why), identity)
      val posting: Option[DocumentPosting] = posts
    })
  }

  private object Noting extends DocumentPosting {
    def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] =
      Right(())
  }

  val tests = Tests {
    test("a plugin whose documents have no posting is posted nothing; one whose documents do is") {
      Vector(new Keeps(name("cards"), None), new Keeps(name("notes"), Some(Noting)))
        .map(_.posts) ==> Vector(false, true)
    }

    test("a plugin's name is lowercase letters, digits and dashes, from a letter") {
      Vector("digest", "wiki-2").map(PluginName.of(_).map(PluginName.value)) ==> Vector(
        Right("digest"),
        Right("wiki-2")
      )
      val why = "a plugin's name is lowercase letters, digits and dashes, starting with a letter"
      Vector("", "Digest", "2wiki", "a:b", "a b").map(PluginName.of) ==> Vector.fill(5)(Left(why))
    }

    test("a post's workflow id is post:plugin:version:cursor:attempt, and parses back") {
      val cursor = CloseOrdinal.of(17).getOrElse(throw new java.lang.AssertionError("o"))
      val ref = PostRef(name("digest"), 2, cursor, 1)
      // A pin of a recorded name: DBOS keeps every post workflow under this id, and the sweep
      // counts a cursor's runs by the prefix.
      WorkflowId.value(ref.workflowId) ==> "post:digest:2:17:1"
      PostRef.prefix(name("digest"), 2, cursor) ==> "post:digest:2:17:"
      PostRef.fromWorkflowId(ref.workflowId) ==> Some(ref)
      Vector(
        "post:digest:2:1",
        "post:Digest:2:1:0",
        "post:digest:x:1:0",
        "post:digest:1:-1:0",
        "post:digest:1:1:-1",
        "close:c:1:2:0"
      )
        .map(s => PostRef.fromWorkflowId(WorkflowId(s))) ==> Vector.fill(6)(None)
    }
  }
}
