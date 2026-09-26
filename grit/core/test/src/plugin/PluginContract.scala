package grit.core.plugin

import java.time.Instant

import grit.core.id.PluginName
import grit.core.period.CloseOrdinal
import grit.core.retention.{Target, Tombstone}
import grit.core.store.{Tombstones, Tx}

import utest.*

/** The contract every [[PluginDocs]], [[CacheDocs]] and [[PluginCursors]] keeps, run against
  * the in-memory fakes in core and the SQL stores in grit.dbos. Tests share the stores'
  * database, so each names its own plugins, and posts from its own close ordinals.
  */
abstract class PluginContract extends TestSuite {

  /** `plugin`'s documents in the store under test, as its surfaces read them. */
  protected def docs(plugin: PluginName): PluginDocs

  /** `plugin`'s documents as posted from the closed period `source`. */
  protected def cache(plugin: PluginName, source: CloseOrdinal): CacheDocs

  /** The cursors under test, over the same documents. */
  protected def cursors: PluginCursors

  /** Where the cursors mark what they leave for deletion. */
  protected def tombstones: Tombstones

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private def name(s: String): PluginName =
    PluginName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def ordinal(n: Long): CloseOrdinal =
    CloseOrdinal.of(n).getOrElse(throw new java.lang.AssertionError(n))

  private val At = Instant.parse("2026-01-01T00:00:00Z")

  /** `plugin` started at `version`, its documents posted from `source`. */
  private def posting(plugin: PluginName, source: Long, version: Int = 1): CacheDocs = {
    transaction(cursors.start(plugin, version, At))
    cache(plugin, ordinal(source))
  }

  val tests = Tests {
    test("a document is got back under its key, replaced by a second put; none is None") {
      val p = name("docs-get")
      val d = posting(p, 1001)
      transaction(d.put("k", ujson.Obj("v" -> 1))) ==> Right(())
      transaction(docs(p).get("k")) ==> Right(Some(ujson.Obj("v" -> 1)))
      transaction(d.put("k", ujson.Obj("v" -> 2)))
      transaction(docs(p).get("k")) ==> Right(Some(ujson.Obj("v" -> 2)))
      transaction(docs(p).get("none")) ==> Right(None)
    }

    test("newest is the documents under a prefix, greatest key first, at most n") {
      val p = name("docs-newest")
      val d = posting(p, 1002)
      transaction {
        for (k <- Vector("a1", "a3", "b9", "a2")) d.put(k, ujson.Str(k))
      }
      transaction(docs(p).newest("a", 2)) ==> Right(
        Vector("a3" -> ujson.Str("a3"), "a2" -> ujson.Str("a2"))
      )
      transaction(docs(p).newest("", 10)).map(_.map(_._1)) ==> Right(Vector("b9", "a3", "a2", "a1"))
    }

    test("one plugin's documents are not another's") {
      val (mine, theirs) = (name("docs-mine"), name("docs-theirs"))
      transaction(posting(mine, 1003).put("k", ujson.Str("mine")))
      transaction(cursors.start(theirs, 1, At))
      transaction(docs(mine).get("k")) ==> Right(Some(ujson.Str("mine")))
      transaction(docs(theirs).get("k")) ==> Right(None)
      transaction(docs(theirs).newest("", 10)) ==> Right(Vector())
    }

    test("a document is deleted with the closing it was posted from, and no other") {
      val p = name("docs-posted")
      transaction(posting(p, 1004).put("a", ujson.Str("a")))
      transaction(cache(p, ordinal(1005)).put("b", ujson.Str("b")))
      transaction(cursors.forgetPosted(ordinal(1004))) ==> Right(())
      transaction(docs(p).newest("", 10)).map(_.map(_._1)) ==> Right(Vector("b"))
    }

    test("a cursor starts at the start, and stays where it was moved") {
      val p = name("cursor-moves")
      transaction(cursors.start(p, 1, At)) ==> Right(CloseOrdinal.Start)
      transaction(cursors.advance(p, 1, ordinal(4))) ==> Right(())
      transaction(cursors.start(p, 1, At)) ==> Right(ordinal(4))
      transaction(cursors.stored()).map(_.filter(_._1 == p)) ==> Right(Vector((p, 1)))
    }

    test(
      "a new version starts the cursor again, reads none of the plugin's documents, no other's"
    ) {
      val (p, other) = (name("cursor-bump"), name("cursor-other"))
      transaction(posting(p, 1006).put("k", ujson.Str("v1")))
      transaction(cursors.advance(p, 1, ordinal(9)))
      transaction(posting(other, 1006).put("k", ujson.Str("kept")))
      transaction(cursors.start(p, 2, At.plusSeconds(60))) ==> Right(CloseOrdinal.Start)
      transaction(docs(p).get("k")) ==> Right(None)
      transaction(docs(other).get("k")) ==> Right(Some(ujson.Str("kept")))
      transaction(tombstones.due(Target.Kind.Restarted, At.plusSeconds(3600), 100))
        .map(_.filter(_.target == Target.Restarted(p))) ==>
        Right(Vector(Tombstone(Target.Restarted(p), At.plusSeconds(60))))
      transaction(cursors.advance(p, 2, ordinal(1)))
      transaction(cache(p, ordinal(1007)).put("k", ujson.Str("v2")))
      transaction(cursors.start(p, 2, At)) ==> Right(ordinal(1))
      transaction(docs(p).get("k")) ==> Right(Some(ujson.Str("v2")))
    }

    test("a version rolled back reads none of its earlier generation's documents") {
      val p = name("cursor-back")
      transaction(posting(p, 1008, version = 1).put("k", ujson.Str("v1")))
      transaction(posting(p, 1009, version = 2).put("k", ujson.Str("v2")))
      transaction(cursors.start(p, 1, At)) ==> Right(CloseOrdinal.Start)
      transaction(docs(p).get("k")) ==> Right(None)
      // Retired, the earlier generations' documents are gone; the current one's are kept.
      transaction(cache(p, ordinal(1010)).put("k", ujson.Str("v1 again")))
      transaction(cursors.retire(p)) ==> Right(())
      transaction(docs(p).get("k")) ==> Right(Some(ujson.Str("v1 again")))
    }

    test("remove deletes a plugin's documents and cursor, and no other's") {
      val (p, other) = (name("remove-me"), name("remove-other"))
      transaction(posting(p, 1011).put("k", ujson.Str("gone")))
      transaction(cursors.advance(p, 1, ordinal(3)))
      transaction(posting(other, 1011).put("k", ujson.Str("kept")))
      transaction(cursors.remove(p)) ==> Right(())
      transaction(cursors.stored()).map(_.map(_._1).filter(Set(p, other))) ==> Right(Vector(other))
      transaction(cursors.start(p, 1, At)) ==> Right(CloseOrdinal.Start)
      transaction(docs(p).get("k")) ==> Right(None)
      transaction(docs(other).get("k")) ==> Right(Some(ujson.Str("kept")))
    }
  }
}
