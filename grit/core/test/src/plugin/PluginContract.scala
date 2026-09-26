package grit.core.plugin

import grit.core.id.PluginName
import grit.core.period.CloseOrdinal
import grit.core.store.Tx

import utest.*

/** The contract every [[PluginDocs]] and [[PluginCursors]] keeps, run against the in-memory
  * fakes in core and the SQL stores in grit.dbos. Tests share the stores' database, so each
  * names its own plugins.
  */
abstract class PluginContract extends TestSuite {

  /** `plugin`'s documents in the store under test. */
  protected def docs(plugin: PluginName): PluginDocs

  /** The cursors under test, over the same documents. */
  protected def cursors: PluginCursors

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private def name(s: String): PluginName =
    PluginName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def ordinal(n: Long): CloseOrdinal =
    CloseOrdinal.of(n).getOrElse(throw new java.lang.AssertionError(n))

  val tests = Tests {
    test("a document is got back under its key, replaced by a second put; none is None") {
      val d = docs(name("docs-get"))
      transaction(d.put("k", ujson.Obj("v" -> 1))) ==> Right(())
      transaction(d.get("k")) ==> Right(Some(ujson.Obj("v" -> 1)))
      transaction(d.put("k", ujson.Obj("v" -> 2)))
      transaction(d.get("k")) ==> Right(Some(ujson.Obj("v" -> 2)))
      transaction(d.get("none")) ==> Right(None)
    }

    test("newest is the documents under a prefix, greatest key first, at most n") {
      val d = docs(name("docs-newest"))
      transaction {
        for (k <- Vector("a1", "a3", "b9", "a2")) d.put(k, ujson.Str(k))
      }
      transaction(d.newest("a", 2)) ==> Right(
        Vector("a3" -> ujson.Str("a3"), "a2" -> ujson.Str("a2"))
      )
      transaction(d.newest("", 10)).map(_.map(_._1)) ==> Right(Vector("b9", "a3", "a2", "a1"))
    }

    test("one plugin's documents are not another's") {
      val (mine, theirs) = (docs(name("docs-mine")), docs(name("docs-theirs")))
      transaction(mine.put("k", ujson.Str("mine")))
      transaction(mine.get("k")) ==> Right(Some(ujson.Str("mine")))
      transaction(theirs.get("k")) ==> Right(None)
      transaction(theirs.newest("", 10)) ==> Right(Vector())
    }

    test("a cursor starts at the start, and stays where it was moved") {
      val p = name("cursor-moves")
      transaction(cursors.start(p, 1)) ==> Right(CloseOrdinal.Start)
      transaction(cursors.advance(p, 1, ordinal(4))) ==> Right(())
      transaction(cursors.start(p, 1)) ==> Right(ordinal(4))
    }

    test("a new version starts the cursor again and clears the plugin's documents, no other's") {
      val (p, other) = (name("cursor-bump"), name("cursor-other"))
      transaction(cursors.start(p, 1))
      transaction(cursors.advance(p, 1, ordinal(9)))
      transaction(docs(p).put("k", ujson.Str("v1")))
      transaction(docs(other).put("k", ujson.Str("kept")))
      transaction(cursors.start(p, 2)) ==> Right(CloseOrdinal.Start)
      transaction(docs(p).get("k")) ==> Right(None)
      transaction(docs(other).get("k")) ==> Right(Some(ujson.Str("kept")))
      transaction(cursors.advance(p, 2, ordinal(1)))
      transaction(docs(p).put("k", ujson.Str("v2")))
      transaction(cursors.start(p, 2)) ==> Right(ordinal(1))
      transaction(docs(p).get("k")) ==> Right(Some(ujson.Str("v2")))
    }
  }
}
