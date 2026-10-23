package grit.core.act

import java.time.Instant

import grit.core.files.{Covers, FormatName, InMemoryPieces, Locus, PieceKeeper, Understood}
import grit.core.id.{FileHandle, PluginName}
import grit.core.place.{Namespace, Place}
import grit.core.retention.Target
import grit.core.store.Tx
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.dbos.sql.TestTx
import grit.prose.form.{Block, Doc, Span, Text}

import utest.*

/** What a piece move's read comes to over a plugin's pieces ([[Piecing.kept]]), the one meaning
  * every [[Keeping]] gives it.
  */
object PiecingTests extends TestSuite {

  private val Plugin: PluginName =
    PluginName.of("piecing").fold(why => throw new java.lang.AssertionError(why), identity)

  private val File: FileHandle =
    FileHandle.read("f1").getOrElse(throw new java.lang.AssertionError("f1"))

  private val Here: Place = Place.under(Namespace.Task, Vector("piecing"))

  private val Tree: Understood =
    Understood(
      Doc(Vector(Block.Paragraph(Text(Vector(Span("Keys rotate yearly.", Set.empty, Vector(Locus.Page(1)))))))),
      Covers.Whole,
      Vector.empty,
      FormatName("test"),
      1
    )

  private def tx(label: Label): Tx = TestTx.fake(Clearance.of(label))

  private def at(minute: Int): Instant =
    Instant.parse("2026-03-01T00:00:00Z").plusSeconds(60L * minute)

  /** The file `a` read and kept by a run at `label`, replacing its pieces at `replacing`. */
  private def kept(pieces: InMemoryPieces, label: Label, replacing: Vector[Label], minute: Int): Pieced =
    Piecing
      .kept(
        Piecing.Read(File, "a", "r1", Here, replacing),
        Right(("a.pdf", Tree)),
        pieces.keeper(Plugin, PieceKeeper.Bound),
        Integrity.Member,
        MovesFixtures.PerChar,
        at(minute)
      )(using tx(label))
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  val tests = Tests {
    test("a read replacing another label withdraws the file's pieces there, and keeps them at the floor") {
      val pieces = new InMemoryPieces
      kept(pieces, TestLabels.Trial, Vector(), 1)
      kept(pieces, Label.Public, Vector(TestLabels.Trial), 2)
      pieces.current(Plugin)(using tx(TestLabels.Trial)).map(_.label) ==> Vector(Label.Public)
    }

    test("a read replacing its own floor keeps the pieces kept there as they were, ids and all") {
      val pieces = new InMemoryPieces
      kept(pieces, Label.Public, Vector(), 1)
      val before = pieces.current(Plugin)(using tx(Label.Public)).map(_.id)
      val again = kept(pieces, Label.Public, Vector(Label.Public), 2)
      val marked =
        pieces.tombstones.declaredDue(Target.Kind.PieceSet, Plugin, at(10), 10)(using tx(Label.Public))
      (again, pieces.current(Plugin)(using tx(Label.Public)).map(_.id) == before, marked) ==>
        (Pieced.Kept(1, 0), true, Right(Vector()))
    }
  }
}
