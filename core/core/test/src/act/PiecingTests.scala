package grit.core.act

import java.time.Instant

import grit.core.files.{
  CoverUnit,
  Covers,
  FormatName,
  InMemoryPieces,
  Kind,
  Limit,
  Locus,
  PieceKeeper,
  Pieces,
  TestPieces,
  Understood
}
import grit.core.id.{FileHandle, PluginName}
import grit.core.message.Tokens
import grit.core.place.{Namespace, Place}
import grit.core.retention.Target
import grit.core.store.Tx
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.dbos.sql.TestTx
import grit.prose.form.{Block, Doc, Level, Span, Text}

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
      Doc(
        Vector(
          Block.Paragraph(
            Text(Vector(Span("Keys rotate yearly.", Set.empty, Vector(Locus.Page(1)))))
          )
        )
      ),
      Covers.Whole,
      Vector.empty,
      FormatName("test"),
      1
    )

  private def tx(label: Label): Tx = TestTx.fake(Clearance.of(label))

  private def at(minute: Int): Instant =
    Instant.parse("2026-03-01T00:00:00Z").plusSeconds(60L * minute)

  /** The file `a`, of `kind` as its source classes it, read as `tree` and kept by a run at
    * `label`, replacing its pieces at `replacing`.
    */
  private def kept(
      pieces: InMemoryPieces,
      label: Label,
      replacing: Vector[Label],
      minute: Int,
      tree: Understood = Tree,
      kind: Kind = Kind.Pdf
  ): Pieced =
    Piecing
      .kept(
        Piecing.Read(File, kind, "a", "r1", TestPieces.pointer("a"), Here, replacing),
        Right(("a.pdf", tree)),
        pieces.keeper(Plugin, PieceKeeper.Bound),
        Integrity.Member,
        MovesFixtures.PerChar,
        at(minute)
      )(using tx(label))
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  val tests = Tests {
    test(
      "a read replacing another label withdraws the file's pieces there, and keeps them at the floor"
    ) {
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
        pieces.tombstones.declaredDue(Target.Kind.PieceSet, Plugin, at(10), 10)(using
          tx(Label.Public)
        )
      (again, pieces.current(Plugin)(using tx(Label.Public)).map(_.id) == before, marked) ==>
        (Pieced.Kept(1, 0), true, Right(Vector()))
    }

    test("a read whose tree yields no piece marks the file unread, saying why") {
      def tree(blocks: Vector[Block[Locus]], covers: Covers) =
        Understood(Doc(blocks), covers, Vector.empty, FormatName("test"), 1)
      val sheet = Block.Heading(Level.of(2), Text(Vector(Span("Revenue", Set.empty, Vector()))))
      val stopped = tree(Vector(sheet), Covers.Part(1, 2, CoverUnit.Sheets, Limit.Time))
      (
        kept(new InMemoryPieces, Label.Public, Vector(), 1, stopped),
        kept(new InMemoryPieces, Label.Public, Vector(), 1, tree(Vector(), Covers.Whole))
      ) ==> (
        Pieced.Unread(
          "Nothing of that file could be read here: it took too long to read whole here."
        ),
        Pieced.Unread("That file has no text in it.")
      )
    }

    test("a survey finds due a file kept by a format the area no longer reads at that version") {
      val pieces = new InMemoryPieces
      kept(pieces, Label.Public, Vector(), 1)
      def due(reads: (FormatName, Int) -> Boolean) =
        Piecing.due(
          Piecing.Survey(Vector("a")),
          pieces.keeper(Plugin, PieceKeeper.Bound),
          reads,
          _ => None
        )(using tx(Label.Public))
      (due((n, v) => n == FormatName("test") && v == 1), due((_, v) => v == 2)) ==>
        (Right(Pieced.Due(Vector())), Right(Pieced.Due(Vector("a"))))
    }

    test(
      "a survey finds due a deck pieced from its PDF once another reader comes first for decks"
    ) {
      val pieces = new InMemoryPieces
      kept(pieces, Label.Public, Vector(), 1, kind = Kind.GoogleSlides)
      def due(first: Kind -> Option[(FormatName, Int)]) =
        Piecing.due(
          Piecing.Survey(Vector("a")),
          pieces.keeper(Plugin, PieceKeeper.Bound),
          (_, _) => true,
          first
        )(using tx(Label.Public))
      val slides = (FormatName("google-slides"), 1)
      (
        due(k => if (k == Kind.GoogleSlides) Some(slides) else None),
        due(k => if (k == Kind.Pdf) Some(slides) else Some((FormatName("test"), 1)))
      ) ==> (Right(Pieced.Due(Vector("a"))), Right(Pieced.Due(Vector())))
    }

    test(
      "a survey never finds due by its first reader a file of a kind no reader comes first for"
    ) {
      val pieces = new InMemoryPieces
      kept(pieces, Label.Public, Vector(), 1, kind = Kind.GoogleSlides)
      Piecing.due(
        Piecing.Survey(Vector("a")),
        pieces.keeper(Plugin, PieceKeeper.Bound),
        (_, _) => true,
        _ => None
      )(using tx(Label.Public)) ==> Right(Pieced.Due(Vector()))
    }

    test("a survey finds due a file cut at another target than the one pieces are cut at now") {
      val pieces = new InMemoryPieces
      val keeper = pieces.keeper(Plugin, PieceKeeper.Bound)
      given Tx = tx(Label.Public)
      val (cut, _) = Pieces.of(Tree, MovesFixtures.PerChar)
      def keptAt(ref: String, target: Tokens) =
        keeper.keep(
          TestPieces.file(ref, s"$ref.pdf", FormatName("test")).copy(target = target),
          Here,
          Integrity.Member,
          cut,
          0,
          at(1)
        )
      val _ = (keptAt("old", Tokens(Tokens.value(Pieces.Target) / 2)), keptAt("now", Pieces.Target))
      Piecing.due(Piecing.Survey(Vector("old", "now")), keeper, (_, _) => true, _ => None) ==>
        Right(Pieced.Due(Vector("old")))
    }
  }
}
