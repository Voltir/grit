package grit.app.main

import grit.core.id.{CorpusName, TurnRef}
import grit.core.period.Probability
import grit.core.place.{Place, Reaches, Service}
import grit.core.recipe.{Offering, Shaping, TurnRecipe}
import grit.core.store.StoreError
import grit.core.triage.{Corpora, Corpus}
import grit.core.visibility.Visibility
import grit.dbos.engine.{Engine, LiveEngine, Reader}
import grit.dbos.sql.TestPostgres
import grit.models.StubClassifier
import grit.turn.{Turn, TurnRecord, TurnWeighing}

import utest.*

/** A message said to grit weighed in its turn against Postgres and DBOS, with the stub
  * classifier: what the `weigh` step records, the ledger row its call leaves, and the triage
  * row it does not.
  */
object MentionWeighLiveTests extends TestSuite {
  import LiveTurn.*

  private def right[A](e: Either[Any, A]): A = e.fold(x => sys.error(x.toString), identity)

  private val github = right(Service.of("github"))

  private val repo = right(CorpusName.of("repo"))

  /** The session reaches `github`, which `repo`, covering every place, supplies. */
  private val knowledge = right(
    Corpora.of(
      Vector(Corpus(repo, "the repository", Place.Everywhere, Some(github)))
    )
  )

  /** A message said to grit offered `github`'s tools only when `repo` reads at least 0.2. */
  private val gated = TurnRecipe.Shipped.copy(addressed =
    Shaping(grit.core.context.Width.Deployed, Offering.BySource(Probability.clamped(0.2)))
  )

  /** The `weigh` step `turn` recorded, read back through its codec. */
  private def weighed(reader: Reader^, turn: TurnRef): Option[TurnWeighing.Weighed] =
    right(TurnRecord.weighed(right(reader.steps(turn.workflowId)))) match {
      case TurnRecord.Weigh.Recorded(w) => w
      case TurnRecord.Weigh.Unrecorded => None
    }

  private def ledger(engine: Engine^, turn: TurnRef) =
    engine.db
      .read(engine.ledger.of(turn.workflowId))
      .fold((e: StoreError) => sys.error(e.toString), identity)

  val tests = Tests {
    test(
      "a message said to grit, gated by source, is weighed once: its answers recorded, its call in the ledger as weigh, no triage kept"
    ) {
      val config = TestPostgres.freshDatabase("mention_weigh")
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        launchIn(
          engine,
          engine.entries,
          new CountingProvider,
          java.nio.file.Path.of("").toAbsolutePath,
          reaches = Vector(Reaches(Place.Everywhere, github)),
          knowledge = knowledge,
          recipe = gated
        )
        val turn = say(engine, "mention")
        val _ = engine.awaitTurn(turn)
        val reader = Reader.open(config, Visibility.Shipped)
        try {
          weighed(reader, turn).collect { case TurnWeighing.Weighed.Asked(asked) =>
            asked.tags.model
          } ==> Some(StubClassifier.Model)
        } finally reader.close()
        ledger(engine, turn).filter(_.entry == TurnWeighing.id(turn)).map(_.model) ==>
          Vector(StubClassifier.Model)
        TurnRecord.role(turn, TurnWeighing.id(turn)) ==> Some(TurnRecord.Role.Weigh)
        val root = right(engine.db.read(engine.entries.ofTurn(turn))).map(_.id)
        right(engine.db.read(engine.triage.of(root))) ==> Map.empty
      } finally engine.close()
    }
  }
}
