package grit.app.main

import java.time.ZoneOffset

import grit.core.clock.Clock
import grit.core.model.{Fact, FactBook}
import grit.core.store.{Jot, ModelFactStore}

/** [[FactBook]] over `store`, each fact kept in a short transaction of its own through
  * `jot`, dated by `clock` (its UTC day) and approved by the person answering in the chat.
  */
final class KeptFacts(jot: Jot^, store: ModelFactStore, clock: Clock^) extends FactBook {

  def keep(fact: Fact): Either[String, Unit] = {
    val now = clock.now()
    jot
      .write(store.keep(fact.profile(now.atOffset(ZoneOffset.UTC).toLocalDate), "chat", now))
      .left
      .map(e => s"the fact could not be kept: $e")
  }
}
