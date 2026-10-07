package grit.eval.harness.capture

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.id.EntryId
import grit.core.store.{Entry, EntryStore, Payload, StoreError, Tx}
import grit.core.triage.{Corpora, Tags, TriageStore}
import grit.lifecycle.triage.TriageQuestions
import grit.models.StubClassifier

/** `store`, keeping what live triage kept before it asked V2 for the heard messages whose text
  * `before` holds: their weighed tags replaced by the stub's answers to v1
  * ([[TriageQuestions.V1]]) under v1's names, so a test reads a database that spans the
  * switch, as one whose earlier rows were converted to v1's names does. Every other message's
  * tags, unanswered tags, and a message not found in `entries`, are kept as given.
  */
final class KeptAsV1(store: TriageStore, entries: EntryStore, before: String => Boolean)
    extends TriageStore {

  def record(entry: EntryId, tags: Tags, at: Instant)(using Tx^): Either[StoreError, Boolean] =
    tags match {
      case Tags.Weighed(_, model, usage) =>
        entries.get(entry).flatMap {
          case Some(Entry(_, _, _, _, _, Payload.Heard(text), _)) if before(text) =>
            val questions = TriageQuestions.V1.questions(Corpora.Empty)
            val answered = StubClassifier
              .answers(ujson.Obj("new_message" -> text), questions.values.toVector)
              .answers
            store.record(
              entry,
              Tags.Weighed(VectorMap.from(questions.keys.zip(answered)), model, usage),
              at
            )
          case _ => store.record(entry, tags, at)
        }
      case Tags.Unanswered(_) => store.record(entry, tags, at)
    }

  def of(entries: Vector[EntryId])(using Tx^): Either[StoreError, Map[EntryId, Tags]] =
    store.of(entries)

  def tagged(from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[TriageStore.Tagged]] = store.tagged(from, until)
}
