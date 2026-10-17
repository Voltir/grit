package grit.kit.rehearse

import scala.collection.mutable

import grit.core.classify.{ClassifierError, Question}
import grit.core.message.Message
import grit.core.model.Pinned
import grit.core.place.Service
import grit.core.provider.ModelRequest

/** What one rehearsal told and its taps were asked, in the order it happened: each operation
  * begun and ended, each piece streamed, and each call a tap ([[Taps]]) was made, with the
  * step begun and not yet ended when it was made. Synchronised, as a tap may be called off the
  * body's thread.
  */
final class Tape extends Rehearsal.Told {

  // Pure data only.
  private val kept = mutable.ArrayBuffer[Tape.Entry]()
  private val open = mutable.ArrayBuffer[String]()

  def apply(event: Rehearsal.Event): Unit = synchronized {
    kept += Tape.Entry.Told(event)
    event match {
      case Rehearsal.Event.Began(step) =>
        open.clear()
        open += step
      case Rehearsal.Event.Ended(_) => open.clear()
      case Rehearsal.Event.Streamed(_, _) => ()
    }
  }

  /** Everything kept so far, in order. */
  def entries: Vector[Tape.Entry] = synchronized(kept.toVector)

  /** The step begun and not yet ended; `None` between operations. */
  private[rehearse] def step: Option[String] = synchronized(open.headOption)

  private[rehearse] def called(call: Tape.Call): Unit = synchronized {
    kept += Tape.Entry.Called(call)
    ()
  }
}

object Tape {

  /** One thing a rehearsal kept. */
  enum Entry {

    /** What its [[Rehearsal]] told. */
    case Told(event: Rehearsal.Event)

    /** A call a tap was made. */
    case Called(call: Call)
  }

  /** A call a tap was made, in `step`, the step begun and not yet ended when it was made
    * (`None`: made between operations).
    */
  enum Call {

    /** `request`, sent under `pinned`; `answered`, what stood in for the model, `None` for the
      * turn's own call, which is never answered ([[Stop]]).
      */
    case Model(
        step: Option[String],
        pinned: Pinned,
        request: ModelRequest,
        answered: Option[Answers.Reply[Message.Assistant]]
    )

    /** `questions` about `state`, sent to `service`; `answered`, what stood in for it. */
    case Classifier(
        step: Option[String],
        service: Service,
        state: ujson.Value,
        questions: Vector[Question],
        answered: Answers.Reply[Either[ClassifierError, grit.core.classify.Answers]]
    )
  }
}
