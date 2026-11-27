package grit.tools

import grit.core.id.TurnRef

/** The refusals an allowlisted `run` has given each turn, kept while its edge runs. */
trait Refusals extends caps.SharedCapability {

  /** How many times `subject` was refused before in `turn`, this refusal counted from now on. */
  def again(turn: TurnRef, subject: String): Int
}

object Refusals {

  /** 64: how many turns [[kept]] remembers, the least recent forgotten first. */
  val Turns: Int = 64

  /** Refusals kept in memory, of the last [[Turns]] turns that were refused anything. */
  def kept(): Refusals^ = new Refusals {
    // Each turn's counts by subject, the most recently refused turn last. Read and changed only
    // under this object's lock.
    private val counts = new java.util.LinkedHashMap[TurnRef, Map[String, Int]](16, 0.75f, true)
    def again(turn: TurnRef, subject: String): Int = synchronized {
      val now = counts.getOrDefault(turn, Map.empty)
      val before = now.getOrElse(subject, 0)
      val _ = counts.put(turn, now.updated(subject, before + 1))
      if (counts.size > Turns) {
        val oldest = counts.keySet.iterator.next()
        val _ = counts.remove(oldest)
      }
      before
    }
  }
}
