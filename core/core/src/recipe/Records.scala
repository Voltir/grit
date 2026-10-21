package grit.core.recipe

import java.time.Instant

import grit.core.document.{DocLabel, DocumentSearch}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Entry, Payload, StoreError, Tx}

/** The team's records near a heard message: what they are searched by. */
object Records {

  /** 400: the most of the thread before a message its records search reads. */
  val TailChars = 400

  /** What the records near `heard` are searched by: its words, a blank line, then the last
    * [[TailChars]] of `before`'s messages (each person's words and each reply's text, in order,
    * a blank line between them; every other entry left out). Only its words when `before` holds
    * no message.
    */
  def query(heard: Entry, before: Vector[Entry]): String = {
    val words = heard.payload.said.getOrElse("")
    val tail = before.flatMap(e => e.payload.said.orElse(replied(e.payload))).mkString("\n\n")
    if (tail.isEmpty) words else s"$words\n\n${tail.takeRight(TailChars)}"
  }

  /** The documents of every enabled plugin, current at `at`, that the transaction reads, best
    * matching `heard` and `before`'s tail ([[query]]), each with its plugin's label: at most
    * `most`, as [[DocumentSearch.nearby]] ranks them.
    */
  def near(documents: DocumentSearch, heard: Entry, before: Vector[Entry], most: Int, at: Instant)(
      using Tx^
  ): Either[StoreError, Vector[(DocumentSearch.Hit, DocLabel)]] =
    documents.nearby(query(heard, before), most, at)

  /** A reply's text; `None` for one with none and every other payload. */
  private def replied(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      Some(blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim).filter(_.nonEmpty)
    case _ => None
  }
}
