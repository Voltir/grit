package grit.eval.harness.evidence

import grit.eval.harness.sent.SentMarkdown

/** A trajectory as a proposer reads it. [[Observable.matched]] reads the kept text, never this. */
object Trajectory {

  /** `kept`, a trajectory as the bench keeps it (its run's lines, then each turn
    * [[SentMarkdown.render]]ed whole), with each tool result's text past `chars` characters cut
    * there and marked as [[SentMarkdown.within]] marks it; every other line as it is.
    */
  def within(kept: String, chars: Int): String = {
    val lines = kept.split("\n", -1).toVector
    val out = Vector.newBuilder[String]
    var i = 0
    while (i < lines.size) {
      val line = lines(i)
      out += line
      i += 1
      val opened =
        if (isResult(line)) lines.lift(i).collect { case Opener(fence) => fence } else None
      opened.foreach { fence =>
        val close = lines.indexOf(fence, i + 1)
        if (close > i) {
          out ++= SentMarkdown.within(lines.slice(i + 1, close).mkString("\n"), chars, None)
          i = close + 1
        }
      }
    }
    out.result().mkString("\n")
  }

  /** `kept`, a trajectory as the bench keeps it, with each part of a rendered turn already shown
    * earlier in it replaced by one line naming where: a request's system prompt or its tools
    * (under their headings, which are kept), or one of its messages (its heading kept, compared
    * without its number), each when its text equals one shown before. Every other line, and the
    * first showing of each part, as it is.
    */
  def deduplicated(kept: String): String = {
    val out = Vector.newBuilder[String]
    val seen = scala.collection.mutable.Map.empty[(String, Vector[String]), String]
    var turn = "?"
    var inMessages = false
    var fence: Option[String] = None
    // The part being read: what it is, where it is, its key's head, and its lines so far.
    var part: Option[(String, String, Vector[String])] = None
    val body = Vector.newBuilder[String]

    def flush(): Unit = {
      part.foreach { (head, where, _) =>
        val lines = body.result()
        val shown = lines.reverse.dropWhile(_.isEmpty).reverse
        val trailing = lines.drop(shown.size)
        seen.get(head -> shown) match {
          case Some(first) => out += s"(the same as $first, shown above)"; out ++= trailing
          case None => seen(head -> shown) = where; out ++= lines
        }
      }
      part = None
      body.clear()
    }

    kept.split("\n", -1).foreach { line =>
      fence match {
        case Some(f) =>
          if (line == f) fence = None
          if (part.isDefined) body += line else out += line
        case None =>
          line match {
            case Opener(f) =>
              fence = Some(f)
              if (part.isDefined) body += line else out += line
            case _ if line.startsWith(SentMarkdown.TurnHeading) =>
              flush()
              turn = line.drop(SentMarkdown.TurnHeading.length).takeWhile(_ != ' ')
              inMessages = false
              out += line
            case _ if line.startsWith(SentMarkdown.SystemHeading) =>
              flush()
              inMessages = false
              out += line
              part = Some(("system", s"turn $turn's system prompt", Vector.empty))
            case _ if line.startsWith(SentMarkdown.ToolsHeading) =>
              flush()
              inMessages = false
              out += line
              part = Some(("tools", s"turn $turn's tools", Vector.empty))
            case _ if line.startsWith(SentMarkdown.MessagesHeading) =>
              flush()
              inMessages = true
              out += line
            case SentMarkdown.MessageHeading(n, role, rest) if inMessages =>
              flush()
              out += line
              part = Some((s"$role$rest", s"turn $turn's message $n", Vector.empty))
            case _ if line.startsWith("#") =>
              flush()
              inMessages = false
              out += line
            case _ =>
              if (part.isDefined) body += line else out += line
          }
      }
    }
    flush()
    out.result().mkString("\n")
  }

  /** How many turns `kept` renders: its lines opening a turn ([[SentMarkdown.TurnHeading]]),
    * outside any fence.
    */
  def turns(kept: String): Int =
    kept
      .split("\n", -1)
      .foldLeft((0, Option.empty[String])) { case ((n, fence), line) =>
        fence match {
          case Some(f) => (n, Option.when(line != f)(f))
          case None =>
            line match {
              case Opener(f) => (n, Some(f))
              case _ => (if (line.startsWith(SentMarkdown.TurnHeading)) n + 1 else n, None)
            }
        }
      }
      ._1

  private def isResult(line: String): Boolean = line match {
    case SentMarkdown.MessageHeading(_, role, _) => role == SentMarkdown.ToolResult
    case _ => false
  }

  /** A line opening a fence as [[SentMarkdown.fenced]] writes it, its fence captured. */
  private val Opener = """(`{3,})[a-z]*""".r
}
