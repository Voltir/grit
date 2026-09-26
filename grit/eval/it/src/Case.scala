package grit.eval

/** One eval conversation: its turns, the question asked after them, which entries a window
  * for that question must contain, and the search query a model might write for it.
  */
final case class Case(
    name: String,
    about: String,
    turns: Vector[Vector[Case.Line]],
    ask: String,
    query: Option[String]
)

object Case {

  /** One message: the user's (`you`) or a reply, and whether the window must contain it. */
  final case class Line(you: Boolean, text: String, must: Boolean)

  private val Must = "[must]"

  /** `text` in the format [[Cases]] describes, or what is wrong with it, by line number. */
  def parse(name: String, text: String): Either[String, Case] = {
    final case class Acc(
        about: Vector[String],
        turns: Vector[Vector[Line]],
        ask: Option[Vector[Line]],
        query: Option[String]
    )
    val lines = text.linesIterator.zipWithIndex.map((l, i) => (l.trim, i + 1)).filter(_._1.nonEmpty)
    lines
      .foldLeft[Either[String, Acc]](Right(Acc(Vector(), Vector(), None, None))) {
        case (Left(e), _) => Left(e)
        case (Right(acc), (line, n)) =>
          def fail(why: String) = Left(s"$name line $n: $why")
          line match {
            case l if l.startsWith("#") => Right(acc.copy(about = acc.about :+ l.drop(1).trim))
            case l if l.startsWith("query:") && acc.query.isEmpty =>
              Right(acc.copy(query = Some(l.drop("query:".length).trim)))
            case l if l.startsWith("query:") => fail("a second query")
            case "turn" if acc.ask.isEmpty => Right(acc.copy(turns = acc.turns :+ Vector()))
            case "turn" => fail("a turn after the ask")
            case "ask" if acc.ask.isEmpty => Right(acc.copy(ask = Some(Vector())))
            case "ask" => fail("a second ask")
            case l =>
              message(l) match {
                case None => fail("expected turn, ask, query:, you: or grit:")
                case Some(m) =>
                  (acc.ask, acc.turns.lastOption) match {
                    case (Some(asked), _) => Right(acc.copy(ask = Some(asked :+ m)))
                    case (None, Some(turn)) =>
                      Right(acc.copy(turns = acc.turns.updated(acc.turns.size - 1, turn :+ m)))
                    case (None, None) => fail("a message before the first turn")
                  }
              }
          }
      }
      .flatMap { acc =>
        acc.ask match {
          case Some(Vector(Line(true, question, false))) =>
            Right(Case(name, acc.about.mkString(" "), acc.turns, question, acc.query))
          case _ => Left(s"$name: the ask must be one unlabelled you: line")
        }
      }
  }

  private def message(line: String): Option[Line] = {
    def labelled(you: Boolean, body: String) = {
      val text = body.trim
      if (text.endsWith(Must)) Line(you, text.dropRight(Must.length).trim, must = true)
      else Line(you, text, must = false)
    }
    if (line.startsWith("you:")) Some(labelled(true, line.drop("you:".length)))
    else if (line.startsWith("grit:")) Some(labelled(false, line.drop("grit:".length)))
    else None
  }
}
