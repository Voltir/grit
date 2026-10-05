package grit.eval

/** One eval conversation: its turns, the question asked after them, which entries a window
  * for that question must contain (or must not), the search query a model might write for
  * it, and the other conversations open or closed beside it, with the scope a window may
  * draw on them within, and the documents a plugin keeps beside it.
  */
final case class Case(
    name: String,
    about: String,
    turns: Vector[Vector[Case.Line]],
    ask: String,
    query: Option[String],
    elsewhere: Vector[Case.Elsewhere] = Vector.empty,
    scope: Option[String] = None,
    documents: Vector[Case.Document] = Vector.empty
)

object Case {

  /** One message: the user's (`you`) or a reply, and whether the window must contain it
    * (`must`) or must not (`never`).
    */
  final case class Line(you: Boolean, text: String, must: Boolean, never: Boolean = false)

  /** Another conversation at `place` (written as a place is, relative to the case's own
    * root): its `turns` in its first period; when `closed`, that period closed with
    * `carried` as its balance's standing lines, and then `reopened`, its second period's
    * turns, open.
    */
  final case class Elsewhere(
      place: String,
      turns: Vector[Vector[Line]],
      closed: Boolean = false,
      carried: Vector[String] = Vector.empty,
      reopened: Vector[Vector[Line]] = Vector.empty
  )

  /** A plugin's document at `place` (written as a place is, relative to the case's own root),
    * its text `lines`, and whether the window must hold it (`must`).
    */
  final case class Document(place: String, lines: Vector[String], must: Boolean)

  private val Must = "[must]"
  private val Never = "[never]"

  /** Where the lines being read go: the case's own turns, or those of its `i`-th other
    * conversation (after its close when `reopened`).
    */
  private enum Into {
    case Own
    case Place(i: Int, reopened: Boolean)
    case Doc(i: Int)
  }

  /** `text` in the format [[Cases]] describes, or what is wrong with it, by line number. */
  def parse(name: String, text: String): Either[String, Case] = {
    final case class Acc(
        about: Vector[String],
        turns: Vector[Vector[Line]],
        ask: Option[Vector[Line]],
        query: Option[String],
        elsewhere: Vector[Elsewhere],
        scope: Option[String],
        into: Into,
        documents: Vector[Document]
    )
    def addTurn(ts: Vector[Vector[Line]]) = ts :+ Vector()
    def addLine(ts: Vector[Vector[Line]], m: Line): Option[Vector[Vector[Line]]] =
      ts.lastOption.map(t => ts.updated(ts.size - 1, t :+ m))
    val lines = text.linesIterator.zipWithIndex.map((l, i) => (l.trim, i + 1)).filter(_._1.nonEmpty)
    lines
      .foldLeft[Either[String, Acc]](
        Right(Acc(Vector(), Vector(), None, None, Vector(), None, Into.Own, Vector()))
      ) {
        case (Left(e), _) => Left(e)
        case (Right(acc), (line, n)) =>
          def fail(why: String) = Left(s"$name line $n: $why")
          def at(i: Int)(f: Elsewhere => Option[Elsewhere]) =
            acc.elsewhere.lift(i).flatMap(f) match {
              case Some(e) => Right(acc.copy(elsewhere = acc.elsewhere.updated(i, e)))
              case None => fail("a message before the first turn")
            }
          line match {
            case l if l.startsWith("#") => Right(acc.copy(about = acc.about :+ l.drop(1).trim))
            case l if l.startsWith("query:") && acc.query.isEmpty =>
              Right(acc.copy(query = Some(l.drop("query:".length).trim)))
            case l if l.startsWith("query:") => fail("a second query")
            case l if l.startsWith("scope ") && acc.scope.isEmpty =>
              Right(acc.copy(scope = Some(l.drop(6).trim)))
            case l if l.startsWith("scope ") => fail("a second scope")
            case l if l.startsWith("place ") && acc.ask.isEmpty =>
              val words = l.drop(6).trim.split("\\s+").toVector
              words.headOption.filter(_.nonEmpty) match {
                case None => fail("a place names where")
                case Some(p) =>
                  Right(
                    acc.copy(
                      elsewhere =
                        acc.elsewhere :+ Elsewhere(p, Vector(), closed = words.contains("closed")),
                      into = Into.Place(acc.elsewhere.size, reopened = false)
                    )
                  )
              }
            case l if l.startsWith("document ") && acc.ask.isEmpty =>
              val words = l.drop(9).trim.split("\\s+").toVector
              words.headOption.filter(_.nonEmpty) match {
                case None => fail("a document names where")
                case Some(p) =>
                  Right(
                    acc.copy(
                      documents = acc.documents :+ Document(p, Vector(), words.contains(Must)),
                      into = Into.Doc(acc.documents.size)
                    )
                  )
              }
            case l if l.startsWith("doc:") =>
              acc.into match {
                case Into.Doc(i) =>
                  acc.documents.lift(i) match {
                    case Some(d) =>
                      Right(
                        acc.copy(documents =
                          acc.documents.updated(i, d.copy(lines = d.lines :+ l.drop(4).trim))
                        )
                      )
                    case None => fail("doc: outside a document")
                  }
                case _ => fail("doc: outside a document")
              }
            case l if l.startsWith("carried:") =>
              acc.into match {
                case Into.Place(i, _) =>
                  at(i)(e => Some(e.copy(closed = true, carried = e.carried :+ l.drop(8).trim)))
                case Into.Own | Into.Doc(_) => fail("carried: outside a place")
              }
            case "reopen" =>
              acc.into match {
                case Into.Place(i, _) =>
                  at(i)(e => Some(e.copy(closed = true))).map(_.copy(into = Into.Place(i, true)))
                case Into.Own | Into.Doc(_) => fail("reopen outside a place")
              }
            case "here" => Right(acc.copy(into = Into.Own))
            case "turn" if acc.ask.isEmpty =>
              acc.into match {
                case Into.Own => Right(acc.copy(turns = addTurn(acc.turns)))
                case Into.Place(i, false) => at(i)(e => Some(e.copy(turns = addTurn(e.turns))))
                case Into.Place(i, true) => at(i)(e => Some(e.copy(reopened = addTurn(e.reopened))))
                case Into.Doc(_) => fail("a turn in a document: here, or place, first")
              }
            case "turn" => fail("a turn after the ask")
            case "ask" if acc.ask.isEmpty => Right(acc.copy(ask = Some(Vector()), into = Into.Own))
            case "ask" => fail("a second ask")
            case l =>
              message(l) match {
                case None =>
                  fail("expected turn, ask, query:, place, here, document, doc:, you: or grit:")
                case Some(m) =>
                  (acc.ask, acc.into) match {
                    case (Some(asked), _) => Right(acc.copy(ask = Some(asked :+ m)))
                    case (None, Into.Own) =>
                      addLine(acc.turns, m) match {
                        case Some(ts) => Right(acc.copy(turns = ts))
                        case None => fail("a message before the first turn")
                      }
                    case (None, Into.Place(i, false)) =>
                      at(i)(e => addLine(e.turns, m).map(ts => e.copy(turns = ts)))
                    case (None, Into.Place(i, true)) =>
                      at(i)(e => addLine(e.reopened, m).map(ts => e.copy(reopened = ts)))
                    case (None, Into.Doc(_)) => fail("a message in a document: doc: its lines")
                  }
              }
          }
      }
      .flatMap { acc =>
        acc.ask match {
          case Some(Vector(Line(true, question, false, false))) =>
            Right(
              Case(
                name,
                acc.about.mkString(" "),
                acc.turns,
                question,
                acc.query,
                acc.elsewhere,
                acc.scope,
                acc.documents
              )
            )
          case _ => Left(s"$name: the ask must be one unlabelled you: line")
        }
      }
  }

  private def message(line: String): Option[Line] = {
    def labelled(you: Boolean, body: String) = {
      val text = body.trim
      if (text.endsWith(Must)) Line(you, text.dropRight(Must.length).trim, must = true)
      else if (text.endsWith(Never))
        Line(you, text.dropRight(Never.length).trim, must = false, never = true)
      else Line(you, text, must = false)
    }
    if (line.startsWith("you:")) Some(labelled(true, line.drop("you:".length)))
    else if (line.startsWith("grit:")) Some(labelled(false, line.drop("grit:".length)))
    else None
  }
}
