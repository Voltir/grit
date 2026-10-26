package grit.core.screen

import grit.core.classify.{Answer, Ask, ClassifierError, StateJson}
import grit.core.either.Traversal
import grit.core.period.Probability

/** A screen: its axis's `floor`, asked first, then the signals of each of its sources in order
  * (`first`, then `rest`), each a yes/no about a [[Screen.Text]].
  */
final case class Screen(floor: Vector[Screen.Fixed], first: Signals, rest: Vector[Signals])
    extends caps.Pure {

  /** Every source's signals, in order. */
  def signals: Vector[Signal] = (first +: rest).flatMap(_.all)

  /** Its questions, read as a [[Screened]] of its [[shape]], as [[Screen.read]] reads them. */
  def ask: Ask[Screen.Text, Screened] = {
    val asked = floor.map(_.question) ++ signals.map(Screen.question)
    // The first of `asked`: its floor's first, or with no floor its first signal's.
    val head = floor.headOption.fold(Screen.question(first.first))(_.question)
    val shape = this.shape
    Screen.yesNos(head, asked.drop(1)).readBy(Screen.read(shape, _))
  }

  /** Its floor's questions alone, read as a source's [[Screen.Baseline]] as
    * [[Screen.readBaseline]] reads them; `None` when it has no floor, which then never fires.
    */
  def baseline: Option[Ask[Screen.Text, Screen.Baseline]] = {
    val size = floor.size
    floor.headOption.map { head =>
      Screen
        .yesNos(head.question, floor.drop(1).map(_.question))
        .readBy(Screen.readBaseline(size, _))
    }
  }

  def shape: Screen.Shape = Screen.Shape(floor.size, signals.map(_.must))

  /** What `fired` names, in screen order: the floor first, then the signals; a position this
    * screen has none at is skipped. Text: read only in a step, recorded nowhere.
    */
  def failed(fired: Fired): Vector[Either[Screen.Fixed, Signal]] = {
    val all = signals
    fired.floor.flatMap(floor.lift).map(Left(_)) ++
      (fired.absent ++ fired.present).sorted.flatMap(all.lift).map(Right(_))
  }
}

object Screen {

  /** The state a screen asks about: `{"text"}`, the text alone. */
  final case class Text(text: String) extends caps.Pure

  object Text {
    given StateJson[Text] = StateJson.instance(t => ujson.Obj("text" -> t.text))
  }

  /** A floor question, `question`, and how a refusal names it to the text's writer, `named`. */
  final case class Fixed(question: String, named: String) extends caps.Pure

  /** What a screen asked, without its text: its floor's size and each signal's `must`. */
  final case class Shape(floor: Int, musts: Vector[Must]) extends caps.Pure

  /** A source's floor answers, in its floor's order: a floor question it answers at or above
    * [[Infers]] is excused.
    */
  final case class Baseline private[screen] (floor: Vector[Probability]) extends caps.Pure

  /** 0.4: a floor or absent answer at or above it reads as the text letting a reader infer it. */
  val Infers: Probability = Probability.clamped(0.4)

  /** 0.5: a present answer at or above it reads as the text stating it. */
  val States: Probability = Probability.clamped(0.5)

  /** An absent signal's question: "Does `text` state, hint at or let a reader infer {infer}?" */
  def absent(infer: String): String = s"Does `text` state, hint at or let a reader infer $infer?"

  /** A present signal's question: "Does `text` state {infer}?" */
  def present(infer: String): String = s"Does `text` state $infer?"

  /** `answers` to a screen of `shape`, as recorded, read again: `Unreadable`, naming positions
    * alone, unless one yes/no each, in order, each a probability, or when `shape` has no
    * signal, as no screen does.
    */
  def read(shape: Shape, answers: Vector[Answer]): Either[ClassifierError, Screened] =
    if (shape.musts.isEmpty) {
      Left(ClassifierError.Unreadable("a screen of no signal"))
    } else {
      probabilities(shape.floor + shape.musts.size, answers).map { ps =>
        val (floor, signals) = ps.splitAt(shape.floor)
        Screened(shape, floor, signals)
      }
    }

  /** As [[read]], for a baseline of `floor` questions. */
  def readBaseline(floor: Int, answers: Vector[Answer]): Either[ClassifierError, Baseline] =
    probabilities(floor, answers).map(Baseline(_))

  private def question(signal: Signal): String = signal.must match {
    case Must.Absent => absent(signal.infer)
    case Must.Present => present(signal.infer)
  }

  private def yesNos(first: String, rest: Vector[String]): Ask[Text, Vector[Double]] =
    Ask.all(Ask.yesNo[Text](first, None, None), rest.map(Ask.yesNo[Text](_, None, None)))

  /** One probability of yes per question, by position; `Unreadable` naming positions alone. */
  private def probabilities(
      questions: Int,
      answers: Vector[Answer]
  ): Either[ClassifierError, Vector[Probability]] =
    if (answers.size != questions) {
      Left(ClassifierError.Unreadable(s"${answers.size} answers to $questions questions"))
    } else {
      Traversal.traverse(answers.zipWithIndex) {
        case (Answer.YesNo(yes), at) =>
          Probability.of(yes).toRight(unreadable(at, "is not a probability"))
        case (_, at) => Left(unreadable(at, "is not a yes/no"))
      }
    }

  private def unreadable(at: Int, why: String): ClassifierError =
    ClassifierError.Unreadable(s"answer ${at + 1} $why")
}
