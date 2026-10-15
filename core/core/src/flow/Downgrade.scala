package grit.core.flow

import scala.compiletime.ops.int.<=

import grit.core.durable.{Flow, Journaled, Step, StepName, Steps}
import grit.core.visibility.{Integrity, Label}

/** A downgrade (ADR 0035): a pure rule that raises a value's integrity (endorsement) or lowers
  * its classification (declassification), described so that [[grit.core.durable.Flows.downgrades]]
  * lists it. Unrecorded: a replay recomputes it from the recorded output it reads, so changing
  * its rule, ceiling or capacity changes that output's encoding, and needs a new epoch or a
  * `Durable.patch` (ADR 0004). Its rule runs only through a [[Raise]], which [[on]], [[step]]
  * and [[raise]] alone make. Two downgrades with one description are one.
  */
final class Downgrade[-A, +B] private[flow] (
    val described: Downgrade.Described[?],
    private[flow] val rule: A -> B
) extends Flow.Node {

  def shown: String = described.shown

  /** `s`, then this over what `of` takes from its recorded output, passed beside it: `None`
    * where it holds nothing to downgrade. Records only `s`.
    */
  def on[R, I, O](s: Step[R, I, O])(of: O -> Option[A]): Flow[R, I, Nothing, (O, Option[B])] =
    Flow.downgraded(s, this)(o => (o, of(o).map(rule)))

  /** A step whose body makes this downgrade through `raise`, which never leaves it: for one made
    * inside a body, such as a tool's call. Listed within the step.
    */
  def step[R, I, O](name: StepName[I])(
      body: (r: R^) -> (raise: Raise[A, B]^) ->{r} I ->{r, raise} O
  )(using Journaled[O]): Step[R, I, O] =
    Step.within[R, I, O](name, this)(r => in => body(r)(new Raise.Of(rule))(in))

  /** This downgrade, for a program's code: `None` unless the program whose steps these are
    * declared it in its `within`.
    */
  // Not `Raise[A, B]^{steps}`: a new capability is a root of its own, never narrowed to
  // another's set (docs/capture-checking.md). The fresh root is made inside the program's body,
  // so it is no more visible past the program than `steps` is (DowngradeCaptureTests).
  def raise(using steps: Steps[?]^): Option[Raise[A, B]^] =
    if (steps.declares(this)) Some(new Raise.Of(rule)) else None

  override def equals(that: Any): Boolean = that match {
    case d: Downgrade[?, ?] => d.described == described
    case _ => false
  }
  override def hashCode: Int = described.##
  override def toString: String = shown
}

/** One downgrade, made where it was handed. */
sealed abstract class Raise[-A, +B] extends caps.SharedCapability {
  def apply(a: A): B
}

object Raise {
  private[flow] final class Of[A, B](rule: A -> B) extends Raise[A, B] {
    def apply(a: A): B = rule(a)
  }
}

object Downgrade {

  /** What a downgrade does, for a reader: which lattice it moves a value on, who decides and how
    * far, and how much of its source's choosing passes. Made only by this package's constructors,
    * so no description pairs free text with a code rule above `Outside`.
    */
  final case class Described[L] private[flow] (
      name: String,
      axis: Axis[L],
      authority: Authority[L],
      capacity: Capacity
  ) {

    /** It in words, e.g. `extraction: raises integrity, by code, to at most [integrity:
      * member], at most 256 bits`.
      */
    def shown: String = {
      val (moves, decided) = axis match {
        case Axis.Integrity =>
          (
            "raises integrity",
            authority match {
              case Authority.Rule(c) => s"by code, to at most ${Integrity.shown(c)}"
              case Authority.Person => "by the answering person, to their own integrity"
            }
          )
        case Axis.Classification =>
          (
            "lowers classification",
            authority match {
              case Authority.Rule(c) => s"by code, to no lower than ${Label.shown(c)}"
              case Authority.Person => "by the answering person, to what their clearance reads"
            }
          )
      }
      val passes = capacity match {
        case Capacity.Bits(n) => s"at most $n bits"
        case Capacity.Text => "free text"
      }
      s"$name: $moves, $decided, $passes"
    }
  }

  /** The lattice a downgrade moves a value on (ADR 0035): integrity, raised; classification,
    * lowered.
    */
  enum Axis[L] {
    case Integrity extends Axis[grit.core.visibility.Integrity]
    case Classification extends Axis[Label]
  }

  /** Who decides a downgrade. `Rule`: code, never past `ceiling`. `Person`: the answering
    * person, at their own integrity; a person's downgrade is made on the step that recorded
    * their answer.
    */
  enum Authority[+L] {
    case Rule(ceiling: L)
    case Person
  }

  /** How much of its source's choosing a downgrade lets pass: at most `n` bits (`Bounded`), or
    * free text, which on the integrity axis reaches `Outside` at most and stays `Tainted`.
    * Classification's constructors, parked until a person approves a card, keep its rule there:
    * free text with a person deciding, or bits with a rule deciding; free text by a rule never.
    */
  enum Capacity {
    case Bits(n: Int)
    case Text
  }

  /** An extraction's endorsement: `answer`, or that there is none, trusted at `level` met with
    * `Member`, so an extraction never reaches `Authority`. Its capacity is the answer's bits
    * and one for whether there is one. Free text has no `Bounded`, so is never endorsed.
    */
  def extraction[T, B <: Int](using
      Bounded[T, B],
      (B <= Bounded.K) =:= true,
      ValueOf[B]
  ): Downgrade[(answer: Option[T], level: Integrity), Trusted[Option[T]]] =
    new Downgrade(
      Described(
        "extraction",
        Axis.Integrity,
        Authority.Rule(Integrity.Member),
        Capacity.Bits(valueOf[B] + 1)
      ),
      in => Trusted.at(in.answer, in.level.meet(Integrity.Member))
    )

  /** A text raised from below `Outside` to it, in the same compartments, when `judge` says so
    * of its raw form, its integrity and what came with it; passed beside what `judge` found.
    * Free text, so `Outside` is its ceiling and it stays `Tainted`.
    */
  private[core] def sanitiser[A, V](name: String)(
      judge: (String, Integrity, A) -> (V, Boolean)
  ): Downgrade[Tainted[(String, A)], (Tainted[String], V)] =
    new Downgrade(
      Described(name, Axis.Integrity, Authority.Rule(Integrity.Outside), Capacity.Text),
      t => {
        // The raw text reaches `judge` alone, which is pure: what leaves is its finding and one
        // bit, which raise the text to `Outside` at most, still tainted.
        val (text, a) = t.open
        val (found, raises) = judge(text, t.integrity, a)
        val at =
          if (raises && t.integrity.belowOutside) t.integrity.join(Integrity.Outside)
          else t.integrity
        (new Tainted(text, at), found)
      }
    )
}
