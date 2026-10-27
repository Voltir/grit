package grit.core.screen

import grit.core.either.Traversal
import grit.core.prompt.Stripped

/** Whether a screened text must not let a reader infer a signal (`Absent`) or must state it
  * (`Present`); `written`, as checks, a bench row and a screening's record write it.
  */
enum Must(val written: String) extends caps.Pure {
  case Absent extends Must("absent")
  case Present extends Must("present")
}

/** A fact a screen looks for, `infer`, one line, and whether it must be absent or present. */
final case class Signal private[screen] (infer: String, must: Must) extends caps.Pure

/** One source's signals, in its order. */
final case class Signals private[screen] (first: Signal, rest: Vector[Signal]) extends caps.Pure {
  def all: Vector[Signal] = first +: rest
}

/** A steward's examples: passages a screen of the signals must pass, and leaks it must refuse. */
final case class Examples private[screen] (accept: Vector[String], refuse: Vector[String])
    extends caps.Pure

/** What a steward writes: `signals` and the `examples` they are benched on. */
final case class Checks private[screen] (signals: Signals, examples: Examples) extends caps.Pure {

  /** Its canonical JSON, each object's keys sorted, written without spaces: what a digest of
    * the checks is taken over.
    */
  def canonical: String = {
    def texts(ts: Vector[String]): ujson.Value = ujson.Arr.from(ts.map(ujson.Str(_)))
    ujson.write(
      ujson.Obj(
        "examples" -> ujson
          .Obj("accept" -> texts(examples.accept), "refuse" -> texts(examples.refuse)),
        "signals" -> ujson.Arr.from(signals.all.map { s =>
          ujson.Obj("infer" -> s.infer, "must" -> s.must.written)
        })
      )
    )
  }
}

object Checks {

  /** 16. */
  val MostSignals: Int = 16

  /** 300: an infer's most characters. */
  val InferChars: Int = 300

  /** 3: the most examples of each kind. */
  val MostExamples: Int = 3

  /** 2,000: an example's most characters. */
  val ExampleChars: Int = 2000

  /** Why checks do not read, by position (each counted from 0), holding nothing of the input. */
  enum Unreadable extends caps.Pure {

    /** No checks: NULL. */
    case Missing

    /** `part` is not of its shape, lacks a key or holds one not known. */
    case Shape(part: Part)

    /** `part` holds none, or more than `most`. */
    case Count(part: Part, most: Int)

    /** The signal at `at` is at fault. */
    case Signal(at: Int, fault: Fault)

    /** The example of `kind` at `at` is at fault. */
    case Example(kind: Benched.Kind, at: Int, fault: Fault)

    /** Its words, positions counted from 1: "signal 3: over 300 characters". */
    def said: String = this match {
      case Unreadable.Missing => "no checks"
      case Unreadable.Shape(part) => s"${partName(part)}: not of their shape"
      case Unreadable.Count(part, most) => s"${partName(part)}: none, or more than $most"
      case Unreadable.Signal(at, fault) => s"signal ${at + 1}: ${faultWords(fault, InferChars)}"
      case Unreadable.Example(kind, at, fault) =>
        val named = kind match {
          case Benched.Kind.Accept => "accept"
          case Benched.Kind.Refuse => "refuse"
        }
        s"$named example ${at + 1}: ${faultWords(fault, ExampleChars)}"
    }
  }

  private def partName(part: Part): String = part match {
    case Part.Checks => "checks"
    case Part.Signals => "signals"
    case Part.Examples => "examples"
    case Part.Accept => "accept"
    case Part.Refuse => "refuse"
  }

  private def faultWords(fault: Fault, most: Int): String = fault match {
    case Fault.Blank => "blank"
    case Fault.Lines => "more than one line"
    case Fault.Long => s"over $most characters"
    case Fault.Must => "must is neither absent nor present"
    case Fault.NotText => "not text"
  }

  /** Where in the checks a shape or count fault lies. */
  enum Part extends caps.Pure {
    case Checks, Signals, Examples, Accept, Refuse
  }

  /** What is wrong with one signal or example: blank, more than one line (an infer), over its
    * most characters, a `must` neither `absent` nor `present`, or not text.
    */
  enum Fault extends caps.Pure {
    case Blank, Lines, Long, Must, NotText
  }

  /** The checks `json` holds, `{"signals": [{"infer", "must"}…], "examples": {"accept": […],
    * "refuse": […]}}`, `must` `absent` or `present`, bounded as above and at least one of each;
    * or why not, the first fault in that order. Each `infer` is read as
    * [[grit.core.prompt.Stripped.of]] leaves it, before it is bounded. `None`, or JSON `null`,
    * is `Missing`.
    */
  def read(json: Option[ujson.Value]): Either[Unreadable, Checks] =
    json.filterNot(_.isNull).toRight(Unreadable.Missing).flatMap { j =>
      for {
        top <- keyed(j, Set("signals", "examples"), Part.Checks)
        signals <- listed(top.get("signals"), Part.Signals, MostSignals)
        read <- Traversal.traverse(signals.zipWithIndex)(signal)
        first <- read.headOption.toRight(Unreadable.Count(Part.Signals, MostSignals))
        examples <- keyed(
          top.getOrElse("examples", ujson.Null),
          Set("accept", "refuse"),
          Part.Examples
        )
        accept <- texts(examples.get("accept"), Benched.Kind.Accept, Part.Accept)
        refuse <- texts(examples.get("refuse"), Benched.Kind.Refuse, Part.Refuse)
      } yield Checks(Signals(first, read.drop(1)), Examples(accept, refuse))
    }

  /** `json`'s fields when it is an object of exactly `keys`. */
  private def keyed(
      json: ujson.Value,
      keys: Set[String],
      part: Part
  ): Either[Unreadable, collection.Map[String, ujson.Value]] =
    json.objOpt.filter(_.keySet == keys).toRight(Unreadable.Shape(part))

  /** `json`'s items when it is a list of 1 to `most`. */
  private def listed(
      json: Option[ujson.Value],
      part: Part,
      most: Int
  ): Either[Unreadable, Vector[ujson.Value]] =
    json
      .flatMap(_.arrOpt)
      .map(_.toVector)
      .toRight(Unreadable.Shape(part))
      .filterOrElse(items => items.nonEmpty && items.size <= most, Unreadable.Count(part, most))

  private def signal(json: ujson.Value, at: Int): Either[Unreadable, Signal] =
    for {
      fields <- keyed(json, Set("infer", "must"), Part.Signals)
      shown = fields.get("infer").map(v => v.strOpt.fold(v)(s => ujson.Str(Stripped.of(s))))
      infer <- text(shown, InferChars, lines = false).left
        .map(Unreadable.Signal(at, _))
      must <- fields
        .get("must")
        .flatMap(_.strOpt)
        .flatMap(m => Must.values.find(_.written == m))
        .toRight(Unreadable.Signal(at, Fault.Must))
    } yield Signal(infer, must)

  private def texts(
      json: Option[ujson.Value],
      kind: Benched.Kind,
      part: Part
  ): Either[Unreadable, Vector[String]] =
    listed(json, part, MostExamples).flatMap { items =>
      Traversal.traverse(items.zipWithIndex) { (item, at) =>
        text(Some(item), ExampleChars, lines = true).left.map(Unreadable.Example(kind, at, _))
      }
    }

  /** `json` as text that is not blank and holds at most `most` characters, and, unless `lines`,
    * one line.
    */
  private def text(json: Option[ujson.Value], most: Int, lines: Boolean): Either[Fault, String] =
    json.flatMap(_.strOpt).toRight(Fault.NotText).flatMap { s =>
      if (s.isBlank) Left(Fault.Blank)
      else if (!lines && s.exists(c => c == '\n' || c == '\r')) Left(Fault.Lines)
      else if (s.codePointCount(0, s.length) > most) Left(Fault.Long)
      else Right(s)
    }
}
