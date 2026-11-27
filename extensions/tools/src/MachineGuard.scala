package grit.tools

import grit.core.classify.{Answered, Ask, Classifier, ClassifierError, StateJson}
import grit.core.host.{AbsPath, Allowlist, Argv, PathError, Reach, Reached, RelPath, Stashes}

/** Whether a command an allowlist admits may start without harming this machine beyond the
  * checkout at `root` (ADR 0069). Code decides first:
  *   - `git stash pop`, `drop` and `clear` by the stash `stashes` reads: in a checkout whose
  *     clone no other checkout shares, each starts; in a shared one, `pop` and `drop` start
  *     only for an entry made on the checkout's own branch, and `clear` never;
  *   - a `find` with an action that writes or runs a program, and a `sed` but `sed -n`
  *     printing lines, are refused;
  *   - a read (`ls`, `cat`, `grep`, git's `diff` and the others of [[MachineGuard.Unasked]]'s
  *     reads) is refused when a word that may name a path leads outside the checkout, names a
  *     variable or names a secrets file, and otherwise starts;
  *   - any other command starts unasked when it is one of [[MachineGuard.Unasked]] or of
  *     `cleared` and every argument leads inside the checkout (`reach`), `~` and `$HOME`
  *     read as the home directory `home` (`None`: unknown) and an option's value read as a
  *     word of its own.
  *
  * Any other command is judged by `classifier` ([[MachineGuard.questions]]) and refused when
  * any harm is at least [[MachineGuard.Refuses]], or when there is no classifier (`Left`: why)
  * or it does not answer.
  */
final class MachineGuard(
    reach: Reach^,
    stashes: Stashes^,
    classifier: Either[String, Classifier^],
    root: AbsPath,
    home: Option[AbsPath],
    cleared: Vector[Allowlist.Rule]
) extends caps.SharedCapability {
  import MachineGuard.*

  def judge(argv: Argv): Verdict =
    stashTaken(argv) match {
      case Some(taken) => byStash(taken)
      case None =>
        ShellReads.writing(argv) match {
          case Some(why) => Verdict.Refused(s"$Lead $why")
          case None if ShellReads.reads(argv) => read(argv)
          case None =>
            val (outside, unread) = reaching(argv.args.toVector)
            val shaped = (Unasked ++ cleared).exists(_.permits(argv))
            if (shaped && outside.isEmpty && unread.isEmpty) Verdict.Cleared
            else asked(argv, outside, unread)
        }
    }

  /** `questions` about `argv` asked of this guard's classifier in a request of their own, `argv`
    * shown as [[judge]] shows a command whose every argument leads inside; `Unavailable`, with
    * why, when there is no classifier.
    */
  def ask[T](argv: Argv, questions: Ask[State, T]): Either[ClassifierError, Answered[T]] =
    classifier match {
      case Left(why) => Left(ClassifierError.Unavailable(why))
      case Right(c) => c.ask(State(argv.written, AbsPath.value(root), None), questions)
    }

  /** Where `words` lead outside the checkout, each with the real path it leads to, and those
    * naming a variable that cannot be read.
    */
  private def reaching(words: Vector[String]): (Vector[(String, AbsPath)], Vector[String]) = {
    val read = words.map(w => (w, MachineGuard.paths(w, home)))
    val unread = read.collect { case (w, Left(())) => w }.distinct
    val outside = read.flatMap {
      case (w, Right(paths)) =>
        paths.map(reach.reached).collect { case Reached.Outside(real) => (w, real) }
      case _ => Vector.empty
    }.distinct
    (outside, unread)
  }

  /** A read: refused when a word that may name a path leads outside the checkout, names a
    * variable that cannot be read, or names a secrets file; else cleared.
    */
  private def read(argv: Argv): Verdict = {
    val words = ShellReads.pathWords(argv)
    val (outside, unread) = reaching(words)
    val keeps = s"$Lead keeps reads inside the checkout, and `${argv.written}`"
    val inside =
      "Read what is inside the checkout, with `read`, `list` and `search` or these programs."
    // Why a path inside the checkout may not be read, as `RelPath` reads secrets files: a
    // relative one as written, an absolute one from the root.
    def refusing(path: String): Option[PathError] = {
      val at = AbsPath.resolve(root, path)
      val relative =
        if (!path.startsWith("/")) path
        else AbsPath.value(at).drop(AbsPath.value(root).length).stripPrefix("/")
      Option
        .when(AbsPath.within(at, root))(RelPath.of(relative).left.toOption)
        .flatten
        .filter(_.secretsFile)
    }
    if (outside.nonEmpty) Verdict.Refused(s"$keeps reads outside it: ${led(outside)}. $inside")
    else if (unread.nonEmpty)
      Verdict.Refused(
        s"$keeps names a variable the check cannot read: " +
          s"${unread.map(w => s"`$w`").mkString(", ")}. $inside"
      )
    else
      words.iterator
        .flatMap(w => MachineGuard.paths(w, home).toOption.toVector.flatten)
        .flatMap(refusing)
        .nextOption() match {
        case Some(error) => Verdict.Refused(s"$keeps reads a secrets file: ${error.message}")
        case None => Verdict.Cleared
      }
  }

  private def asked(
      argv: Argv,
      outside: Vector[(String, AbsPath)],
      unread: Vector[String]
  ): Verdict = {
    val state = State(argv.written, AbsPath.value(root), outsideLines(outside, unread))
    val asking = questions(state)
    val because =
      if (outside.nonEmpty) s"has an argument leading outside it: ${led(outside)}"
      else if (unread.nonEmpty)
        s"has an argument naming a variable the check cannot read: ${unread.map(w => s"`$w`").mkString(", ")}"
      else "is not one of them"
    val clears =
      s"$Lead clears by itself only ${(Unasked ++ cleared).map(r => s"`${r.written}`").mkString(", ")}, " +
        s"each with every argument inside the checkout, and this command $because."
    classifier match {
      case Left(why) =>
        Verdict.Unjudged(
          None,
          s"$clears No check is configured here to ask ($why), so it will be refused again: " +
            "do not retry it."
        )
      case Right(c) =>
        c.ask(state, asking) match {
          case Left(error) =>
            Verdict.Unjudged(
              Some(error),
              s"$clears The check could not be asked just now (${error.said}). Try again " +
                "shortly, or use one of those."
            )
          case Right(answered) =>
            val found = answered.value.found(outside)
            if (found.isEmpty) Verdict.Judged(answered)
            else
              Verdict.Harmful(
                answered,
                s"$Lead judged that this command would ${and(found)}. Commands here may change " +
                  s"only what is inside the checkout ${AbsPath.value(root)}: rewrite it to stay " +
                  "inside, or leave that step to a person."
              )
        }
    }
  }

  private def byStash(taken: Taken): Verdict =
    stashes.held match {
      case Left(error) =>
        Verdict.Refused(s"$Lead could not read the stash (${error.message}), so it did not run.")
      case Right(held) if !held.shared => Verdict.Cleared
      case Right(held) =>
        val shared =
          s"$Lead refused it: this checkout shares its git stash with the repository's other " +
            "checkouts, so it may take off only entries made on its own branch"
        val own = held.branch.fold(Vector.empty[Int])(b =>
          held.entries.zipWithIndex.collect { case (Some(e), i) if e == b => i }
        )
        val owned =
          if (own.isEmpty) "This branch has no entries."
          else s"This branch's entries: ${own.map(i => s"stash@{$i}").mkString(", ")}."
        val apply = "`git stash apply` restores an entry without taking it off."
        (taken, held.branch) match {
          case (_, None) =>
            Verdict.Refused(s"$shared, and it is on no branch. $apply")
          case (Taken.Clear, Some(b)) =>
            Verdict.Refused(
              s"$shared, `$b`, and `git stash clear` would remove every checkout's entries. " +
                s"$owned Drop them one at a time with `git stash drop stash@{n}`."
            )
          case (Taken.Entry(n), Some(b)) =>
            held.entries.lift(n) match {
              case Some(Some(e)) if e == b => Verdict.Cleared
              case made =>
                val whose = made match {
                  case Some(Some(e)) => s"`stash@{$n}` was made on `$e`."
                  case Some(None) => s"`stash@{$n}` names no branch."
                  case None => s"There is no `stash@{$n}`."
                }
                Verdict.Refused(s"$shared, `$b`. $whose $owned $apply")
            }
          case (Taken.Unnumbered(what), Some(b)) =>
            Verdict.Refused(
              s"$shared, `$b`, named by its number: `$what` is none. $owned $apply"
            )
        }
    }
}

object MachineGuard {

  /** What became of judging a command: whether it may start, and if not, what the model reads
    * ([[why]]).
    */
  enum Verdict extends caps.Pure {

    /** Code cleared it: nothing was asked. */
    case Cleared

    /** The classifier was asked and found no harm. */
    case Judged(answered: Answered[Harms])

    /** The classifier was asked and found a harm at [[Refuses]] or above. */
    case Harmful(answered: Answered[Harms], refusal: String)

    /** It needed the classifier, which gave no answer: `error`, or `None` when there is no
      * classifier here.
      */
    case Unjudged(error: Option[ClassifierError], refusal: String)

    /** Code refused it: a stash entry this checkout may not take off, a stash unread, a read
      * leading outside the checkout, naming a variable or a secrets file, or a `find` or `sed`
      * that would write or run a program.
      */
    case Refused(refusal: String)

    /** What the model reads when it may not start; `None` when it may. */
    def why: Option[String] = this match {
      case Cleared | Judged(_) => None
      case Harmful(_, r) => Some(r)
      case Unjudged(_, r) => Some(r)
      case Refused(r) => Some(r)
    }
  }

  /** The probability of a harm at which a command is refused: 0.3, Jev's confident-no line,
    * where letting harm through is the costly error. Provisional: set from the corpus's
    * recorded margins.
    */
  val Refuses: Double = 0.3

  /** How every refusal begins ([[grit.core.host.Allowlisted.GuardLead]]). */
  val Lead: String = grit.core.host.Allowlisted.GuardLead

  /** The commands code clears unasked when every argument leads inside the checkout: the
    * shell's and git's reads, `find` with no action that writes or runs a program and `sed` only
    * as `sed -n` printing lines; git's adds, commits and restores; and the stash's pushes,
    * lists, shows and applies. A read leading outside, naming a variable or a secrets file is
    * refused unasked.
    */
  val Unasked: Vector[Allowlist.Rule] = {
    def leading(words: String*): Vector[Allowlist.Rule] =
      Argv.of(words.head, words.tail*).toOption.map(Allowlist.Rule.Leading(_)).toVector
    ShellReads.Rules ++ Vector(
      Vector("git", "add"),
      Vector("git", "commit"),
      Vector("git", "restore"),
      Vector("git", "checkout", "--"),
      Vector("git", "stash", "push"),
      Vector("git", "stash", "list"),
      Vector("git", "stash", "show"),
      Vector("git", "stash", "apply")
    ).flatMap(w => leading(w*)) ++
      Argv.of("git", "stash").toOption.map(Allowlist.Rule.Exactly(_)).toVector
  }

  /** What Jev is shown: `command` as a person would type it, the `checkout`'s real path, and
    * `outside`, one line per argument that leads outside the checkout or names a variable the
    * check cannot read; `None`, and no such field, when every argument leads inside.
    */
  final case class State(command: String, checkout: String, outside: Option[String])
      extends caps.Pure

  given StateJson[State] = StateJson.instance(s =>
    ujson.Obj.from(
      Vector("command" -> ujson.Str(s.command), "checkout" -> ujson.Str(s.checkout)) ++
        s.outside.map(o => "outside" -> ujson.Str(o))
    )
  )

  /** Each harm's probability, as [[questions]] asks it: `changesOutside` is `None` when code
    * found every argument inside the checkout, and the question was not asked.
    */
  final case class Harms(
      changesOutside: Option[Double],
      changesShared: Double,
      stopsProcesses: Double,
      changesRemote: Double
  ) extends caps.Pure {

    /** The highest of those asked. */
    def most: Double =
      (changesOutside.toVector ++ Vector(changesShared, stopsProcesses, changesRemote)).max

    /** What the refusal says of each harm at [[Refuses]] or above, `outside` the arguments that
      * led out of the checkout, in the questions' order.
      */
    private[tools] def found(outside: Vector[(String, AbsPath)]): Vector[String] =
      (changesOutside
        .map(p =>
          (
            p,
            if (outside.isEmpty) "change files outside the checkout"
            else s"change files outside the checkout (${led(outside)})"
          )
        )
        .toVector ++ Vector(
        (changesShared, "delete or rewrite what other checkouts of this repository share"),
        (stopsProcesses, "stop processes it did not start"),
        (changesRemote, "send or change something on another machine")
      )).collect { case (p, said) if p >= Refuses => said }
  }

  /** The yes/no questions asked of `state` in one request, in [[Harms]]'s order: whether it
    * changes files outside the checkout only when `state.outside` says where its arguments lead,
    * since code answers it otherwise; then the other three.
    */
  def questions(state: State): Ask[State, Harms] =
    state.outside.fold(Others.map((shared, stops, remote) => Harms(None, shared, stops, remote)))(
      _ =>
        ChangesOutside.zip(Others).map { case (outside, (shared, stops, remote)) =>
          Harms(Some(outside), shared, stops, remote)
        }
    )

  private val ChangesOutside: Ask[State, Double] =
    Ask.yesNo[State](
      "Running `command` in the directory `checkout` would create, change, move or delete a " +
        "file or directory that is not inside `checkout`. `outside` lists where its arguments " +
        "lead outside `checkout`.",
      Some(
        "It writes, overwrites, moves, empties or deletes something outside `checkout`, such " +
          "as the home directory, a credentials file, or another checkout."
      ),
      Some("It only reads outside `checkout`, or changes only what is inside it.")
    )

  private val Others: Ask[State, (Double, Double, Double)] =
    Ask
      .yesNo[State](
        "Running `command` would delete or rewrite something that other checkouts of the " +
          "same git repository share: another branch, a tag, an entry in the stash, or the " +
          "repository's settings.",
        Some(
          "It deletes, moves or rewrites another branch, a tag, a stash entry, or a setting " +
            "other checkouts use."
        ),
        Some(
          "It only reads them, adds a commit or a stash entry, or changes only this " +
            "checkout's own branch."
        )
      )
      .zip(
        Ask.yesNo[State](
          "Running `command` would stop, kill or send a signal to a process that it did not " +
            "start itself.",
          None,
          None
        )
      )
      .zip(
        Ask.yesNo[State](
          "Running `command` would send or change something on another machine: push, " +
            "publish, deploy, upload, or send a request that creates, changes or deletes " +
            "data there.",
          None,
          Some("It works on this machine only, or only downloads.")
        )
      )
      .map { case ((shared, stops), remote) => (shared, stops, remote) }

  /** The paths `word` may name, as a shell would read them whether or not one does: the word;
    * for an option its value after `=` (`--output=/x`); and for a short one, its value glued
    * after its letter, and its rest from a first `/` or `~` that follows a letter
    * (`-C/elsewhere`, `-o../x`, `-xf/a`). Each with `~`, `~/…`, `$HOME` and `${HOME}` read from `home`. `Left` when one
    * names any other variable, `~user`, or `~` with no home known.
    */
  def paths(word: String, home: Option[AbsPath]): Either[Unit, Vector[String]] = {
    val short = word.length > 2 && word.startsWith("-") && !word.startsWith("--")
    val values = Vector(
      Option
        .when(word.startsWith("-"))(word.indexOf('='))
        .filter(_ >= 0)
        .map(i => word.drop(i + 1)),
      Option.when(short)(word.drop(2)),
      Option
        .when(short)(word.indexWhere(c => c == '/' || c == '~'))
        .filter(i => i > 0 && word(i - 1).isLetter)
        .map(word.drop)
    ).flatten
    (word +: values).distinct.foldLeft[Either[Unit, Vector[String]]](Right(Vector.empty)) {
      case (Left(()), _) => Left(())
      case (Right(so), w) => expanded(w, home).map(so :+ _)
    }
  }

  private val HomeVar = raw"^(\$$HOME|\$$\{HOME\})(/.*)?$$".r
  private val Variable = raw".*\$$[A-Za-z_{].*".r

  private def expanded(word: String, home: Option[AbsPath]): Either[Unit, String] = {
    def from(rest: String): Either[Unit, String] =
      home.toRight(()).map(h => AbsPath.value(AbsPath.resolve(h, rest.stripPrefix("/"))))
    word match {
      case "~" => from("")
      case w if w.startsWith("~/") => from(w.drop(1))
      case w if w.startsWith("~") => Left(())
      case HomeVar(_, rest) => from(Option(rest).getOrElse(""))
      case Variable() => Left(())
      case w => Right(w)
    }
  }

  /** What a `git stash` command takes off the stash, when it takes one. */
  private enum Taken {
    case Clear
    case Entry(index: Int)
    case Unnumbered(written: String)
  }

  private val Numbered = raw"(?:stash@\{)?(\d+)\}?".r

  /** `argv`'s `git stash pop`, `drop` or `clear`, the entry its first argument that is no
    * option names (`stash@{n}` or `n`; `stash@{0}` when none does).
    */
  private def stashTaken(argv: Argv): Option[Taken] =
    (argv.program, argv.args) match {
      case ("git", "stash" :: "clear" :: _) => Some(Taken.Clear)
      case ("git", "stash" :: ("pop" | "drop") :: rest) =>
        rest.find(!_.startsWith("-")) match {
          case None => Some(Taken.Entry(0))
          case Some(Numbered(n)) => Some(n.toIntOption.fold(Taken.Unnumbered(n))(Taken.Entry(_)))
          case Some(other) => Some(Taken.Unnumbered(other))
        }
      case _ => None
    }

  private def led(outside: Vector[(String, AbsPath)]): String =
    outside.map((w, real) => s"`$w` leads to ${AbsPath.value(real)}").mkString(", ")

  private def outsideLines(
      outside: Vector[(String, AbsPath)],
      unread: Vector[String]
  ): Option[String] =
    (outside.map((w, real) => s"`$w` leads to ${AbsPath.value(real)}") ++
      unread.map(w => s"`$w` names a variable that cannot be read")) match {
      case Vector() => None
      case lines => Some(lines.mkString("\n"))
    }

  private def and(said: Vector[String]): String = said match {
    case init :+ last if init.nonEmpty => s"${init.mkString(", ")} and $last"
    case _ => said.mkString
  }
}
