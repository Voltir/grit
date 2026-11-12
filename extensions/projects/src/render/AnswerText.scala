package grit.projects.render

import java.nio.charset.StandardCharsets
import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.FiniteDuration

import grit.core.claim.{Claim, ClaimRole}
import grit.core.id.ClaimHolder
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.name.{ArtifactKey, KindName, StateName}

/** What the projects plugin's commands answer, the same for every edge that asks. */
object AnswerText {

  /** One line per head, in the order given: `<key> (<kind>, <state>): <title>`; `none` when
    * there is no head.
    */
  def heads(heads: Vector[ArtifactHead], none: String): String =
    if (heads.isEmpty) none else heads.map(head(_, kind = true, state = true)).mkString("\n")

  /** What a [[listed]] line shows besides its key, its title, its size and the day it changed:
    * its kind and its state when `kind` and `state` (false when the asker fixed it), its parent
    * unless it is `besides`, and its body's [[Body.lead]] when `opening`.
    */
  final case class LineParts(
      kind: Boolean,
      state: Boolean,
      besides: Option[ArtifactKey],
      opening: Boolean
  ) extends caps.Pure

  /** What `project_list` answers of `rows`, every artifact that matched, in the order given.
    *
    * A first line counts them, `<n> match: <kind> <n> (<state> <n>, …); …`, each kind and each
    * state most first, then by name: only the states when `parts` shows no kind, only the
    * kinds when it shows no state, neither when it shows neither; then, when `leftOut` (the
    * ended artifacts that would otherwise have matched) is above 0, `<leftOut> ended ones not
    * listed; ended: true lists them.` (for one, `1 ended one not listed; ended: true lists
    * it.`). Then, past the first `skip` rows, at most `most`, one line each:
    * `<key> (<kind>, <state>): <title> · under <parent> · <size> KB · changed <day> · <beneath>
    * under it · <lead>`, leaving out what `parts` does not show, the parent when there is none,
    * and `beneath` when it is 0; `<size>` is its markdown's UTF-8 bytes in KiB, rounded up to a
    * tenth, `<day>` the UTC date it was `written`. When rows are left past them, a last line:
    * `<n> more not shown; skip: <next> shows the next, or <narrow>.` With `skip` at or past the
    * end, `No line past <n>.` in place of the rows. With no row, the one line `No artifact
    * matches.`, then the ended count when above 0.
    */
  def listed(
      rows: Vector[(artifact: Artifact, written: Instant, beneath: Int)],
      parts: LineParts,
      skip: Int,
      most: Int,
      leftOut: Int,
      narrow: String
  ): String = {
    val ended =
      if (leftOut == 0) ""
      else if (leftOut == 1) " 1 ended one not listed; ended: true lists it."
      else s" $leftOut ended ones not listed; ended: true lists them."
    if (rows.isEmpty) s"No artifact matches.$ended"
    else {
      val heads = rows.map(_.artifact.head)
      val counted = (parts.kind, parts.state) match {
        case (false, false) => ""
        case (false, true) => s": ${states(heads)}"
        case (true, false) =>
          ": " + split(heads, h => KindName.value(h.kind))
            .map((k: String, hs: Vector[ArtifactHead]) => s"$k ${hs.size}")
            .mkString("; ")
        case (true, true) => s": ${kindsAndStates(heads)}"
      }
      val lines = rows
        .map { r =>
          val h = r.artifact.head
          val bytes = Body.markdown(r.artifact.body).getBytes(StandardCharsets.UTF_8).length.toLong
          val tenths = math.max(1L, (bytes * 10L + 1023L) / 1024L)
          (Vector(head(h, parts.kind, parts.state)) ++
            h.parent.filterNot(parts.besides.contains).map(p => s"under ${p.value}") ++
            Vector(
              s"${tenths / 10}.${tenths % 10} KB",
              s"changed ${r.written.atZone(ZoneOffset.UTC).toLocalDate}"
            ) ++
            Option.when(r.beneath > 0)(s"${r.beneath} under it") ++
            Option.when(parts.opening)(Body.lead(r.artifact.body)).filter(_.nonEmpty))
            .mkString(" · ")
        }
      (s"${rows.size} match$counted.$ended" +:
        paged(lines, skip, most, next => s"; skip: $next shows the next, or $narrow."))
        .mkString("\n")
    }
  }

  /** The lines of `lines` after the first `skip`, at most `most`; then, when lines are left past
    * them, `<n> more not shown` and the words `after` gives for the `skip` that shows the next.
    * With `skip` above 0 and at or past the end, the one line `No line past <n>.`
    */
  private def paged(
      lines: Vector[String],
      skip: Int,
      most: Int,
      after: Int -> String
  ): Vector[String] =
    if (skip > 0 && skip >= lines.size) Vector(s"No line past ${lines.size}.")
    else {
      val page = lines.slice(skip, skip + most)
      val left = lines.size - skip - page.size
      page ++ Option.when(left > 0)(s"$left more not shown${after(skip + page.size)}")
    }

  /** Each name `name` gives among `among` and the heads it gives it to, most first, then by name. */
  private def split(
      among: Vector[ArtifactHead],
      name: ArtifactHead -> String
  ): Vector[(String, Vector[ArtifactHead])] =
    among
      .groupBy(name)
      .toVector
      .sortBy((n: String, hs: Vector[ArtifactHead]) => (-hs.size, n))

  /** `<state> <n>, …`, most first. */
  private def states(among: Vector[ArtifactHead]): String =
    split(among, h => StateName.value(h.state))
      .map((s: String, hs: Vector[ArtifactHead]) => s"$s ${hs.size}")
      .mkString(", ")

  /** `<kind> <n> (<state> <n>, …); …`, most first. */
  private def kindsAndStates(among: Vector[ArtifactHead]): String =
    split(among, h => KindName.value(h.kind))
      .map((k: String, hs: Vector[ArtifactHead]) => s"$k ${hs.size} (${states(hs)})")
      .mkString("; ")

  /** That what was withdrawn stays readable `readable` long, in whole days. */
  private def readableFor(readable: FiniteDuration): String =
    s"Their history stays readable for ${readable.toDays} days: project_show with an `at` " +
      "before now, and project_history."

  /** `<key> (<kind>, <state>): <title>`, the kind and the state each only when asked for, the
    * parentheses only when either is.
    */
  private def head(h: ArtifactHead, kind: Boolean, state: Boolean): String = {
    val named =
      Option.when(kind)(KindName.value(h.kind)) ++ Option.when(state)(StateName.value(h.state))
    s"${h.key.value}${if (named.isEmpty) "" else named.mkString(" (", ", ", ")")}: ${h.title}"
  }

  /** One line per version, in the order given: `<version> <written> by <by> (<state>): <title>`,
    * leaving out `by <by> ` when the head records no writer, and `<version> <written> withdrawn`
    * for a version with no head; `none` when there is none.
    */
  def versions(
      versions: Vector[(version: Long, written: Instant, head: Option[ArtifactHead])],
      none: String
  ): String =
    if (versions.isEmpty) none
    else
      versions
        .map(v =>
          v.head.fold(s"${v.version} ${v.written} withdrawn")(h =>
            s"${v.version} ${v.written} ${h.by.fold("")(b => s"by $b ")}" +
              s"(${StateName.value(h.state)}): ${h.title}"
          )
        )
        .mkString("\n")

  /** A line diff of `older` to `newer`, two texts named `from` and `to`: a `--- <from>` and a
    * `+++ <to>` line, then every line of either in order, `- ` before one only `older` holds,
    * `+ ` before one only `newer` holds and two spaces before one both hold, each removal before
    * the addition replacing it.
    */
  def diff(from: String, older: String, to: String, newer: String): String = {
    val (a, b) = (older.linesIterator.toVector, newer.linesIterator.toVector)
    // longest(i)(j): the longest common subsequence of a.drop(i) and b.drop(j).
    val longest = Array.ofDim[Int](a.size + 1, b.size + 1)
    for (i <- a.indices.reverse; j <- b.indices.reverse)
      longest(i)(j) =
        if (a(i) == b(j)) longest(i + 1)(j + 1) + 1
        else longest(i + 1)(j).max(longest(i)(j + 1))
    val out = Vector.newBuilder[String]
    var (i, j) = (0, 0)
    while (i < a.size || j < b.size)
      if (i < a.size && j < b.size && a(i) == b(j)) { out += s"  ${a(i)}"; i += 1; j += 1 }
      else if (j == b.size || (i < a.size && longest(i + 1)(j) >= longest(i)(j + 1))) {
        out += s"- ${a(i)}"; i += 1
      } else { out += s"+ ${b(j)}"; j += 1 }
    (Vector(s"--- $from", s"+++ $to") ++ out.result()).mkString("\n")
  }

  /** What a withdrawal answers: a `Withdrawn: ` line naming `gone`, in the order given; a
    * `Citations of them dropped from: ` line naming `uncited` when there are any; and that their
    * history stays readable `readable` long, in whole days, through `project_show` and
    * `project_history`.
    */
  def withdrawn(
      gone: Vector[ArtifactKey],
      uncited: Vector[ArtifactKey],
      readable: FiniteDuration
  ): String =
    (Vector(s"Withdrawn: ${gone.map(_.value).mkString(", ")}") ++
      Option.when(uncited.nonEmpty)(
        s"Citations of them dropped from: ${uncited.map(_.value).mkString(", ")}"
      ) :+
      readableFor(readable)).mkString("\n")

  /** Which lines of a sweep's answer list: those that stayed, were rewritten, or were withdrawn;
    * `word`, what `project_sweep`'s `show` names it by.
    */
  enum SweptGroup(val word: String) extends caps.Pure {
    case Stayed extends SweptGroup("stayed")
    case Rewritten extends SweptGroup("rewritten")
    case Withdrawn extends SweptGroup("withdrawn")
  }

  /** What a sweep answers of `swept`, before it is done when `dry`, else after: `Withdrew <n>:
    * <kind> <n> (<state> <n>, …); …`, or `Withdrew none.`; `Citations of them dropped from <n>.`
    * when any was rewritten; `<n> old enough stayed: <reason> <n>; …` when any stayed, reasons
    * most first, then by name; these count everything, whatever `show` and `skip`. Then the
    * lines of `show`'s group, or of all three in that order when `None`: those that stayed
    * (`<key> (<kind>, <state>): <title> · stays: <why>`, naming each other artifact with its kind
    * and state), those rewritten (`… · rewritten without its citations of what went`), those
    * withdrawn (`… · withdrawn`), each group by key; past the first `skip`, at most `most`, then
    * `<n> more not shown.` when lines are left past them, or `No line past <n>.` alone when `skip`
    * is at or past the end; and, when something went, that their history stays readable
    * `readable` long, in whole days. With `dry`, a first line `Dry run: nothing withdrawn or
    * changed.`, the counts `Would withdraw` and `Would drop citations of them from`, the lines
    * ending `would be rewritten without its citations of what goes` and `would be withdrawn`, a
    * cut line `<n> more not shown; skip: <next> shows the next, or show one of stayed,
    * rewritten, withdrawn.` (without its `show` words when `show` is given), and no line on
    * history.
    */
  def swept(
      swept: Project.Swept,
      dry: Boolean,
      show: Option[SweptGroup],
      skip: Int,
      most: Int,
      readable: FiniteDuration
  ): String = {
    val gone = swept.withdrawn.gone
    val uncited = swept.withdrawn.uncited
    def other(h: ArtifactHead): String =
      s"${h.key.value} (${KindName.value(h.kind)}, ${StateName.value(h.state)})"
    def why(s: Project.Stay): String = s match {
      case Project.Stay.Claimed(holder) => s"claimed for ${ClaimHolder.value(holder)}"
      case Project.Stay.Cited(first, rest) =>
        s"cited by ${(first +: rest).map(other).mkString(", ")}"
      case Project.Stay.Above(child) => s"${other(child)} under it stays"
    }
    def reason(s: Project.Stay): String = s match {
      case _: Project.Stay.Claimed => "claimed"
      case _: Project.Stay.Cited => "still cited"
      case _: Project.Stay.Above => "has one under it that stays"
    }
    def line(a: Artifact, end: String): String =
      s"${head(a.head, kind = true, state = true)} · $end"
    val withdrew =
      if (gone.isEmpty) "none." else s"${gone.size}: ${kindsAndStates(gone.map(_.head))}."
    val counts = Vector(if (dry) s"Would withdraw $withdrew" else s"Withdrew $withdrew") ++
      Option.when(uncited.nonEmpty)(
        if (dry) s"Would drop citations of them from ${uncited.size}."
        else s"Citations of them dropped from ${uncited.size}."
      ) ++
      Option.when(swept.stayed.nonEmpty)(
        s"${swept.stayed.size} old enough stayed: " +
          swept.stayed
            .map(s => reason(s.why))
            .groupMapReduce(identity)(_ => 1)(_ + _)
            .toVector
            .sortBy((r: String, n: Int) => (-n, r))
            .map((r: String, n: Int) => s"$r $n")
            .mkString("; ") + "."
      )
    def group(g: SweptGroup): Vector[String] = g match {
      case SweptGroup.Stayed => swept.stayed.map(s => line(s.artifact, s"stays: ${why(s.why)}"))
      case SweptGroup.Rewritten =>
        uncited.map(
          line(
            _,
            if (dry) "would be rewritten without its citations of what goes"
            else "rewritten without its citations of what went"
          )
        )
      case SweptGroup.Withdrawn =>
        gone.sortBy(_.head.key.value).map(line(_, if (dry) "would be withdrawn" else "withdrawn"))
    }
    val lines = show.fold(SweptGroup.values.toVector.flatMap(group))(group)
    val after = (next: Int) =>
      if (!dry) "."
      else if (show.isDefined) s"; skip: $next shows the next."
      else
        s"; skip: $next shows the next, or show one of " +
          s"${SweptGroup.values.map(_.word).mkString(", ")}."
    (Option.when(dry)("Dry run: nothing withdrawn or changed.").toVector ++ counts ++
      paged(lines, skip, most, after) ++
      Option.when(!dry && gone.nonEmpty)(readableFor(readable))).mkString("\n")
  }

  /** A claim made as a JSON object: `claimed`, the sentence `said`; `brief`, `kept` when its
    * brief was kept, else `none`, with `note`, the sentence `unkept` says why.
    */
  def claimed(said: String, unkept: Option[String]): String =
    ujson.write(
      ujson.Obj.from(
        Seq("claimed" -> ujson.Str(said), "brief" -> ujson.Str(unkept.fold("kept")(_ => "none"))) ++
          unkept.map(n => "note" -> ujson.Str(n))
      )
    )

  /** A freed claim as a JSON object a program reads: `released`, the artifact's key, and
    * `holder`, who it was claimed for, which no longer works it.
    */
  def released(key: ArtifactKey, holder: ClaimHolder): String =
    ujson.write(ujson.Obj("released" -> key.value, "holder" -> ClaimHolder.value(holder)))

  /** Live claims as a JSON object a program reads: `claims`, each its artifact's `key`, its
    * `role`, `holder`, who it is claimed for, `by`, who claimed it (written as given), `since`,
    * when it was made, and `lapses`, when it lapses unless its holder keeps working; in the order
    * given.
    */
  def claims(held: Vector[(ArtifactKey, Claim, String)]): String =
    ujson.write(
      ujson.Obj(
        "claims" -> ujson.Arr.from(held.map { (key, c, by) =>
          ujson.Obj(
            "key" -> key.value,
            "role" -> ClaimRole.value(c.role),
            "holder" -> ClaimHolder.value(c.holder),
            "by" -> by,
            "since" -> c.made.toString,
            "lapses" -> c.lapses.toString
          )
        })
      )
    )

  /** A page of export files as a JSON object a program reads: `files`, each its `path` and its
    * `text`, in order; and `next`, the path to ask for the page after from, or `null` on the
    * last page.
    */
  def page(files: Vector[(String, String)], next: Option[String]): String =
    ujson.write(
      ujson.Obj(
        "files" -> ujson.Arr.from(files.map((p, t) => ujson.Obj("path" -> p, "text" -> t))),
        "next" -> next.fold[ujson.Value](ujson.Null)(ujson.Str(_))
      )
    )
}
