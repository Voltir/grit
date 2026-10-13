package grit.core.model

/** How hard a reasoning model is asked to think, from least to most. */
enum Effort {
  case Minimal, Low, Medium, High, XHigh, Max
}

/** Which pair does one job, with its output budget (`maxTokens`, at least 1) and the effort
  * asked of it (`None`: none is asked, and the model's default holds).
  */
final case class Assignment(ref: ModelRef, maxTokens: Int, effort: Option[Effort]) derives CanEqual

/** How strong a pair a call is made with, least first: a ladder's rungs are graded by it, and
  * a turn's effort stage asks for one.
  */
enum Grade derives CanEqual {
  case Light, Standard, Strong
}

/** A pair a role may use, at `grade`. */
final case class Rung(grade: Grade, assignment: Assignment) derives CanEqual

/** A role's rungs, in the order they are preferred. */
final case class Ladder private (first: Rung, rest: Vector[Rung]) derives CanEqual {
  def rungs: Vector[Rung] = first +: rest

  /** Each rung through `f`, in place. */
  def map(f: Rung => Rung): Ladder = Ladder(f(first), rest.map(f))

  /** This ladder with `rung` after its others, preferred least. */
  def :+(rung: Rung): Ladder = Ladder(first, rest :+ rung)
}

object Ladder {

  /** `rungs` as a ladder; `Left` when there are none. */
  def of(rungs: Vector[Rung]): Either[String, Ladder] = rungs match {
    case first +: rest => Right(Ladder(first, rest))
    case _ => Left("a ladder has no rungs")
  }

  /** One [[Grade.Standard]] rung: a role assigned one pair. */
  def one(assignment: Assignment): Ladder = Ladder(Rung(Grade.Standard, assignment), Vector.empty)
}

/** Which pairs do each job: answering the turn, summarising it, writing its search query,
  * and writing the closing of a period grit only heard (`heard`: nothing in it was said to
  * grit). A person's decision; a probe proposes settings of pairs, never an assignment.
  */
final case class Policy(turn: Ladder, summary: Ladder, query: Ladder, heard: Ladder)

object Policy {

  /** Each role assigned one pair ([[Ladder.one]]). */
  def one(turn: Assignment, summary: Assignment, query: Assignment, heard: Assignment): Policy =
    Policy(Ladder.one(turn), Ladder.one(summary), Ladder.one(query), Ladder.one(heard))
}

/** A catalog's content hash: equal content, equal version. */
opaque type CatalogVersion = String

object CatalogVersion {
  def apply(hash: String): CatalogVersion = hash
  def value(v: CatalogVersion): String = v
}

/** The policy, and every profile known, one per pair; identified by its content. */
final case class Catalog private (
    policy: Policy,
    profiles: Vector[Profile],
    version: CatalogVersion
) {

  /** The profile for `ref`, if there is one. */
  def profile(ref: ModelRef): Option[Profile] = profiles.find(_.ref == ref)

  /** What each role's calls are made under, for a turn starting now. */
  def pin: TurnProfile = TurnProfile.of(pins(policy.turn), pins(policy.summary), pins(policy.query))

  /** What the closing of a period grit only heard is written under: the policy's `heard`
    * role. No turn's call, so no [[TurnProfile]] holds it.
    */
  def heard: Pins = pins(policy.heard)

  private def pins(ladder: Ladder): Pins = {
    def pinned(r: Rung) =
      Pinned(version, r.grade, r.assignment, Settings.of(profile(r.assignment.ref)))
    Pins(pinned(ladder.first), ladder.rest.map(pinned))
  }

  /** This catalog under `policy` instead. */
  def withPolicy(policy: Policy): Catalog = Catalog.of(policy, profiles)

  /** This catalog with `more` laid over its profiles ([[Profile.overlaid]]). */
  def overlaid(more: Vector[Profile]): Catalog = Catalog.of(policy, profiles ++ more)
}

object Catalog {

  /** `profiles` merged to one per pair, the later laid over the earlier
    * ([[Profile.overlaid]]), kept in the order each pair first appears; versioned by
    * [[CatalogJson]]'s form of the result.
    */
  def of(policy: Policy, profiles: Vector[Profile]): Catalog = {
    val merged = profiles.foldLeft(Vector.empty[Profile]) { (acc, p) =>
      if (acc.exists(_.ref == p.ref)) acc.map(q => if (q.ref == p.ref) q.overlaid(p) else q)
      else acc :+ p
    }
    Catalog(policy, merged, CatalogVersion(Hash.of(CatalogJson.content(policy, merged))))
  }
}

/** What one rung of a role's calls in a turn are made under: the catalog it came from, the
  * rung's grade and assignment, and the settings its pair resolves to.
  */
final case class Pinned(
    version: CatalogVersion,
    grade: Grade,
    assignment: Assignment,
    settings: Settings
) derives CanEqual

/** One role's pins, in its ladder's order: [[Catalog.pin]] pins each rung. */
final case class Pins private[model] (first: Pinned, rest: Vector[Pinned]) derives CanEqual {
  def all: Vector[Pinned] = first +: rest
}

object Pins {

  /** `all` as a role's pins; `Left` when there are none. */
  def of(all: Vector[Pinned]): Either[String, Pins] = all match {
    case first +: rest => Right(Pins(first, rest))
    case _ => Left("a role has no pins")
  }

  /** One pin: a role assigned one pair. */
  def one(pinned: Pinned): Pins = Pins(pinned, Vector.empty)
}

/** A [[TurnProfile]]'s content hash. */
opaque type TurnProfileId = String

object TurnProfileId {
  def apply(hash: String): TurnProfileId = hash
  def value(id: TurnProfileId): String = id
}

/** What every role's calls in one turn are made under; identified by its content, so turns
  * under the same catalog and policy share one.
  */
final case class TurnProfile private (
    turn: Pins,
    summary: Pins,
    query: Pins,
    id: TurnProfileId
)

object TurnProfile {

  /** The three roles' pins, identified by [[CatalogJson]]'s form of them. */
  def of(turn: Pins, summary: Pins, query: Pins): TurnProfile =
    TurnProfile(
      turn,
      summary,
      query,
      TurnProfileId(Hash.of(CatalogJson.profilePins(turn, summary, query)))
    )
}

/** A short content hash of the compact JSON ([[grit.core.id.ShortHash]]). */
private object Hash {
  def of(content: ujson.Value): String = grit.core.id.ShortHash.of(ujson.write(content))
}
