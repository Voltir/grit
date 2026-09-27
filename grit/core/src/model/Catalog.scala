package grit.core.model

/** How hard a reasoning model is asked to think, from least to most. */
enum Effort {
  case Minimal, Low, Medium, High, XHigh, Max
}

/** Which pair does one job, with its output budget (`maxTokens`, at least 1) and the effort
  * asked of it (`None`: none is asked, and the model's default holds).
  */
final case class Assignment(ref: ModelRef, maxTokens: Int, effort: Option[Effort])

/** Which pair does each job: answering the turn, summarising it, and writing its search
  * query. A person's decision; a probe proposes facts about pairs, never an assignment.
  */
final case class Policy(turn: Assignment, summary: Assignment, query: Assignment)

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
  def pin: TurnProfile = {
    def pinned(a: Assignment) = Pinned(version, a, Settings.of(profile(a.ref)))
    TurnProfile.of(pinned(policy.turn), pinned(policy.summary), pinned(policy.query))
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

/** What one role's calls in a turn are made under: the catalog it came from, the role's
  * assignment, and the settings its pair resolves to.
  */
final case class Pinned(version: CatalogVersion, assignment: Assignment, settings: Settings)

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
    turn: Pinned,
    summary: Pinned,
    query: Pinned,
    id: TurnProfileId
)

object TurnProfile {

  /** The three roles' pins, identified by [[CatalogJson]]'s form of them. */
  def of(turn: Pinned, summary: Pinned, query: Pinned): TurnProfile =
    TurnProfile(
      turn,
      summary,
      query,
      TurnProfileId(Hash.of(CatalogJson.pins(turn, summary, query)))
    )
}

/** A short content hash of the compact JSON ([[grit.core.id.ShortHash]]). */
private object Hash {
  def of(content: ujson.Value): String = grit.core.id.ShortHash.of(ujson.write(content))
}
