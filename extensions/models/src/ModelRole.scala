package grit.models

import grit.core.model.{Catalog, Grade, Ladder, Pins, Policy}

/** What a model is used for, and the environment variables that override its ladder in the
  * [[Policy]] for one run: for each grade, the model and the one upstream (its
  * [[Grade.Standard]] ones are the role's own, `modelVar` and `upstreamVar`), and the
  * standard rung's output budget.
  */
enum ModelRole(prefix: String) {

  /** Answers the user: the turn's own model call. */
  case Turn extends ModelRole("GRIT_")

  /** Writes each turn's short summary. */
  case Summary extends ModelRole("GRIT_SUMMARY_")

  /** Writes the search query retrieval ranks a turn's earlier entries by. */
  case Query extends ModelRole("GRIT_QUERY_")

  /** Writes the closing of a period grit only heard ([[Policy]]'s `heard`). */
  case Heard extends ModelRole("GRIT_HEARD_")

  /** Writes a dreaming job's summaries ([[Policy]]'s `dream`). */
  case Dream extends ModelRole("GRIT_DREAM_")

  /** `GRIT_MODEL`, `GRIT_SUMMARY_MODEL`, `GRIT_QUERY_MODEL`, `GRIT_HEARD_MODEL`,
    * `GRIT_DREAM_MODEL`.
    */
  def modelVar: String = s"${prefix}MODEL"

  /** `GRIT_MAX_TOKENS`, `GRIT_SUMMARY_MAX_TOKENS`, `GRIT_QUERY_MAX_TOKENS`,
    * `GRIT_HEARD_MAX_TOKENS`, `GRIT_DREAM_MAX_TOKENS`.
    */
  def maxTokensVar: String = s"${prefix}MAX_TOKENS"

  /** `GRIT_PROVIDER`, `GRIT_SUMMARY_PROVIDER`, `GRIT_QUERY_PROVIDER`, `GRIT_HEARD_PROVIDER`,
    * `GRIT_DREAM_PROVIDER`.
    */
  def upstreamVar: String = s"${prefix}PROVIDER"

  /** The variable naming this role's model at `grade`: `GRIT_STRONG_MODEL`,
    * `GRIT_SUMMARY_LIGHT_MODEL`; at [[Grade.Standard]], [[modelVar]].
    */
  def modelVar(grade: Grade): String = s"$prefix${graded(grade)}MODEL"

  /** The variable naming this role's one upstream at `grade`: `GRIT_STRONG_PROVIDER`; at
    * [[Grade.Standard]], [[upstreamVar]].
    */
  def upstreamVar(grade: Grade): String = s"$prefix${graded(grade)}PROVIDER"

  private def graded(grade: Grade): String = grade match {
    case Grade.Standard => ""
    case Grade.Light => "LIGHT_"
    case Grade.Strong => "STRONG_"
  }

  /** This role's ladder in `policy`. */
  def in(policy: Policy): Ladder = this match {
    case Turn => policy.turn
    case Summary => policy.summary
    case Query => policy.query
    case Heard => policy.heard
    case Dream => policy.dream
  }

  /** This role's pins in `catalog`, for a call starting now. */
  def in(catalog: Catalog): Pins = this match {
    case Turn => catalog.pin.turn
    case Summary => catalog.pin.summary
    case Query => catalog.pin.query
    case Heard => catalog.heard
    case Dream => catalog.dream
  }
}
