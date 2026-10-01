package grit.models

import grit.core.model.{Assignment, Pinned, Policy, TurnProfile}

/** What a model is used for, and the environment variables that override its assignment in
  * the [[Policy]] for one run: the model, the output budget and the one upstream.
  */
enum ModelRole(val modelVar: String, val maxTokensVar: String, val upstreamVar: String) {

  /** Answers the user: the turn's own model call. */
  case Turn extends ModelRole("GRIT_MODEL", "GRIT_MAX_TOKENS", "GRIT_PROVIDER")

  /** Writes each turn's short summary. */
  case Summary
      extends ModelRole("GRIT_SUMMARY_MODEL", "GRIT_SUMMARY_MAX_TOKENS", "GRIT_SUMMARY_PROVIDER")

  /** Writes the search query retrieval ranks a turn's earlier entries by. */
  case Query extends ModelRole("GRIT_QUERY_MODEL", "GRIT_QUERY_MAX_TOKENS", "GRIT_QUERY_PROVIDER")

  /** This role's assignment in `policy`. */
  def in(policy: Policy): Assignment = this match {
    case Turn => policy.turn
    case Summary => policy.summary
    case Query => policy.query
  }

  /** This role's pin in `profile`. */
  def in(profile: TurnProfile): Pinned = this match {
    case Turn => profile.turn
    case Summary => profile.summary
    case Query => profile.query
  }
}
