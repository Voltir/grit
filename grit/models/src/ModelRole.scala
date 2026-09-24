package grit.models

/** What a model is used for. Each role names its own model and output budget, so
  * housekeeping can run on a cheaper model than the one that answers.
  */
enum ModelRole(val modelVar: String, val maxTokensVar: String, val defaultMaxTokens: Int) {

  /** Answers the user: the turn's own model call. */
  case Turn extends ModelRole("GRIT_MODEL", "GRIT_MAX_TOKENS", 4096)

  /** Writes each turn's short summary. Defaults to the turn's model; its budget leaves
    * room for a reasoning model to think before it writes.
    */
  case Summary extends ModelRole("GRIT_SUMMARY_MODEL", "GRIT_SUMMARY_MAX_TOKENS", 1024)
}
