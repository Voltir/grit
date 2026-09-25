package grit.models

/** What a model is used for. Each role names its own model, output budget and upstreams,
  * so housekeeping can run on a cheaper model than the one that answers.
  */
enum ModelRole(
    val modelVar: String,
    val maxTokensVar: String,
    val defaultMaxTokens: Int,
    val upstreamsVar: String,
    val strictVar: String
) {

  /** Answers the user: the turn's own model call. */
  case Turn
      extends ModelRole(
        "GRIT_MODEL",
        "GRIT_MAX_TOKENS",
        4096,
        "GRIT_PROVIDER",
        "GRIT_STRICT_TOOLS"
      )

  /** Writes each turn's short summary. Defaults to the turn's model; its budget leaves
    * room for a reasoning model to think before it writes.
    */
  case Summary
      extends ModelRole(
        "GRIT_SUMMARY_MODEL",
        "GRIT_SUMMARY_MAX_TOKENS",
        1024,
        "GRIT_SUMMARY_PROVIDER",
        "GRIT_SUMMARY_STRICT_TOOLS"
      )

  /** Writes the search query retrieval ranks a turn's earlier entries by. Defaults to the
    * turn's model, with the summary's budget.
    */
  case Query
      extends ModelRole(
        "GRIT_QUERY_MODEL",
        "GRIT_QUERY_MAX_TOKENS",
        1024,
        "GRIT_QUERY_PROVIDER",
        "GRIT_QUERY_STRICT_TOOLS"
      )
}
