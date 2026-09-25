package grit.core.tool

/** Whether a person approves each call of a tool before it runs. */
enum Gate[-A] {

  /** It runs without asking: a tool that only reads. */
  case Free

  /** Each call waits for a person's answer; `describe` is what they are shown of a call's
    * arguments. Pure, so the text approved is fixed before anything runs.
    */
  case Ask(describe: A -> String)
}
