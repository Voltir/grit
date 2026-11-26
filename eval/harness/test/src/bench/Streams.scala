package grit.eval.harness.bench

/** Canned `stream-json` output of a Claude Code run, shaped as `claude -p --output-format
  * stream-json --verbose` writes it: an init message, each assistant message once a content
  * block (so a call's id repeats, its usage growing), tool results as user messages, a
  * `permission_denied` notice, and a result message.
  */
object Streams {

  /** Where a stream names its checkout, replaced by [[at]]. */
  val Checkout = "@CHECKOUT@"

  /** [[run]] with its checkout at `checkout`. */
  def at(checkout: String): Vector[String] = run.map(_.replace(Checkout, checkout))

  private def assistant(id: String, content: String, usage: String) =
    s"""{"type":"assistant","message":{"id":"$id","type":"message","role":"assistant","model":"claude-haiku-5-5-20261001","content":[$content],"stop_reason":"tool_use","usage":{$usage}},"parent_tool_use_id":null,"session_id":"5e55-a1"}"""

  private def result(id: String, content: String) =
    s"""{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"$id",$content}]},"parent_tool_use_id":null,"session_id":"5e55-a1"}"""

  /** Three calls: a read (its id on two lines, the second's usage the last), a command the rules
    * refuse, and an edit of `a.scala` that also runs a suite, runs the bare gate and commits,
    * fails a second edit, and runs the gate twice with flags alone; then a result stopped at its turn cap. 48 bytes of tool
    * results come before the edit, and the calls total 6,262 input tokens, of them 4,100 read
    * from the cache and 2,150 written to it, and 90 output in their events; the result's
    * `modelUsage` gives 500 output, the figure the run's usage is read from.
    */
  val run: Vector[String] = Vector(
    s"""{"type":"system","subtype":"init","cwd":"$Checkout","session_id":"5e55-a1","tools":["Bash","Read","Edit","Write","Glob","Grep","mcp__scout__show"],"mcp_servers":[{"name":"scout","status":"connected"}],"model":"claude-haiku-5-5-20261001","permissionMode":"default","apiKeySource":"apiKeyHelper"}""",
    assistant(
      "msg_01a",
      """{"type":"text","text":"Reading the file first."}""",
      """"input_tokens":3,"cache_creation_input_tokens":2000,"cache_read_input_tokens":0,"output_tokens":5"""
    ),
    assistant(
      "msg_01a",
      s"""{"type":"tool_use","id":"toolu_01","name":"Read","input":{"file_path":"$Checkout/a.scala"}}""",
      """"input_tokens":3,"cache_creation_input_tokens":2000,"cache_read_input_tokens":0,"output_tokens":40"""
    ),
    result("toolu_01", """"content":"object A\n""""),
    assistant(
      "msg_01b",
      """{"type":"tool_use","id":"toolu_02","name":"Bash","input":{"command":"curl example.com"}}""",
      """"input_tokens":5,"cache_creation_input_tokens":100,"cache_read_input_tokens":2000,"output_tokens":20"""
    ),
    """{"type":"system","subtype":"permission_denied","tool_name":"Bash","tool_use_id":"toolu_02","session_id":"5e55-a1"}""",
    result(
      "toolu_02",
      """"is_error":true,"content":[{"type":"text","text":"Permission to use Bash has been denied."}]"""
    ),
    assistant(
      "msg_01c",
      s"""{"type":"tool_use","id":"toolu_03","name":"Edit","input":{"file_path":"$Checkout/a.scala","old_string":"object A","new_string":"object A { val x = 1 }"}}""",
      """"input_tokens":4,"cache_creation_input_tokens":50,"cache_read_input_tokens":2100,"output_tokens":30"""
    ),
    result("toolu_03", s""""content":"The file $Checkout/a.scala has been updated.""""),
    assistant(
      "msg_01c",
      s"""{"type":"tool_use","id":"toolu_04","name":"Bash","input":{"command":"cd $Checkout && scripts/check grit.m 2>&1"}}""",
      """"input_tokens":4,"cache_creation_input_tokens":50,"cache_read_input_tokens":2100,"output_tokens":30"""
    ),
    result("toolu_04", """"content":"unit: 3 passed""""),
    assistant(
      "msg_01c",
      """{"type":"tool_use","id":"toolu_05","name":"Bash","input":{"command":"scripts/check && git commit -m \"m: x is 1\""}}""",
      """"input_tokens":4,"cache_creation_input_tokens":50,"cache_read_input_tokens":2100,"output_tokens":30"""
    ),
    result("toolu_05", """"content":"[main 1a2b3c4] m: x is 1""""),
    assistant(
      "msg_01c",
      s"""{"type":"tool_use","id":"toolu_06","name":"MultiEdit","input":{"file_path":"$Checkout/a.scala","edits":[{"old_string":"object B","new_string":"object C"}]}}""",
      """"input_tokens":4,"cache_creation_input_tokens":50,"cache_read_input_tokens":2100,"output_tokens":30"""
    ),
    result(
      "toolu_06",
      """"is_error":true,"content":"<tool_use_error>String to replace not found in file.</tool_use_error>""""
    ),
    assistant(
      "msg_01c",
      """{"type":"tool_use","id":"toolu_07","name":"Bash","input":{"command":"scripts/check --no-unused"}}""",
      """"input_tokens":4,"cache_creation_input_tokens":50,"cache_read_input_tokens":2100,"output_tokens":30"""
    ),
    result("toolu_07", """"content":"unit: 3 passed""""),
    assistant(
      "msg_01c",
      """{"type":"tool_use","id":"toolu_08","name":"Bash","input":{"command":"scripts/check --it-reached 2>&1 | tail -3"}}""",
      """"input_tokens":4,"cache_creation_input_tokens":50,"cache_read_input_tokens":2100,"output_tokens":30"""
    ),
    result("toolu_08", """"content":"it: 1 passed""""),
    """{"type":"result","subtype":"error_max_turns","is_error":true,"duration_ms":12000,"duration_api_ms":9000,"num_turns":3,"session_id":"5e55-a1","total_cost_usd":0.0031,"usage":{"input_tokens":12,"cache_creation_input_tokens":2150,"cache_read_input_tokens":4100,"output_tokens":90},"modelUsage":{"claude-haiku-5-5":{"inputTokens":12,"outputTokens":500,"cacheReadInputTokens":4100,"cacheCreationInputTokens":2150,"costUSD":0.0031}},"permission_denials":[{"tool_name":"Bash","tool_use_id":"toolu_02","tool_input":{"command":"curl example.com"}}]}"""
  )
}
