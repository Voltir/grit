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
    * refuse, and an edit of `a.scala`; then a result stopped at its turn cap. 48 bytes of tool
    * results come before the edit, and the calls total 6,262 input tokens, of them 4,100 read
    * from the cache and 2,150 written to it, and 90 output.
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
    """{"type":"result","subtype":"error_max_turns","is_error":true,"duration_ms":12000,"duration_api_ms":9000,"num_turns":3,"session_id":"5e55-a1","total_cost_usd":0.0031,"usage":{"input_tokens":12,"cache_creation_input_tokens":2150,"cache_read_input_tokens":4100,"output_tokens":90},"permission_denials":[{"tool_name":"Bash","tool_use_id":"toolu_02","tool_input":{"command":"curl example.com"}}]}"""
  )
}
