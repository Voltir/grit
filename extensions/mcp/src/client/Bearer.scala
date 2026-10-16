package grit.mcp.client

import grit.core.edge.{EdgeRefusal, Variable}

/** A credential; never shown. */
// Read by grit.mcp's two HTTP clients alone: the MCP client and Drive's.
final class Bearer private (private[mcp] val value: String) {
  override def toString: String = "Bearer(<redacted>)"
}

object Bearer {

  /** `token`'s value in `env`: [[EdgeRefusal.Missing]] when it is unset, and
    * [[EdgeRefusal.Malformed]] when it is blank or holds a character that is not visible ASCII
    * (a space or a line break included), each naming the variable and never its value.
    */
  def of(env: Map[String, String], token: Variable): Either[EdgeRefusal, Bearer] =
    env.get(Variable.value(token)) match {
      case None => Left(EdgeRefusal.Missing(token))
      case Some(v) => read(v).left.map(EdgeRefusal.Malformed(token, _))
    }

  /** `value` as a bearer, or why not: it is blank, or holds a character that is not visible
    * ASCII; never quoting it.
    */
  private[client] def read(value: String): Either[String, Bearer] =
    if (value.trim.isEmpty) Left("it is blank")
    else if (!value.forall(c => c >= 0x21 && c <= 0x7e))
      Left("it holds a character that is not visible ASCII (a space or a line break?)")
    else Right(new Bearer(value))
}
