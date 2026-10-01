package grit.mcp.client

import grit.core.edge.{EdgeRefusal, Variable}

/** A credential; never shown. */
final class Bearer private (private[client] val value: String) {
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
      case Some(v) if v.trim.isEmpty => Left(EdgeRefusal.Malformed(token, "it is blank"))
      case Some(v) if !v.forall(c => c >= 0x21 && c <= 0x7e) =>
        Left(
          EdgeRefusal.Malformed(
            token,
            "it holds a character that is not visible ASCII (a space or a line break?)"
          )
        )
      case Some(v) => Right(new Bearer(v))
    }
}
