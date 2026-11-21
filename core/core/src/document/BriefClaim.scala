package grit.core.document

import grit.core.claim.ClaimRole
import grit.core.id.{ClaimHolder, DocKey, SessionId}
import grit.core.json.Fields

/** What a brief's data tells an edge of the claim it is for, in core's terms: `subject`, its
  * claim's subject, by which the edge finds that claim; and `takeover`, how a person moves the
  * claim to another session, when its plugin names a way.
  */
final case class BriefClaim(subject: DocKey, takeover: Option[Takeover]) extends caps.Pure

object BriefClaim {

  /** What `brief`'s data says of its claim; `None` when it says nothing that reads. */
  def of(brief: Document): Option[BriefClaim] =
    (for {
      claim <- Fields("a brief's data", brief.data).obj(Claim)
      fields = Fields("a brief's claim", claim)
      subject <- fields.str(Subject).flatMap(DocKey.of)
      takeover <- fields
        .added(TakeoverField)
        .flatMap(Fields.opt(_) { t =>
          val read = Fields("a brief's takeover", t)
          for {
            command <- read.str(Command)
            key <- read.str(Key)
          } yield Takeover(command, key)
        })
    } yield BriefClaim(subject, takeover)).toOption

  /** `data` saying `c` of its brief's claim, as [[of]] reads it back. */
  def kept(data: ujson.Obj, c: BriefClaim): ujson.Obj = {
    val claim = ujson.Obj(Subject -> DocKey.value(c.subject))
    c.takeover.foreach(t => claim(TakeoverField) = ujson.Obj(Command -> t.command, Key -> t.key))
    ujson.Obj.from(data.value.toSeq :+ (Claim -> claim))
  }

  private val Claim = "claim"
  private val Subject = "subject"
  private val TakeoverField = "takeover"
  private val Command = "command"
  private val Key = "key"
}

/** How a person moves a claim to another session themselves: `command`, the words they run (a
  * deployment's command line and its plugin's command, `bin/kalandra project_claim`), given
  * `key`, the claim's subject as that command names it, and the arguments [[Takeover.said]]
  * writes.
  */
final case class Takeover(command: String, key: String) extends caps.Pure

object Takeover {

  /** The sentence telling a person how to move a claim, as `role` for `holder`, to `holder`'s
    * session `session`, `here` naming where (`here`, `to your session`): its command and its
    * arguments, `key`, `role`, `holder` and `takeover`, as one line of JSON.
    */
  def said(
      t: Takeover,
      role: ClaimRole,
      holder: ClaimHolder,
      session: SessionId,
      here: String
  ): String = {
    val args = ujson.Obj(
      "key" -> t.key,
      "role" -> ClaimRole.value(role),
      "holder" -> ClaimHolder.value(holder),
      "takeover" -> session.written
    )
    s"To move it $here now, the person who made the claim runs ${t.command} themselves, " +
      s"outside any session, with: ${args.render()}"
  }
}
