package grit.dbos.sql

import scala.util.Using

import grit.core.prompt.Voice
import grit.core.store.{StoreError, Tx, VoiceStore}

/** [[VoiceStore]] over the one row of `grit.voice`: a named voice by its key, or the person's
  * own words. `Invalid` when stored own words break [[Voice.of]]'s rules, as they can once
  * changed by hand.
  */
final class SqlVoiceStore extends VoiceStore {
  import SqlEntryStore.attempt

  def current()(using tx: Tx^): Either[StoreError, Voice] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement("SELECT kind, text FROM grit.voice")) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          if (!rs.next()) Right(Voice.Default)
          else {
            val text = rs.getString("text")
            rs.getString("kind") match {
              case "own" =>
                Voice.of(text).left.map(why => StoreError.Invalid(s"voice: $why"))
              case _ =>
                // A name another build set, which this one does not know: never a failed turn.
                Right(Voice.named(text).getOrElse(Voice.Default))
            }
          }
        }
      }
    }.flatten
  }

  def set(voice: Voice)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val (kind, text) = voice match {
      case n: Voice.Named => ("named", n.key)
      case Voice.Own(words) => ("own", words)
    }
    attempt {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.voice (kind, text) VALUES (?, ?)
            |ON CONFLICT (one) DO UPDATE SET kind = EXCLUDED.kind, text = EXCLUDED.text""".stripMargin
        )
      ) { ps =>
        ps.setString(1, kind)
        ps.setString(2, text)
        ps.executeUpdate()
        ()
      }
    }
  }
}
