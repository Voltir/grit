package grit.core.id

/** A SHA-256 digest, by which bytes are kept content-addressed: equal bytes, an equal digest. */
opaque type Sha256 = String

object Sha256 {

  /** The digest written as `hex`: 64 lowercase hex digits; `None` for any other text. */
  def read(hex: String): Option[Sha256] =
    Option.when(
      hex.length == 64 && hex.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
    )(
      hex
    )

  /** The digest whose 32 bytes are `digest`; `None` for any other length. */
  def of(digest: Vector[Byte]): Option[Sha256] =
    Option.when(digest.length == 32)(digest.map(b => f"$b%02x").mkString)

  /** The digest of `bytes`. */
  def digest(bytes: Vector[Byte]): Sha256 = {
    val md = java.security.MessageDigest.getInstance("SHA-256")
    bytes.foreach(md.update)
    md.digest().map(b => f"$b%02x").mkString
  }

  extension (s: Sha256) {

    /** As 64 lowercase hex digits, as [[read]] reads it. */
    def hex: String = s

    /** Its 32 bytes. */
    def bytes: Vector[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toVector
  }

  given CanEqual[Sha256, Sha256] = CanEqual.derived
}
