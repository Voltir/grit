package grit.outline.locate

import java.nio.charset.StandardCharsets

/** The words of each source file as last read, kept from one search to the next: a word is a whole run of word
  * characters (`isWordChar`). A file's words are scanned again whenever its bytes differ from
  * those last read, so a search over them answers as a scan of the file's text now would.
  */
final class SourceWords private (byFile: Map[os.Path, SourceWords.Scanned]) {

  /** `file`'s words now, its text now (decoded only when asked for), and these words with `file`'s replaced by them. */
  private[locate] def read(file: os.Path): (SourceWords.Words, () => String, SourceWords) = {
    val bytes = os.read.bytes(file)
    lazy val text = new String(bytes, StandardCharsets.UTF_8)
    byFile.get(file) match {
      case Some(known) if java.util.Arrays.equals(known.bytes, bytes) =>
        (known.words, () => text, this)
      case _ =>
        val words = SourceWords.Words.of(text)
        (
          words,
          () => text,
          new SourceWords(byFile.updated(file, SourceWords.Scanned(bytes, words)))
        )
    }
  }

  /** These words for `files` alone. */
  private[locate] def only(files: Set[os.Path]): SourceWords =
    new SourceWords(byFile.filter { case (p, _) => files.contains(p) })
}

object SourceWords {

  /** No file's words known. */
  val empty: SourceWords = new SourceWords(Map.empty)

  /** Whether `c` is a word character of a mention, as `\b` reads one: an ASCII letter, digit or `_`. A `$` is not, so
    * an interpolated `$Name` mentions `Name`; a glob of a definition's name ([[WordGlob]]) counts `$` as part of it.
    */
  private[locate] def isWordChar(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

  private final case class Scanned(bytes: Array[Byte], words: Words)

  /** The distinct words of one text, sorted. */
  private[locate] final class Words private (sorted: Array[String]) {

    /** Whether `word` is one of them. */
    def contains(word: String): Boolean =
      sorted.search(word) match {
        case scala.collection.Searching.Found(_) => true
        case _ => false
      }
  }

  private[locate] object Words {

    /** The words of `text`. */
    def of(text: String): Words = {
      val found = scala.collection.mutable.HashSet.empty[String]
      var i = 0
      while (i < text.length) {
        if (isWordChar(text.charAt(i))) {
          var j = i + 1
          while (j < text.length && isWordChar(text.charAt(j))) j += 1
          found += text.substring(i, j)
          i = j
        } else i += 1
      }
      new Words(found.toArray.sorted)
    }
  }
}
