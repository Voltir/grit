package grit.formats.pdf

import java.io.ByteArrayOutputStream
import java.nio.file.Path

import grit.formats.choice.Fixtures

import org.apache.pdfbox.pdmodel.encryption.{AccessPermission, StandardProtectionPolicy}
import org.apache.pdfbox.pdmodel.font.{PDType1Font, Standard14Fonts}
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.{
  PDDocumentOutline,
  PDOutlineItem
}
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}

/** PDFs a test writes with PDFBox: pages of lines, bookmarks, and a password. */
object Pdfs {

  /** A line at `y` points from the page's foot, `size` points high. */
  final case class Line(text: String, y: Float, size: Float = 11f)

  /** A bookmark to `page` (from 1), at `level` (from 1). */
  final case class Mark(title: String, page: Int, level: Int = 1)

  /** A page's lines from the top, 14 points apart, from 720. */
  def lines(texts: String*): Vector[Line] =
    texts.toVector.zipWithIndex.map((t, i) => Line(t, 720f - 14f * i))

  /** How the file is locked: with a password to open it, or only marked not to be copied. */
  enum Lock { case Open, NoCopy }

  /** A PDF of `pages`, with `marks` as its bookmarks, written to a file named `name`. */
  def file(
      name: String,
      pages: Vector[Vector[Line]],
      marks: Vector[Mark] = Vector.empty,
      lock: Option[Lock] = None,
      drawn: Boolean = false
  ): Path = {
    val doc = new PDDocument()
    try {
      val font = new PDType1Font(Standard14Fonts.FontName.HELVETICA)
      val made = pages.map { lines =>
        val page = new PDPage()
        doc.addPage(page)
        val out = new PDPageContentStream(doc, page)
        try {
          if (drawn) { out.addRect(50f, 50f, 200f, 200f); out.fill() }
          lines.foreach { l =>
            out.beginText()
            out.setFont(font, l.size)
            out.newLineAtOffset(72f, l.y)
            out.showText(l.text)
            out.endText()
          }
        } finally out.close()
        page
      }
      if (marks.nonEmpty) {
        val outline = new PDDocumentOutline()
        doc.getDocumentCatalog.setDocumentOutline(outline)
        val parents = scala.collection.mutable.Map.empty[Int, PDOutlineItem]
        marks.foreach { m =>
          val item = new PDOutlineItem()
          item.setTitle(m.title)
          made.lift(m.page - 1).foreach(item.setDestination)
          parents.get(m.level - 1).fold(outline.addLast(item))(_.addLast(item))
          parents(m.level) = item
        }
      }
      lock.foreach { l =>
        val permission = new AccessPermission()
        permission.setCanExtractContent(false)
        val user = l match { case Lock.Open => "open-sesame"; case Lock.NoCopy => "" }
        val policy = new StandardProtectionPolicy("owner-secret", user, permission)
        policy.setEncryptionKeyLength(128)
        doc.protect(policy)
      }
      val bytes = new ByteArrayOutputStream()
      doc.save(bytes)
      Fixtures.file(name, bytes.toByteArray.toVector)
    } finally doc.close()
  }
}
