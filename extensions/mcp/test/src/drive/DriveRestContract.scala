package grit.mcp.drive

import grit.core.identity.Email

import utest.*

/** What [[DriveRest]] needs of Drive's answers, and reads of them, for a service account shared
  * into shared drives. The recordings keep it in the unit tier ([[DriveRestRecordedTests]],
  * over [[FakeDriveServer]]).
  */
trait DriveRestContract extends TestSuite {

  /** Drive's API as the account reaches it. */
  protected def rest: DriveRest^

  /** Words some shared files hold. */
  protected def words: String

  /** A file at a shared drive's top, and that drive. */
  protected def topFile: (FileId, FileId)

  /** A file in a folder, and that folder. */
  protected def folderFile: (FileId, FileId)

  /** A Doc, a Sheet, a text file whose content starts with a byte-order mark, and a folder. */
  protected def doc: FileId
  protected def sheet: FileId
  protected def textFile: FileId
  protected def folder: FileId

  /** An id that names nothing shared with the account. */
  protected def missing: FileId

  /** The account's own address. */
  protected def self: Email

  val tests = Tests {
    test("a search finds files holding its words, each naming its drive, at most a page of them") {
      val hits =
        rest.search(words, None).fold(e => throw new java.lang.AssertionError(e.message), identity)
      (
        hits.files.nonEmpty,
        hits.files.size <= DriveRest.PageSize,
        hits.files.forall(_.drive.nonEmpty)
      ) ==>
        (true, true, true)
    }

    test("the shared drives the account is a member of are named, every hit's drive among them") {
      val named = rest.drives()
      val hits = rest.search(words, None).map(_.files.flatMap(_.drive).toSet)
      (
        named.map(_.names.values.forall(_.nonEmpty)),
        hits.flatMap(h => named.map(n => h.subsetOf(n.names.keySet)))
      ) ==> (Right(true), Right(true))
    }

    test("a shared drive's top file lies within its drive, and a folder's file within its folder") {
      (rest.file(topFile._1).map(_.within), rest.file(folderFile._1).map(_.within)) ==>
        (Right(Some(topFile._2)), Right(Some(folderFile._2)))
    }

    test(
      "a shared drive's and a folder's own files are listed, each lying directly within it and saying when it last changed"
    ) {
      def listed(in: FileId): Vector[DriveFile] =
        rest.children(in).fold(e => throw new java.lang.AssertionError(e.message), _.files)
      val top = listed(topFile._2)
      val inFolder = listed(folderFile._2)
      (
        top.forall(_.within.contains(topFile._2)),
        top.exists(_.id == topFile._1),
        inFolder.forall(_.within.contains(folderFile._2)),
        inFolder.exists(_.id == folderFile._1),
        (top ++ inFolder).forall(_.changed.nonEmpty),
        top.size <= DriveRest.ListMost
      ) ==> (true, true, true, true, true, true)
    }

    test("a file's grants are read whole, and name the account's own address") {
      val grants = rest.grants(doc)
      (
        grants.map(_.isDefined),
        grants.map(_.toVector.flatten.exists {
          case Grant.Person(email, _) => email == self
          case _ => false
        })
      ) ==> (Right(true), Right(true))
    }

    test(
      "a Doc, a Sheet and a text file are read as text with no byte-order mark; a folder is not read, in words"
    ) {
      def text(id: FileId) = rest.file(id).flatMap(rest.text)
      (
        text(doc).map(t => t.nonEmpty && !t.startsWith("\uFEFF")),
        text(sheet).map(t => t.contains(",") && !t.startsWith("\uFEFF")),
        text(textFile).map(t => t.nonEmpty && !t.startsWith("\uFEFF")),
        text(folder).left.map(_.message)
      ) ==> (
        Right(true),
        Right(true),
        Right(true),
        Left("Only Docs, Sheets and text files in Drive can be read here, not a folder.")
      )
    }

    test("an id naming nothing shared is hidden, its file and its grants alike") {
      (rest.file(missing), rest.grants(missing)) ==> (
        Left(DriveError.Hidden),
        Left(DriveError.Hidden)
      )
    }
  }
}
