package grit.core.host

/** The blobs contract, kept by the test blobs. */
object TempBlobsTests extends BlobsContract {
  protected def fresh[A](body: Blobs^ => A): A = TempBlobs.around(body)
}
