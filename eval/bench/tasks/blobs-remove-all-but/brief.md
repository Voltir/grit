# Blobs removes every blob not named

**Folded:** "remove each blob not named" is in `FileArea.sweep`'s body and the bench's prune would copy it: a final `Blobs.removeAllBut(named: Set[Sha256]): Either[BlobError, Int]` ("every blob not in `named` removed, how many"), `FileArea.sweep` moved onto it in its own commit.

**core: Blobs removes every blob not named; FileArea's sweep moves onto it.** `BlobsContract` cases "removeAllBut removes every blob not named and keeps the named, counting them" and "a removal failing partway leaves the blobs removed before it removed and the rest kept" (over a wrapper whose second `remove` fails), red first; `FilesContract` green.
