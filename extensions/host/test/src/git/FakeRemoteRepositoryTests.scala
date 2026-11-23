package grit.host.git

import grit.core.host.{RemoteRepository, RemoteRepositoryContract}
import grit.host.git.GitFixture.FakeRemoteRepository

/** [[RemoteRepositoryContract]] over the fixture's [[FakeRemoteRepository]], so the fake the
  * branch's tests run against keeps what `GitHubRepository` keeps.
  */
object FakeRemoteRepositoryTests extends RemoteRepositoryContract {

  protected def fresh[A](body: Held^ => A): A = {
    val fake = new FakeRemoteRepository
    body(new Held {
      val repository: RemoteRepository^ = fake
      def close(branch: String, head: String): Unit = fake.close(branch, head)
      def merge(branch: String, head: String): Unit = fake.merge(branch, head)
      def opened: Int = fake.opens.size
    })
  }
}
