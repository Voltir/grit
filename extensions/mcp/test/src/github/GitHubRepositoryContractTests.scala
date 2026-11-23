package grit.mcp.github
import grit.core.host.{RemoteRepository, RemoteRepositoryContract}

import utest.*

/** [[RemoteRepositoryContract]] over [[GitHubRepository]] and [[FakeGitHubServer]]. */
object GitHubRepositoryContractTests extends RemoteRepositoryContract {

  protected def fresh[A](body: Held^ => A): A = {
    val fake = FakeGitHubServer.start()
    try
      body(new Held {
        val repository: RemoteRepository^ = fake.repository()
        def close(branch: String, head: String): Unit = fake.close(branch, head)
        def merge(branch: String, head: String): Unit = fake.merge(branch, head)
        def opened: Int = fake.opened
      })
    finally fake.stop()
  }
}
