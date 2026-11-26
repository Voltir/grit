package grit.kit.deployment

import java.nio.file.Path

import grit.core.place.Repository

/** A repository's clone on this machine, at `at`, and the directory its working copies are made
  * under, `copies` ([[grit.host.git.WorkingCopies]]): worktrees of the clone, never a second one.
  * `copies/{name}` is grit's alone: a sweep deletes every directory there that holds no lock. Each
  * run's branch is cut from the clone's origin's default branch, fetched first, never from the
  * clone's HEAD, so the clone needs an origin. Declared for a repository a coding subagent works
  * in, it lets any turn believed at that type's `requires`, in a room that is not a direct
  * message's and whose classification the repository's label dominates, start a child that runs
  * there what the type's allowlist permits, as the user the deployment is served as.
  */
final case class RepositoryClone(repository: Repository, at: Path, copies: Path) extends caps.Pure
