package grit.knowledge.plugin

import grit.knowledge.github.GitHubCards

/** Where cards come from; Drive is a later case. */
enum CardSource extends caps.Pure {
  case GitHub(cards: GitHubCards)
}
