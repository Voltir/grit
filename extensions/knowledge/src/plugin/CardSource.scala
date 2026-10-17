package grit.knowledge.plugin

import grit.knowledge.drive.DriveCards
import grit.knowledge.github.GitHubCards

/** Where cards come from. */
enum CardSource extends caps.Pure {
  case GitHub(cards: GitHubCards)
  case Drive(cards: DriveCards)
}
