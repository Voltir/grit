package grit.lifecycle.shadow

import grit.lifecycle.triage.{TriageQuestion, TriageQuestions}

/** What a shadow asks of a heard message, over the state triage's question is asked about
  * ([[grit.lifecycle.triage.TriageInput.read]]).
  */
enum ShadowQuestion {

  /** Triage's question in `wording`, its answers kept in the order asked
    * ([[grit.core.triage.ShadowAnswers.Worded]]).
    */
  case Worded(wording: TriageQuestion.Wording)

  /** `questions`, asking each per-source question of the knowledge sources covering the
    * place of the message's conversation (none when the conversation is not found), its
    * answers kept under their names ([[grit.core.triage.ShadowAnswers.Named]]).
    */
  case Named(questions: TriageQuestions)
}
