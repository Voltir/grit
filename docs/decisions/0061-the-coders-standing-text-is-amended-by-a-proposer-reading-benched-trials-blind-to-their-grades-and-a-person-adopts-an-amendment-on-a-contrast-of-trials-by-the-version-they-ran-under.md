# 0061. The coder's standing text is amended by a proposer reading benched trials blind to their grades, and a person adopts an amendment on a contrast of trials by the version they ran under

Status: accepted (2026-11-10)

Context: how a grit coder works is a standing text, a section of an artifact a deployment keeps
under the projects method (ADR 0049), given to `grit work` as its first message. Improving it by
hand reads every trial's transcript and remembers nothing between rounds; letting a model edit it
directly makes the coder's rule whatever the last run said. Turned down: the proposer editing the
standing text (a change nobody judged becomes every coder's rule); the proposer run by the
engine after each bench (spending unattended is the driver's policy, ADR 0058); showing the
proposer the trials' grades (it would write to the grader, and a grade shape in its evidence is
how that leaks); summaries of trials in place of transcripts (a model call over each, and what the
coder was shown is the evidence); adopting on the proposer's word or a single trial (one run of
one task is noise).

Decision:

- **A trial is kept with the standing text it ran under.** The bench (`eval/bench/README.md`)
  keeps each trial of a frozen task, its transcript and its diff, and for the grit arm the
  standing text's sha256 (`bench_trials.standing`), the text itself a blob beside the transcript
  (`Standing`). Trials of one task are compared by that version, never by the file a run read.
- **A proposer is run by command, on chosen trials of one task.** `scripts/eval propose` writes
  an evidence checkout (`EvidenceCheckout.write`): the task blind to its grading (`BlindTask`: its
  base and brief, no check, commit or known diff), the standing text with the version of the
  section it is the text of (`Proposal.Against`), each chosen trial's transcript as its turn was
  shown it (`Bench.trajectory`, each part a turn showed before marked once,
  `Trajectory.deduplicated`, each tool result cut, `Trajectory.within`), and every amendment
  proposed before with what became of it. The text it is said (`Proposal.Said`) points at them.
- **The evidence is screened for the bench's grade shapes.** Every file the checkout is to hold
  is built through `Shown.of`, which refuses the whole checkout, naming each file and shape
  (`GradeFound`), when any holds one; nothing is written beside it.
- **The proposer proposes and nothing more.** Its turns are run under `TurnCommands.Proposing`:
  offered one tool, `project_propose`, which keeps a new artifact of the forced kind in that
  kind's first state and refuses a key the project holds. Every shipped command stays a person's;
  no turn moves an artifact out of its first state. A kind the method does not declare is
  refused at the deployment (`ProjectsRefusal.NoKind`).
- **An amendment is a diff against a named version.** Its `## Diff` states the section version
  it is against, so a proposal made over a text since changed is seen as such, and an adopted
  one is applied to the version it read.
- **A person adopts.** Adoption is a new version of the standing text's section, kept by the
  person; the next bench runs under it, and its trials stand beside the earlier version's for
  the contrast that judges the amendment. What became of each amendment is written back where
  the next proposer reads it.

Consequences: the coder's rule changes only through a kept, versioned text, and every change
traces to the trials that argued for it and the trials that followed it. The proposer never sees
a grade, so it cannot write to the grader; it sees cut tool results, so a long run's evidence
stays about one size. Each proposer reads the amendments before it, so a turned-down idea is
not proposed afresh without new evidence. The person's pick is the gate for now, a start:
evidence-driven adoption over the same trials and versions is where this is meant to go.
Enforced by the harness's unit suites, `ProposalTests` (what each evidence file holds and the
said text's list) and `ShownTests` (a grade shape refuses the checkout).
