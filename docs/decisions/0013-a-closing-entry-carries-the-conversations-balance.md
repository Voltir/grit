# 0013. A closing entry carries the conversation's balance, and closings have a retention window

Status: accepted (2026-09-26)

Context: under ADR 0011 a closing entry covers only its own period, and the next period
opens from the last few of them. Running it for real showed three problems:

- What the window shows depends on how recent a period is. An item opened three
  closes ago and never mentioned again drops out while still open.
- Topic state lived in raw entries and was lost when they were purged.
- A closing written from earlier closings compounds loss. One recorded "no backup
  frequency was recorded" as a fact after an earlier close dropped the detail.

Closing entries were also kept forever, so storage grew without bound. The alternatives
were:

- a cumulative summary rewritten at each close, which compounds loss faster;
- a tiered rollup of closings (daily, weekly, monthly), which re-summarises at every tier;
- keeping the last K closings and accepting the drop-out.

Decision:

- **A closing entry has two parts.** Its *flows* record what happened in its own period,
  as before. Its *balance* is the conversation's state after that period: open items,
  standing decisions and topics. Each balance line is keyed and records the period that
  added it and the last period that touched it. The next period opens from the previous
  closing's balance and flows.
- **The balance changes only by edits.** The closing writer is shown the previous balance
  as already known. It proposes edits (add, resolve, drop, touch) and never rewrites a
  line. A line no edit names is copied verbatim. What is absent is an open item, never
  a fact. A period with nothing new carries the balance forward with no model call.
- **The balance has a size cap.** When it is over the cap, the least recently touched
  lines are evicted, deterministically and with no model. The closing's flows list the
  evictions. An evicted line is moved, never rewritten: its text stays in the closing
  that added it. The cap is data, like the windows.
- **Closings have a retention window of their own**, longer than raw entries' and hard
  in the same way. The exception is the latest closing of a conversation, which is its
  whole opening state and stays while the conversation is alive. A conversation quiet for
  longer than the window is removed entirely.
- Distilling across conversations, and remembering beyond the window, is a plugin's job.
  A plugin writes its own documents with their sources named and never edits a balance.

Consequences:

- An open item stays in view until it is resolved or evicted, and topic state outlives
  the purge.
- A carried line reads the same in its fortieth period as in its first.
- Everything kept after a close is bounded: close rate × retention window × (cap + one
  period's flows), plus one closing per live conversation.
- A long open period can post a checkpoint the same way without sealing. The last
  checkpoint of a period is then its closing.
- What was evicted, and not taken by a plugin inside the window, is eventually
  forgotten. Nothing is kept forever except what a plugin chose to keep.
- ADR 0011's per-period closing entries and their last-K opening are superseded, and
  so is its rule that closing entries are kept until a rollup bounds them. Nothing
  enforces this yet.
