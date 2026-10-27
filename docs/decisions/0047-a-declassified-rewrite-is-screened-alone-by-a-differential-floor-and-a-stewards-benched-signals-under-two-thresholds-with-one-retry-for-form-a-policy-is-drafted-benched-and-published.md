# 0047. A declassified rewrite is screened alone, by a differential floor and a steward's benched signals under two thresholds, with one retry for form; a policy is drafted, benched and published

Status: accepted (2026-10-26)

Context: a rewrite declassified under a compartment's policy crosses to readers outside the
compartment, so something must judge it before it does. Measured on labelled rewrites, turned
down: screening with the source and instructions beside the rewrite (clean and leak scores
overlapped); floor questions about notes, withheld matter or directives (fired on ordinary
documents, refused every control); an absolute floor (refused 8 of 8 faithful rewrites of
first-person or self-describing sources, which threads and an assistant's own messages are); a
floor asked of added lines only (5 of 7 cleans refused); one threshold of 0.5 (passed leaks at
0.46 and 0.48); an absent threshold below 0.4 (clean and leak overlap from 0.16 to 0.39, so it
trades real clean refusals for weak leaks about 1:1); the signals in the rewriter's prompt (the
leaks moved under the check); signals left unbenched as advice (one cold signal refused 15 of 15
clean texts, which only its bench showed).

Decision:

- **Screened alone.** The screen's state is the text alone. A fixed floor asks whether it
  refers to itself as a rewrite or its instructions, and whether it speaks as "I" about what
  it shares or leaves out. The floor is differential: a floor answer fails only when the
  source, asked the same, scores below the threshold (`Screened.fired`). The source is asked
  only when a rewrite's floor fires, once per declassify.
- **Signals are the steward's**, each a thing a reader must not infer (`absent`) or must find
  stated (`present`), read by `Checks.read` and written by [the guide](../declassification-signals.md).
  Two thresholds, core constants: `Screen.Infers` (0.4) for floor and absent answers,
  `Screen.States` (0.5) for present ones. No weights, no per-signal thresholds.
- **One retry, for form only** (`Declassifying.Again`): when only floor or present answers
  fired, the rewrite is asked once more, naming what fired, never a score. An absent failure
  is final. Retries of absent refusals passed while still leaking in 3 of 17 (Wilson
  0.06–0.41): a retry strips what the check named and rewords the inference below it.
- **Every version is benched** (`Bench`): the steward's accept and refuse examples, each
  screened by the signals alone (the floor needs a source to be differential). The verdict is
  kept under a `Declassified.Screening`, a digest of the checks, floor, templates and
  thresholds, so an edited version or a changed screen is unbenched until benched again;
  `Declassifying.onBench` is the one rule. A version unbenched or failed in force makes
  nothing new.
- **Drafted, benched, published.** A version is a draft or published. Each run benches the
  version in force and its latest draft (`DocumentKeeper.benchable`); the operator's publish
  updates nothing without a passing bench, and picks the mode. A compartment's first version
  has nothing in force to be benched beside, so it is inserted published, and rewrites nothing
  until its own bench passes.
- **Two meanings of current.** *In force*, for writing and rewriting: the newest published
  version with text, under its document's newest application; only one version ever is.
  *Stands*, for retention (`Declassified.Made.stands`): a rewrite under the version in force,
  or, after a `keep` publish, under an earlier one from `PolicyApplication.InForce.keptFrom` on,
  until the version in force rewrites or withholds its document. A `withdraw` publish, or a
  revocation, leaves only the in-force version's rewrites standing.

Consequences:

- First-person and self-describing sources declassify. **The floor's hole:** a writer's own
  voice passes when it matches a voice the source already has (4 of 8 planted leaks); the real
  writer produced that voice once in 97 texts.
- **Fidelity is the known gap.** A rewrite that invents a fact passes unless a signal names
  it: the policy's stand-in is an ordered untruth, so "untrue or disputed" cannot be core.
- A bench catches only the leaks the steward imagined. It does catch a badly phrased signal
  before it rules: on its first real bench, a signal phrased as an exclusion ("a note to its
  readers …; X is not such a statement") refused the accept example at 0.56, and the steward
  dropped it before publishing.
- The shipped screen agreed with the measured one: 68 of 68 labelled verdicts matched, one rep
  of 136 flipped at the threshold (0.44 to 0.37), and no answer moved more than 0.09.
- A policy's signal and example text names the secret, so it is read only inside the
  screening, rewrite and bench steps; what leaves a step is answers and positions.
- Enforced by the compiler (private constructors, `private[core]` checks), probes
  (`DeclassifyCaptureTests`), the replay gate over job histories, and `ScreenTests`.
