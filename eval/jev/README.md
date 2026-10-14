# Jev experiments

Scripts that ask Jev, grit's classifier, real questions about labelled text, to decide what a
question's wording and thresholds should be before grit relies on them. They are experiment
harnesses, not tests: nothing here runs in `scripts/check`, and nothing is unit tested. Each
folder is one experiment, runnable with `python3 -I`, its results written under `results/`
(gitignored) or wherever its arguments say.

- **`task-calibration/`**: whether a turn's root is hard, and what kind it is (seven kinds),
  over hand-labelled synthetic roots (`labels.jsonl`, `followup_labels.jsonl`, built from the
  `corpus_*.py` files). `FINDINGS.md` holds what it measured and the thresholds `Router` took.
- **`integrity-features/`**: whether text read from a source addresses its reader with
  directives, claims an identity, and so on, over injected and benign spans. `fetch.sh` fetches
  the third-party text it is built from into `data/` (gitignored); the corpus is built from it.

Every run reads the key from `JEV_API_KEY` alone and never prints it, sends a generic
User-Agent and nothing that identifies whoever runs it, and stops itself before a call that
would cross its `--budget-cents`.
