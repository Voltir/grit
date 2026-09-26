# 0012. A quiet period closes when a classifier judges it finished, or lapses

Status: accepted (2026-09-26), revised (2026-09-26)

Context: ADR 0011 closed a period on a deadline that activity pushes out by an idle window
and an explicit signal (`/done`, a ✅) pulls in to a grace window. In practice people do
not say they are done, so nearly every period would lapse after a day, and a resolved close
would be rare. The alternatives: keep the signal and live with lapses; shorten the idle
window, which closes periods mid-thought; or judge when a quiet period is finished.

Decision:

- **No explicit signal.** Two windows run from a period's newest activity: a short
  *settle* window, after which the classifier is asked, once per quiet stretch, whether
  anyone is waiting on anything: the person, something else, or nobody; and the *idle*
  window, after which it lapses, as before.
- **A verdict of nobody at or above a threshold closes it at once**, resolved, with that
  probability as its confidence. Any other verdict leaves the idle deadline. New activity
  makes the verdict stale, and a new quiet stretch a new question.
- **Every verdict is kept**, weighed or not (a classifier that fails is a verdict too), so
  nothing is asked twice without new activity, and a period is asked a bounded number of
  times. Verdicts are the data the threshold is tuned on.
- The settle window, the threshold and the number of asks are data, like the idle window;
  a threshold of 1 turns judging off.
- When a period is due and when it is asked are defined once, over its activity and
  latest verdict, never mirrored in SQL.

Consequences:

- A person does nothing to close a period, and one nobody is waiting on closes within the
  settle window instead of a day.
- The question was first "is it finished?", with finished, waiting on the person, waiting
  on something else and unclear. A recap or a lookup has no goal to finish: those periods
  scored 0.66 finished and lapsed, with nothing left open. Asking whether anyone is
  waiting closes them as readily as a finished task. Verdicts of the two questions are not
  comparable, and none of the first were kept.
- Each quiet stretch costs at most one classifier call; a period of many stretches costs at
  most the ask budget.
- A wrong verdict of nobody closes a period early. Its next message opens the next
  period from the closing entry, as after any close.
- An edge's explicit signal (a ✅) could return later as activity that asks at once; it is
  not built.
