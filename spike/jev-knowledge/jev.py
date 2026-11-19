"""Direct Jev (typesafe.ai System One) calls in grit's wire form (JevJson.request), with a hard
spend cap enforced from the API's reported input tokens across every run of this spike.

ask(tag, state, questions) -> (answers by q-id, usage). questions are grit wire dicts:
  {"type":"choice","instructions":..,"criteria":{key: desc}}
  {"type":"noul","instructions":..}
  {"type":"score","instructions":..,"criteria":[level, ...]}
Every call is appended to data/calls.jsonl (request and response: it holds text, so it stays
untracked); spend.json (tokens and cents per tag, no text) is the ledger the cap reads.
"""
import json, os, time, urllib.request, urllib.error, hashlib, threading

HERE = os.path.dirname(os.path.abspath(__file__))
ENV = "/home/nickchilders/Projects/kalandra/.env"
URL = "https://api.typesafe.ai/v1/systemone"
MODEL = "jev-latest"
USD_PER_M = 0.042
CAP_USD = 0.60
LEDGER = os.path.join(HERE, "spend.json")
LOG = os.path.join(HERE, "data", "calls.jsonl")
CACHE = os.path.join(HERE, "data", "cache.jsonl")
_lock = threading.Lock()


def _key():
    with open(ENV) as f:
        for line in f:
            if line.startswith("JEV_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")
    raise SystemExit("no JEV_API_KEY")


_KEY = None


def _ledger():
    try:
        with open(LEDGER) as f:
            return json.load(f)
    except FileNotFoundError:
        return {"tokens": 0, "calls": 0, "usd": 0.0, "by_tag": {}}


def _cache():
    c = {}
    if os.path.exists(CACHE):
        with open(CACHE) as f:
            for line in f:
                r = json.loads(line)
                c[r["h"]] = r
    return c


_CACHED = None


def spent():
    return _ledger()


class CapReached(Exception):
    pass


def ask(tag, state, questions, model=MODEL):
    """Answers keyed q1.. and the usage; served from the cache when the same request was made."""
    global _KEY, _CACHED
    body = {"model": model, "state": state,
            "questions": {f"q{i+1}": q for i, q in enumerate(questions)}}
    text = json.dumps(body, sort_keys=False)
    h = hashlib.sha256(text.encode()).hexdigest()
    with _lock:
        if _CACHED is None:
            _CACHED = _cache()
        if h in _CACHED:
            r = _CACHED[h]
            return r["answers"], r["usage"]
        led = _ledger()
        if led["usd"] >= CAP_USD:
            raise CapReached(f"cap reached: ${led['usd']:.4f}")
        if _KEY is None:
            _KEY = _key()
    req = urllib.request.Request(URL, data=text.encode(),
                                 headers={"Authorization": f"Bearer {_KEY}",
                                          "Content-Type": "application/json"})
    out, err = None, None
    for attempt in range(5):
        try:
            with urllib.request.urlopen(req, timeout=60) as r:
                out = json.loads(r.read())
            break
        except urllib.error.HTTPError as e:
            detail = e.read().decode(errors="replace")[:500]
            if (e.code == 429 or e.code >= 500) and attempt < 4:
                time.sleep(2 ** attempt)
                continue
            err = {"status": e.code, "detail": detail}
            break
        except (urllib.error.URLError, TimeoutError) as e:
            if attempt < 4:
                time.sleep(2 ** attempt)
                continue
            err = {"status": None, "detail": str(e)}
    with _lock:
        tok = (out or {}).get("usage", {}).get("input_tokens", 0)
        led = _ledger()
        led["tokens"] += tok
        led["calls"] += 1
        led["usd"] = led["tokens"] * USD_PER_M / 1e6
        t = led["by_tag"].setdefault(tag, {"tokens": 0, "calls": 0, "errors": 0})
        t["tokens"] += tok
        t["calls"] += 1
        t["errors"] += 1 if err else 0
        t["cents"] = round(t["tokens"] * USD_PER_M / 1e4, 4)
        led["cents"] = round(led["usd"] * 100, 4)
        with open(LEDGER, "w") as f:
            json.dump(led, f, indent=1, sort_keys=True)
        with open(LOG, "a") as f:
            f.write(json.dumps({"tag": tag, "h": h, "request": body, "response": out, "error": err}) + "\n")
        if err:
            return {"_error": err}, {"input_tokens": 0}
        r = {"h": h, "answers": out["answers"], "usage": out.get("usage", {}), "model": out.get("model")}
        _CACHED[h] = r
        with open(CACHE, "a") as f:
            f.write(json.dumps(r) + "\n")
        return out["answers"], out.get("usage", {})


def choice_probs(a):
    """A choice answer's (choice, {key: p})."""
    return a.get("choice"), {k: float(v) for k, v in (a.get("probabilities") or {}).items()}


def yes(a):
    return float(a["noul"])
