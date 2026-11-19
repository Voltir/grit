"""Read-only access to the snapshot database named by SPIKE_DB, through docker exec."""
import json, os, subprocess

DB = os.environ["SPIKE_DB"]


def rows(sql):
    """Rows of `sql`, each a list of column strings (psql -At, unit-separator delimited)."""
    out = subprocess.run(
        ["docker", "exec", "-i", "-e", "PGOPTIONS=-c default_transaction_read_only=on",
         "grit-postgres-1", "psql", "-U", "grit", "-d", DB, "-At", "-F", "\x1f", "-R", "\x1e",
         "-v", "ON_ERROR_STOP=1", "-c", sql],
        capture_output=True, text=True, check=True).stdout
    out = out[:-1] if out.endswith("\n") else out
    return [r.split("\x1f") for r in out.split("\x1e") if r != ""]


def jrows(sql):
    """Rows of a one-column query returning json (json_agg / row_to_json)."""
    return [json.loads(r[0]) for r in rows(sql)]


def decode(s):
    """A JSON value nested in JSON strings, decoded until it is no longer a string."""
    v = s
    while isinstance(v, str):
        try:
            v = json.loads(v)
        except ValueError:
            return v
    return v
